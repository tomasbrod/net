unit DHT;
{
 implementation of custom dht, based on pastry and kademlia.
 keyspace is divided into buckets of limited capacity
 node belongs to bucket, where at least 'depth' bits match 'prefix'
 node priority: old>new>dead
 TODO: weight nodes by IP-Address common prefix length.
}

{used by: messages, fileshare}

INTERFACE
uses Classes,sysutils,ObjectModel,HostKey,Crypto,ServerLoop,opcode,inifiles;

TYPE
  tPID=tKey20;
  tPeer_ptr=^tPeer;
  tBucket_ptr=^tBucket;

tPeer=object
    ID   :tPID;
    Addr :tNetAddr;
    ReqDelta:word;
    LastMsg,
    LastReply,
    VerifiedTill    :tMtime;
    function Assigned: boolean;
    procedure Clear;
    function FullyValid: boolean;
    private
    Challenge:tEccKey;
    end;

tBucket=object
    Prefix: tPID;
    Depth:  byte;
    ModifyTime: tMTime;
    peer:   array [1..6] of tPeer;
    next: ^tBucket;
    function MatchPrefix(const tp:tPID):boolean;
    function IDString:string;
    private
    desperate:word;
    procedure Refresh;
    end;

tSearchNode=record
    ID   :tPID;
    Addr :tNetAddr;
    LastReq:tMTime;
    reqc:byte;{number of requests}
    rplc:byte;{1=replied with cap, 2=replied with peers}
    end;

tCustomSearch=class(tTask)
    Target:tPID;
    TrID:Word;
    Query:tCustomMemoryStream;
    Nodes:array [0..10] of tSearchNode;
    constructor Create;
    protected
    procedure Cleanup; override;
    function  AddNode(const iID:tPID; const iAddr:tNetAddr) :integer; virtual;
    procedure AddNodes(s:tStream);
    procedure LoadNodes; {from dht and node cache} virtual;
    procedure Step;
    procedure HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; st:tStream); virtual;
    private
    procedure IntHandleReply(msg:tSMsg);
    end;
tSearch=class(tCustomSearch)
    constructor Create( const iTarget: tPID );
    end;

var MyID: tPID;

procedure NodeBootstrap(const contact:tNetAddr);
function CheckNode(const id: tPID; const addr: tNetAddr; recv:boolean): boolean;
function FindBucket(const prefix:tPID):tBucket_ptr; overload;
function GetFirstBucket:tBucket_ptr;

procedure GetNodes(r:tStream; const Target: tPID; max: word);
procedure SendNodes(const rcpt: tNetAddr; const Target: tPID; TrID:word);

IMPLEMENTATION

const
  crdRecvd=0;           {ReqDelta set on message reception}
  crdDoPingThr=1;       {ping often when rd>this}
  crdDontPingThr=4;     {ping less often whien rd>this}
  crdVerifyError=5;     {set on verify error}
  crdOthersCanReAddThr=6;{peer reinitialized when rd>this and suggested by other peer}
  crdInvalidateThr=4;   {request verify when rd>this}
  crdReplacableThr=2;   {new peer can replace old when rd>this}
  cBanDuration=10*60*1000;
  cVerifiedDuration=32*60*1000;
  cStichRar=7;
  cNudgeQuietThr=12*1000;
  cRefreshWaitBase=18*1000;
  cRefreshWaitMul=600;
  cRefreshWaitOther=30*1000;
  cRefreshWaitRtrDiv=3;
  cRecheckThr=cRefreshWaitOther;
  cNodesDat='nodes.dat';
  cMaxNodesDat=12;
  cInitAdd=6;           {n of peers to add from dht}
  cInitWait=800;        {? Init to Step delay}
  cAddWait=1;           {new peers to Step delay}
  cStepRqc=3;           {max requests per Step}
  cStepMinDelay=800;    {min delta of requests to same peer}
  cStepPeerReqc=6;      {max (unsuccessful) requests to peer}
  cStepRplc=6;          {? something per Step}
  cStepPeriod=900;      {max period between Steps}

var Table:^tBucket;
{deepest first}
var log:tEventLog;

function tBucket.MatchPrefix(const tp:tPID):boolean;
 begin
 result:=(depth=0)or(PrefixLength(prefix,tp)>=depth);
end;

function GetFirstBucket:tBucket_ptr;
  begin
  result:=Table;
end;

function tPeer.Assigned: boolean;
  begin result:=self.ReqDelta<255 end;
procedure tPeer.Clear;
  begin
  ReqDelta:=255;
  Addr.Clear;
  {rest is not needed}
  FillChar(ID,20,0);
  LastMsg:=0;
  LastReply:=0;
  VerifiedTill:=0;
end;

function tPeer.FullyValid: boolean;
  begin
  result:=
     (self.ReqDelta<crdDontPingThr)
  and(self.VerifiedTill>ServerLoop.mNow)
end;

procedure VerifyInit(b:tBucket_ptr; i:byte); forward;

function tBucket.IDString:string;
  var l:byte;
  begin
  l:=depth div 8;
  if (depth mod 8)>0 then inc(l);
  if l<1 then l:=1;
  SetLength(result,l*2);
  ToHex(@result[1],prefix,l);
  result:=result+'/'+IntToStr(depth);
end;

function FindBucket(const prefix:tPID):tBucket_ptr; overload;
 var cur:^tBucket;
 begin
 cur:=Table;
 result:=nil;
 while (cur<>nil) and (result=nil) do begin
  if cur^.MatchPrefix(prefix) {first matching is deepest}
   then result:=cur;
  cur:=cur^.next;
 end;
 assert(assigned(result) xor (Table=nil));
end;

(*** Mighty Split and Check procedures ***)

procedure SplitBucket(ob:tBucket_ptr);
 procedure Toggle(var prefix:tPID; bit:byte);
  begin
  prefix[bit div 8]:= prefix[bit div 8] xor ($80 shr (bit mod 8));
 end;
 var nb:tBucket_ptr;
 var i:byte;
 begin
 log.info(' SplitBucket %s/%d',[string(ob^.prefix),ob^.depth]);
 {find pref to old bucket, in order to unlink}
 if ob=Table then table:=table^.next else begin
 nb:=Table;
 while assigned(nb) and (nb^.next<>ob) do nb:=nb^.next;
 assert(assigned(nb),'old bucket not in table');
 {unlink}
 nb^.next:=nb^.next^.next; nb:=nil;
 end;
 {increase depth of this bucket}
 Inc(ob^.depth);
 ob^.ModifyTime:=mNow;
 {create new bucket with toggled bit}
 New(nb);
 nb^:=ob^;
 Toggle(nb^.Prefix,nb^.depth-1);
 nb^.next:=ob;
 {clear nodes that do not belong in bucket}
 for i:=1 to high(tBucket.peer) do begin
  if ob^.peer[i].ReqDelta>=255 then continue;
  if ob^.MatchPrefix(ob^.peer[i].id)
   then nb^.peer[i].Clear
   else ob^.peer[i].Clear;
 end;
 log.debug(' -> %s/%d',[string(ob^.prefix),ob^.depth]);
 for i:=1 to high(tBucket.peer) do if ob^.peer[i].ReqDelta<255
  then log.debug(' -> -> %s',[string(ob^.peer[i].id)]);
 log.debug(' -> %s/%d',[string(nb^.prefix),nb^.depth]);
 for i:=1 to high(tBucket.peer) do if nb^.peer[i].ReqDelta<255
  then log.debug(' -> -> %s',[string(nb^.peer[i].id)]);
 if table=nil then table:=nb else begin
  ob:=Table;
  while assigned(ob^.next)and (ob^.next^.depth<=nb^.depth) do ob:=ob^.next;
  ob^.next:=nb;
  log.debug(' -> after /%d',[ob^.depth]);
 end;
 Shedule(2000,@nb^.Refresh);
end;

function CheckNode(const id: tPID; const addr: tNetAddr; recv:boolean): boolean;
 {return false if node is banned}
  var b:^tBucket;
  var i,ifree,idup,iold:byte;
  var adm,idm:boolean;
  label again;
  begin
  Assert(not addr.isNil,'CheckNode with nil address');
  CheckNode:=false;
  if id=MyID then exit;
  again:
  b:=FindBucket(id);
  if not assigned(b) then begin
    New(Table); b:=Table;
    b^.Prefix:=MyID;
    b^.Depth:=0;
    b^.ModifyTime:=mNow;
    b^.next:=nil;
    b^.desperate:=3;
    for i:=1 to high(b^.peer) do b^.peer[i].Clear;
    Shedule(2000,@b^.Refresh);
  end;
  {order: update, free, banned, split, bad}
  ifree:=0;idup:=0;iold:=0;
  for i:=1 to high(b^.peer) do begin
    adm:=(b^.peer[i].Addr=addr);
    if b^.peer[i].ReqDelta<255 then begin
      {the peer slot is assigned}
      if adm or (b^.peer[i].ID=ID) then
        idup:=i
      else if b^.peer[i].ReqDelta>crdReplacableThr
        then iold:=i;
    end else begin
      if adm and ((b^.peer[i].LastReply+cBanDuration)>ServerLoop.MNow)
        then exit; {reject for recent auth failure}
      if ifree=0 then ifree:=i;
    end;
  end;
  if (idup>0) and ( (recv and (b^.peer[i].ReqDelta>crdReplacableThr))
      or(not recv and (b^.peer[i].ReqDelta>crdOthersCanReAddThr)) )
    then begin
    ifree:=idup; idup:=0
  end;
  if idup>0 then begin
    {updating}
    adm:=(b^.peer[i].Addr=addr);
    idm:=(b^.peer[i].ID=ID);
    if adm and idm then begin
      CheckNode:=true;
      if recv and ((b^.peer[idup].VerifiedTill > ServerLoop.MNow)
                     or (not PublicPoWReady)) then begin
        {only update by self and verified, else waiting for auth}
        b^.peer[idup].LastReply:=mNow;
        b^.peer[idup].ReqDelta:=0;
      end {else dont refresh by others};
      b^.peer[idup].LastMsg:=mNow;
    end else begin
      if (b^.peer[idup].VerifiedTill < (ServerLoop.MNow+cNudgeQuietThr)) then
        VerifyInit(b,idup);
    end;
  end else begin
    {inserting}
    if (ifree=0) and b^.MatchPrefix(MyID) then begin
      SplitBucket(b);
      goto again;
    end;
    if ifree>0 then i:=ifree
    else if iold>0 then i:=iold
    else exit;
    {add node here}
    log.info(' AddNode %s%s to %s#%d',[string(id),string(addr),b^.IDString,i]);
    b^.ModifyTime:=mNow;
    b^.peer[i].ID:=ID;
    b^.peer[i].Addr:=Addr;
    b^.peer[i].LastMsg:=mNow;
    b^.peer[i].LastReply:=0;
    if recv then b^.peer[i].LastReply:=mNow;
    b^.peer[I].ReqDelta:=0;
    b^.peer[I].VerifiedTill:=0;
    VerifyInit(b,i);
    CheckNode:=true;
  end
end;

procedure GetNodes(r:tStream; const Target: tPID; max: word);
  var i,ctrl:integer;
  var bucket:tBucket_ptr;
  begin
  ctrl:=0;
  bucket:=DHT.FindBucket(Target);
  while assigned(bucket) do begin
    //if r.WRBufLen<36 then break;
    for i:=1 to high(bucket^.peer) do if bucket^.peer[i].Assigned then begin
      r.Write(bucket^.peer[i].Addr,18);
      r.Write(bucket^.peer[i].ID,20);
    end;
    bucket:=bucket^.next;
    if (bucket=nil) and (ctrl=0) then begin
      bucket:=GetFirstBucket;
      ctrl:=1;
    end;
  end;
end;

procedure SendNodes(const rcpt: tNetAddr; const Target: tPID; trid:word);
  var r:tMemoryStream;
  begin
  r:=tMemoryStream.Create; try
  r.WriteByte(opcode.dhtNodes);
  r.Write(trid,2);
  r.Write(dht.MyID,20);
  GetNodes(r,target,(cDGramSz-23) div 36);
  ServerLoop.SendMessage(r.Memory^,r.Size,rcpt);
  finally r.Free end;
end;

(*** Protocol Communication ***)

procedure RecvBeatQ(msg:tSMsg);
  var s:tMemoryStream absolute msg.st;
  var sID:tPID;
  var Target:tPID;
  var mark:word;
  var r:tMemoryStream;
  var Hops:integer;
  begin
  s.skip(1);
  s.RB(sID,20);
  s.RB(Target,20);
  s.RB(mark,2);
  Hops:=s.R1-msg.TTL;
  //writeln('DHT.BeatQ: ',string(msg.source),' Request for ',string(Target^),' HC',Hops);
  Hops:=hops;{???}
  if not CheckNode(sID,msg.source,true) then exit;
  r:=tMemoryStream.Create; try
  r.W1(opcode.dhtBeatR);
  r.WB(MyID,20);
  r.WB(mark,2);
  r.W1(GetSocketTTL(msg.source));
  GetNodes(r,target,(cDGramSz-24) div 36);
  // todo!     if list.p^.addr=msg.source then continue;
  SendMessage(r.Memory^,r.size,msg.source);
  finally r.Free end;
end;

procedure SendBeat(const contact:tNetAddr; const forid: tPID; mark:word);
 var r:tMemoryStream;
 begin
 //writeln('DHT.SendBeat: to ',string(contact),' for ',string(forid));
 r:=tMemoryStream.Create; try
 r.W1(opcode.dhtBeatQ);
 r.WB(MyID,sizeof(tPID));
 r.WB(ForID,sizeof(tPID));
 r.WB(mark,2);
 r.W1(GetSocketTTL(contact));
 SendMessage(r.Memory^,r.size,contact);
 finally r.Free end;
end;

procedure RecvBeatR(msg:tSMsg);
  var ID:tPID;
  var Addr:tNetAddr;
  begin
  msg.st.skip(1);
  msg.st.RB(ID,20);
  msg.st.skip(3); {todo,ttl}
  //writeln('DHT.BeatR: ',string(msg.source),' is ',string(ID^));
  if not CheckNode(ID,msg.source,true) then exit;
  while msg.st.Left>36 do begin
    msg.st.RB(Addr,18);
    msg.st.RB(ID,20);
    CheckNode(ID,Addr,false);
  end;
end;

{Messages: (Still valid?)
 d)VfyCh: op, SendPub, PoWork, Challenge, Ver
 e)VfyRe: op, SendPub, PoWork, Respoonse, Ver
           1,      32,     36,        32, =101    +35    
}

procedure SendCheck(var p:tPeer);
  var r:tMemoryStream;
  begin
  r:=tMemoryStream.Create;
  r.W1(opcode.dhtCheckQ);
  assert(PublicPoWReady);
  r.WB(HostKey.PublicKey,sizeof(HostKey.PublicKey));
  r.WB(HostKey.PublicPoW,sizeof(HostKey.PublicPoW));
  r.WB(p.Challenge,sizeof(tEccKey));
  r.WB(VersionString[1],Length(VersionString));
  log.debug(' SendCheck: to %s size=%dB',[string(p.addr),r.Size]);
  SendMessage(r.Memory^,r.Size,p.Addr);
  r.Free;
end;

procedure RecvCheckQ(msg:tSMsg);
  var id:tPID;
  var pub:tEccKey;
  var pow:tPoWRec;
  var challenge:tEccKey;
  var right_resp:tEccKey;
  var r:tMemoryStream;
  begin
  msg.st.skip(1);
  msg.st.RB(pub,sizeof(tEccKey));
  msg.st.RB(pow,sizeof(tPoWRec));
  msg.st.RB(challenge,sizeof(tEccKey));
  {Pub->ID}
  SHA256_Buffer(id,20{<-},Pub,sizeof(pub));
  {CheckNode}
  if not CheckNode(id,msg.source,true) then exit;
  if PublicPoWReady then begin
    {Verify PoW}
    if not HostKey.VerifyPoW(pow,pub) then begin
      log.warn(' CheckQ: Invalid PoW in request from %s',[string(msg.source)]);
    exit end;
    {Solve C/R}
    HostKey.CreateResponse(Challenge, right_resp, pub);
    {reply}
    r:=tMemoryStream.Create;
    r.w1(opcode.dhtCheckR);
    r.wb(HostKey.PublicKey,sizeof(HostKey.PublicKey));
    r.wb(HostKey.PublicPoW,sizeof(HostKey.PublicPoW));
    r.wb(right_resp,sizeof(right_resp));
    r.wb(VersionString[1],Length(VersionString));
    //writeln('DHT.CheckQ: responding to ',string(msg.source),' ',r.length,'B');
    SendMessage(r.Memory^,r.Size,msg.source);
    r.Free;
  end else log.warn(' CheckQ: not responding to %s, PoW is not ready.',[string(msg.source)]);
end;

procedure RecvCheckR(msg:tSMsg);
  var b:^tBucket;
  var i:byte;
  var id:tPID;
  var pub:^tEccKey;
  var pow:^tPoWRec;
  var resp:^tEccKey;
  var right_resp:tEccKey;
  begin
  msg.st.skip(1);
  msg.st.RB(pub,sizeof(tEccKey));
  msg.st.RB(pow,sizeof(tPoWRec));
  msg.st.RB(resp,sizeof(tEccKey));
  {Pub->ID}
  SHA256_Buffer(id,sizeof(id),Pub^,sizeof(pub^));
  {ID->bkt:idx}
  b:=FindBucket(id);
  if not assigned(b) then exit;
  i:=1; while b^.peer[i].ID<>ID do begin
    inc(i); if i>high(b^.peer) then exit;
  end;
  {drop banned n unknown}
  //if b^.peer[i].Banned then exit;
  b^.peer[i].LastMsg:=mNow;
  b^.peer[i].LastReply:=mNow;
  {Verify PoW}
  if not HostKey.VerifyPoW(pow^,pub^) then begin
    b^.peer[i].ReqDelta:=255;
    log.warn(' CheckR: Invalid PoW in response from %s',[string(msg.source)]);
  exit end;
  {Verify C/R}
  HostKey.CreateResponse(b^.peer[i].Challenge, right_resp, pub^);
  if CompareByte(resp^,right_resp,sizeof(right_resp))<>0 then begin
    log.warn('DHT.CheckR: Invalid C/R in response from %s',[string(msg.source)]);
    b^.peer[i].ReqDelta:=255;
  exit end;
  {set node verified, rqd, last}
  b^.peer[i].VerifiedTill:=cVerifiedDuration;
  b^.peer[i].ReqDelta:=0;
  log.info(' CheckR: Valid response from %s',[string(msg.source)]);
end;

procedure RecvGetNodes(Msg:tSMsg);
  var trid:word;
  var sID:tPID;
  var Target:tKey20;
  begin
  msg.st.skip(1);
  msg.st.rb(trid,2);
  msg.st.RB(sID,20);
  if not DHT.CheckNode(sID, msg.source, true) then exit;
  msg.st.rb(Target,20);
  log.debug(' RecvGetNodes from %s',[string(msg.source)]);
  SendNodes(msg.source, target, trid);
end;

procedure NodeBootstrap(const contact:tNetAddr);
 begin
 SendBeat(contact,MyID,0);
 //SendBeat(contact,MyID,0); {xD}
end;

(*** The bucket Refresh procedure ***)

procedure tBucket.Refresh;
 var my,rtr,stich:boolean;
 var i,ol:byte;
 var wait:LongWord;
 var debug:ansistring;
 procedure lSend(var peer:tPeer; const trg:tPID);
  begin
  if (peer.VerifiedTill < (ServerLoop.MNow+cRecheckThr)) or (not PublicPoWReady)
  then SendBeat(peer.addr,trg,0)
  else SendCheck(peer);
  Inc(peer.ReqDelta);
 end;
 begin
 debug:='';debug:=debug;
 my:=MatchPrefix(MyID);
 ol:=0; rtr:=false;
 {1 of 10 times try to contact dead nodes in attempt to recover from network split}
 //debug:='DHT.Refresh('+self.IDString+')';
 stich:=Random(cStichRar)=0;
 for i:=1 to high(tBucket.peer)
  do if peer[i].ReqDelta<255 then begin
   if peer[i].ReqDelta>=crdDoPingThr then begin
    if (peer[i].ReqDelta<=crdDontPingThr) xor stich then begin
     {this will get rid of half-dead nodes}
     //writeln(debug,' R',peer[i].ReqDelta,' ',copy(string(peer[i].id),1,6),string(peer[i].addr));
     lSend(peer[i],prefix);{todo: use random target with prefix}
     rtr:=true;
    end
   end
   else if (ol=0) or (peer[i].LastReply<peer[ol].LastReply)
        then ol:=i;
 end;
 {now nudge the most quiet peer, but not too often}
 if (ol>0) and ((mNow-peer[ol].LastMsg)>cNudgeQuietThr) then begin
  //writeln(debug,' T',mNow-peer[ol].LastMsgFrom,' ',string(peer[ol].addr));
  lSend(peer[ol],MyID);
 end;
 {try to recover bucket full of bad nodes}
 if (ol=0){and(not rtr)} then begin
  {list.Init(Prefix); TODO
  list.bans:=true;
  list.maxRD:=desperate; list.Next;
  if assigned(list.bkt) then begin
   //writeln(debug,' V ',copy(string(list.p^.id),1,6),string(list.p^.addr));
   lSend(list.p^,prefix);
  end else} inc(desperate);
 end else desperate:=3;
 if my
  then wait:=cRefreshWaitBase+(depth*cRefreshWaitMul)
  else wait:=cRefreshWaitOther;
 if rtr and(not stich) then wait:=wait div cRefreshWaitRtrDiv;
 Shedule(wait,@Refresh);
end;
 
procedure VerifyInit(b:tBucket_ptr; i:byte);
  begin
  with b^.peer[i] do begin
    VerifiedTill:=0;
    //maybe inside the if?
    if PublicPoWReady then begin
      HostKey.CreateChallenge(challenge);
      SendCheck(b^.peer[i]);
    end;
end end;

(*** The Search Object ***)

constructor tCustomSearch.Create;
  var i:integer;
  begin
  for i:=high(nodes) downto 0 do Nodes[i].Addr.Clear;
  NewMsgTr(TrID, @IntHandleReply);
  tTask.Create;
  log.debug(' Lookup@%s: Initialize',[string(@self)]);
end;
  
constructor tSearch.Create( const iTarget: tPID );
  begin
  tCustomSearch.Create;
  Target:=iTarget;
  Query:=tMemoryStream.Create;
  with Query do begin
    //SetSize(43);
    WriteByte(opcode.dhtGetNodes);
    Write(TrID,2);
    Write(DHT.MyID,20);
    Write(Target,20);
  end;
  LoadNodes;
  Step;
end;

function tCustomSearch.AddNode(const iID:tPID; const iAddr:tNetAddr) :integer;
  var tpfl,idx,j:byte;
  begin
  assert(not iAddr.isNil,'AddNode with nil address');
  if iID=MyID then exit;
  idx:=0; result:=-1;
  tpfl:=PrefixLength(iid,Target);
  {write('dhtLookup.AddPeer@',string(@self),' tpfl=',tpfl,' addr=',string(iaddr));}
  for idx:=0 to high(nodes) do begin
    if nodes[idx].addr.isNil then break;
    if nodes[idx].addr=iaddr then begin
      result:=idx;
      {writeln(' update ',idx);}
    exit end;
    if PrefixLength(nodes[idx].id,Target)<tpfl then break;
  end;
  {writeln(' insert ',idx);}
  log.debug(' Lookup@%s: Add [%d] %s',[string(iAddr),string(@self),idx]);
  for j:=high(nodes)-1 downto idx do nodes[j+1]:=nodes[j];
  nodes[idx].id:=iid; nodes[idx].addr:=iaddr;
  nodes[idx].reqc:=0; nodes[idx].rplc:=0;
  result:=idx;
  {if assigned(OnProgress) then OnProgress(tpfl,nodes[idx]);}
end;

procedure tCustomSearch.Step;
  var ix:byte;
  var r:tMemoryStream;
  var rqc,rpc,again:byte;
  begin
  {send request to at most 3 peers,
   count nodes that replied
  }
  rqc:=0;rpc:=0; again:=0;
  repeat
  for ix:=0 to high(nodes) do begin
    if nodes[ix].addr.isNil then break;
    if (rqc>=cStepRqc)or(rpc>=cStepRplc) then break;
    if nodes[ix].rplc>=2 then inc(rpc)
    else if (nodes[ix].reqc<=cStepPeerReqc)
         and(rqc<cStepRqc)
         {and again...}
    then begin
        inc(rqc);
        if (mNow-nodes[ix].LastReq)<cStepMinDelay then continue;
        if nodes[ix].rplc=0 then begin
          ServerLoop.SendMessage(Query.Memory^,Query.Size,nodes[ix].Addr);
        end else begin
          r:=tmemorystream.create;
          //r.SetSize(43);
          r.WriteByte(opcode.dhtGetNodes);
          r.Write(self.TrID,2);
          r.Write(DHT.MyID,20);
          r.Write(Target,20);
          ServerLoop.SendMessage(R.memory^,R.size,nodes[ix].Addr);
          r.Free;
        end;
        log.debug('Lookup@%s: [%d,%d,%d] %s',[string(@self),ix,rqc,nodes[ix].reqc,string(nodes[ix].addr)]);
        inc(nodes[ix].reqc);
        nodes[ix].LastReq:=MNow;
    end;
  end; inc(again);
  until (again>1)or(rqc>=cStepRqc)or(rpc>=cStepRplc);
  if rqc=0 then begin
    log.debug('Lookup@%s: Exhausted',[string(@self)]);
    SendEvent(tevError,nil);
  end
  else Shedule(cStepPeriod,@Step);
end;

procedure tCustomSearch.IntHandleReply(msg:tSMsg);
  var sender:tPID;
  var op:byte;
  begin
  op:=msg.st.readbyte;
  msg.st.skip(2{trid});
  msg.st.rb(sender,20);
  if not DHT.CheckNode(sender, msg.source, true) then exit;
  HandleReply(msg.source,sender,op, msg.st);
end;

procedure tCustomSearch.AddNodes(s:tStream);
  var id:tPID;
  var ad:tNetAddr;
  var node:integer;
  begin
  log.debug(' dhtLookup.AddNodes stream offset %d',[s.position]);
  while s.left>=36 do begin
    s.rb(ad,18);
    s.rb(id,20);
    if DHT.CheckNode(id, ad, false) then continue; {is this needed?}
    node:=self.AddNode(id, ad);
    if node<>-1 then ;
  end;
  UnShedule(@Step);
  Shedule(cAddWait,@Step);
end;

procedure tCustomSearch.HandleReply(var sAddr: tNetAddr; var sPID: tPID; op:byte; st:tStream);
  var node:integer;
  begin
  node:=self.AddNode(sPID, sAddr);
  if {node=0} sPID=Target then begin
    log.debug(' Lookup@%s: Target Found',[string(@self)]);
    SendEvent(tevComplete,nil);
  exit end;
  if OP=opcode.dhtNodes then begin
    log.debug(' Lookup@%s: Nodes from %s',[string(@self),string(sAddr)]);
    if node<>-1 then nodes[node].rplc:=2;
    self.AddNodes(st);
  end else begin
    log.warn('Lookup@%s: Unknown from %s',[string(@self),string(sAddr)]);
    {not expecting any values}
    {warning, prehaps?}
  end;
end;

procedure tCustomSearch.Cleanup;
  begin
  log.debug('Lookup@%s: Cleanup',[string(@self)]);
  Query.Free;
  SetMsgTr(TrID, nil);
  UnShedule(@Step); {this is very important}
end;

procedure tCustomSearch.LoadNodes;
  var bucket:tBucket_ptr;
  var i,n:integer;
  var ctrl:byte;
  begin
  log.debug('Lookup@%s: Load Nodes',[string(@self)]);
  ctrl:=0;
  bucket:=DHT.FindBucket(Target);
  while assigned(bucket) do begin
    for i:=1 to high(bucket^.peer) do begin
      if bucket^.peer[i].Addr.isNil then break;
      n:=AddNode(bucket^.peer[i].ID,bucket^.peer[i].Addr);
      if n>cInitAdd then exit;
    end;
    bucket:=bucket^.next;
    if (bucket=nil) and (ctrl=0) then begin
      {not enough nodes in best bucket, add whole dht,
      this is not fast operation optimize?}
      bucket:=GetFirstBucket;
      ctrl:=1;
    end;
  end;
end;

(*** Persistent data Manager ***)

type tPersist=object
  statef:tFileStream;
  bootf:tStringList;
  readcnt:longword;
  booti:integer;
  procedure OpenState;
  procedure ReadState;
  procedure ReadBS;
  procedure SaveState;
  end;
var Persist:tPersist;

procedure tPersist.OpenState;
  begin
  readcnt:=0;
  booti:=0;
  bootf:=tStringList.Create;
  ServerLoop.Config.ReadSectionValues('bootstrap',bootf,[svoIncludeInvalid]);
  try
    statef:=tFileStream.Create(cNodesDat,fmOpenRead);
    shedule(1,@ReadState);
  except
    on e:eInOutError do begin
      log.error(' Boot: Cannot open %s %s',[cNodesDat,e.Message]);
      readcnt:=0;
      shedule(1,@ReadBS);
  end end;
end;
procedure tPersist.ReadState;
  var addr:tNetAddr;
  begin
  try
    if statef.Left=0 then raise eReadPastEoF.Create('eof'); //temporary fix
    statef.Read(addr,sizeof(addr));
    //writeln('DHT.ReadState: ',string(addr));
    NodeBootstrap(addr);
    shedule(200,@ReadState);
    inc(readcnt);
  except on e: eReadPastEoF do begin
    //writeln('DHT.Boot: Pinged ',readcnt,' nodes from '+cNodesDat);
    statef.Free;
    readcnt:=0;
    shedule(1,@ReadBS);
  end end;
end;
procedure tPersist.ReadBS;
  var addr:tNetAddr;
  begin
  if booti>=bootf.count then begin
    shedule(5000,@SaveState);
    //writeln('DHT.Boot: Pinged ',readcnt,' nodes from [bootstrap]');
    bootf.Free;
  end else begin
    addr.FromString(bootf[booti]);
    inc(booti);
    //writeln('DHT.ReadBoot: ',string(addr));
    NodeBootstrap(addr);
    inc(readcnt);
    shedule(300,@ReadBS);
  end;
end;
procedure tPersist.SaveState;
  var bkt:^tBucket;
  var p:integer;
  var cntr:word;
  begin
  //writeln('DHT.SaveState');
  statef:=tfilestream.create(cNodesDat,fmOpenWrite);
  FileTruncate(statef.handle,0);
  bkt:=Table;
  cntr:=0;
  while assigned(bkt) and (cntr<cMaxNodesDat) do begin
    for p:=1 to high(bkt^.peer) do begin
      if bkt^.peer[p].Addr.IsNil then continue;
      statef.Write(bkt^.peer[p].Addr,sizeof(tNetAddr));
      inc(cntr);
    end;
    bkt:=bkt^.next;
  end;
  statef.Free;
  shedule(61273,@SaveState);
end;

BEGIN
  ServerLoop.CreateLog(log,'DHT');
  assert((sizeof(tNetAddr)+sizeof(tPID))=38);
  SetupOpcode(opcode.dhtBeatQ,@recvBeatQ);
  SetupOpcode(opcode.dhtBeatR,@recvBeatR);
  SetupOpcode(opcode.dhtCheckQ,@recvCheckQ);
  SetupOpcode(opcode.dhtCheckR,@recvCheckR);
  SetupOpcode(opcode.dhtGetNodes,@recvGetNodes);
  SHA256_Buffer(MyID,sizeof(MyID),PublicKey,sizeof(PublicKey));
  log.info(' set ID to %S from HostKey',[string(MyID)]);
  Persist.OpenState;
END.
