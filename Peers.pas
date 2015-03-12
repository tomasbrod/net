unit Peers;

INTERFACE
uses NetAddr
    ,SysUtils
    ,EventLog
    ;

var Log:tEventLog;

const cAkafukaCooldown = 5000{ms} /MSecsPerDay;
const cMaxRetry = 0{times};
const cMaxDelta = 6000{ms} /MSecsPerDay; {10 minutes to ping? Heh!}
const cAkafukaPeriod = 10000{ms} /MSecsPerDay;
const cAkafukaUnknown  = 10;

{Generic packet}
type
 tPktype =byte;
 tPacket = packed object
  { Base object for all packets }
  pktype :tPktype;
  procedure Create ( const itp :tPktype );
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t; Len:LongInt ); overload;
  procedure AfterProcessing( const from:netaddr.t);
 end;

{Packet structures}
const cAkafuka = 1;
const cAkafukaN ='Akafuka';
type tAkafuka =packed object(tPacket)
 {
  A Ping-Pong packet.
  Name is reference to Zidan (Dávid) Sufusky Sufurky
 }
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  procedure Create; overload;
 end;

const cFundeluka = 2;
const cFundelukaN = 'Fundeluka';
type tFundeluka= packed object(tPacket)
  procedure Handle( const from: NetAddr.t);
  procedure Send( const rcpt: NetAddr.t);
  procedure Create; overload;
 end;

{Peer list}
type tInfo= record
  Addr: NetAddr.t;
  Delta: tTime;
  Since: tDateTime;
 end;
procedure Get( out info:tInfo; const addr:netaddr.t ); overload;
procedure Get( out info:tInfo; var p:pointer ); overload;
procedure Add( addr :NetAddr.T );
{procedure GetList; unimplemented;}
var ConnectedCount:word unimplemented;
var ConnectedCountAccurate:boolean experimental;
 
var
 SendProc: procedure(const rcpt:netaddr.t; var Data; Len:LongInt);
 StateChangeProc :procedure( event: byte; info:tInfo );

procedure DoAkafuka;
{
 Send Akafuke to all peers.
 Remove not responding peers.
}

procedure SaveState;
procedure LoadState;

type {Exceptions}
 eNoAddress = class(Exception)
 {
  Exception signaling that recipient is not in list of peers.
  Thus no packet can be sent.
 }
  Addr: NetAddr.t;
  constructor Create( iAddr: NetAddr.t );
 end;
 eUnknownSender = class(Exception)
 {
  Exception signaling that sender is not in database
 }
  Addr : NetAddr.t; { id of erroring peer }
  constructor Create( iAddr : NetAddr.t );
 end;
 
procedure SelfTest;
 deprecated;

IMPLEMENTATION
uses 
     DataBase
    ,LinkedList
    ;

{ Data storage }

type tNodeWithAddress=class(LinkedList.tDLNode)
  addr:netaddr.t;
  function Search(iaddr:netaddr.t):tNodeWithAddress;
 end;

type tPeersList=class(tNodeWithAddress)
 delta:tTime;
 since:tDateTime;
 end;
var PeerList:tPeersList;

type tPendingList=class(tNodeWithAddress)
 ofpeer:tPeersList;
 since:tdatetime;
 retry:word;
 procedure CopyFrom(peer:tPeersList);
 procedure FreeSelfAndPeer;
 end;
var PendingList:tPendingList;

function tNodeWithAddress.Search(iaddr:netaddr.t):tNodeWithAddress;
 var cur:tNodeWithAddress;
 begin
 cur:=tNodeWithAddress(self.next);
 if assigned(cur) then
 repeat
  if cur.addr=iaddr then begin
   if cur=self then break;
   cur.unlink;
   self.Insert(cur); {DYNAMIC programming :)}
   result:=cur;
   exit;
  end;
  cur:=tNodeWithAddress(cur.Next);
 until cur=self; {becouse the list is circular}
 result:=nil;
end;

procedure tPendingList.CopyFrom(peer:tPeersList);
 begin
 inherited;
 ofpeer:=peer;
 addr:=peer.addr;
 retry:=0;
 since:=now;
end;

procedure tPendingList.FreeSelfAndPeer;
 begin
 if assigned(ofpeer) then ofpeer.free;
 free;
end;

{Private forward declarations}

procedure Add( addr :NetAddr.T; peer:tPeersList ); forward;

{*********************************
 *********** Addresses *********** 
 *********************************}

procedure tPacket.Handle( const from:netaddr.t );
 { do nothing }
 begin
 (*log.debug('Received #'+IntToStr(pktype)+' From '+String(from));*)
end;

{*********************************
 *********** Akafuka   ***********
 *********************************}

procedure tAkafuka.Send( const rcpt: NetAddr.t);
 { Add to pendging }
 begin
 (*log.debug('Sending Akafuka to '+string(rcpt));*)
 inherited Send( rcpt, sizeof(SELF) );
end;

procedure DispatchStateEvent(new,deleted:boolean; peer:tPeersList);
 var event:byte;
 var info:tInfo;
 begin
 (*log.debug('Peer '+string(from)+': delta='+FloatToStr(peer.delta*MSecsPerDay)+' retry='+IntToStr(pending.retry));*)
 if not assigned(StateChangeProc) then exit;
 info.addr:=peer.addr;
 info.delta:=peer.delta;
 info.since:=peer.since;
 event:=0;
 if new then event:=1;
 if deleted then event:=2;
 StateChangeProc(event,info);
end;

procedure tAkafuka.Handle( const from: NetAddr.t);
 { Send fundeluka. If not in roster then send akafuka. }
 var fundeluka:tFundeluka;
 var peer:tPeersList;
 begin
 inherited;
 ConnectedCountAccurate:=false;
 (*log.debug('Received '+cAkafukaN+' from '+string(from));*)
 fundeluka.Create;
 if {timesincelast>cAkafukaCooldown} true
  then fundeluka.Send(from);
 peer:=tPeersList(PeerList.Search(from));
 if not assigned(peer)
  then Peers.Add(from,nil);
end;

procedure tFundeluka.Handle( const from: NetAddr.t);
 { Move from pending to roster and update delta. }
 var isnew:boolean;
 var peer:tPeersList;
 var pending:tPendingList;
 begin
 inherited;
 ConnectedCountAccurate:=false;
 (*log.debug('Received '+cFundelukaN+' from '+string(from));*)
 pending:=tPendingList(PendingList.Search(from));
 if not assigned(pending) then assert(false);
 peer:=tPeersList(pending.ofpeer);
 isnew:= not assigned(peer);
 if isnew then begin peer:=tPeersList.Create ; peer.addr:=from; PeerList.Insert(peer) end;
 peer.delta:=now-pending.since;
 peer.since:=now;
 pending.free;
 DispatchStateEvent(isnew,false,peer);
end;

procedure DoAkafuka;
 { Retry pending. Check roster for old peers and send them akafuka. }
 var pending:tPendingList;
 var peer:tPeersList;
 var akafuka:tAkafuka;
 begin
 //log.debug('DoAkafuka');
 //log.debug('Retry pending');
 pending:=tPendingList(PendingList.Next);
 while pending<>PendingList do begin
  if (now-pending.since)>cMaxDelta then begin
   if pending.retry>cMaxRetry then begin
    if assigned(pending.ofpeer) then DispatchStateEvent(false,true,pending.ofpeer);
    log.debug('Peer '+string(pending.addr)+' dropped, not responding');
    pending.freeselfandpeer;
    pending:=tPendingList(PendingList.Next); continue; {because current is nil, so we cannot go to current.next}
   end else begin
    log.debug('Retry pending '+string(pending.addr));
    akafuka.create;
    akafuka.send(pending.addr);
    inc(pending.retry);
    pending.since:=now;
   end;
  end;
  pending:=tPendingList(pending.next);
 end;
 //log.debug('Akafuka old peers');
 peer:=tPeersList(PeerList.Next);
 ConnectedCount:=0;
 while peer<>PeerList do begin
  Inc(ConnectedCount);
  if (now-peer.since)>cAkafukaPeriod then begin
   //log.debug('Peer '+string(peer.addr)+' is too old ('+FloatToStr((now-peer.since)*MsecsPerDay)+'), akafuka');
   Peers.Add(peer.addr,peer);
  end;
  peer:=tPeersList(peer.next);
 end;
 ConnectedCountAccurate:=true;
 //log.debug('Akafuka complete');
end;
 
procedure Add( addr :NetAddr.T; peer:tPeersList );
 { add to pending, send akafuka }
 var pending:tPendingList;
 var akafuka:tAkafuka;
 begin
 ConnectedCountAccurate:=false;
 pending:=tPendingList(PendingList.Search(addr));
 if assigned(pending) then begin
  (*log.debug('Already pending');*)
  exit;
 end;
  pending:=tPendingList.Create;
  pending.addr:=addr;
  pending.retry:=0;
  pending.since:=now;
  pending.ofpeer:=peer;
  PendingList.Insert(pending);
  akafuka.create;
  akafuka.send(addr);
end;

procedure Add( addr :NetAddr.T );
 var live:tPeersList;
 begin
 log.debug('Try to add peer '+string(addr));
 if assigned(PeerList.Search(addr)) then exit;
 Add(addr,nil);
end;

procedure Get( out info:tInfo; const addr:netaddr.t);
 var peer:tPeersList;
 begin
 peer:=tPeersList(PeerList.Search(addr));
 info.addr:=peer.addr;
 info.delta:=peer.delta;
 info.since:=peer.since;
end;

procedure Get( out info:tInfo; var p:pointer ); overload;
 begin
 if not assigned(p) then p:=PeerList.Next else p:=tPeersList(p).Next;
 if p=pointer(PeerList) then begin p:=nil; exit; end;
 info.addr:=tPeersList(p).addr;
 info.delta:=tPeersList(p).delta;
 info.since:=tPeersList(p).since;
end;

{ *** Saving And Loading *** }

type tDiskPeerInfo=record
 addr:netaddr.t;
 end;

procedure SaveState;
 var f: file of tDiskPeerInfo;
 var v: tDiskPeerInfo;
 var p:tPeersList;
 var c:word=0;
 begin
 DataBase.dbAssign(f,'peers.dat');
 reset(f);
 truncate(f);
 p:=tPeersList(PeerList.next);
 while p<>PeerList do begin
  v.addr:=p.addr;
  write(f,v);
  p:=tPeersList(p.next);
  inc(c);
 end;
 close(f);
 log.info('Saved '+IntToStr(c)+' peers to presistent storage');
end;
 
procedure LoadState;
 var f: file of tDiskPeerInfo;
 var v: tDiskPeerInfo;
 var c:word=0;
 begin
 log.debug('Begin reading list of peers');
 {$I+}
 DataBase.dbAssign(f,'peers.dat');
 reset(f);
 while not eof(f) do begin
  read(f,v);
  Peers.Add(v.addr);
  inc(c);
 end;
 close(f);
 log.info('Restored '+IntToStr(c)+' peers from presistent storage');
 ConnectedCountAccurate:=false;
end;
 
{ *** Simple Uninteresting Bullshit ***}

procedure tFundeluka.Create;
begin
 inherited Create(cFundeluka);
end;

procedure tAkafuka.Create;
begin
 inherited Create(cAkafuka);
end;

procedure tFundeluka.Send( const rcpt:NetAddr.t );
begin
 (*log.debug('Sending '+cFundelukaN+' to '+string(rcpt));*)
 inherited Send( rcpt, sizeof(SELF) );
end;

procedure tPacket.Create ( const itp :tPktype );
 begin
 pktype := itp;
end;

procedure tPacket.AfterProcessing( const from: netaddr.t);
 begin
end;

procedure tPacket.Send( const rcpt:netaddr.t; Len:LongInt);
 var saddr,sid :string;
 begin
 rcpt.ToString(saddr);
 (*log.debug('Sending #'+IntToStr(pktype)+' to '+saddr+'');*)
 Assert( SendProc <> nil );
 SendProc( rcpt, self, Len );
end;

constructor eNoAddress.Create( iAddr : NetAddr.t );
var idstr:string;
begin
 iaddr.ToString(idstr);
 inherited Create( 'No Address associated to Peer '+idstr );
 addr:=iaddr;
end;

constructor eUnknownSender.Create( iAddr : NetAddr.t );
 begin
 inherited Create( 'Unknown sender :'+String(iAddr));
 Addr:=iAddr;
end;

procedure SelfTest;
 begin
 AbstractError;
end;

INITIALIZATION
 PeerList:=tPeersList.CreateRoot;
 PendingList:=tPendingList.CreateRoot;
FINALIZATION
 PendingList.Free;
 PeerList.Free;
END.
