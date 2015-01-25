unit Neighb experimental;

(*
 To store and search neighbours of peers
*)

INTERFACE

type tPID=object(tHash);

var PersonID:tPID;
var MachineID:tPID;

type tInfo=object
 {procedure GoAll; (*sorted by key*)}
 procedure GoPID(apid: tPID); (*search for a key, hop count ascending*)
 procedure GoAddr(aaddr: netaddr.t); (*search for a address*) unimplemented;
 function  Next:boolean; (*nove to next (or first)*)
 public {Informations}
 pid:tPID;
 hop:word;
 addr:netaddr.t;
 procedure PeerInfo(out info:Peers.tInfo);
 private
 pr,pn:pointer;
 byaddr:boolean;
end;

procedure NotifyPeerState( event: byte; info:Peers.tInfo );

type tNeighb=object(tPacket)
 pid:tPID;
 hop:word2;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(apid: tPID);
end;

IMPLEMENTATION

type
 tNodeWithPID=class(tDLNode)
  pid:tPID;
  function Search(const apid:tPID):tPidList;
 end;
 tNeighbList=class(tDLNode)
  hop:word;
  pid:tPID;
  addr:netaddr.t;
  pidl:tByPidList; {for unlinking}
  h1list:tByHop1List; {ditto}
 end;
 tByPIDList=class(tNodeWithPID)
  n:tNeighbList{root};
 end;
 tByHop1List=class(tDLNode)
  n:tNeighbList;
 end;

var Addrs:tNeighbList;
var PIDs :tByPIDList;
var Hop1s:tByHop1List;

function tNodeWithPID.Search(const apid:tPID):tPidList;
 var cur:tPidList;
 begin
 cur:=tNodeWithPID(self.next);
 if assigned(cur) then
 repeat
  if cur.id=apid then begin
   if cur=self then break;
   {cur.Unlink;
   self.Insert(cur);}
   result:=cur;
   exit;
  end;
  cur:=tNodeWithPID(cur.Next);
 until cur=self; {becouse the list is circular}
 result:=nil;
end;

procedure tKeyInfo.GoPID(apid:tPID);
 var pi:tByPID;
 begin
 pid.Clear;
 hop:=0;
 addr.clear;
 pn:=nil;
 pi:=PIDs.Search(apid);
 if pr=nil then raise DataBase.eSearch.Create;
 pr:=pi.n;
end;

function tKeyInfo.Next:boolean;
 begin
 if byaddr then AbstractError;
 if not assigned(pn) then pn:=pr;
 pn:=tNeighbList(pn).Next;
 result:=assigned(pn) and (pn<>pr);
 if not result then exit;
 pid:=tNeighbList(pn).pid;
 hop:=tNeighbList(pn).hop;
 addr:=tNeighbList(pn).addr;
end;

procedure PeerInfo(out info:Peers.tInfo);
 begin
 Peers.Get(info,Addr);
end;
