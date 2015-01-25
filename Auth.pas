unit Auth deprecated;

(*

 Authentificate peers with publickey and provide access to peers by subkey 
 and masterkey id.
 
 No master and subkeys. Peer will auth with single masterkey. Subkey is 
 choosed by gpg. Person and Machine keys will be managed as two separate 
 auths. One address can auth with multiple keys.

*)

INTERFACE

type tPID=object(tHash);

var MasterID:tPID;
var SubID:tPID;

type tKeyInfo=object
 public {Listing}
 procedure GoAll;
 function  Next:boolean;
 public {By subkey}
 procedure Search(akey: tPID); (*search for a key*)
 public {Informations}
 pid:tPID;
 procedure Address(out addr:netaddr.t);
 procedure PeerInfo(out info:Peers.tInfo);
 private
 mp {:tMasterList} :pointer;
end;
 
procedure Translate(const pid:tPID; out addr:NetAddr.t);(*
 get address of online peer with the given subkey*)

var OnAuth=procedure (pid:tPID; addr: NetAddr.t);

type tAuthRequest=object(tPacket)
 asid:tPID;
 why: byte;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(asid: tPID);
end;

type tKeyFilePtr=record
 chk:Transfer.tFID;
 updated:tDateTime;
end;

IMPLEMENTATION

type
 tPIDList=class(tDLNode)
  id:tPID;
  function Search(const apid:tPID):tPidList;
 end;

 tKeyList=class(tPidList)
  Addr:netaddr.t;
  master:tMasterList;
  updated:tDateTime;
 end;

var MasterKeys:tKeyList;

function tPidList.Search(const apid:tPID):tPidList;
 var cur:tPidList;
 begin
 cur:=tPidList(self.next);
 if assigned(cur) then
 repeat
  if cur.id=apid then begin
   if cur=self then break;
   cur.unlink;
   self.Insert(cur); {DYNAMIC programming :)}
   result:=cur;
   exit;
  end;
  cur:=tPidList(cur.Next);
 until cur=self; {becouse the list is circular}
 result:=nil;
end;

procedure tKeyInfo.GoAll;
 begin
 pid.Clear;
 mp:=MasterKeys;
end;

function tKeyInfo.Next:boolean;
 begin
 assert(assigned(mp));
 mp:=tKeyList(mp).Next;
 result:=assigned(mp);
 if result then pid:=tMasterList(mp).id;
end;

procedure tKeyInfo.Search(akey: tPID);
 begin
 mp:=MasterKeys.Search(key);
 if not assigned(sp) then raise DataBase.eSearch.Create;
 pid:=tKeyList(mp).id;
end;
 
procedure tKeyInfo.Address(out addr:netaddr.t);
 begin
 assert(assigned(mp));
 Addr:=tKeyList(mp).Addr;
end;

procedure PeerInfo(out info:Peers.tInfo);
 begin
 Peers.Get(info,Address);
end;
