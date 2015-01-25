unit Auth;

(*

 Authentificate peers with publickey and provide access to peers by subkey 
 and masterkey id.

*)

INTERFACE

type tPID=object(tHash);

var MasterID:tPID;
var SubID:tPID;

type tKeyInfo=object
 public {Masterkey listing}
 procedure GoAll;
 function  NextMaster:boolean;
 public {Subkey listing}
 procedure GoMaster(key: tPID); (*search for a master key*)
 function  NextSub:boolean; (* load info of next (or first) subkey, false if last*)
 public {By subkey}
 procedure GetSub(key: tPID); (*search for a subkey*)
 public {Informations}
 master:tPID;
 subkey:tPID;
 procedure Address(out addr:netaddr.t);
 procedure PeerInfo(out info:Peers.tInfo);
 private
 mp,sp:pointer;
end;
 
procedure Translate(const subkey:tPID; out addr:NetAddr.t);(*
 get address of online peer with the given subkey*)

var OnAuth=procedure (pid:tPID; addr: NetAddr.t);

type tAuthRequest=object(tPacket)
 challenge:array [1..255] of byte;
 why: byte;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create;
end;

type tAuthResponse=object(tPacket)
 master,sub:tPID;
 sig:tSHA1_DSA2048_Signature;
 procedure Handle( const from: NetAddr.t);
 procedure Send( const rcpt: NetAddr.t);
 procedure Create(var challenge:array of byte);
end;

