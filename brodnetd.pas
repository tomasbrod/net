PROGRAM brodnetd;

uses cmem,cthreads,ServerLoop, ObjectModel
  ,Database
  ,Store
	,HostKey
  ,RPCsrv
//	,OTServer
	,DHT
//  ,Mutable
	;

BEGIN
// Database.FreeKeyList;
 ServerLoop.Main
END.
