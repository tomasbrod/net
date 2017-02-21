PROGRAM brodnetd;

uses cmem,cthreads,ServerLoop, ObjectModel
  ,Database
  ,Store
	,HostKey
	,OTServer
	,DHT
	,Ctrl
  ,dhlt
  ,Mutable
	;

BEGIN
 Database.FreeKeyList;
 ServerLoop.Main
END.
