PROGRAM brodnetd;

uses cmem,cthreads,ServerLoop, ObjectModel
  ,Database
  ,Store
	,HostKey
	,OTServer
	,DHT
	,Ctrl
  //,ott
  ,dhlt
  //,TestMutable
  //,TestMutator
	;

BEGIN
 Database.FreeKeyList;
 ServerLoop.Main
END.
