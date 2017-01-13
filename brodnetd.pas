PROGRAM brodnetd;

uses cmem,cthreads,ServerLoop, ObjectModel
  ,Database
  ,Store2
	,HostKey
	,OTServer
	,DHT
  ,ProfCache
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
