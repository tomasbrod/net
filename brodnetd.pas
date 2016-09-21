PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses cthreads,ServerLoop, ObjectModel
  ,Database
  ,Store2
	,ECC
	,OTServer
 	,ObjTrans
  ,Fetch
	,dht
	,Ctrl
  //,ott
  //,dhlt
  //,TestMutable
  //,TestMutator
	;

BEGIN
 Database.FreeKeyList;
 ServerLoop.Main
END.
