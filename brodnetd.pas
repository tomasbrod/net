PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses cthreads,ServerLoop
	,OTServer
 	,ObjTrans
	,dht
	,dhtBootStatic
	,dhtPersist
	,ECC
	,Ctrl
  ,dhlt
  ,TestPRC
  ,TestPRL
	;

BEGIN
 ServerLoop.Main;
END.
