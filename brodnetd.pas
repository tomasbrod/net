PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses cthreads,ServerLoop
	,TestWatch
	,TestChat
	,AsyncProcess
	,OTServer
 	,ObjTrans
	,dht
	,dhtBootStatic
	,dhtPersist
	,ECC
  ,dhlt
  ,TestPRC
	;

BEGIN
 ServerLoop.Main;
END.
