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
	;

BEGIN
 ServerLoop.Main;
END.
