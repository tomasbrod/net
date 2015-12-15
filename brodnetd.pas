PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses cthreads,ServerLoop
	,TestWatch
	,TestChat
	,AsyncProcess
	,Upload
	,Download
	,TestFS
	,dht
	,dhtBootStatic
	,dhtPersist
	;

BEGIN
 ServerLoop.Main;
END.