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
	,TestDHT
	;

BEGIN
 ServerLoop.Main;
END.