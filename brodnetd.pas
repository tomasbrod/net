PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses cthreads,ServerLoop
	,TestWatch
	,TestChat
	,AsyncProcess
	,Upload
	,Download
	;

BEGIN
 ServerLoop.Main;
END.