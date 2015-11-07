PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses ServerLoop
	,TestWatch
	,TestTC
	,TestChat
	,AsyncProcess
	,Upload
	,TestFS
	,Download
	;

BEGIN
 ServerLoop.Main;
END.