PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses ServerLoop
	,TestWatch
	//,TestTC
	//,TestChat
	,AsyncProcess
	,upmgr
	,TestFS
	;

BEGIN
 ServerLoop.Main;
END.