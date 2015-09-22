PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses ServerLoop
    ,TestWatch
    ,TestTC
	,TestChat
    ;

BEGIN
 ServerLoop.Main;
END.