PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses ServerLoop
    ,TestWatch
    ,TestTC
    ;

BEGIN
 ServerLoop.Main;
END.