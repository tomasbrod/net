PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses ServerLoop
    ,TestWatch
    ;

BEGIN
 ServerLoop.Main;
END.