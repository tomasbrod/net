PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses ServerLoop
    ,TestWatch
    ,BootWeb2
    ;

BEGIN
 ServerLoop.Main;
END.