PROGRAM brodnetd;

{ Poll loop. Read message, get handler, exec handler. }
uses ServerLoop
    ,TestWatch
    ,BootWeb
    ;

BEGIN
 ServerLoop.Main;
END.