UNIT Controll;
{Remote controll of brodnetd, server part}
INTERFACE
IMPLEMENTATION
USES ServerLoop
    ,Sockets
    ,NetAddr
;
{ create listenning socket, register it to watch and handle connections }
{ socket dgram, local only }
{ socket stream inet, use first word as opsize}

procedure Init