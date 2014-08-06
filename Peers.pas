unit Peers;

INTERFACE
uses Sockets
    ,UnixType
    ;

TYPE{s}

 tNetAddrLargest=object
  data :record
  case family :word of 
   AF_INET :( inet :record 
     sin_port: cushort;
     sin_addr: in_addr;
   end; );
   0 :(
    poepwopz_pad :array [0..128] of byte;
   ); 
  end;
 end;


IMPLEMENTATION

END.
