unit Peers;

INTERFACE
uses Sockets
    ,UnixType
    ,GeneralPacket
    ,Keys
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

procedure Assoc( fpr :keys.tFingerprint );
 unimplemented;

procedure Save( really{?} :boolean );
 unimplemented;

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
 unimplemented;

IMPLEMENTATION

procedure Assoc( fpr :keys.tFingerprint );
begin
end;

procedure Save( really{?} :boolean );
begin
end;

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
begin
end;



END.
