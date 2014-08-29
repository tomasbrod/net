unit Peers;

INTERFACE
uses Sockets
    ,UnixType
    ,GeneralPacket
    ,Keys
    ;

TYPE{s}

 tNetAddrLargest=object
  function Length :word;
  private
  data :record
  case family :word of 
   AF_INET :( inet :record 
     sin_port: cushort;
     sin_addr: in_addr;
   end; );
   0 :(
    pad_pV4IlkA4mKQL :array [0..128] of byte;
   ); 
  end;
 end;

procedure Assoc( fpr :keys.tFingerprint );
(* Associate sender with fpr *)
 unimplemented;

procedure Save( really{?} :boolean );
(* Save the currently selected peer's info *)
 unimplemented;

function TimeSinceLast( pktype :GeneralPacket.tPkType ): System.tTime;
(* returns time since last packet from the peer of that type arrived *)
 unimplemented;

function Select( fpr :keys.tFingerprint );

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
