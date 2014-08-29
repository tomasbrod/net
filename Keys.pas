unit Keys;

INTERFACE

TYPE{s}

 tFingerprint=object
  private
  data :array [0..20] of byte;
 end;
 tHash=object
  procedure Clear;
  function isNil :boolean;
  private
  data :array [0..20] of byte;
 end;

VAR
 MyFilgerPrint: tFingerPrint unimplemented;
 

IMPLEMENTATION

procedure tHash.Clear;
begin abstracterror; end;

function tHash.isNil :boolean;
begin
 abstracterror;
end;

END.
