unit Keys;

INTERFACE

TYPE{s}

 tHash=object
  procedure Clear;
  function isNil :boolean;
  procedure ToString( var s :string );
  procedure FromString( s :string );
  private
  data :array [0..20] of byte;
 end;
 tFingerprint =tHash Deprecated;

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
