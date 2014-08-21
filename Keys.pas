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
  data :array [0..32] of byte;
 end;
 

IMPLEMENTATION

END.
