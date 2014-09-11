unit Log;

INTERFACE
uses SysUtils
    ;

var F :TextFile;

procedure msg( s: string );

procedure Init ( fn :tFileName );

IMPLEMENTATION

procedure msg( s :string );
 var prefix : string;
 begin
 prefix:=DateTimeToStr(Now)+' ';
 WriteLn( F, prefix, S );
end;

procedure Init ( fn :tFileName );
 begin
 Assign( F, fn );
 try
  Append( F );
 except
  on eInOutError do ReWrite( F );
 end;
end;

FINALIZATION
 try
  Flush(F);
  Close(F);
 except
 end;

END.
