unit Log deprecated;

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
 prefix:=DateTimeToStr(Now)+' '+IntToStr(GetProcessID)+' ';
 WriteLn( F, prefix, S );
end;

procedure Init ( fn :tFileName );
 begin
 {$WARNING We really have to implement log file locking!}
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
