unit ContentHasher;

{ Hash computing (sha1) functionality from split Keys unit. }

INTERFACE
uses ContentHash
{$IFDEF ClassStreamSupport},Classes{$ENDIF};

type tHashSHA1=object(ContentHash.t)
{$IFDEF ClassStreamSupport}
 procedure Compute ( s: tStream ); overload;
{$ENDIF}
 procedure Compute ( var s: file; count:longword ); overload;
 procedure Compute ( s: string ); overload;
end;

IMPLEMENTATION
uses SHA1;

{$IFDEF ClassStreamSupport}
procedure tHashSHA1.Compute ( s: tStream ); overload;
 var ctx:tSHA1Context;
 var digest:tSHA1Digest;
 var buf: array [1..512] of byte;
 var bl: word;
 begin
 SHA1Init( ctx );
 while true do begin
  bl:= s.Read( buf, sizeof(buf) );
  if bl=0 then break;
  SHA1Update( ctx, buf, bl );
 end;
 SHA1Final( ctx, digest );
 for bl:=0 to 19 do data[bl+1]:=digest[bl];
end;
{$ENDIF}

procedure tHashSHA1.Compute ( var s: file; count:longword );
 var ctx:tSHA1Context;
 var digest:tSHA1Digest;
 var buf: array [1..512] of byte;
 var bl: longword;
 begin
 SHA1Init( ctx );
 while count>0 do begin
  bl:=count; if bl>sizeof(buf) then bl:=sizeof(buf);
  BlockRead( s, buf, bl, bl );
  Dec(count,bl);
  if bl=0 then break;
  SHA1Update( ctx, buf, bl );
 end;
 SHA1Final( ctx, digest );
 for bl:=0 to 19 do data[bl+1]:=digest[bl];
end;

procedure tHashSHA1.Compute ( s: string ); overload;
 var digest:tSHA1Digest;
 var bl: word;
 begin
 digest:=SHA1String( s );
 for bl:=0 to 19 do data[bl+1]:=digest[bl];
end;

END.