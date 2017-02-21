uses cmem, SysUtils, Crypto, ObjectModel;

var bzc: tBZipContext;
var rv: integer;
var inp: string = 'Allowable values range from 0 to 250 inclusive. 0 is a special case, equivalent to using the default value of 30.';
var outbf: array [0..4095] of byte;
begin
  bzc.bzalloc:=nil;
  bzc.bzfree:=nil;
  bzc.opaque:=nil;
  rv:= BZ2_bzCompressInit(bzc, 9, 99, 0 );
  assert(rv=BZ_OK);
  bzc.next_in:=@inp[1];
  bzc.avail_in:=length(inp);
  bzc.next_out:=@outbf;
  bzc.avail_out:=sizeof(outbf);
  rv:= BZ2_bzCompress(bzc, BZ_FLUSH);
  writeln('ret: ',rv, ' avail_out: ', bzc.avail_out,' ',sizeof(outbf)-bzc.avail_out,' avail_in: ',bzc.avail_in,' ',length(inp));
end.
  
