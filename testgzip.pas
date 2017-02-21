uses cmem, SysUtils, Crypto, ObjectModel, Strutils;

var gzc: tGZip;
var rv, defsz: integer;
var inp: string = 'Maximum value for windowBits in deflateInit2 and inflateInit2. WARNING: reducing MAX_WBITS makes minigzip unable to extract .gz files created by gzip. (Files created by minigzip can still be extracted by gzip.)';
var outbf: array [0..4095] of byte;
var recon: array [0..1023] of char;
var hexed: array [0..8191] of char;
begin
  gzc.InitDeflate;
  gzc.next_in:=@inp[1];
  gzc.avail_in:=length(inp);
  gzc.next_out:=@outbf;
  repeat
    gzc.avail_out:=16;
    gzc.Deflate;
    defsz:=pbyte(gzc.next_out)-pbyte(@outbf);
    writeln('Aret: avail_out: ', gzc.avail_out,' ',defsz,' avail_in: ',gzc.avail_in,' ',length(inp));
  until gzc.eof;
  BinToHex(@outbf, @hexed, defsz);
  writeln(hexed);
  deflateEnd(gzc);
  rv:=inflateInit2_(gzc, -15, ZLIB_VERSION,sizeof(gzc));
  assert(rv=Z_OK);
  gzc.next_in:=@outbf;
  gzc.avail_in:=defsz;
  gzc.next_out:=@recon;
  gzc.avail_out:=sizeof(recon);
  rv:= inflate(gzc, Z_NO_FLUSH);
  writeln('ret: ',rv, ' avail_out: ', gzc.avail_out,' ',sizeof(recon)-gzc.avail_out,' avail_in: ',gzc.avail_in,' ',defsz);
  writeln(recon);
end.

begin
  gzc.InitDelfate(...);
  repeat
    fill input buffer
    gzc.Deflate;
    if endofstream then addpadding;
    while output>blocksize
      encrypt
      write
  until endofstream;
end.
