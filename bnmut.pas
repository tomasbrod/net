program bnmut;
USES MemStream,NetAddr,SysUtils,ed25519,Sha512;
{$I Mutable-file.pas}

function GetPubFromSecFile(const fn:string):tKey32;
  var sf:file of tKey64;
  var sec:tKey64;
  begin
  try
  assign(sf,fn);
  reset(sf);
  read(sf,sec);
  finally
  close(sf);
  end;
  CreateKeyPair(result, sec);
end;
function GetPubHash(const pub:tKey32):tKey20;
  var hash:tSha512Context;
  begin
  Sha512Init(hash);
  Sha512Update(hash,pub,sizeof(pub));
  Sha512Final(hash,result,sizeof(result));
end;

procedure CopyUpdate( const infn, keyfn, outfn:string);
  var inf,outf:file of byte;
  var keyf:file of tKey64;
  var key:tKey64;
  var hdr:tMutHdr;
  var bufr,buf:pointer;
  var datasize:integer;
  begin
  {$I+}
  assign(inf,infn); reset(inf);
  assign(outf,outfn); rewrite(outf);
  assign(keyf,keyfn); reset(keyf);
  read(keyf,key); Close(keyf);
  hdr.Magic:=cMutHdrMagic;
  CreateKeyPair(hdr.pub,key);
  hdr.Note:='';
  hdr.Ver:=0;
  hdr.Day:=trunc(Now-cMutEpoch);
  Seek(outf,sizeof(hdr));
  datasize:=FileSize(inf);
  writeln('Signing ',datasize,'B with ',string(GetPubHash(hdr.pub)));
  bufr:=GetMem(datasize+64); {first 64B of header are signed}
  Move(hdr,bufr^,64);
  buf:=bufr+64;
  BlockRead(inf,buf^,datasize);
  BlockWrite(outf,buf^,datasize);
  ed25519.Sign(hdr.Sig, bufr^, datasize+64, hdr.Pub, key);
  Seek(outf,0);
  BlockWrite(outf,hdr,sizeof(hdr));
  Close(inf);
  Close(outf);
  halt(0); 
end;

procedure Update( const infn, keyfn:string);
  var inf:file of byte;
  var keyf:file of tKey64;
  var key:tKey64;
  var nPub:tKey32;
  var nDay:LongWord;
  var hdr:tMutHdr;
  var bufr,buf:pointer;
  var datasize:integer;
  begin
  {$I+}
  assign(inf,infn); reset(inf);
  assign(keyf,keyfn); reset(keyf);
  read(keyf,key); Close(keyf);
  BlockRead(inf,hdr,sizeof(hdr));
  if CompareByte(hdr.Magic,cMutHdrMagic,sizeof(hdr.Magic)) <> 0 then begin
    writeln('File is not BNMut, or is corrupted.');
    Writeln('Use bnmut input key output, to create mutable from any data');
    close(inf);
    halt(2);
  end;
  CreateKeyPair(nPub,key);
  nDay:=trunc(Now-cMutEpoch);
  hdr.pub:=nPub;
  hdr.Ver:=LongWord(hdr.Ver)+1;
  hdr.Day:=nDay;
  datasize:=FileSize(inf)-sizeof(hdr);
  writeln('Signing ',datasize,'B with ',string(GetPubHash(hdr.pub)));
  bufr:=GetMem(datasize+64); {first 64B of header are signed}
  Move(hdr,bufr^,64);
  buf:=bufr+64;
  BlockRead(inf,buf^,datasize);
  ed25519.Sign(hdr.Sig, bufr^, datasize+64, hdr.Pub, key);
  Seek(inf,0);
  BlockWrite(inf,hdr,sizeof(hdr));
  Close(inf);
  halt(0); 
end;

procedure Verify( const infn:string);
  var inf:file of byte;
  var hdr:tMutHdr;
  var buf:array [1..512] of byte;
  var red:integer;
  var hash:tSha512Context;
  begin
  {$I+}
  Assign(inf,infn); ReSet(inf);
  BlockRead(inf,hdr,sizeof(hdr));
  if CompareByte(hdr.Magic,cMutHdrMagic,sizeof(hdr.Magic)) <> 0 then begin
    writeln('File is Corrupted');
    close(inf);
    halt(2);
  end;
  Sha512Init(hash);
  Sha512Update(hash,hdr,64);
  BlockRead(inf,buf,sizeof(buf),red);
  while red>0 do begin
    Sha512Update(hash,buf,red);
    BlockRead(inf,buf,sizeof(buf),red);
  end;
  Close(inf);
  if ed25519.Verify2(hash, hdr.Sig, hdr.Pub) then begin
    writeln('Signature OK');
    //writeln('Pub: ',string(hdr.pub));
    writeln('ID: ',string(GetPubHash(hdr.pub)));
    writeln('Version: ',LongWord(hdr.Day),'+',LongWord(hdr.Ver));
    halt(0);
  end else begin
    Writeln('Signature verification FAILED');
    Halt(1);
  end;
end;

{ verify file.in}
{ sign file.in key.in file.out}
{ update file.mod key.in}
BEGIN
  case system.paramcount of
    1: Verify(paramstr(1));
    2: Update(paramstr(1),paramstr(2));
    3: CopyUpdate(paramstr(1),paramstr(2),paramstr(3));
    else begin
      writeln('usage: bnmut [input [key [output]]]');
      writeln('create mutable from datafile: bnmut datafile.in secretkey mutable.out');
      writeln('update mutable signature: bnmut mutable secretkey');
      writeln('verify mutable signature: bnmut mutable') end;
  end;
END.

