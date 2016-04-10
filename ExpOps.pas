unit ExpOps;

INTERFACE
uses MemStream,NetAddr,ed25519,Sha512;
{$I Mutable-file.pas}

procedure UpdateMutSig(var mf:file; sec:tKey64);
procedure UpdateMutSig(var mf:file; const sec:tKey64; const pub:tKey32);

{function  VerifyMutSig(var ctx:tSha512Context; const hdr:tMutHdr):boolean;
 just ed25519.verify2}

IMPLEMENTATION
uses SysUtils;

procedure UpdateMutSig(var mf:file; sec:tKey64);
  var pub:tKey32;
  begin
  CreateKeyPair(pub,sec);
  UpdateMutSig(mf,sec,pub);
end;

procedure UpdateMutSig(var mf:file; const sec:tKey64; const pub:tKey32);
  var nDay:LongWord;
  var hdr:tMutHdr;
  var bufr,buf:pointer;
  var datasize:integer;
  begin
  {$I+}
  BlockRead(mf,hdr,sizeof(hdr));
  if CompareByte(hdr.Magic,cMutHdrMagic,sizeof(hdr.Magic)) <> 0 then begin
    raise eFormatError.create('Invalid magic sequence');
  end;
  nDay:=trunc(Now-cMutEpoch);
  hdr.pub:=Pub;
  hdr.Ver:=LongWord(hdr.Ver)+1;
  hdr.Day:=nDay;
  datasize:=FileSize(mf)-sizeof(hdr);
  bufr:=GetMem(datasize+64); {first 64B of header are signed}
  Move(hdr,bufr^,64);
  buf:=bufr+64;
  BlockRead(mf,buf^,datasize);
  ed25519.Sign(hdr.Sig, bufr^, datasize+64, pub, sec);
  Seek(mf,0);
  BlockWrite(mf,hdr,sizeof(hdr));
end;

END.
