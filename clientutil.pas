unit ClientUtil;
INTERFACE
uses ObjectModel;

type tIPCConnection=object
  req,res:tMemoryStream;
  resc:byte;
  socket:tHandle;
  AutoConnect: procedure;
  constructor Init;
  destructor Done;
  procedure ConnectUnix(const path:string);
  procedure Disconnect;
  procedure SendReq;
  procedure RecvRes;
  procedure MakeCall;
  procedure SendData();
  procedure RecvData(howmuch:LongWord);
end;

type tShortKeyID=array [0..2] of byte;
type tKeyring = OBJECT
  constructor Init(const ipath:ansistring);
  destructor Done;
  procedure AddSeed( const pub: tKey32; const seed: tKey32; use,enc: char );
  function  GetSeed( out seed: tKey32; use: char; id: tShortKeyID ): char;
  {GetCert( out sig:tSig; out expire: Int64; out pub: tKey32; use: char);}
  private
  path: String;
  procedure verifyfirstline(var l:ansistring);
  public
  Note: String;
  PUB1: tKey32;
  ID1, ID2: tKey24;
  certD, certS, certE: record
    exp: Int64;
    sig: tKey64
  end;
  
end;

type eKeyringSyntax=class(eXception) end;
  
IMPLEMENTATION
uses SysUtils,Crypto,ed25519,Sockets;

constructor tKeyring.Init(const ipath:ansistring);
  {var l:ansistring;
  tfh:textfile;}
  begin
  path:=ipath;
  {assign(tfh,path);
  reset(tfh);
  readln(tfh,l);
  if not AnsiStartsStr('BRODNET KEYRING',l)
    then raise eFormatError.Create('Invalid Keyring');}
  {note: destructor is called when error occurs in constructor}
end;

procedure tKeyring.verifyfirstline(var l:ansistring);
  begin
  if not StrCompAt(l,0,'BRODNET KEYRING')
    then raise eKeyringSyntax.Create('Invalid Keyring Header');
end;
  

destructor tKeyring.Done;
  begin
  path:='';
  {close(tfh);}
end;

function tKeyring.GetSeed( out seed: tKey32; use: char; id: tShortKeyID ): char;
  var l:ansistring;
  var q:string[6];
  tfh:textfile;
  begin
  result:=' ';
  SetLength(q,6);
  ObjectModel.BinToHex(@q[1],id,3);
  assign(tfh,path);
  reset(tfh);
  readln(tfh,l); verifyfirstline(l);
  while not eof(tfh) do begin
    readln(tfh,l);
    if (l[1]='S') and (length(l)<>75) then
      raise eKeyringSyntax.Create('Keyring Syntax error');
    if (l[1]='S') and (l[3]=Use) and StrCompAt(l,5,q) then begin
      result:=l[2];
      if HexToBin(seed, @l[12], 32) <32
        then raise eConvertError.Create('Keyring Unhex error');
      exit;
    end;
  end;
  result:=' ';
  close(tfh);
end;

procedure tKeyring.AddSeed( const pub: tKey32; const seed: tKey32; use,enc: char );
  var af:textfile;
  var l:ansistring;
  begin
  assign(af,path);
  append(af);
  SetLength(l,75);
  l:='S'+enc+use+' '+BinToHexStr(pub,3)+' '+BinToHexStr(seed,32);
  writeln(af,l);
  close(af);
end;




constructor tIPCConnection.Init;
  begin
  socket:=-1;
  req.Init(4096);
  res.Init(req.base,0,req.size);
  resc:=255;
  socket:=-1;
  AutoConnect:=nil;
  req.Skip(2);
end;

destructor tIPCConnection.Done;
  begin
  req.Free;
  resc:=255;
  if socket<>-1 then Disconnect;
  socket:=-1;
end;

procedure tIPCConnection.Disconnect;
  begin
  fpShutdown(socket,2);
  CloseSocket(socket);
  socket:=-1;
end;

procedure tIPCConnection.ConnectUnix(const path:string);
  var addr:Sockets.sockaddr_un;
  begin
  addr.sun_family:=AF_UNIX;
  addr.sun_path:=path;
  socket:=fpSocket(addr.sun_family,SOCK_STREAM,0);
  SC(@fpSocket,socket);
  SC(@fpConnect,fpConnect(socket,@addr,sizeof(addr)));
end;

procedure tIPCConnection.SendReq;
  begin
  req.Seek(0);
  req.writeword2(req.length-4);
  self.SendData;
  req.Seek(2);
  req.Trunc;
end;

procedure tIPCConnection.RecvRes;
  var len:integer;
  begin
  {read header}
  res.Seek(0);
  res.Trunc;
  self.RecvData(2);
  res.Seek(0);
  len:=res.ReadWord2;
  {read body}
  if (len+2)>res.size then raise eInvalidMemStreamAccess.Create('Message Too Long');
  self.RecvData(len);
  res.Seek(2);
  resc:=res.ReadByte;
end;

procedure tIPCConnection.MakeCall;
  begin
  SendReq;
  RecvRes;
end;

procedure tIPCConnection.SendData();
  var wd,len:LongInt;
  var src:pointer;
  begin
  src:=@req.base;
  len:=req.vlength;
  while len>0 do begin
    wd:=fpSend(socket,src,len,0);
    if wd<=0 then SC(@fpSend,wd-1);
    src:=src+wd;
    len:=len-wd;
  end;
end;

procedure tIPCConnection.RecvData(howmuch:LongWord);
  var dst:pointer;
  var rd:LongInt;
  begin
  dst:=res.WrBuf;
  assert(res.WrBufLen>=howmuch);
  while howmuch>0 do begin
    rd:=fpRecv(socket,dst,howmuch,0);
    if rd<=0 then SC(@fpRecv,rd-1);
    dst:=dst+rd;
    howmuch:=howmuch-rd;
  end;
end;

END.
