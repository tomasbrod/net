unit NetAddr;

INTERFACE
uses Sockets
    ,SysUtils
    ;

TYPE
 
 tFamily=(afNil=0, afInet=1 );

 { Network-byte-ordered words }
 Word2=array [1..2] of byte; {0..65535}
 Word4=array [1..4] of byte; {0..4294967295}

 { Object to store Socket Address }
 t= packed object

  function Length :word;
  { Returns length of the structure based on address family }

  procedure ToSocket( var sockaddr :tSockAddr );
  {$HINT Not actually testet, byt seems to work}
  procedure FromSocket( var sockaddr :tSockAddr );
  {$HINT Not actually testet, byt seems to work}

  procedure ToString( var str :String );
  {$HINT Not actually testet, byt seems to work}
  procedure FromString( str :String );
  {$HINT Not actually testet, byt seems to work}

  procedure LocalHost( af: tFamily );
  {Generate localhost address of address family af.}
  {$HINT Not actually testet, byt seems to work}
  
  procedure Clear;
  function  isNil:boolean;

  public
  data :packed record
  case Family : tFamily of 
   { note: maximum family is 32 so byte is enough }
   afInet :( inet :packed record 
    port: Word;
    addr: tInAddr;
   end; );
   afNil :(
    pad_pV4IlkA4mKQL :packed array [0..128] of byte;
   ); 
  end;
 end;
 
operator := (net : Word2) host:word;
operator := (net : Word4) host:Dword;
operator := (host : word) net:Word2;
operator := (host : Dword) net:Word4;

Operator = (aa, ab :t) b : boolean;

operator := ( at : t) aString : string;
operator := ( aString : string) at : t;

{
Operator := (aa :t ) r : pSockAddr;
Operator := ( aa :pSockAddr ) r : t;
}

type eSocket=class(Exception)
 code :integer;
 constructor Create( icode: integer; msg: string );
end;

procedure CheckSocket; inline;

IMPLEMENTATION
uses 
     DataBase
    ,UnixType
     ;

Operator = (aa, ab :Sockets.tInAddr) b : boolean;
begin
 b:=aa.s_addr=ab.s_addr;
end;

Operator = (aa, ab :t) b : boolean;
begin
 b:=false;
 if aa.data.Family<>ab.data.Family then exit;
 case aa.data.Family of
  afInet: if (aa.data.inet.port<>ab.data.inet.port) or (aa.data.inet.addr<>ab.data.inet.addr) then exit;
  afNil: {null addresses are always equal};
  else AbstractError; 
 end;
 b:=true;
end;

function t.Length :Word;
begin
 result:=(sizeof(self)-sizeof(data))+sizeof(data.Family);
 case data.Family of
  afNil: ;
  afInet: result+=sizeof( data.inet );
  else result:=sizeof(self);
 end;
end;

procedure t.ToSocket( var sockaddr :tSockAddr );
begin
 case data.family of
  afInet: begin
   sockaddr.sa_family:=Sockets.AF_INET;
   Move(data.inet, sockaddr.sa_data, sizeof(data.inet) );
  end;
  else AbstractError; 
 end;
end;

procedure t.FromSocket( var sockaddr :tSockAddr );
begin
 case sockaddr.sa_family of
  Sockets.AF_INET: begin
   data.family:=afInet;
   move(sockaddr.sa_data, data.inet, sizeof(data.inet) );
  end;
  else raise Exception.Create('Unknown AF '+IntToStr(sockaddr.sa_family));
 end;
end;

procedure t.ToString( var str :String );
 begin
 case data.Family of
  afInet: begin
   str:='//ip4/'+Sockets.NetAddrToStr(data.inet.addr)+
    '/'+IntToStr(ShortNetToHost(data.inet.port));
  end;
  afNil: str:='nil';
  else AbstractError;
 end;
end;

procedure t.FromString( str :String );
 var i:integer;
 var fam:string;
 begin
 if Copy(str,1,2)<>'//' then raise eConvertError.Create('');
 Delete(str,1,2);
 i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
 fam:=copy(str,1,i-1);
 delete(str,1,i);
 if fam='ip4' then begin
  data.family:=afInet;

  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.inet.addr:=StrToNetAddr(fam);
  
  i:=pos('/',str); if i=0 then i:=System.Length(str)+1;
  fam:=copy(str,1,i-1);
  delete(str,1,i);
  data.inet.port:=ShortHostToNet(StrToInt(fam));
  
 end else if fam='nil' then begin
  data.family:=afNil;
 end else AbstractError;
end;

const cLocalHostIP4:Sockets.tInAddr=( s_bytes:(127,0,0,1) );
const cLocalIP4Port:word=1030;

procedure t.LocalHost( af: tFamily );
 begin
 data.Family:=af;
 case af of
  afInet: begin
   data.inet.port:=ShortHostToNet(cLocalIP4Port);
   data.inet.addr:=cLocalHostIP4;
  end;
  afNil: ;
  else AbstractError;
 end;
end;

procedure t.Clear;
 begin
 self.data.family:=afNil;
end;

function  t.isNil:boolean;
 begin
 isNil:= self.data.family=afNil;
end;

operator := ( at : t) aString : string;
 begin
 at.ToString( aString );
end;
operator := ( aString : string) at : t;
 begin
 at.FromString( aString );
end;

constructor eSocket.Create( icode: integer; msg: string );
 begin
 inherited Create(msg);
 code:=icode;
end;

procedure CheckSocket;
 inline;
var e:cint;
begin
 e:=SocketError;
 if e<>0 then raise eSocket.Create(e, '...');
end;

operator := (net : Word2) host:word;
 var pnet:^word;
 begin
 pnet:=@net;
 host:=ShortNetToHost( pnet^ );
end;
operator := (net : Word4) host:Dword;
 var pnet:^DWord;
 begin
 pnet:=@net;
 host:=NetToHost( pnet^ );
end;

operator := (host : word) net:Word2;
 var pnet:^Word;
 begin
 pnet:=@net;
 pnet^:= ShortHostToNet( host );
end;
operator := (host : Dword) net:Word4;
 var pnet:^DWord;
 begin
 pnet:=@net;
 pnet^:=HostToNet( host );
end;

END.
