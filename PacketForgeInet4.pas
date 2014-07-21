program PacketForgeInet4;
{$MODE OBJFPC}

USES SysUtils,Classes,Sockets {,Bluetooth};

TYPE {Temporialy type aliases}

 TSocketAddr=packed record
  Family: sa_family_t;
  data: array [0..107] of char;
 end;
  

VAR
 AddrLen: TSockLen;
 Addr: sockaddr;
 Socket: TSocket;
 databuf: array [0..4096] of byte;
 dataLen: 0..65535;
 tmp: Cardinal;
 Inputf, Outputf: file;
 Buffer:TMemoryStream;
 str:string;

BEGIN

 Buffer:=TMemoryStream.Create;

 with sockaddr_in(Addr) do begin
  sin_family:=AF_INET;
  sin_port:=htons(1030);
  sin_addr:=StrToHostAddr('127.0.0.1');
 end;
 
 Assign(Inputf,'');
 ReSet(Inputf,1);
 Assign(Outputf,'');
 ReWrite(Outputf,1);
 
 AddrLen:=Sizeof(sockaddr_in);
 BlockWrite(Outputf,AddrLen,sizeof(AddrLen),tmp);
 BlockWrite(Outputf,Addr,AddrLen,tmp);

 repeat
  BlockRead(Inputf,DataBuf,sizeof(DataBuf),tmp);
  Buffer.Write(DataBuf,tmp);
 until true;
 
 DataLen:=Buffer.size;
 BlockWrite(Outputf,DataLen,2 );
 BlockWrite(Outputf,Buffer.Memory, Buffer.Size);

END.
