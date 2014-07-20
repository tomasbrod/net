program LowSend;
{$MODE OBJFPC}

{

 This is a helper program to send network packets ower various protocols.
 
 We accept data on INPUT. First there is a network address specifier.
 Then follows the size and a data.

}


USES SysUtils,Sockets {,Bluetooth};

procedure BufRead(var F :file; var Buf; const count :Cardinal);
var Result :Cardinal;
begin
 BlockRead( F, Buf, Count, Result);
 if Result < Count then raise Exception.Create('Input Read error');
end;

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
 Inputf: file;

BEGIN

 Assign(Inputf,'');
 ReSet(Inputf,1);
 
 BufRead(Inputf,AddrLen,sizeof(AddrLen));
 BufRead(Inputf,Addr,AddrLen);
 
 case Addr.SA_Family of
  {Begin the SIMPLE networks }
  AF_INET,AF_INET6: begin
   Socket:= fpSocket(Addr.SA_Family,SOCK_DGRAM,0);
   fpConnect(Socket,@Addr,AddrLen);
  end;
 end;
 
 BufRead(Inputf,DataLen,sizeof(DataLen));
 if DataLen>sizeof(DataBuf) then raise Exception.Create('Data Too Long');
 BufRead(Inputf,DataBuf,DataLen);

 fpSend(Socket, @DataBuf[0], DataLen, 0);

   
   
   

END.
