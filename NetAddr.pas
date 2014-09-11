unit NetAddr;

INTERFACE
uses Sockets
    ;

TYPE

 t= packed object
 { Object to store Socket Address }

  function Length :word;
  { Returns length of the structure based on address family }

  procedure ToSocket( var sockaddr :tSockAddr );
   unimplemented;
  procedure FromSocket( var sockaddr :tSockAddr );
   unimplemented;

  public
  data :packed record
  case Family : (afNil=0, afInet=1 ) of 
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

Operator = (aa, ab :t) b : boolean;

{
Operator := (aa :t ) r : pSockAddr;
Operator := ( aa :pSockAddr ) r : t;
}

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
 AbstractError;
end;

procedure t.FromSocket( var sockaddr :tSockAddr );
begin
 AbstractError;
end;

END.