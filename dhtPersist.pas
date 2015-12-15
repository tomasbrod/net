unit dhtPersist;
{
  Initialize DHT local node ID from file.
  Restore DHT routing table from file.
  Save DHT routing table on exit and periodically.
}

INTERFACE

IMPLEMENTATION
uses NetAddr,ServerLoop,DHT,SysUtils,Store1;

const ndfn='nodes.dat';
const idfn='idhash.txt';

procedure Save;
 var nd: FILE of tPeerPub;
 var node:tPeerPub;
 var nnp:pointer=nil;
 var nni:byte=0;
 begin
 assign(nd,'_'+ndfn);
 ReWrite(nd);
 try
 repeat
  dht.GetNextNode(nnp,nni,node);
  if node.addr.isNil then break;
  writeln('dhtPersist: save ',string(node.addr));
  write(nd,node);
 until false;
 finally
  close(nd);
 end;
 rename(nd,ndfn);
end;

procedure Load;
 var nd: FILE of tPeerPub;
 var node:tPeerPub;
 var pos:Word;
 begin
 assign(nd,ndfn);
 try
  ReSet(nd);
 except
  writeln('dhtPersist: can not open state file ',ndfn);
 exit end;
 {need to read the file backwards}
 pos:=FileSize(nd);
 if pos=0 then exit;
 for pos:=pos-1 downto 0 do begin
  Seek(nd,pos);
  Read(nd,node);
  writeln('dhtPersist: load ',string(node.addr));
  dht.InsertNode(node);
 end;
 close(nd);
end;

procedure LoadID;
 var nd: TextFile;
 var line:string;
 begin
 assign(nd,idfn);
 try
  ReSet(nd);
 except
  writeln('dhtPersist: can not open id file ',idfn);
 exit end;
 readln(nd,line);
 writeln('dhtPersist: nodeID ',line);
 dht.MyID:=line;
 close(nd);
end;

type t=object
 pot:procedure;
 procedure Init;
 procedure doPeriodic;
 procedure doSoon;
end;

procedure doLast;
 begin
 Save;
end;

procedure t.doSoon;
 begin
 Load;
end;

procedure t.doPeriodic;
 begin
 Save;
 Shedule(40000,@doPeriodic);
end;

procedure t.Init;
 begin
 LoadID;
 Shedule(2000,@doSoon);
 Shedule(20000,@doPeriodic);
 {
 pot:=ServerLoop.OnTerminate;
 ServerLoop.OnTerminate:=@doLast;
 }
end;

var o:t;
BEGIN
 o.init;
END.