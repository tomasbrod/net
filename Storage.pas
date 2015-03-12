unit Storage;

(* Objectives:

 Managment of files in filestore. Deletion of unused files. Reference 
 counting and expiration. Bytes used by storage. Limit on storage size.

*)

INTERFACE
uses NetAddr, Neighb, Transfer;

procedure AddReference( name: tFID; referrer: Transfer.tBy );
procedure FreeReference( name:tFID; referrer: Transfer.tBy );

function UsedSpace :LongWord;
var TresholdMB :Word;
var LimitMB    :Word;

const cTimeWait=1;
const cPublished=2;

{ demon interface }
procedure Init;
procedure NotifyIdle;
procedure NotifyTransfer( id :tFID; done,total:longword; by: tBys );
procedure NotifyShutdown;

IMPLEMENTATION
uses DataBase;

var LastScan:tDateTime;
var UsedSpaceCached:LongInt;

procedure AssignDb(var f: file; const id: tFID; const ext :string);
 begin
 DataBase.AssignDB(f, 'chkref'+DirectorySeparator+string(id)+'.'+ext);
end;

function UsedSpace:LongWord;
 begin result:=UsedSpaceCached;
 //fuck logic
end;

type tMeta=record
 expires:tDateTime;
 refs:set of tBy;
end;

procedure AddReference( name: tFID; referrer: Transfer.tBy );
 var metaf:file of tMeta;
 var meta:tMeta;
 begin
 log.debug('AddReference #'+IntToStr(referrer)+' '+string(name));
 AssignDb(metaf,name,'dat');
 reset(metaf);
 if FileSize(metaf)=0 then with meta do begin
  expires:=IncMinute(now,30);
  refs:=[cTimeWait];
 end else read(metaf,meta);
 include(meta.refs,referrer);
 seek(metaf,0);
 write(metaf,meta);
 close(metaf);
end;

procedure FreeReference( name:tFID; referrer: Transfer.tBy );
 var metaf:file of tMeta;
 var dataf:file;
 var meta:tMeta;
 begin
 log.debug('FreeReference #'+IntToStr(referrer)+' '+string(name));
 AssignDb(metaf,name,'dat');
 reset(metaf);
 if FileSize(metaf)=0 then begin close(metaf); erase(metaf); exit end;
 read(metaf,meta);
 exclude(meta.refs,referrer);
 reset(metaf);
 write(metaf,meta);
 close(metaf);
 if meta.refs=[] then begin
  log.Info('Delete '+string(name));
  Transfer.RecvFileAbort(name);
  Database.DbAssign(dataf,'chk/'+string(name));
  reset(dataf);
  Dec(UsedSpaceCached,FileSize(dataf));
  close(dataf);
  erase(dataf);
  erase(metaf);
 end;
end;

procedure NotifyTransfer( id :tFID; done,total:longword; by: tBys );
 begin
 if done=total then begin
  Inc(UsedSpaceCached,total);
 end else AddReference(id,cTimeWait);
  {Add a time ref to ensure the file is deleted if something goes wrong}
end;

END.