unit Storage;

(* Objectives:

 Managment of files in filestore. Deletion of unused files. Reference 
 counting and expiration. Bytes used by storage. Limit on storage size.

*)

INTERFACE
uses NetAddr, Neighb, Transfer, SysUtils,EventLog;

procedure AddReference( name: tFID; referrer: Transfer.tBy );
procedure FreeReference( name:tFID; referrer: Transfer.tBy );

function UsedSpace :LongWord;
var TresholdMB :Word;
var LimitMB    :Word;

const cTimeWait=1;
const cPublished=2;

{ demon interface }
procedure Init;
const cScanPeriod:tDateTime= 2 /MinsPerDay;
procedure NotifyIdle;
procedure NotifyTransfer( id :tFID; done,total:longword; by: tBys );
procedure NotifyQuit;
var Log:tEventLog;

IMPLEMENTATION
uses DataBase,Keys;

var LastScan:tDateTime;
var UsedSpaceCached:LongInt;

procedure dbAssign(var f: file; const id: tFID; const ext :string);
 begin
 DataBase.dbAssign(f, 'chkref'+DirectorySeparator+string(id)+'.'+ext);
end;

function UsedSpace:LongWord;
 begin 
  if UsedSpaceCached>=0 then result:=UsedSpaceCached else result:=0;
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
 dbAssign(metaf,name,'dat');
 reset(metaf);
 if FileSize(metaf)=0 then with meta do begin
  expires:=now+(30/MinsPerDay);
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
 dbAssign(metaf,name,'dat');
 reset(metaf);
 if FileSize(metaf)=0 then begin close(metaf); erase(metaf); exit end;
 read(metaf,meta);
 exclude(meta.refs,referrer);
 reset(metaf);
 write(metaf,meta);
 close(metaf);
end;

procedure DoScan;
 var sr:tSearchRec;
 var rv:LongInt;
 var prefix:string;
 var stat: array [tBy] of word;
 var by:word;
 var preusage:LongWord;
 procedure DoFile;
  var metaf:file of tMeta;
  var meta:tMeta;
  var dataf:file of byte;
  var id:tFID;
  var idstr:string[40];
  var by:word;
  begin
  idstr:=copy(sr.name,1,40);
  try id:=tFid(tHash(idstr)); except on eConvertError do begin
   log.error('Found invalid file in chkref dir! '+sr.name);
   exit;
  end; end;
  assign(metaf,prefix+sr.name);
  reset(metaf); read(metaf,meta); close(metaf);
  if meta.expires<now then exclude(meta.refs,cTimeWait) (*else log.debug('e '+DateTimeToStr(meta.expires))*);
  if meta.refs=[] then begin
   log.debug('Delete '+idstr);
   Transfer.RecvFileAbort(id);
   Database.DbAssign(dataf,'chk/'+idstr);
   reset(dataf);
   Dec(UsedSpaceCached,FileSize(dataf));
   close(dataf);
   erase(dataf);
   erase(metaf);
  end else for by:=low(tby) to high(tby) do if by in meta.refs then begin
   inc( stat[by] );
   (*log.debug('r '+IntToStr(by));*)
  end;
 end;
 begin
 (*log.debug('Scan start');*)
 preusage:=usedspace;
 for by:=low(tby) to high(tby) do stat[by]:=0;
 prefix:=DataBase.Prefix+DirectorySeparator+'chkref'+DirectorySeparator;
 rv:=FindFirst(Prefix+'*.dat',0,sr);
 while rv=0 do begin
  (*log.debug('F '+sr.Name);*)
  DoFile;
  rv:=FindNext(sr);
 end;
 FindClose(sr);
 for by:=low(tby) to high(tby) do if stat[by]>0 then
  log.info('Scan result: '+IntToStr(stat[by])+' files kept becouse #'+IntToStr(by));
 (*log.debug('scan finish');*)
 log.info('Deleted '+IntToStr(preusage-usedspace)+' bytes of junk.');
 LastScan:=now;
end;

procedure NotifyTransfer( id :tFID; done,total:longword; by: tBys );
 begin
 if done=total then begin
  Inc(UsedSpaceCached,total);
 end else AddReference(id,cTimeWait);
  {Add a time ref to ensure the file is deleted if something goes wrong}
end;
procedure Init;
 begin
 LastScan:=now;
 UsedSpaceCached:=0;
 {trigger scan?}
end;
procedure NotifyIdle;
 begin
 if (now-LastScan)>cScanPeriod then begin
  DoScan;
 end;
end;
procedure NotifyQuit;
 begin
 DoScan;
end;

END.