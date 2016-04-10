UNIT Profile;
{
  structures of User Profile
}
INTERFACE
USES NetAddr,MemStream,ExpOps,SysUtils;

const
  pfHeader:packed array [1..4] of byte=($42,$4E,$50,$1A);
  pfName=2;{1..1}
  pfHost=4;{0..n}
  pfLink=5;{0..n}
  pfMotd=6;{0..1}
  pfSig=127;

type tProfileHeader=record
  Magic:packed array [1..4] of byte;
  Nick:string[11];
  end;
type tProfileField=packed record
  tag:byte;
  len:Word2;
  end;

type tProfileRead=object(tCommonStream)
  mut:boolean;
  blk:tCommonStream;
  tag:byte;
  flen,vofs:LongWord;
  Nick:string[11];
  constructor Init(const initstream:tCommonStream);
  function NextEntry:boolean;
  function NextEntry(itag:byte):boolean;
  procedure Seek(absolute:LongWord); virtual;
  function  Tell:LongWord; virtual;
  function  Length:LongWord; virtual;
  procedure Read(out buf; cnt:Word); virtual; overload;
end;


IMPLEMENTATION

constructor tProfileRead.Init(const initstream:tCommonStream);
  var hd:tProfileHeader;
  begin
  flen:=0;
  vofs:=0;
  blk:=initstream;
  blk.Read(hd,sizeof(hd));
  if CompareWord(hd.magic,cMutHdrMagic,3)=0 then begin
    blk.skip(sizeof(tMutHdr)-sizeof(hd));
    blk.Read(hd,sizeof(hd));
    mut:=true;
  end else mut:=false;
  if CompareDWord(hd.magic,pfHeader,1)<>0
    then raise eFormatError.create('Invalid magic sequence');
  Nick:=hd.Nick;
end;
function tProfileRead.NextEntry:boolean;
  var eh:tProfileField;
  begin
  try
  {skip rest of entry}
  blk.Skip(flen-vofs);
  {read header}
  blk.Read(eh,sizeof(eh));
  except on eReadPastEoF do begin result:=false; exit end end;
  flen:=word(eh.len);
  vofs:=0;
  tag:=eh.tag;
  result:=true;
end;
function tProfileRead.NextEntry(itag:byte):boolean;
  begin
  result:=false;{wtf?}
  repeat
    result:=NextEntry;
  until (not result)or(tag=itag);
end;
procedure tProfileRead.Seek(absolute:LongWord);
  begin
  if absolute>=flen then raise eReadPastEoF.create('Seek out of Bounds');
  vofs:=absolute;
end;
function  tProfileRead.Tell:LongWord;
  begin result:=vofs end;
function  tProfileRead.Length:LongWord;
  begin result:=flen end;
procedure tProfileRead.Read(out buf; cnt:Word);
  begin
  if (vofs+cnt)>=flen then raise eReadPastEoF.create('Read out of Bounds');
  blk.Read(buf,cnt);
  vofs:=vofs+cnt;
end;

END.
