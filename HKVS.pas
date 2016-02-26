UNIT HKVS;
{
 Hash-Key to fixed-length-Value Store
 TODO: store bucket headers at start of file
}
INTERFACE
USES MemStream,NetAddr,SysUtils;

{key is fixed tKey20}
type tHKVS=object
  valsz,bucksz:Word;
  keybuf,keyfre:tKey20;
  valbuf:pointer;
  index:file of byte;
  function GetVal( key: tKey20; out value ):boolean;
  procedure SetVal( key: tkey20; var value );
  procedure Init( filename: string; ivalsz: word; ibktsz:word );
  //procedure Done;
  private{$warning Use some sort of locking}
  function mkrix(key:tKey20):word;
  function mkfofs(bko:LongWord; ri:word):LongWord;
  end;

IMPLEMENTATION

const cMagic:packed array [1..3] of byte=($42,$64,$1A);
type tBktHdr=packed record
  magic:packed array [1..3] of byte;
  depth:byte;
  bktsz:word2;
  valsz:word2;
  other:Word4;
  prefix:tKey20;
  end;

function inBkt(const bkt:tBktHdr; const key:tKey20): boolean;
  begin
  result:=PrefixLength(bkt.prefix,key)>=bkt.depth;
end;
function tHKVS.mkfofs(bko:LongWord; ri:word):LongWord;
  begin
  result:=bko+sizeof(tBktHdr)+(ri*(sizeof(tKey20)+valsz));
end;
function tHKVS.mkrix(key:tKey20):word;
  var ri:DWORD;
  begin
  ri:=      (key[19] or (key[18] shl 8) or (key[17] shl 16) or (key[16] shl 24));
  ri:=ri xor(key[15] or (key[14] shl 8) or (key[13] shl 16) or (key[12] shl 24));
  ri:=ri xor(key[11] or (key[10] shl 8) or (key[09] shl 16) or (key[08] shl 24));
  ri:=ri xor(key[07] or (key[06] shl 8) or (key[05] shl 16) or (key[04] shl 24));
  ri:=ri xor(key[03] or (key[02] shl 8) or (key[01] shl 16) or (key[00] shl 24));
  ri:=ri xor (ri shr 13) xor (ri shr 7) xor (ri shr 23);
  result:=ri mod bucksz;
end;
  

procedure tHKVS.SetVal( key: tkey20; var value );
  var bkt:tBktHdr;
  var ofs,ri:LongWord;
  procedure SplitBucket;
    var nbk:tBktHdr;
    var nof,i:LongWord;
    procedure Toggle(var prefix:tKey20; bit:byte);
      begin
      assert(bit<160);
      prefix[bit div 8]:= prefix[bit div 8] xor ($80 shr (bit mod 8));
    end;
    begin
    writeln('HKVS: Splitting! xD /',bkt.depth);
    {create new bucket}
    inc(bkt.depth);
    nbk:=bkt;
    toggle(nbk.prefix,nbk.depth-1);
    nof:=FileSize(index);
    bkt.other:=nof;
    //writeln('left ',string(bkt.prefix),'/',bkt.depth,' @',ofs);
    //writeln('righ ',string(nbk.prefix),'/',bkt.depth,' @',ofs);
    Seek(index,nof);
    BlockWrite(index,nbk,sizeof(nbk));
    Seek(index,ofs);
    BlockWrite(index,bkt,sizeof(bkt));
    {move items to new bucket}
    for i:=0 to bucksz-1 do begin
      Seek(index,mkfofs(ofs,i));
      BlockRead(index,keybuf,20);
      //write(' - ',string(keybuf),' ');
      if inBkt(nbk,keybuf) then begin
        assert(not inBkt(bkt,keybuf));
        //writeln('move');
        BlockRead(index,valbuf^,valsz);
        Seek(index,mkfofs(nof,i));
        Blockwrite(index,keybuf,20);
        Blockwrite(index,valbuf^,valsz);
        if keybuf<>keyfre then begin
          Seek(index,mkfofs(ofs,i));
          FillChar(valbuf^,valsz,$FF);
          BlockWrite(index,keyfre,20);
          BlockWrite(index,valbuf^,valsz);
        end;
      end else begin
        //writeln('keep');
        Seek(index,mkfofs(nof,i));
        FillChar(valbuf^,valsz,$FF);
        BlockWrite(index,keyfre,20);
        BlockWrite(index,valbuf^,valsz);
      end;
    end;
  end;
  {**MAIN SetVal**}
  begin
  repeat
    ofs:=0;
    repeat
      seek(index,ofs);
      blockread(index,bkt,sizeof(tBktHdr));
      //writeln('bksearch ',string(bkt.prefix),'/',bkt.depth,' @',ofs, 'm',PrefixLength(bkt.prefix,key));
      if inBkt(bkt,key)
        then break
        else ofs:=bkt.other;
    until false;
    {found bucket, find pos in bucket}
    ri:=mkrix(key);
    seek(index,mkfofs(ofs,ri));
    //writeln('write cons bktofs=',ofs,' ri=',ri,' pos=',FilePos(index));
    BlockRead(index,keybuf,sizeof(tKey20));
    //writeln('there is:', string(keybuf));
    if (keybuf=keyfre)or(keybuf=key) then break {free space}
    else SplitBucket;
  until false;
  Seek(index,mkfofs(ofs,ri));
  BlockWrite(index,key,sizeof(tKey20));
  BlockWrite(index,value,valsz);
end;

function tHKVS.GetVal( key: tKey20; out value ):boolean;
  var bkt:tBktHdr;
  var ofs,ri:LongWord;
  begin
  ofs:=0;
  repeat
    seek(index,ofs);
    blockread(index,bkt,sizeof(tBktHdr));
    if inBkt(bkt,key)
      then break
      else ofs:=bkt.other; {note: one bucket must exist for any key}
  until false;
  {found bucket, find pos in bucket}
  ri:=mkrix(key);
  seek(index,mkfofs(ofs,ri));
  //writeln('read from bktofs=',ofs,' ri=',ri,' pos=',FilePos(index));
  BlockRead(index,keybuf,20);
  result:=keybuf=key;
  if result then BlockRead(index,value,valsz);
end;

procedure tHKVS.Init( filename: string; ivalsz: word; ibktsz:word );
  var hdr0:tBktHdr;
  var i:integer;
  begin
  bucksz:=ibktsz;
  valsz:=ivalsz;
  assert(sizeof(tBktHdr)=32);
  assert(sizeof(tKey20)=20);
  FillChar(keyfre,20,$FF);
  valbuf:=GetMem(valsz);
  FillChar(valbuf^,valsz,$FF);
  assign(index,filename);
  try
    ReSet(index,1);
  except on e:eInOutError do begin
    if e.ErrorCode=2 then begin
      ReWrite(index);
      hdr0.magic:=cMagic;
      hdr0.depth:=0;
      hdr0.bktsz:=bucksz;
      hdr0.valsz:=valsz;
      hdr0.other:=$FFFFFFFF;
      FillChar(hdr0.prefix,sizeof(tKey20),0);
      BlockWrite(index,hdr0,sizeof(hdr0));
      for i:=1 to bucksz do begin
        blockwrite(index,keyfre,sizeof(keyfre));
        blockwrite(index,valbuf^,valsz);
      end;
      seek(index,0);
    end else raise;
  end end;
  if FileSize(index)<mkfofs(0,bucksz) then eXception.Create('Database file invalid size');
  try
    blockread(index,hdr0,sizeof(hdr0));
    if CompareByte(hdr0.magic,cMagic,sizeof(cMagic))<>0 then raise eXception.Create('Database file invalid magic');
    if Word(hdr0.valsz)<>valsz then raise eXception.Create('Database field size mismatch');
    if Word(hdr0.bktsz)<>bucksz then raise eXception.Create('Database bucket size mismatch');
  except close(index); FreeMem(valbuf); raise end;
end;

END.
