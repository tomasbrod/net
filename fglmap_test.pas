uses Classes,FGL,SysUtils;

type tProc=procedure(a:byte);

var smap:TFPSMap;

procedure proc1(a:byte);
  begin
  writeln('proc1');
end;
procedure proc2(a:byte);
  begin
  writeln('proc2');
end;

var i:integer;
var kp,vp:pointer;

var w:word;
var r:tProc;

begin
  smap:=TFPSMap.Create(sizeof(Word),sizeof(tProc));
  smap.Duplicates:=dupError;
  smap.Sorted:=true;
  w:=68; r:=@proc1; smap.Add(@w,@r);
  w:=12; r:=@proc2; smap.Add(@w,@r);
  writeln('proc1=',PtrUInt(@proc1));
  writeln('map contents:');
  for i:=0 to smap.count-1 do begin
    kp:=smap.keys[i];
    vp:=smap.data[i];
    writeln(' ',word(kp^),'->',PtrUInt(vp^));
    writeln(' KeyData[',word(kp^),'] = ',PtrUInt(smap.KeyData[kp]^));
    write(' calling KeyData[]: ');
    tProc(smap.KeyData[kp]^)(9);
  end;
  w:=99;
  kp:=@w;
    writeln(' IndexOf[',word(kp^),'] = ',smap.IndexOf(kp));
    writeln(' KeyData[',word(kp^),'] = ',PtrUInt(smap.KeyData[kp]^));
  
end.
