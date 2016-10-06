
constructor tConfigFile.Init(var fs:tFileStream);
  var l:LongWord;
  var s:pchar;
  var a:ppchar;
  var ps,pe:pchar;
  var i:integer;
  begin
  secarr:=nil;
  l:=fs.Length;
  s:=GetMem(l+2);
  a:=GetMem(1024*sizeof(pointer));
  fs.Read((s+1)^,l);
  s[0]:=#10;
  s[l]:=#0;
  assert(strlen(s)=l);
  a[0]:=s;
  i:=0;
  pe:=s;
  repeat
    ps:=strpos(pe,#10#91);
    if ps=nil then break;
    ps[0]:=#0;
    ps:=ps+2;
    pe:=strpos(ps,#93);
    if pe=nil then break;
    pe[0]:=#0;
    pe:=pe+1;
    a[i+1]:=ps;
    i:=i+1;
  until false;
  secarr:=GetMem(sizeof(pchar)*(i+2));
  Move(a^,secarr^,sizeof(pchar)*(i+1));
  FreeMem(a);
  secarr[i+1]:=nil;
end;

function tConfigFile.GetSection(name:pchar):pchar;
  var i:integer;
  var r:pchar;
  var nl,l:longint;
  begin
  nl:=strlen(name);
  if nl>0 then begin
    i:=1;
    while assigned(secarr[i]) do begin
      r:=secarr[i];
      l:=strlen(r);
      if (l=nl) and (StrIComp(r,name)=0) then begin
        r:=r+l+1;
        result:=r;
        exit;
      end;
      i:=i+1;
    end;
    result:=nil;
  end
  else GetSection:=secarr[0];
end;
destructor tConfigFile.Done;
  begin
  FreeMem(secarr[0]);
  FreeMem(secarr);
end;
constructor tConfigSection.Init(var cfg:tConfigFile; name:pchar);
  begin
  sect:=cfg.GetSection(name);
  line:=sect;
end;
procedure tConfigSection.Reset;
  begin
  line:=sect;
end;
function tConfigSection.GetKey(name:string):string;
  var k,e:pchar;
  var nd:ansistring;
  begin
  result:=#1;
  nd:=#10+name+'=';
  k:=strpos(sect,pchar(nd));
  if k=nil then exit;
  k:=k+length(name)+2;
  e:=strpos(k,#10);
  if e=nil then e:=strend(k);
  result:=copy(k,1,e-k);
end;
function tConfigSection.GetLine:string;
  var l,e:pchar;
  begin
  e:=line;
  repeat
    l:=e;
    if l=nil then break;
    e:=strpos(l,#10);
    if e<>nil then e:=e+1;
  until (e=nil)or( (e>(l+1))and(l[0]<>';') );
  line:=e;
  if l<>nil then begin
    if e=nil then result:=l
    else result:=copy(l,1,(e-l)-1);
  end else result:='';
end;
