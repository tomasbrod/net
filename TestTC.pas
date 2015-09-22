unit TestTC;
INTERFACE
IMPLEMENTATION
USES ServerLoop
    ,TC
    ,MemStream
    ,NetAddr
    ,SysUtils
    ;
type t=object
 tcs:TC.tTCS;
 cnt:byte;
 buf: array [1..4096] of char;
 procedure CanSend;
end;

procedure t.CanSend;
 var s:tMemoryStream;
 var size:word;
 begin
 s.Init(@buf,0,4096);
 size:=tcs.MaxSize(4096);
 tcs.WriteHeaders(s);
 if size>s.size then size:=s.size;
 s.Skip(size-1);
 s.WriteByte(9);
 tcs.Send(s);
end;

procedure Test;
 var o:^t;
 var oi:word;
 const opt='-test-tc';
 begin
 oi:=OptIndex(opt);
 if oi>0 then begin
  assert(OptParamCount(oi)=1,opt+'(rcpt:tNetAddr) '+IntToStr(OptParamCount(oi)));
  New(o);
  with o^ do begin
   cnt:=0;
   tcs.Init(paramstr(oi+1));
   tcs.CanSend:=@CanSend;
   tcs.Start;
  end;
 end;
end;
        
BEGIN
 test;
END.