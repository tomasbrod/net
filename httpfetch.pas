{program}
{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpclient;

var
  i : Integer;
  ost: tHandleStream;
  lbuf:ansistring;

function m:byte;
begin
  if (ParamCount<>1) then
    begin
    writeln('Usage : ',ExtractFileName(ParamStr(0)), ' URL');
    writeln('stdout=body, headers discarded, status 0 OK, 1 err, 2 exception, more');
    exit(1);
    end;
  ost:=tHandleStream.Create(1); {cannot really fail}
  With TFPHTTPClient.Create(Nil) do
    try
      try
       Get(ParamStr(1),ost);
      except
       writeln('exception');
       exit(2);
      end;
      {
      Writeln('Response headers:');
      For I:=0 to ResponseHeaders.Count-1 do
        Writeln(ResponseHeaders[i]);
      }
      exit(0);
    finally
      Free;
      ost.Free;
    end;
end;
BEGIN
halt(m);
end.

