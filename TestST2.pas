USES ObjectModel,Store2,sysutils;

var str:string;
var so:tSObj;
var ms:tMemoryStream;

BEGIN
  writeln('Test HashObject Stream');
  ms.Init(69);
  ms.WriteShortString('Hello, World!');
  ms.seek(0);
  so.HashObjectStream(ms);
  ms.Free;
  writeln(so.ReadShortString);
  so.Reference(-1);
  try
    so.Init(so.fid);
    writeln('Object found after unref xD');
  except
    on eObjectNF do;
  end;
  writeln('Test HashObject Reference');
  so.HashObjectLinkOrRef('/etc/hostname');
  writeln(so.ReadStringAll);
  so.Reference(-1);
  try
    so.Init(so.fid);
    writeln('Object found after unref xD');
  except
    on eObjectNF do;
  end;
END.
