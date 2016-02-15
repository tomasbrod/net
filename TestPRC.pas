unit TestPRC;
interface
uses MemStream,ProfileCache;

implementation
uses Store2;
var fid:tFID;
var ocr:boolean;

BEGIN
  writeln('TestPRC: going to insert profile.dat');
  HashObjectCopy('profile.dat', fid);
  writeln('TestPRC: FileID: ',string(fid));
  ocr:=CacheProfile(fid);
  writeln('TestPRC: verify result: ',ocr);
END.
