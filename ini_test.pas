{$MODE ObjFPC}
uses Classes,inifiles,SysUtils;

var ini: tIniFile;
var inis: tFileStream;
var sec: tStringList;

begin
  inis:=tFileStream.Create('bn.cfg',fmOpenRead);
  ini:=tIniFile.Create(inis,[{ifoStripComments,}ifoStripQuotes{,ifoStripInvalid}]);
  ini.WriteString('main','var','---');
  sec:=tStringList.Create;
  ini.ReadSectionValues('list',sec,[svoIncludeInvalid]);
  //ini.UpdateFile;
  writeln('o: ',ini.ReadString('main','var','---'));
  writeln('O: ',ini.ReadString('main','','---'));
  writeln('p: ',ini.ReadString('','nosect','---'));
  writeln('s: ',sec.commatext);
end.
