unit TestMutable;
interface
uses ServerLoop,MemStream,Mutable;

implementation
uses Store2;
var prid:tFID;
var pro:tSObj;
var ocr:boolean;
var oi:byte;
var sfn:string;

BEGIN
  oi:=OptIndex('-test-mut');
  if (oi>0) then begin
    assert(OptParamCount(oi)=1);
    sfn:=paramstr(oi+1);
    writeln('TestPRC: going to insert ',sfn);
    HashObjectCopy(sfn, pro.fid);
    pro.Init(pro.fid);
    writeln('TestPRC: FileID: ',string(pro.fid),' small=',pro.small,' base=',pro.base);
    ocr:=SetMutable(pro,prid);
    if ocr then writeln('TestPRC: success, loginpubhash=',string(prid))
    else writeln('TestPRC: invalid or corrupt profile file');
  end;
END.
