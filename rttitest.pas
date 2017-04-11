program rttitest;

uses
    TypInfo,
    ObjectModel;
type tKey20=packed array [0..19] of byte;
type
    TMyRec2= packed record
      a,b:LongWord;
    end;
    TMyRec = record
        k :tKey20;
        p1: Integer;
        p2: string;
        b: byte;
        sb: shortint;
        w: word;
        sw: smallint;
        d: dword;
        sd: longint;
        q: qword;
        sq: int64;
        cust: -15..33000;
        p:^byte;
        rec: tMyRec2;
        arr: array [1..3] of byte;
        dynarr: array of word;
    end;

procedure DumpTypeInfo(ti: PTypeInfo);
  var
    td: PTypeData;
    mf: PManagedField;
    i: integer;
  begin
    Writeln('Type name: ',ti^.Name);
    Writeln('Type kind: ',ti^.Kind);
    td := GetTypeData(ti);
    case ti^.Kind of
      tkUnKnown, tkLString, tkWString, tkVariant, tkUString: writeln('- some string');
      tkAString: writeln('- ansi string');
        {CodePage: Word;	Codepage for a single-byte string type}
      tkInteger, tkChar, tkEnumeration, tkBool, tkWChar: begin
        writeln('- ordinary');
        writeln('-- OrdType: ',td^.OrdType);
        writeln('-- MinValue: ',td^.MinValue);
        writeln('-- MaxValue: ',td^.MaxValue);
                    {NameList: ShortString;}
        end;
      tkSet: begin
        writeln('- set type');
        writeln('-- OrdType: ',td^.OrdType);
        writeln('-- CompType: ',td^.CompType^.Name);
        end;
      tkFloat: begin
        writeln('- floaring point');
        writeln('-- FloatType: ', td^.FloatType);
        end;
      tkSString:  begin
        writeln('- short string');
        writeln('-- MaxLength: ', td^.MaxLength);
        end;
      tkRecord: begin
        writeln('- record');
        writeln('-- RecSize: ',td^.RecSize);
        writeln('-- ManagedFldCount:', td^.TotalFieldCount);
        mf := pointer(@td^.TotalFieldCount)+SizeOf(td^.TotalFieldCount);
        for i:=0 to td^.TotalFieldCount-1 do begin
          writeln('-- field ',i,' at offset ',mf[i].FldOffset);
          DumpTypeInfo(mf[i].TypeRef);
        end;
      end;
      tkInt64: writeln('- int64');
          {MinInt64Value: Int64;
          MaxInt64Value: Int64;}
      tkQWord: writeln('- qword');
          {MinQWordValue: QWord;
          MaxQWordValue: QWord;}
      tkArray: writeln('- array');
          {ArrayData: TArrayTypeData;}
      tkDynArray: writeln('- dynamic array');
          {elSize: PtrUInt;
          elType2: PTypeInfo;
          varType: LongInt;
          elType: PTypeInfo;
          DynUnitName: ShortStringBase;}
      tkPointer: writeln('- pointer');
        {RefType: PTypeInfo;}
      else writeln('Invalid TypeKind');
    end;
end;

procedure Serialize( s: tStream; var data; ti: tTypeInfo);
  var td: PTypeData;
  var mf: PManagedField;
  var i: integer;
  var bs: Pointer;
  begin
  bs:=@data;
  assert(assigned(bs));
  assert(assigned(ti));
  assert(assigned(s));
  Writeln('Serializing: name=',ti^.Name,' kind=',ti^.Kind);;
  td := GetTypeData(ti);
  case ti^.Kind of
    {tkUnKnown, tkLString, tkWString, tkVariant, tkUString: writeln('- some string');}
    {tkAString:}
      {should CodePage:Word;  be considered?}
    tkInteger, tkChar, tkEnumeration, tkBool, tkWChar: begin
      case td^.OrdType of
        otUByte: s.W1(bs^);
        otUWord: s.W2(bs^);
        otULong: s.W4(bs^);
        ,otUQWord...
        otSByte,otSWord,otSLong,otSQWord
      writeln('- ordinary');
      writeln('-- OrdType: ',td^.OrdType);
      writeln('-- MinValue: ',td^.MinValue);
      writeln('-- MaxValue: ',td^.MaxValue);
                  {NameList: ShortString;}
      end;
    tkSet: begin
      writeln('- set type');
      writeln('-- OrdType: ',td^.OrdType);
      writeln('-- CompType: ',td^.CompType^.Name);
      end;
    tkFloat: begin
      writeln('- floaring point');
      writeln('-- FloatType: ', td^.FloatType);
      end;
    tkSString:  begin
      writeln('- short string');
      writeln('-- MaxLength: ', td^.MaxLength);
      end;
    tkRecord: begin
      writeln('- record');
      writeln('-- RecSize: ',td^.RecSize);
      writeln('-- ManagedFldCount:', td^.TotalFieldCount);
      mf := pointer(@td^.TotalFieldCount)+SizeOf(td^.TotalFieldCount);
      for i:=0 to td^.TotalFieldCount-1 do begin
        writeln('-- field ',i,' at offset ',mf[i].FldOffset);
        DumpTypeInfo(mf[i].TypeRef);
      end;
    end;
    tkInt64: writeln('- int64');
        {MinInt64Value: Int64;
        MaxInt64Value: Int64;}
    tkQWord: writeln('- qword');
        {MinQWordValue: QWord;
        MaxQWordValue: QWord;}
    tkArray: writeln('- array');
        {ArrayData: TArrayTypeData;}
    tkDynArray: writeln('- dynamic array');
        {elSize: PtrUInt;
        elType2: PTypeInfo;
        varType: LongInt;
        elType: PTypeInfo;
        DynUnitName: ShortStringBase;}
    tkPointer: writeln('- pointer');
      {RefType: PTypeInfo;}
    else writeln('Invalid TypeKind');
  end;

var r: TMyRec;

begin
    r.p1 := 312;
    r.p2 := 'foo-bar';

    DumpTypeInfo(TypeInfo(r));

    {
    mf := p; // And now in the mf we have data about first record's field
    Writeln(mf^.TypeRef^.Name);

    Write(r.p1); // Current value
    f := @r;
    Inc(f, mf^.FldOffset); // Point to the first field
    Integer(f^) := 645; // Set field value
    Writeln(r.p1); // New value

    // Repeat for the second field
    Inc(p, SizeOf(TManagedField));
    mf := p;
    Writeln(mf^.TypeRef^.Name);

    Write(r.p2);
    f := @r;
    Inc(f, mf^.FldOffset);
    string(f^) := 'abrakadabra';
    Writeln(r.p2);
    }
end.
