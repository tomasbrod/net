uses HKVS,MemStream;
var db:tHKVS;
var hodnota,h2:string[19];
var kluc:tKey20;

BEGIN
	db.Init('tets-hkvs.dat', 20);
  hodnota:='Bugs should be repo';
  kluc:='E205D4BF17A85B67B2440FFC83A223D01F98126D';
	db.SetVal(kluc,hodnota);
  kluc:='A205D3BF17A85B67B2440FFC83A223D01F98125D';
	db.SetVal(kluc,hodnota);
  kluc:='3205D2BF17A85B67B2440FFC83A223D01F98121D';
	db.SetVal(kluc,hodnota);
  if db.GetVal(kluc,h2)
    then writeln('Value found: ',h2)
    else writeln('Value not found');
END.
