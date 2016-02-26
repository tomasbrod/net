UNIT Profile;
{
  structures of User Profile
}
INTERFACE
USES NetAddr,MemStream;

const
  pfHeader:packed array [1..4] of byte=($42,$4E,$50,$1A);
  pfName=2;
  pfHost=4;
  pfLink=5;
  pfMotd=6;
  pfSig=127;

type tProfileHeader=record
  Magic:packed array [1..4] of byte;
  LoginPub:tKey32;
  Update:Word4;
  pad:byte;
  Nick:string[11];
  end;
type tProfileField=packed record
  len:Word2;
  tag:byte;
  end;
type tProfileSig=packed record
  Len1:byte; {0}
  Len2:byte; {65}
  tag:byte; {pfSig=127}
  Sig:tKey64;
  end;

IMPLEMENTATION

END.
