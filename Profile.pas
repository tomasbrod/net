UNIT Profile;
{
  structures of User Profile
}
INTERFACE
USES NetAddr,MemStream;

const
  pfHeader:LongWord=$EB4A8A65;
  pfName=2;
  pfHost=4;
  pfLink=5;
  pfMotd=6;
  pfSig=127;

type tProfileHeader=record
  Magic:Word4;
  LoginPub:tKey32;
  Updated:Word4;
  UpdatedBy:byte;
  Nick:string[11];
  end;
type tProfileSig=packed record
  Len1:byte; {0}
  Len2:byte; {65}
  tag:byte; {pfSig=127}
  Sig:tKey64;
  end;

IMPLEMENTATION

END.
