unit Tracker;
{
 A tracker protocol on top of DHT.
}

INTERFACE
uses ServerLoop,ObjectModel,ECC,sha512,DHT;


IMPLEMENTATION
uses opcode,gitver,sysutils;

{Server Impl:
 Requesthandler
 - yes, keep data in memory, LL popularity ordering
 - save and purge periodically
}

END.
