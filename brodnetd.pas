PROGRAM brodnetd;

uses cmem,cthreads,ServerLoop, ObjectModel
  ,Database
	,OTServer
	,DHT
  ,dhlt
//  ,Mutable
	;

{$INCLUDE gitver.inc}
const LVER_MINOR: integer= VER_MINOR; public;
const LVER_PATCH: integer= VER_PATCH; public;
const LVER_HASH: shortstring= VER_HASH; public;
const LVER_CLEAN:boolean= VER_CLEAN; public;
const LVER_BUILD: integer= VER_BUILD; public;
//const LVER_BRANCH='dev';
//const LVER_USER='tomas@manganp';

BEGIN
// Database.FreeKeyList;
 ServerLoop.Main
END.
