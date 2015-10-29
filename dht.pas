unit DHT;
{
 implementation of custom dht, based on pastry and kademlia.
 keyspace is divided into buckets of limited capacity
 node belongs to bucket, where at least 'depth' bits match 'prefix'
}

{used by: messages, fileshare}

INTERFACE
uses NetAddr;
type tPID=array [0..19] of byte;
var MyID:tPID;
procedure AddNode(const contact:tNetAddr);
procedure Get(const id:tPID; out result:array of tNetAddr);

IMPLEMENTATION

END.