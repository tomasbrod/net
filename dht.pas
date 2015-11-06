unit DHT;
{
 implementation of custom dht, based on pastry and kademlia.
 keyspace is divided into buckets of limited capacity
 node belongs to bucket, where at least 'depth' bits match 'prefix'
 old>new,
 new>dead
}

{used by: messages, fileshare}

INTERFACE
uses NetAddr;
type tPID=array [0..19] of byte;
var MyID:tPID;
procedure AddNode(const contact:tNetAddr);
procedure Get(const id:tPID; out result:array of tNetAddr);

IMPLEMENTATION
uses dhroute;

{communication:
 ping-pong;
 request for ... key
 ...: node, file, profile, ?dir
  profile: key, firends, dirs
 nodes
 values
}

END.