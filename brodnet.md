BRODNET {wip}
=======

BrodNet is an Information sharing Peer to Peer program.

Expectations
------

* usable behind NAT and Firewalls
* low overhead _binary_ protocol
* distributed lookup using DHT
* p2p file transfer
* reliable messaging
* personal web pages
* message feeds
* directories
* keyword searching

Architecture
------

Daemon running in background.
Connand-line and graphical user interfaces.

What is done [done]
------

* Default UDP port: 3511.
* Secure DHT (at least protocol is secure, unsing ECC and PoW).
* File storage
* File Transfer over datagram connection
* minimal Command line interface

Hostlist [hostlist]
--------

Experimental HTTP hostlist. currently abandoned

See [current state](hostlist.php) of the hostlist. Hostlist url for brodnetd
is: `http://brod.holes.sk/comp.net/hostlist.php`.

Cmd interface for file access
--------

### User to Daemon

* must be read and hashed

* local copy
  - PUTCP(filename)
  - user
  - on btrfs/zfs use cp --reflink
* local linking
  - PUTLN(filename)
  - soft_link in object database
  - or unix hardlink
  - chmod
  - hash on every load
* local rename
  - PUTMV(filename)
  - fetch tmp
  - daemon running under user account
* net copy
  - PUT

### Daemon to User

* local copy
  - GETCP
  - on btrfs/zfs use cp --reflink
* local link
  - GETLN
  - unix hard link
* net copy
  - GET

Protocol Specification [proto]
--------

**NOTE: This description is currently ahead of implementation!**

Protocol is datagram based. Datagram can arrive whole, multiple times,
delayed or not at all.
All words are in big endian byte order. Numbers in text are decimal
unless prefixed with '$' then hexadecimal or '%' then binary.

First byte of Datagram is Opcode.
If Opcode is less than 128 then it is simple dgram and handled  by this opcode number.
Refer to opcode.pas for opcode symbolic names.
If Opcode is 128 and greater then it is reply datagram and handled by transaction
number encoded in next 2 bytes.

### Other structures [proto-struct]

New netowrk address format:

~~~
type tNetAddr is record {ObjectModel.pas}
  port : 2 byte word;
  if ip4 then
    ip4_prefix: 12 byte = 00000000000000000000FFFF;
    ip4 : 4 byte IPv4 address;
  end
  if ip6 then
    ip6 : 16 byte IPv6 address;
  end
end 18B

type timestamp is 2 byte word = trunc( CurrentDay - '1st January of 2010' );
~~~

### ot protocol [proto-ot]

Object Transfer protocol.
otData=op$08 otCtrl=op$09 ObjTrans.pas OTServer.pas Fetch.pas

Client downloads file from server. Transfer is initiated by client with
[otCtrl](#proto-otCtrl) datagram. Server then replies with otData datagrams.
All file data and auxilary messages from server to client go with
[otData](#proto-otData) opcode.
Protocol supports up to 255 channels to download multiple files from
same server. Channels can be assigned relative priorities.
Multisource downloading is supported, but requires smarter client.
Partial downloads are supported with segments (offset+length). Files up
to 512GB are supported. Up to 18 segments of 4GB total can be requested
at once. There are no unnecessary handshakes, data transfer starts
after just one round-trip!

To start transfer clients sends otCtrl datagram with
minor opcode otReq + priority, channel is arbitrary to identify replies,
file_id is sha256 truncated to 20 bytes. To request whole file
set offset to 0 and lenght to $FFFFFFFF.
After every request server should reply with info dgram
(opode=otData,channel as specified in reqest, offset_high=otINFO).
The length field in info is total lengt of the file regardles of
requested segments.
Field max_seg_req is number of segment that can be requested in single
request, more segments are ignored. Field server_socket_ttl is the TTL
of datagrams while leavind server, useful for prioritizing sources in
multisource downloading.
The data of the requesed file are send in normal otData datagrams.
If the requested file was not found otNotFound is sent instead of otInfo.
On other server failure otFail with error code and message is sent.

When at least 8 data packets arrived from a server (independent of
channel number) and at least 400 ms passed then the client must send
otCtrl/otSPEED, the rate field is round((RcvdByte/TimeMS)*16).
Server then adjusts it's sending rate and/or datagram size to utilize
maximum of network bandwidth and not overload it.
When otData/otSINC datagram is recieved, client must reply with
otCtrl/otSIACK, mark set same as otSINC mark and slen equal to size
of the otSINC datagram. The server adjusts transfer parameters and
may send otData/otRateInfo with debug info about the adjust.

When the server has no more data to send, it sends otData/otEoT so
client can proceed with new requests. This datagram must be resent
at least 6 times and if no requests appear, the channel is closed.
The channel can be explicity closed with otCtrl/otFin.
Also otFin must be sent if client recieves an unexpected datagram.

Since the network connection is unreliable, the client should
utilize timeouts and resend requests. Often a piece in middle of file
is lost, client should include this piece in the next request.
Request to channel where transfer is active aborts that transfer, but
the datagrams may still be in transit and arrive mixed. Therefore client
should not reuse channels for other files immediately.




When client recieves enough packets it sends report.
When different mark -> reset packet counters.
Server waits some time for the report based on sending speed and estimated rtt.
If no report arrives, rates are reset to slow and later the session is closed.

ECN: Send otData datagrams as ecn enabled. When CN marked packet is recieved, send
otInfo immediately with CN flag set.

#### otCtrl [proto-otCtrl]

~~~
datagram otRequest is
  opcode : byte = $04;
  channel : byte;
  priority: byte;
  flags : 1 byte;
  file_id : 24 byte;
  segments : array [until end of datagram] of
    offset : 5 byte word;
    length : 4 byte word;
  end
end

datagram otReport is
  opcode : byte = $05;
  mark : byte;
  flags: byte;
  server_socket_ttl : byte;
  rate : 4 byte word = round((ByteCnt/(mNow-mStart))*16);
  slen : 2 byte word;
end

datagram otStop is
  opcode : byte = $06;
  channel : byte;
end

datagram otData is
  opcode : byte = $08;
  mark : 1 byte;
  channel : byte;
  offset : 5 byte word;
  data : byte [left];
end;

datagram otInfo is
  opcode : byte = $07;
  channel : byte;
  flags : 1 byte;
  error_code : byte;
    0 OK
    1 End of Transmission
    2 Debug message
    3 Object Not Found
    4 Channel out of range
    5 Storage Error
    6 Overload
    7 other failure
    8 Use Multicast
    9 Use TCP
  max_channels : byte;
  max_segments : byte;
  server_socket_ttl : byte;
  if channel<>0 then
    file_id : 24 byte;
    file_length : 5 byte word;
    req_length : 4 byte word;
  end
  message : char to end of datagram;
end

datagram otDataSync is
  opcode : byte = $09;
  mark : 1 byte;
  channel : byte;
  offset : 5 byte word;
  data : byte [left];
end;

~~~

TODO: Update the text to reflect protocol changes. Implement protocol changes.
Fix error near ObjTrans.pas:382.

For multicast: flag in request, pick group(?) 239/8, special info with mcast address,
special data with stream identification.

### dht protocol [proto-dht]

Distributed Hash Table protocol.
dhtBeatQ=op$0A dhtBeatR=op$0B dhtCheckQ=op$0C dhtCheckR=op$0D
dhtCheckS=op$0E dhtResult=op$0F dht.pas dhtLookup.pas dhtBoot.pas

This DHT is simillar to Kademlia and bittorrent DHT, except it uses
different encoding. Working of Kademlia are not described here (yet).
Node IDs are 160 bit or 20 byte sha512 hashes of node public key. This
ID selection is enforced by challenge-response authentication with
optional proof-of-work (dhtCheck). Multiple query types are supported:
each type has own query and result opcodes.
Maitenace queries are separated from real queries.

For Pings and routing Bucket maintenace [dhtBeat](#proto-dhtBeat)
operations are used.
Sender field is always node ID of sening node. Target is the query
target and mark is arbitrary 2 byte transaction identifier.
dhtBeatQ results always in dhtBeatR unless the sending node is banned.
There are no pings, beat requests are used instead with the benefit of
additional node discovery.

Every new node inserted to routing table should be C-R authenticated.
Auth sequence is initiated with [dhtChekQ](#proto-dhtCheck)
sent by Cnode to Rnode.
PublicKey field is key of Cnode, ProofOfWork of Cnode, Challenge is
random bytes and version is string. Rnode validates the PoW and
calculates the response as sha512 hashe of the Challenge and shared
secret calculated from Rnode private and Cnode public keys. Rnode
replies with dhtCheckR: pow, pub and version of Rnode and the
calculated Response. Cnode then verifies the PoW and response.

Normal DHT queries are performed usding specialized opcodes but each must
include fields from dhtGetNodes below. Responses are sent with opcode >$80 and
same transaction id as request. If no values are available, list of other nodes
is sent instead (dhtNodes). [dhtGetNodes](#proto-dhtGetNodes) request never
returns values, usefull to search for nodes. Any new nodes discovered as
byproduct of search must be inserted into routing table if they fit.

#### DHT Services

* Node search
* Message
* Tracker / Swarm
* Profile
* Small Data (8k data stored under arbitrary key)

#### dhtBeat [proto-dhtBeat]

~~~
datagram dhtBeatQ is {beat query}
  opcode : byte = $0A;
  sender : 20 byte;
  target : 20 byte;
  mark : 2 byte;
  server_socket_ttl : byte;
end

datagram dhtBeatR is {beat reply}
  opcode : byte = $0B;
  sender : 20 byte;
  mark : 2 byte;
  server_socket_ttl : byte;
  nodes : array [until end] of
    addr : tNetAddr 18B;
    id : 20 byte;
  end
end
~~~

#### dhtCheck [proto-dhtCheck]

~~~
datagram dhtCheckQ is
  opcode : byte = $0C;
  PublicKey : 32 byte;
  ProofOfWork : 36 byte record of
    data : 32 byte;
    timestamp: 2 byte word;
  end
  Challenge : 32 byte;
  Version : char to end of datagram;
end

datagram dhtCheckR is
  opcode : byte = $0D;
  PublicKey : 32 byte;
  ProofOfWork : 36 byte;
  Response : 32 byte;
  Version : char to end of datagram;
end
~~~

Add QueryForwardResponse for Checks.

#### dhtGetNodes [proto-dhtNodes]

~~~
datagram dhtGetNodes is
  opcode : byte = $10;
  trid : 2 byte;
  sender : 20 byte;
  target : 20 byte;
  {other requests have more fields here}
end

datagram dhtNodes is
  opcode : byte = $90;
  trid : 2 byte;
  nodes : array [until end] of
    addr : tNetAddr 18B;
    id : 20 byte;
  end
end
~~~

#### dhtTestQuery [proto-dhtTest]

~~~
datagram dhtTestQuery is
  opcode : byte = $11;
  trid : 2 byte;
  sender : 20 byte;
  target : 20 byte;
  msg    : char[] = 'Hello!'
end

datagram dhtTestResult is
  opcode : byte = $91;
  trid : 2 byte;
  msg    : char[] = 'Schmeterling!'
end
~~~

### Tracker [proto-tk]

DHT service. Used to find who has a resource and announce nodes that
has the resource.

tkQuery returns tkResult. tkAnnounce returns dhtNodes or saves sender id
and addr and returns tkAnnOK, useful when doing dhtQuery.
For peer exchange set pex flag on tkAnnounce, returns tkResult.

~~~
type flags is 1 byte bitpacked record
  bits 4..0 is set of
    exchange : (announce) return tkResult
    direct   : (query) only direct references
    recent   : (query) only announced in last 5m
    longterm : (announce) keep indirect ref for longer
    longterm : (query) only announced with this flag
  end
  bits 7..5 is enumerated (info for stats, queries only)
    0: other
    1: file    : looking for files
    2: index   : looking for index files
    3: mutable : looking for mutable
    4: group   : looking for network group
  end
end

datagram tkQuery
  opcode : byte = $14;
  trid : 2 byte;
  sender : 20 byte;
  target : 20 byte;
  flags  : byte;
end

datagram tkResult
  opcode : byte = $94;
  trid : 2 byte;
  addr_c : byte; {highest bit set if has the resource}
  hit_period : byte;
  server_socket_ttl : byte;
  direct : array [addrc] of tNetAddr 18B;
  indir  : array [] of
    pid  : 20 byte node id;
  end
end

datagram tkAnnounce
  opcode : byte = $15;
  trid : 2 byte;
  sender : 20 byte;
  target : 20 byte;
  flags  : byte;
  server_socket_ttl : byte;
end

datagram tkAnnOK
  opcode : byte = $95;
  trid : 2 byte;
  hit_period : byte;
  nodes : array [until end] of
    addr : tNetAddr 18B;
    id : 20 byte;
  end
end

datagram trackerQueryOrig
  opcode : byte = $?
  trid : 2 byte;
  sender : 20 byte;
  target : 20 byte;
  flags  : byte;
end
datagram trackerQueryFwd
  opcode : byte = $?
  trid : 2 byte;
  orig: tNetAddr 18B;
  sender : 20 byte;
  target : 20 byte;
  flags  : byte;
end
~~~

- UC 1: get peers from dht
  - DHT(tkQuery) -> dhtNodes + tkResult
- UC 2: get more peers from peers
  - (direct) tkQuery -> tkResult
- UC 3: announce
  - tkQuery with Announce flag set
  - to known peers and dht

OP -> nA -> nB   Q1 Q2
 ^ <-------/     R

### Profile [proto-prof]

User Profile service is separate from generic mutable becouse it is provides
much more important. User profile is stored in nodes that have ID closest to
the profile ID. Also on nodes in the User's swarm. Node where profile ID
belongs to its own DHT bucket must store the profile.

To update profile from network, execute DHT query with profQuery datagram with
+target = ProfID and +update +file_id = version of profile that you already
have, zero othervise. When contacted node has newer version than you,
profResult is returned, else dhtNodes. After the search is exhausted, fetch the
latest version (with fallback). After the profile is fetched and verified, send
queries to nodes that repoted older versions than the current but never than
your previous. Update is published the same way.

Stage 1 implementation:
Every node that learns of a profile will fetch, verify and store it.

~~~
datagram profQuery
  opcode: byte;
  trid: 2 byte;
  sender: 20 byte;
  target: 20 byte;
  file_id : 20 byte;
  update: 6 byte word;
end

datagram profResult is
  opcode: byte;
  trid: 2 byte;
  file_id : 20 byte;
  update: 6 byte word;
  server_socket_ttl : byte;
  nodes : array [until end] of
    addr : tNetAddr 18B;
    id : 20 byte;
  end
end
~~~

Master key is used to certify

Profile file format.

~~~
file Profile is
  signed_data:
    magic: 7B = 'BNProf'#26;
    format: 1B = 6;
    nick: 12B string;
    updated: 6B timestamp;
    signed_data_2:
      expires: 6B timestamp;
      master_key: 32B;
      sign_key: 32B;
    master_sig: 64B signature of signed_data_2;
    encr_key: 32B;
    reserved: 32B;
    old_encr_keys: array[ 1B ] of 32B;
    fullname: string[ 1B ];
    hosts: array[ 1B ] of 20B;
    backup: array[ 1B ] of 20B;
    textnote: string[ 1B ];
  signature: 64B signature of signed_data;
~~~

### Message [proto-msg]

Messages are delivered as a notice only and the recipient retrives the message
file using [ot](#proto-ot) protocol. Sender tries to contact nodes in the
following order:

* the node with recipient ID
* additional nodes from profile
* backup nodes from profile
* friends of the sender (?)
* any node along the DHT query to recipient node

Additionally any node contacted during the query may offer to store
just the notice and forward it as soon as it hears from recipient. This
is most effective for nodes closest to recipient ID or additional host
ID. Becouse when a node goes online, it searches for its ID to populate
its routing buckets and has high chance to hit nodes that were closest
during the query.

When recipient, additional(?) or backup node fully recieves a message it
creates and signs a reception notice and send it as a reply for next
message queries. When a node not higher in the above list recieves this
reception notice it should delete that message and keep forwarding the
notice for a while. This prevents delivered messages from wasting
storage space.

When no reception signature is available then msgResult is used instead of msgReceipt.
Client then can use the provided nodes to advance it's DHT search.

For example (A) Friend of sender contacts a (B) backup node about some
message but B already has that message (or a reception notice). B
replies with a reception notice. A deletes that message and keeps the
notice.

Nodes specified in recipient's profile must be configured to accept
messages for it (by the profile ovner).

After period of inactivity reception notices are deleted.
Old undelivered messages can be deleted too but after much longer

About the flags: Dont store: This message must not be fetched, stored nor
forwarded by nodes along the path. Notices should be stored. Original sender:
message was generated on this node. Notification: Message for target node was
last seen at last_avl_at. This datagram is only sent when this node is
contacted by target node. When this datagram is received by node other than
target, msgResult is sent with Not Target state, no aditional contacts, and
must not be processed further. Transfer: Synchronisation of users devices.
Don't store flag must also be set. Automatic: Message was generated
automatically. For stats counting.

Original sender must keep the message and search for recipients until a valid
reception notice is received from the recipient node or the message expires.

~~~
datagram msgQuery is
  opcode : byte = $18;
  mark : 2 byte;
  flags: 1 byte set of
    0: store please
    1: original sender
    2: notification (last_avl_at is not nil)
    3: transfer
    4: automatic
  sender : 20 byte;
  target : 20 byte;
  message_fid : 20 byte;
  rcpt_id: 20 byte;
  rcpt_prof_ver: 6 byte word;
  last_avl_at: tNetAddr;
  server_socket_ttl : byte;
end (108 bytes)

type msg_state is 1 byte enumerated of
  0: internal server error
  1: message too old
  2: message invalid
  5: wont store, reject notify
  6: wont store, would notify
  7: wont store, will notify
  10: would store, would notify
  11: would store, will notify
  12: will store as stranger
  13: will store as backup
  16: will store as ovner
  17: already stored
  24: already delivered to backup
  25: already delivered
  26: not target
end

datagram msgResult is
  opcode : byte = $98;
  mark : 2 byte;
  state : msg_state 1 byte;
  profile_ver: 6 byte word;
  profile_fid: 20 byte;
  nodes : array [until end] of
    addr : tNetAddr 18B;
    id : 20 byte;
  end
end

datagram msgReceipt is
  opcode : byte = $97;
  mark : 2 byte;
  state : msg_state 1 byte;
  host_pub : 32 byte;
  message_fid : 24 byte;
  rcpt_user_id : 24 byte;
  profile_ver: 6 byte word;
  sign_time: 6 byte word;
  signature : 64 byte;
end (152 bytes)
~~~

When user wants to insert a message into a network: store encoded message as
object and command that the object is a message to process.

* Check if sender is local user
* validate message against sender profile
* do Full Accept of the message

Full accept
: Fetch Message, fetch all profiles, delivery to all recipients
Lite accept
: Fetch message, fetch only +rcpt_id profile, delivery to _rcpt_id hosts
Notify accept
: Don't fetch, keep notice in memory for when +target connects

* process msgQuery
* decide wheter to accept message
* fetch message from +sender
* start process for each recipient user
  * StartDeliveryFor(msg_fid, rcpt_uid, expires, sender, original/backup/other);
  * task state saved in DB under key (dbMessage, recipientUID, messageFID)
* update profile form query +sender
* sequentially msgQuery to nodes from user profile
* sometimes do query to userID
  * to update profile and store notificaton there
* delay, repeat

When to set +store flag? ...

Message file format.

~~~
file Message
  hashed_data:
    magic: 8 byte = 'BNMesag'#26;
    sender_id: 24 byte;
    sender_encr_key: 32 byte;
    send_date: 6 byte word;
    rcpt_count: byte;
    flags: 1 byte;
    expires: 4 byte word;
    array [rcpt_count] of record
      rcpt_id: 24 byte;
      rcpt_key_prefix: 7 byte;
      priority: byte;
      encrypted_session_key: 40 byte AES_WRAP(kek, session_key, default_iv);
    end record;
  end hashed_data;
  encrypted data AES_CBC( session_key, 0, DEFLATE( this ))
    hashed_data:
      extra_size: 2 byte word;
      extra: array [extra_size] of byte;
      message_body: array [left] of byte;
    end hashed_data;
  end encrypted;
  array [rcpt_count] of record
    encrypted_hash: 32 byte SHA256(kek,SHA256(hashed_data));
  end record;
end
~~~

Change to 16 byte key and mac?
for hash: 64 byte align

Crypto:

session_key = RND() 32B
encrypted_session_key = AES_ECB(key=kek, session_key), 2 blocks
encrypted_data = AES_CBC

It is then MAC'ed for each recipient individually, to preserve the intent of the original sender, and to prevent recipients from maliciously altering the message. For example, if Alice is sending to Bob and Charlie, Bob should not be able to rewrite the message and pass it to Charlie without Charlie detecting the attack.

RPC
-----

CLI utility 'bnc' communicates to daemon via RPC interface over TCP or unix
stream socket. SOCK_STREAM was choosen for it's reliability and availability
over both UNIX and INET address families. On top of stream a packet layer is
implemented. For every RPC request there is at least one reply. Some special
requests have more replies.

Simple password authentication will be added.

~~~
datagram rpc_to_server is
  length:  2 byte word;
  command: 2 byte word;
  data:;
end
datagram rpc_to_client is
  length: 2 byte word;
  status: byte;
  data:;
end
~~~
