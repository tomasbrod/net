unit BNBTKVS;

(*
The database file is divided into equally sized pages.

The tree starts at root page. There are two versions of root page, but only the
newer and valid one is used when opening the database. A root page, in addition
to standard tree page content contains:
pointer to allocator page
and checksum of itself.

Allocator structures (free space) is stored in memory. On commit, it is
serialized into linked list of pages. Pointer and checksum of this data is
saved into the root page. Writing this list will be tricky, becouse more pages
may need to be allocated while some data is already written. If the structure
is corrupted, it can be reconstructed by traversing the whole tree.

Internal nodes contain b key-pointer pairs. Each i-th pointer points to subtree
that contains keys x so that k[i] <= x < k[i+1]. For b-th no k[i+1] key exists,
so infinity is substituted.

Leaf nodes contain key-value pairs. Mixing pair types in one page (node) is not
allowed (b+tree property).

Each node has a height. Leaf nodes have height 0. Each node has height one less
than it's parent. The height is saved only at root node, other heights can be
computed.

Shortest separtators are used to reduce space used in internal nodes. Inside
nodes, longest prefix is used to compress the key. This is stored in the node
header and can optionally use the path prefix.
