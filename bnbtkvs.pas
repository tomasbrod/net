unit BNBTKVS;

(*
The database file is divided into equally sized pages.

The tree starts at root page. There are two versions of root page, but only the
newer and valid one is used when opening the database.
