UNIT BtrDbase;
(* B+Tree based key-value database. *)

{ Notes:
  *block device backed storage
  *memory-mapping
  *node types
    - superblock
    - branch
    - leaf
    - data
    - free list
    - unstable array spill
  *file structure
    - primary branch area
      - (0-3) superblocks
      - branches
      - roots
    - secondary branch area
      - branches ???
    - mixed area
      - leaves
      - data
      - branches
    - (E) end marker page
  * Primary Branch Area is fully mmap()ed at all time
  * secondary branch area is optional, and mmaped
  * Mixed area will be either read() or mmap()d on demand

  * Free space management
    - can't overwrite page immediately
      - not even by linked list pointer
    - (A) linked list of stable free pages
    - (B) array of unstable freed pages in superblock
    - page must be on array in previous superblock before it can be written
    - possible extent allocator for continuous files

  * Superblock:
    - magic
    - generation number
    - configuration ?
    - root pointer
    - freelist first pointer (primary,secondary,mixed)
    - unstable array
    - unstable array spill pointer
    - free extent tree ?

  * Superblock Lite:
    - magic
    - generation number
    - branch pointer
    - freelist first pointer (primary,secondary,mixed)
    - unstable array pointer
  * Branch:
    - ident
    - (height, parent, generation, ...)
    - prefix
    - separator-pointer pairs
  * Leaf:
    - ident
    - prefix
    - key-value pairs

  Read transactions hold reference to Root, so the roots must not be rewritten.
    - root -> == branch
    - superblock!=root

  * When block is freed:
    - added to unstable list
    - 

  RTrx gen=1
  WTrx gen=1 -> 2, A:U->F, B:x->F, C:F->x
    - 
}
INTERFACE
IMPLEMENTATION
uses bn_avl_tree;

Type tDatabase= Class
  type tGenNum= QWord;
  type tPageix= DWord;
  type tRefCount= LongWord;
  type tLLPageGenNode=class
    next: tLLPageGenNode;
    gen: tGenNum;
    addr: tPageix;
  end;
  type tLLGeneration=class
    private {ro by transactions}
    gen: tGenNum;
    root: tPageix;
    private {atomic by transaction}
    refc: tRefCount;
    private {under write mutex}
    next: tLLGeneration;
    sync: boolean;
  end;

  public

  WriteMutex: TRTLCriticalSection;

  (* Memorry-mapped Window into the Primary Branch Area *)
  PrimaryWindow: Pointer;
  WindowSize: LongWord;

  (* Current writeback root *)
  Generation: tLLGeneration

  (* Current synced root *)
  GenerationSync: tLLGeneration;

  (* Generation number on which the oldest open transaction was created *)
  OldestGeneration: tLLGeneration;
  LatestGeneration: tLLGeneration;

  (* Pages that can be used for allocation if node.gen<FirstTransaction Populated during BeginWriteTrx from
   * synced unstable list. Flushed on Sync (not Confirm). *)
  ReusableFirst tLLPageGenNode;
  ReusableLast: tLLPageGenNode;

  AllocatedPages: tAVLTree;
end;

BEGIN
END.
