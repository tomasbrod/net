unit Download;
{manage downloads}

INTERFACE
{todo}

IMPLEMENTATION
{
 Same idea here as upmgr. Have an tAggr for each peer reporting speed and 
 tJob for each file request saving to file and reqesting missed segments. 
 The aggr should have limit of paralele jobs. Jobs will first be linked in 
 tAggr queue and then started as slots become available.
 
 After node restart, notify requester with a No-Source error. But should 
 not be forgotten. More advanced DM could consult CHK or Category the file 
 was found.
}



END.