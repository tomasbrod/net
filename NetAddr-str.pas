{include file to convert network address to human readable string}

{distinguish ip versions:
 if the first 12 bytes are FFh it is ipv4
 cannot be 0 becouse ::1 is localhost
 }

{Format of IPv4 address:
 ddd.ddd.ddd.ddd}

{Format of IPv6 address:
 xxxx:xxxx:xxxx:xxxx:xxxx:xxxx:xxxx:xxxx}

{currently using libc implementation}