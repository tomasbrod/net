#/bin/bash
set -x
sudo /usr/sbin/tcpdump -i 5 -nntxq -s0 host 127.0.0.1 and udp port 1030
