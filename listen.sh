#/bin/bash
set -x
sudo /usr/sbin/tcpdump -i 5 -nntxq -s0 udp port 1030
