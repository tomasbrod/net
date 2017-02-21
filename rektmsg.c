#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#define BUFLEN 512
#define NPACK 10
#define PORT 9930

void diep(const char *s) {
    perror(s);
    exit(1);
}

int main(void) {
    struct sockaddr_in si_me = {0}, si_other = {0};
    int s, i, slen = sizeof(si_other);
    struct msghdr msg = {0};

    char cmsg[CMSG_SPACE(1024)];
    msg.msg_control = cmsg;
    msg.msg_controllen = sizeof(cmsg);

    struct iovec iovec_1;
    msg.msg_iov =  &iovec_1;
    msg.msg_iovlen = 1;
    msg.msg_iov->iov_base = malloc(BUFLEN);
    msg.msg_iov->iov_len = BUFLEN;

    char buf[BUFLEN];

    if ((s = socket(AF_INET,SOCK_DGRAM, IPPROTO_UDP)) == -1)
        diep("socket");

    si_me.sin_family = AF_INET;
    si_me.sin_port = htons(PORT);
    si_me.sin_addr.s_addr = htonl(INADDR_ANY);
    if (bind(s, (struct sockaddr *)&si_me, sizeof(si_me)) == -1)
        diep("bind");

    unsigned char set = 1;
    if(setsockopt(s, IPPROTO_IP, IP_RECVTOS, &set,sizeof(set))<0)
    {
        printf("cannot set recvtos\n");
    }
    if(setsockopt(s, IPPROTO_IP, IP_RECVTTL, &set,sizeof(set))<0)
    {
        printf("cannot set recvTTL\n");
    }
    for (i = 0; i < NPACK; i++)
    {

        if (recvmsg(s, &msg, 0) < 0)
                    diep("sendmsg()");

        printf("The len of control message = %ld\n", msg.msg_controllen);
        // control ancillary data
        struct cmsghdr * cmsg = CMSG_FIRSTHDR(&msg);
        while(cmsg != NULL)
        {
            printf("YES\n");
            if(cmsg->cmsg_level == IPPROTO_IP && cmsg->cmsg_type == IP_TOS)
            {
                int tos = * (int *) CMSG_DATA(cmsg);
                printf("the tos = %i\n", tos);
            }
            cmsg = CMSG_NXTHDR(&msg, cmsg);
        }
        printf("EMPTY\n");

        printf("Received %X\n", *(int *) msg.msg_iov->iov_base);

    }
    return 0;
}  
