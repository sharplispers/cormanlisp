;;;; winsock.lisp -- Winsock definitions for Corman Lisp

;;;; Author:        Roger Corman
;;;; Created:       8/10/99

;;;; This is (for now) just a no-frills equivalent of the following
;;;; file:
;;;;
;;;;   WINSOCK.H
;;;; LC   14/8/18   Modified for IPv6   WinSock2.h and WS2tcpip.h

(defpackage "WINSOCK"
  (:use "COMMON-LISP" "C-TYPES")
  (:shadow "LISTEN"))
(in-package :winsock)

(export '(*addr-size* c-to-addr addr-to-c get-addr-info get-name-info address-family-available-p))

#! (:export t :library "WS2_32")
/* 
 * Basic system type definitions, taken from the BSD file sys/types.h.
 */
typedef unsigned char   u_char;
typedef unsigned short  u_short;
typedef unsigned int    u_int;
typedef unsigned long   u_long;
typedef unsigned short  WORD;		// XXX RGC addition.

/*
 * The new type to be used in all
 * instances which refer to sockets.
 */
typedef u_int           SOCKET;

/*
 * Select uses arrays of SOCKETs.  These macros manipulate such
 * arrays.  FD_SETSIZE may be defined by the user before including
 * this file, but the default here should be >= 64.
 *
 * CAVEAT IMPLEMENTOR and USER: THESE MACROS AND TYPES MUST BE
 * INCLUDED IN WINSOCK.H EXACTLY AS SHOWN HERE.
 */
//#ifndef FD_SETSIZE
#define FD_SETSIZE      64
//#endif /* FD_SETSIZE */

typedef struct fd_set {
        u_int   fd_count;               /* how many are SET? */
        SOCKET  fd_array[FD_SETSIZE];   /* an array of SOCKETs */
} fd_set;

//#ifdef __cplusplus
//extern "C" {
//#endif

/*extern*/ int PASCAL /*FAR*/ __WSAFDIsSet(SOCKET, fd_set FAR *);

//#ifdef __cplusplus
//}
//#endif

/*
#define FD_CLR(fd, set) do { \
    u_int __i; \
    for (__i = 0; __i < ((fd_set FAR *)(set))->fd_count ;
          __i++) { \
        if (((fd_set FAR *)(set))->fd_array[__i] == fd) { \
            while (__i < ((fd_set FAR *)(set))->fd_count-1) { \
                ((fd_set FAR *)(set))->fd_array[__i] = \
                    ((fd_set FAR *)(set))->fd_array[__i+1]; \
                __i++; \
            } \
            ((fd_set FAR *)(set))->fd_count--; \
            break; \
        } \
    } \
} while(0)

#define FD_SET(fd, set) do { \
    if (((fd_set FAR *)(set))->fd_count < FD_SETSIZE) \
        ((fd_set FAR *)(set))->fd_array[((fd_set FAR *)(set))->fd_count++]=(fd);\
} while(0)

#define FD_ZERO(set) (((fd_set FAR *)(set))->fd_count=0)

#define FD_ISSET(fd, set) __WSAFDIsSet((SOCKET)(fd), (fd_set FAR *)(set))
*/

/*
 * Structure used in select() call, taken from the BSD file sys/time.h.
 */
struct timeval {
        long    tv_sec;         /* seconds */
        long    tv_usec;        /* and microseconds */
};

/*
 * Operations on timevals.
 *
 * NB: timercmp does not work for >= or <=.
 */
/*
#define timerisset(tvp)         ((tvp)->tv_sec || (tvp)->tv_usec)
#define timercmp(tvp, uvp, cmp) \
        ((tvp)->tv_sec cmp (uvp)->tv_sec || \
         (tvp)->tv_sec == (uvp)->tv_sec && (tvp)->tv_usec cmp (uvp)->tv_usec)
#define timerclear(tvp)         (tvp)->tv_sec = (tvp)->tv_usec = 0
*/
/*
 * Commands for ioctlsocket(),  taken from the BSD file fcntl.h.
 *
 *
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 128 bytes.
 */
#define IOCPARM_MASK    0x7f            /* parameters must be < 128 bytes */
#define IOC_VOID        0x20000000      /* no parameters */
#define IOC_OUT         0x40000000      /* copy out parameters */
#define IOC_IN          0x80000000      /* copy in parameters */
#define IOC_INOUT       (IOC_IN|IOC_OUT)
                                        /* 0x20000000 distinguishes new &
                                           old ioctl's */
//#define _IO(x,y)        (IOC_VOID|((x)<<8)|(y))

//#define _IOR(x,y,t)     (IOC_OUT|(((long)sizeof(t)&IOCPARM_MASK)<<16)|((x)<<8)|(y))

//#define _IOW(x,y,t)     (IOC_IN|(((long)sizeof(t)&IOCPARM_MASK)<<16)|((x)<<8)|(y))

//#define FIONREAD    _IOR('f', 127, u_long) /* get # bytes to read */
//#define FIONBIO     _IOW('f', 126, u_long) /* set/clear non-blocking i/o */
//#define FIOASYNC    _IOW('f', 125, u_long) /* set/clear async i/o */

/* Socket I/O Controls */
//#define SIOCSHIWAT  _IOW('s',  0, u_long)  /* set high watermark */
//#define SIOCGHIWAT  _IOR('s',  1, u_long)  /* get high watermark */
//#define SIOCSLOWAT  _IOW('s',  2, u_long)  /* set low watermark */
//#define SIOCGLOWAT  _IOR('s',  3, u_long)  /* get low watermark */
//#define SIOCATMARK  _IOR('s',  7, u_long)  /* at oob mark? */

/*
 * Structures returned by network data base library, taken from the
 * BSD file netdb.h.  All addresses are supplied in host order, and
 * returned in network order (suitable for use in system calls).
 */

struct  hostent {
        char    FAR * h_name;           /* official name of host */
        char    FAR * FAR * h_aliases;  /* alias list */
        short   h_addrtype;             /* host address type */
        short   h_length;               /* length of address */
        char    FAR * FAR * h_addr_list; /* list of addresses */
//#define h_addr  h_addr_list[0]          /* address, for backward compat */
};

/*
 * It is assumed here that a network number
 * fits in 32 bits.
 */
struct  netent {
        char    FAR * n_name;           /* official name of net */
        char    FAR * FAR * n_aliases;  /* alias list */
        short   n_addrtype;             /* net address type */
        u_long  n_net;                  /* network # */
};

struct  servent {
        char    FAR * s_name;           /* official service name */
        char    FAR * FAR * s_aliases;  /* alias list */
        short   s_port;                 /* port # */
        char    FAR * s_proto;          /* protocol to use */
};

struct  protoent {
        char    FAR * p_name;           /* official protocol name */
        char    FAR * FAR * p_aliases;  /* alias list */
        short   p_proto;                /* protocol # */
};

/*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981, taken from the BSD file netinet/in.h.
 */

/*
 * Protocols
 */
#define IPPROTO_IP              0               /* dummy for IP */
#define IPPROTO_ICMP            1               /* control message protocol */
#define IPPROTO_IGMP            2               /* group management protocol */
#define IPPROTO_GGP             3               /* gateway^2 (deprecated) */
#define IPPROTO_TCP             6               /* tcp */
#define IPPROTO_PUP             12              /* pup */
#define IPPROTO_UDP             17              /* user datagram protocol */
#define IPPROTO_IDP             22              /* xns idp */
#define IPPROTO_ND              77              /* UNOFFICIAL net disk proto */

#define IPPROTO_RAW             255             /* raw IP packet */
#define IPPROTO_MAX             256

/*
 * Port/socket numbers: network standard functions
 */
#define IPPORT_ECHO             7
#define IPPORT_DISCARD          9
#define IPPORT_SYSTAT           11
#define IPPORT_DAYTIME          13
#define IPPORT_NETSTAT          15
#define IPPORT_FTP              21
#define IPPORT_TELNET           23
#define IPPORT_SMTP             25
#define IPPORT_TIMESERVER       37
#define IPPORT_NAMESERVER       42
#define IPPORT_WHOIS            43
#define IPPORT_MTP              57

/*
 * Port/socket numbers: host specific functions
 */
#define IPPORT_TFTP             69
#define IPPORT_RJE              77
#define IPPORT_FINGER           79
#define IPPORT_TTYLINK          87
#define IPPORT_SUPDUP           95

/*
 * UNIX TCP sockets
 */
#define IPPORT_EXECSERVER       512
#define IPPORT_LOGINSERVER      513
#define IPPORT_CMDSERVER        514
#define IPPORT_EFSSERVER        520

/*
 * UNIX UDP sockets
 */
#define IPPORT_BIFFUDP          512
#define IPPORT_WHOSERVER        513
#define IPPORT_ROUTESERVER      520
                                        /* 520+1 also used */

/*
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 */
#define IPPORT_RESERVED         1024

/*
 * Link numbers
 */
#define IMPLINK_IP              155
#define IMPLINK_LOWEXPER        156
#define IMPLINK_HIGHEXPER       158

/*
 * Internet address (old style... should be updated)
 */

//struct in_addr {
 //       union {
 //               struct { u_char s_b1,s_b2,s_b3,s_b4; } S_un_b;
 //               struct { u_short s_w1,s_w2; } S_un_w;
 //               u_long S_addr;
 //       } S_un;
//#define s_addr  S_un.S_addr
                                /* can be used for most tcp & ip code */
//#define s_host  S_un.S_un_b.s_b2
                                /* host on imp */
//#define s_net   S_un.S_un_b.s_b1
                                /* network */
//#define s_imp   S_un.S_un_w.s_w2
                                /* imp */
//#define s_impno S_un.S_un_b.s_b4
                                /* imp # */
//#define s_lh    S_un.S_un_b.s_b3
                                /* logical host */
//};
struct in_addr {
	u_long S_addr;
		  };

/*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 */
//#define IN_CLASSA(i)            (((long)(i) & 0x80000000) == 0)
#define IN_CLASSA_NET           0xff000000
#define IN_CLASSA_NSHIFT        24
#define IN_CLASSA_HOST          0x00ffffff
#define IN_CLASSA_MAX           128

//#define IN_CLASSB(i)            (((long)(i) & 0xc0000000) == 0x80000000)
#define IN_CLASSB_NET           0xffff0000
#define IN_CLASSB_NSHIFT        16
#define IN_CLASSB_HOST          0x0000ffff
#define IN_CLASSB_MAX           65536

//#define IN_CLASSC(i)            (((long)(i) & 0xe0000000) == 0xc0000000)
#define IN_CLASSC_NET           0xffffff00
#define IN_CLASSC_NSHIFT        8
#define IN_CLASSC_HOST          0x000000ff

#define INADDR_ANY              /*(u_long)*/0x00000000
#define INADDR_LOOPBACK         0x7f000001
#define INADDR_BROADCAST        /*(u_long)*/0xffffffff
#define INADDR_NONE             0xffffffff

/*
 * Socket address, internet style.
 */
struct sockaddr_in {
        short   sin_family;
        u_short sin_port;
        /*struct*/  in_addr sin_addr;
        char    sin_zero[8];
};

#define WSADESCRIPTION_LEN      256
#define WSASYS_STATUS_LEN       128

typedef struct WSAData {
        unsigned short          wVersion;
        unsigned short          wHighVersion;
        char                    szDescription[/*WSADESCRIPTION_LEN+1*/257];
        char                    szSystemStatus[/*WSASYS_STATUS_LEN+1*/129];
        unsigned short          iMaxSockets;
        unsigned short          iMaxUdpDg;
        char FAR *              lpVendorInfo;
} WSADATA;

typedef WSADATA FAR *LPWSADATA;

/*
 * Options for use with [gs]etsockopt at the IP level.
 */
#define IP_OPTIONS          1           /* set/get IP per-packet options    */
#define IP_MULTICAST_IF     2           /* set/get IP multicast interface   */
#define IP_MULTICAST_TTL    3           /* set/get IP multicast timetolive  */
#define IP_MULTICAST_LOOP   4           /* set/get IP multicast loopback    */
#define IP_ADD_MEMBERSHIP   5           /* add  an IP group membership      */
#define IP_DROP_MEMBERSHIP  6           /* drop an IP group membership      */
#define IP_TTL              7           /* set/get IP Time To Live          */
#define IP_TOS              8           /* set/get IP Type Of Service       */
#define IP_DONTFRAGMENT     9           /* set/get IP Don't Fragment flag   */


#define IP_DEFAULT_MULTICAST_TTL   1    /* normally limit m'casts to 1 hop  */
#define IP_DEFAULT_MULTICAST_LOOP  1    /* normally hear sends if a member  */
#define IP_MAX_MEMBERSHIPS         20   /* per socket; must fit in one mbuf */

/*
 * Argument structure for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP.
 */
struct ip_mreq {
        /*struct*/ in_addr  imr_multiaddr;  /* IP multicast address of group */
        /*struct*/ in_addr  imr_interface;  /* local IP address of interface */
};

/*
 * Definitions related to sockets: types, address families, options,
 * taken from the BSD file sys/socket.h.
 */

/*
 * This is used instead of -1, since the
 * SOCKET type is unsigned.
 */
#define INVALID_SOCKET  		0xffffffff 
#define SOCKET_ERROR            (-1)

/*
 * Types
 */
#define SOCK_STREAM     1               /* stream socket */
#define SOCK_DGRAM      2               /* datagram socket */
#define SOCK_RAW        3               /* raw-protocol interface */
#define SOCK_RDM        4               /* reliably-delivered message */
#define SOCK_SEQPACKET  5               /* sequenced packet stream */

/*
 * Option flags per-socket.
 */
#define SO_DEBUG        0x0001          /* turn on debugging info recording */
#define SO_ACCEPTCONN   0x0002          /* socket has had listen() */
#define SO_REUSEADDR    0x0004          /* allow local address reuse */
#define SO_KEEPALIVE    0x0008          /* keep connections alive */
#define SO_DONTROUTE    0x0010          /* just use interface addresses */
#define SO_BROADCAST    0x0020          /* permit sending of broadcast msgs */
#define SO_USELOOPBACK  0x0040          /* bypass hardware when possible */
#define SO_LINGER       0x0080          /* linger on close if data present */
#define SO_OOBINLINE    0x0100          /* leave received OOB data in line */

//#define SO_DONTLINGER   (u_int)(~SO_LINGER)

/*
 * Additional options.
 */
#define SO_SNDBUF       0x1001          /* send buffer size */
#define SO_RCVBUF       0x1002          /* receive buffer size */
#define SO_SNDLOWAT     0x1003          /* send low-water mark */
#define SO_RCVLOWAT     0x1004          /* receive low-water mark */
#define SO_SNDTIMEO     0x1005          /* send timeout */
#define SO_RCVTIMEO     0x1006          /* receive timeout */
#define SO_ERROR        0x1007          /* get error status and clear */
#define SO_TYPE         0x1008          /* get socket type */

/*
 * Options for connect and disconnect data and options.  Used only by
 * non-TCP/IP transports such as DECNet, OSI TP4, etc.
 */
#define SO_CONNDATA     0x7000
#define SO_CONNOPT      0x7001
#define SO_DISCDATA     0x7002
#define SO_DISCOPT      0x7003
#define SO_CONNDATALEN  0x7004
#define SO_CONNOPTLEN   0x7005
#define SO_DISCDATALEN  0x7006
#define SO_DISCOPTLEN   0x7007

/*
 * Option for opening sockets for synchronous access.
 */
#define SO_OPENTYPE     0x7008

#define SO_SYNCHRONOUS_ALERT    0x10
#define SO_SYNCHRONOUS_NONALERT 0x20

/*
 * Other NT-specific options.
 */
#define SO_MAXDG        0x7009
#define SO_MAXPATHDG    0x700A
#define SO_UPDATE_ACCEPT_CONTEXT 0x700B
#define SO_CONNECT_TIME 0x700C

/*
 * TCP options.
 */
#define TCP_NODELAY     0x0001
#define TCP_BSDURGENT   0x7000

/*
 * Address families.
 */
#define AF_UNSPEC       0               /* unspecified */
#define AF_UNIX         1               /* local to host (pipes, portals) */
#define AF_INET         2               /* internetwork: UDP, TCP, etc. */
#define AF_IMPLINK      3               /* arpanet imp addresses */
#define AF_PUP          4               /* pup protocols: e.g. BSP */
#define AF_CHAOS        5               /* mit CHAOS protocols */
#define AF_IPX          6               /* IPX and SPX */
#define AF_NS           6               /* XEROX NS protocols */
#define AF_ISO          7               /* ISO protocols */
#define AF_OSI          AF_ISO          /* OSI is ISO */
#define AF_ECMA         8               /* european computer manufacturers */
#define AF_DATAKIT      9               /* datakit protocols */
#define AF_CCITT        10              /* CCITT protocols, X.25 etc */
#define AF_SNA          11              /* IBM SNA */
#define AF_DECnet       12              /* DECnet */
#define AF_DLI          13              /* Direct data link interface */
#define AF_LAT          14              /* LAT */
#define AF_HYLINK       15              /* NSC Hyperchannel */
#define AF_APPLETALK    16              /* AppleTalk */
#define AF_NETBIOS      17              /* NetBios-style addresses */
#define AF_VOICEVIEW    18              /* VoiceView */
#define AF_FIREFOX      19              /* FireFox */
#define AF_UNKNOWN1     20              /* Somebody is using this! */
#define AF_BAN          21              /* Banyan */
#define AF_ATM          22              /* Native ATM Services */
#define AF_INET6        23             /* Internetwork Version 6 */

#define AF_MAX          24 // 32

/*
 * Structure used by kernel to store most
 * addresses.
 */
struct sockaddr {
        u_short sa_family;              /* address family */
        char    sa_data[14];            /* up to 14 bytes of direct address */
};

/*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 */
struct sockproto {
        u_short sp_family;              /* address family */
        u_short sp_protocol;            /* protocol */
};

/*
 * Protocol families, same as address families for now.
 */
#define PF_UNSPEC       AF_UNSPEC
#define PF_UNIX         AF_UNIX
#define PF_INET         AF_INET
#define PF_INET6        AF_INET6
#define PF_IMPLINK      AF_IMPLINK
#define PF_PUP          AF_PUP
#define PF_CHAOS        AF_CHAOS
#define PF_NS           AF_NS
#define PF_IPX          AF_IPX
#define PF_ISO          AF_ISO
#define PF_OSI          AF_OSI
#define PF_ECMA         AF_ECMA
#define PF_DATAKIT      AF_DATAKIT
#define PF_CCITT        AF_CCITT
#define PF_SNA          AF_SNA
#define PF_DECnet       AF_DECnet
#define PF_DLI          AF_DLI
#define PF_LAT          AF_LAT
#define PF_HYLINK       AF_HYLINK
#define PF_APPLETALK    AF_APPLETALK
#define PF_VOICEVIEW    AF_VOICEVIEW
#define PF_FIREFOX      AF_FIREFOX
#define PF_UNKNOWN1     AF_UNKNOWN1
#define PF_BAN          AF_BAN

#define PF_MAX          AF_MAX

/*
 * Structure used for manipulating linger option.
 */
struct  linger {
        u_short l_onoff;                /* option on/off */
        u_short l_linger;               /* linger time */
};

/*
 * Level number for (get/set)sockopt() to apply to socket itself.
 */
#define SOL_SOCKET      0xffff          /* options for socket level */

/*
 * Maximum queue length specifiable by listen.
 */
#define SOMAXCONN       5

#define MSG_OOB         0x1             /* process out-of-band data */
#define MSG_PEEK        0x2             /* peek at incoming message */
#define MSG_DONTROUTE   0x4             /* send without using routing tables */

#define MSG_MAXIOVLEN   16

#define MSG_PARTIAL     0x8000          /* partial send or recv for message xport */

/*
 * Define constant based on rfc883, used by gethostbyxxxx() calls.
 */
#define MAXGETHOSTSTRUCT        1024

/*
 * Define flags to be used with the WSAAsyncSelect() call.
 */
#define FD_READ         0x01
#define FD_WRITE        0x02
#define FD_OOB          0x04
#define FD_ACCEPT       0x08
#define FD_CONNECT      0x10
#define FD_CLOSE        0x20

/*
 * All Windows Sockets error constants are biased by WSABASEERR from
 * the "normal"
 */
#define WSABASEERR              10000
/*
 * Windows Sockets definitions of regular Microsoft C error constants
 */
#define WSAEINTR                (WSABASEERR+4)
#define WSAEBADF                (WSABASEERR+9)
#define WSAEACCES               (WSABASEERR+13)
#define WSAEFAULT               (WSABASEERR+14)
#define WSAEINVAL               (WSABASEERR+22)
#define WSAEMFILE               (WSABASEERR+24)

/*
 * Windows Sockets definitions of regular Berkeley error constants
 */
#define WSAEWOULDBLOCK          (WSABASEERR+35)
#define WSAEINPROGRESS          (WSABASEERR+36)
#define WSAEALREADY             (WSABASEERR+37)
#define WSAENOTSOCK             (WSABASEERR+38)
#define WSAEDESTADDRREQ         (WSABASEERR+39)
#define WSAEMSGSIZE             (WSABASEERR+40)
#define WSAEPROTOTYPE           (WSABASEERR+41)
#define WSAENOPROTOOPT          (WSABASEERR+42)
#define WSAEPROTONOSUPPORT      (WSABASEERR+43)
#define WSAESOCKTNOSUPPORT      (WSABASEERR+44)
#define WSAEOPNOTSUPP           (WSABASEERR+45)
#define WSAEPFNOSUPPORT         (WSABASEERR+46)
#define WSAEAFNOSUPPORT         (WSABASEERR+47)
#define WSAEADDRINUSE           (WSABASEERR+48)
#define WSAEADDRNOTAVAIL        (WSABASEERR+49)
#define WSAENETDOWN             (WSABASEERR+50)
#define WSAENETUNREACH          (WSABASEERR+51)
#define WSAENETRESET            (WSABASEERR+52)
#define WSAECONNABORTED         (WSABASEERR+53)
#define WSAECONNRESET           (WSABASEERR+54)
#define WSAENOBUFS              (WSABASEERR+55)
#define WSAEISCONN              (WSABASEERR+56)
#define WSAENOTCONN             (WSABASEERR+57)
#define WSAESHUTDOWN            (WSABASEERR+58)
#define WSAETOOMANYREFS         (WSABASEERR+59)
#define WSAETIMEDOUT            (WSABASEERR+60)
#define WSAECONNREFUSED         (WSABASEERR+61)
#define WSAELOOP                (WSABASEERR+62)
#define WSAENAMETOOLONG         (WSABASEERR+63)
#define WSAEHOSTDOWN            (WSABASEERR+64)
#define WSAEHOSTUNREACH         (WSABASEERR+65)
#define WSAENOTEMPTY            (WSABASEERR+66)
#define WSAEPROCLIM             (WSABASEERR+67)
#define WSAEUSERS               (WSABASEERR+68)
#define WSAEDQUOT               (WSABASEERR+69)
#define WSAESTALE               (WSABASEERR+70)
#define WSAEREMOTE              (WSABASEERR+71)

#define WSAEDISCON              (WSABASEERR+101)

/*
 * Extended Windows Sockets error constant definitions
 */
#define WSASYSNOTREADY          (WSABASEERR+91)
#define WSAVERNOTSUPPORTED      (WSABASEERR+92)
#define WSANOTINITIALISED       (WSABASEERR+93)

/*
 * Error return codes from gethostbyname() and gethostbyaddr()
 * (when using the resolver). Note that these errors are
 * retrieved via WSAGetLastError() and must therefore follow
 * the rules for avoiding clashes with error numbers from
 * specific implementations or language run-time systems.
 * For this reason the codes are based at WSABASEERR+1001.
 * Note also that [WSA]NO_ADDRESS is defined only for
 * compatibility purposes.
 */

//#define h_errno         WSAGetLastError()

/* Authoritative Answer: Host not found */
#define WSAHOST_NOT_FOUND       (WSABASEERR+1001)
#define HOST_NOT_FOUND          WSAHOST_NOT_FOUND

/* Non-Authoritative: Host not found, or SERVERFAIL */
#define WSATRY_AGAIN            (WSABASEERR+1002)
#define TRY_AGAIN               WSATRY_AGAIN

/* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
#define WSANO_RECOVERY          (WSABASEERR+1003)
#define NO_RECOVERY             WSANO_RECOVERY

/* Valid name, no data record of requested type */
#define WSANO_DATA              (WSABASEERR+1004)
#define NO_DATA                 WSANO_DATA

/* no address, look for MX record */
#define WSANO_ADDRESS           WSANO_DATA
#define NO_ADDRESS              WSANO_ADDRESS

!#

#! (:export t :library "WS2_32")

/* Socket function prototypes */

//#ifdef __cplusplus
//extern "C" {
//#endif

SOCKET PASCAL FAR accept (SOCKET s, sockaddr FAR *addr,
                          int FAR *addrlen);

int PASCAL FAR bind (SOCKET s, const sockaddr FAR *addr, int namelen);

int PASCAL FAR closesocket (SOCKET s);

int PASCAL FAR connect (SOCKET s, const sockaddr FAR *name, int namelen);

int PASCAL FAR ioctlsocket (SOCKET s, long cmd, u_long FAR *argp);

int PASCAL FAR getpeername (SOCKET s, sockaddr FAR *name,
                            int FAR * namelen);

int PASCAL FAR getsockname (SOCKET s, sockaddr FAR *name,
                            int FAR * namelen);

int PASCAL FAR getsockopt (SOCKET s, int level, int optname,
                           char FAR * optval, int FAR *optlen);

u_long PASCAL FAR htonl (u_long hostlong);

u_short PASCAL FAR htons (u_short hostshort);

unsigned long PASCAL FAR inet_addr (const char FAR * cp);

char FAR * PASCAL FAR inet_ntoa (in_addr in);

int PASCAL FAR listen (SOCKET s, int backlog);

u_long PASCAL FAR ntohl (u_long netlong);

u_short PASCAL FAR ntohs (u_short netshort);

int PASCAL FAR recv (SOCKET s, char FAR * buf, int len, int flags);

int PASCAL FAR recvfrom (SOCKET s, char FAR * buf, int len, int flags,
                         sockaddr FAR *from, int FAR * fromlen);

int PASCAL FAR select (int nfds, fd_set FAR *readfds, fd_set FAR *writefds,
                       fd_set FAR *exceptfds, const timeval FAR *timeout);

int PASCAL FAR send (SOCKET s, const char FAR * buf, int len, int flags);

int PASCAL FAR sendto (SOCKET s, const char FAR * buf, int len, int flags,
                       const sockaddr FAR *to, int tolen);

int PASCAL FAR setsockopt (SOCKET s, int level, int optname,
                           const char FAR * optval, int optlen);

int PASCAL FAR shutdown (SOCKET s, int how);

SOCKET PASCAL FAR socket (int af, int type, int protocol);

/* Database function prototypes */

hostent FAR * PASCAL FAR gethostbyaddr(const char FAR * addr,
                                              int len, int type);

hostent FAR * PASCAL FAR gethostbyname(const char FAR * name);

int PASCAL FAR gethostname (char FAR * name, int namelen);

servent FAR * PASCAL FAR getservbyport(int port, const char FAR * proto);

servent FAR * PASCAL FAR getservbyname(const char FAR * name,
                                              const char FAR * proto);

protoent FAR * PASCAL FAR getprotobynumber(int proto);

protoent FAR * PASCAL FAR getprotobyname(const char FAR * name);

/* Microsoft Windows Extension function prototypes */

int PASCAL FAR WSAStartup(WORD wVersionRequired, LPWSADATA lpWSAData);

int PASCAL FAR WSACleanup();

void PASCAL FAR WSASetLastError(int iError);

int PASCAL FAR WSAGetLastError();

BOOL PASCAL FAR WSAIsBlocking();

int PASCAL FAR WSAUnhookBlockingHook();

FARPROC PASCAL FAR WSASetBlockingHook(FARPROC lpBlockFunc);

int PASCAL FAR WSACancelBlockingCall();

HANDLE PASCAL FAR WSAAsyncGetServByName(HWND hWnd, u_int wMsg,
                                        const char FAR * name,
                                        const char FAR * proto,
                                        char FAR * buf, int buflen);

HANDLE PASCAL FAR WSAAsyncGetServByPort(HWND hWnd, u_int wMsg, int port,
                                        const char FAR * proto, char FAR * buf,
                                        int buflen);

HANDLE PASCAL FAR WSAAsyncGetProtoByName(HWND hWnd, u_int wMsg,
                                         const char FAR * name, char FAR * buf,
                                         int buflen);

HANDLE PASCAL FAR WSAAsyncGetProtoByNumber(HWND hWnd, u_int wMsg,
                                           int number, char FAR * buf,
                                           int buflen);

HANDLE PASCAL FAR WSAAsyncGetHostByName(HWND hWnd, u_int wMsg,
                                        const char FAR * name, char FAR * buf,
                                        int buflen);

HANDLE PASCAL FAR WSAAsyncGetHostByAddr(HWND hWnd, u_int wMsg,
                                        const char FAR * addr, int len, int type,
                                        char FAR * buf, int buflen);

int PASCAL FAR WSACancelAsyncRequest(HANDLE hAsyncTaskHandle);

int PASCAL FAR WSAAsyncSelect(SOCKET s, HWND hWnd, u_int wMsg,
                               long lEvent);

int PASCAL FAR WSARecvEx (SOCKET s, char FAR * buf, int len, int FAR *flags);

typedef struct _TRANSMIT_FILE_BUFFERS {
    void* Head;
    unsigned long HeadLength;
    void* Tail;
    unsigned long TailLength;
} TRANSMIT_FILE_BUFFERS, *PTRANSMIT_FILE_BUFFERS, *LPTRANSMIT_FILE_BUFFERS;

#define TF_DISCONNECT       0x01
#define TF_REUSE_SOCKET     0x02
#define TF_WRITE_BEHIND     0x04

BOOL
PASCAL FAR
TransmitFile (
    /*IN*/ SOCKET hSocket,
    /*IN*/ HANDLE hFile,
    /*IN*/ DWORD nNumberOfBytesToWrite,
    /*IN*/ DWORD nNumberOfBytesPerSend,
    /*IN*/ LPOVERLAPPED lpOverlapped,
    /*IN*/ LPTRANSMIT_FILE_BUFFERS lpTransmitBuffers,
    /*IN*/ DWORD dwReserved
    );

BOOL
PASCAL FAR
AcceptEx (
    /*IN*/ SOCKET sListenSocket,
    /*IN*/ SOCKET sAcceptSocket,
    /*IN*/ PVOID lpOutputBuffer,
    /*IN*/ DWORD dwReceiveDataLength,
    /*IN*/ DWORD dwLocalAddressLength,
    /*IN*/ DWORD dwRemoteAddressLength,
    /*OUT*/ LPDWORD lpdwBytesReceived,
    /*IN*/ LPOVERLAPPED lpOverlapped
    );

VOID
PASCAL FAR
GetAcceptExSockaddrs (
    /*IN*/ PVOID lpOutputBuffer,
    /*IN*/ DWORD dwReceiveDataLength,
    /*IN*/ DWORD dwLocalAddressLength,
    /*IN*/ DWORD dwRemoteAddressLength,
    /*OUT*/ sockaddr **LocalSockaddr,
    /*OUT*/ LPINT LocalSockaddrLength,
    /*OUT*/ sockaddr **RemoteSockaddr,
    /*OUT*/ LPINT RemoteSockaddrLength
    );

//#ifdef __cplusplus
//}
//#endif

/* Microsoft Windows Extended data types */
typedef struct sockaddr SOCKADDR;
typedef struct sockaddr *PSOCKADDR;
typedef struct sockaddr FAR *LPSOCKADDR;

typedef struct sockaddr_in SOCKADDR_IN;
typedef struct sockaddr_in *PSOCKADDR_IN;
typedef struct sockaddr_in FAR *LPSOCKADDR_IN;

typedef struct linger LINGER;
typedef struct linger *PLINGER;
typedef struct linger FAR *LPLINGER;

typedef struct in_addr IN_ADDR;
typedef struct in_addr *PIN_ADDR;
typedef struct in_addr FAR *LPIN_ADDR;

typedef struct fd_set FD_SET;
typedef struct fd_set *PFD_SET;
typedef struct fd_set FAR *LPFD_SET;

typedef struct hostent HOSTENT;
typedef struct hostent *PHOSTENT;
typedef struct hostent FAR *LPHOSTENT;

typedef struct servent SERVENT;
typedef struct servent *PSERVENT;
typedef struct servent FAR *LPSERVENT;

typedef struct protoent PROTOENT;
typedef struct protoent *PPROTOENT;
typedef struct protoent FAR *LPPROTOENT;

typedef struct timeval TIMEVAL;
typedef struct timeval *PTIMEVAL;
typedef struct timeval FAR *LPTIMEVAL;

/*
 * Windows message parameter composition and decomposition
 * macros.
 *
 * WSAMAKEASYNCREPLY is intended for use by the Windows Sockets implementation
 * when constructing the response to a WSAAsyncGetXByY() routine.
 */
//#define WSAMAKEASYNCREPLY(buflen,error)     MAKELONG(buflen,error)
/*
 * WSAMAKESELECTREPLY is intended for use by the Windows Sockets implementation
 * when constructing the response to WSAAsyncSelect().
 */
//#define WSAMAKESELECTREPLY(event,error)     MAKELONG(event,error)
/*
 * WSAGETASYNCBUFLEN is intended for use by the Windows Sockets application
 * to extract the buffer length from the lParam in the response
 * to a WSAGetXByY().
 */
//#define WSAGETASYNCBUFLEN(lParam)           LOWORD(lParam)
/*
 * WSAGETASYNCERROR is intended for use by the Windows Sockets application
 * to extract the error code from the lParam in the response
 * to a WSAGetXByY().
 */
//#define WSAGETASYNCERROR(lParam)            HIWORD(lParam)
/*
 * WSAGETSELECTEVENT is intended for use by the Windows Sockets application
 * to extract the event code from the lParam in the response
 * to a WSAAsyncSelect().
 */
//#define WSAGETSELECTEVENT(lParam)           LOWORD(lParam)
/*
 * WSAGETSELECTERROR is intended for use by the Windows Sockets application
 * to extract the error code from the lParam in the response
 * to a WSAAsyncSelect().
 */
//#define WSAGETSELECTERROR(lParam)           HIWORD(lParam)

//#endif  /* _WINSOCKAPI_ */

/* IPv6 definitions */

struct in6_addr {
    u_short Word[8]; // An IPv6 address formatted as an array of eight u_shorts.
};

/* IPv6 socket address structure, RFC 2553 */

struct sockaddr_in6 {
                short        sin6_family;      /* AF_INET6 */
                u_short    sin6_port;          /* Transport level port number */
                u_long     sin6_flowinfo;    /* IPv6 flow information */
                in6_addr  sin6_addr;         /* struct IPv6 address */
                u_long     sin6_scope_id;  /* set of interfaces for a scope */
};                                                        /* length 28. sockaddr_storage 128 */

/* Structure used in getaddrinfo() call */

typedef struct addrinfo {
  int             ai_flags;              /* AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST */
  int             ai_family;            /* PF_xxx */
  int             ai_socktype;        /* SOCK_xxx */
  int             ai_protocol;         /* 0 or IPPROTO_xxx for IPv4 and IPv6 */
  u_int         ai_addrlen;          /* size_t Length of ai_addr */
  char          *ai_canonname;  /* Canonical name for nodename */
  sockaddr  *ai_addr;              /* struct Binary address */
  void          *ai_next;              /* struct addrinfo Next structure in linked list */
} ADDRINFO, FAR * LPADDRINFO;

/* Flags used in "hints" argument to getaddrinfo() */

#define AI_PASSIVE             0x1  /* Socket address will be used in bind() call */
#define AI_CANONNAME     0x2  /* Return canonical name in first ai_canonname */
#define AI_NUMERICHOST 0x4  /* Nodename must be a numeric address string */

int
PASCAL FAR
getaddrinfo(
    const char FAR *  nodename,
    const char FAR *  servname,
    const addrinfo FAR *  hints,
    addrinfo FAR * FAR *  res
    );

void
PASCAL FAR
freeaddrinfo(
    addrinfo FAR *  ai
    );

#define NI_MAXHOST  1025  /* Max size of a fully-qualified domain name */
#define NI_MAXSERV      32  /* Max size of a service name */

// #define INET_ADDRSTRLEN  16 /* Max size of numeric form of IPv4 address */
// #define INET6_ADDRSTRLEN 46 /* Max size of numeric form of IPv6 address */

/* Flags for getnameinfo() */

#define NI_NOFQDN             0x01  /* Only return nodename portion for local hosts */
#define NI_NUMERICHOST  0x02  /* Return numeric form of the host's address */
#define NI_NAMEREQD        0x04  /* Error if the host's name not in DNS */
#define NI_NUMERICSERV  0x08  /* Return numeric form of the service (port #) */
#define NI_DGRAM              0x10  /* Service is a datagram service */

int
PASCAL FAR
getnameinfo(
    const sockaddr FAR *  sa,
    int  salen,
    char FAR *  host,
    u_long  hostlen,
    char FAR *  serv,
    u_long  servlen,
    int  flags
    );

!#

;; protocol enumeration functionality

#!  (:export t :library "WS2_32")
#define MAX_PROTOCOL_CHAIN 7

#define BASE_PROTOCOL      1
#define LAYERED_PROTOCOL   0

#define WSAPROTOCOL_LEN  255
!#

(win32:defwinstruct WSAPROTOCOLCHAIN
      ((ChainLen win32::int)
       (ChainEntries (win32::DWORD MAX_PROTOCOL_CHAIN))))
(win32:defwintype LPWSAPROTOCOLCHAIN (WSAPROTOCOLCHAIN *))
            
(win32:defwinstruct WSAPROTOCOL_INFOW
      ((dwServiceFlags1 win32::DWORD)
       (dwServiceFlags2 win32::DWORD)
       (dwServiceFlags3 win32::DWORD)
       (dwServiceFlags4 win32::DWORD)
       (dwProviderFlags win32::DWORD)
       (ProviderId win32::GUID)
       (dwCatalogEntryId win32::DWORD)
       (ProtocolChain WSAPROTOCOLCHAIN)
       (iVersion win32::int)
       (iAddressFamily win32::int)
       (iMaxSockAddr win32::int)
       (iMinSockAddr win32::int)
       (iSocketType win32::int)
       (iProtocol win32::int)
       (iProtocolMaxOffset win32::int)
       (iNetworkByteOrder win32::int)
       (iSecurityScheme win32::int)
       (dwMessageSize win32::DWORD)
       (dwProviderReserved win32::DWORD)
       (szProtocol (win32::WCHAR #| WSAPROTOCOL_LEN + 1 |# 256))))
(win32:defwintype LPWSAPROTOCOL_INFOW (WSAPROTOCOL_INFOW *))

(win32:defwinstruct WSAPROTOCOL_INFOA
      ((dwServiceFlags1 win32::DWORD)
       (dwServiceFlags2 win32::DWORD)
       (dwServiceFlags3 win32::DWORD)
       (dwServiceFlags4 win32::DWORD)
       (dwProviderFlags win32::DWORD)
       (ProviderId win32::GUID)
       (dwCatalogEntryId win32::DWORD)
       (ProtocolChain WSAPROTOCOLCHAIN)
       (iVersion win32::int)
       (iAddressFamily win32::int)
       (iMaxSockAddr win32::int)
       (iMinSockAddr win32::int)
       (iSocketType win32::int)
       (iProtocol win32::int)
       (iProtocolMaxOffset win32::int)
       (iNetworkByteOrder win32::int)
       (iSecurityScheme win32::int)
       (dwMessageSize win32::DWORD)
       (dwProviderReserved win32::DWORD)
       (szProtocol (win32::CHAR #| WSAPROTOCOL_LEN + 1 |# 256))))
(win32:defwintype LPWSAPROTOCOL_INFOA (WSAPROTOCOL_INFOA *))

#! (:export t :library "WS2_32")
/* Flag bit definitions for dwProviderFlags */
#define PFL_MULTIPLE_PROTO_ENTRIES          0x00000001
#define PFL_RECOMMENDED_PROTO_ENTRY         0x00000002
#define PFL_HIDDEN                          0x00000004
#define PFL_MATCHES_PROTOCOL_ZERO           0x00000008
#define PFL_NETWORKDIRECT_PROVIDER          0x00000010

/* Flag bit definitions for dwServiceFlags1 */
#define XP1_CONNECTIONLESS                  0x00000001
#define XP1_GUARANTEED_DELIVERY             0x00000002
#define XP1_GUARANTEED_ORDER                0x00000004
#define XP1_MESSAGE_ORIENTED                0x00000008
#define XP1_PSEUDO_STREAM                   0x00000010
#define XP1_GRACEFUL_CLOSE                  0x00000020
#define XP1_EXPEDITED_DATA                  0x00000040
#define XP1_CONNECT_DATA                    0x00000080
#define XP1_DISCONNECT_DATA                 0x00000100
#define XP1_SUPPORT_BROADCAST               0x00000200
#define XP1_SUPPORT_MULTIPOINT              0x00000400
#define XP1_MULTIPOINT_CONTROL_PLANE        0x00000800
#define XP1_MULTIPOINT_DATA_PLANE           0x00001000
#define XP1_QOS_SUPPORTED                   0x00002000
#define XP1_INTERRUPT                       0x00004000
#define XP1_UNI_SEND                        0x00008000
#define XP1_UNI_RECV                        0x00010000
#define XP1_IFS_HANDLES                     0x00020000
#define XP1_PARTIAL_MESSAGE                 0x00040000
#define XP1_SAN_SUPPORT_SDP                 0x00080000

int PASCAL FAR WSCEnumProtocols(
  LPINT               lpiProtocols,
  LPWSAPROTOCOL_INFOW lpProtocolBuffer,
  LPDWORD             lpdwBufferLength,
  LPINT               lpErrno
);

!#

(defvar *addr-size* 28) 

(defun clear-mem (buffer size) (dotimes (n size) (setf (cref (byte *) buffer n) 0))) ; define calloc?

(defun c-to-addr (sockaddr)
    (let ((dotted (malloc 64)))
        (if (eq (cref (:short *) sockaddr *) AF_INET)
            (with-c-struct (ignore sockaddr sockaddr_in)
                (getnameinfo sockaddr 16 dotted 64 null 0 NI_NUMERICHOST)
                (list :family 'af_inet
                       :port (ntohs sin_port)
                       :ipaddr (c-bytes-to-lisp-bytes sin_addr 4)
                       :dotted (c-string-to-lisp-string dotted)))
            (with-c-struct (ignore sockaddr sockaddr_in6)
                (getnameinfo sockaddr 28 dotted 64 null 0 NI_NUMERICHOST)
                (list :family 'af_inet6
                       :port (ntohs sin6_port)
                       :flow sin6_flowinfo
                       :ipaddr (c-bytes-to-lisp-bytes sin6_addr 16)
                       :scope sin6_scope_id
                       :dotted (c-string-to-lisp-string dotted))))))

(defun addr-to-c (addr)
    (let ((family (symbol-value (getf addr :family))) (c (malloc *addr-size*)))
        (if (eq family AF_INET)
            (with-c-struct (ignore c sockaddr_in)
                (setq sin_family family sin_port (htons (getf addr :port)))
                (lisp-bytes-to-c-bytes (getf addr :ipaddr) sin_addr))
            (with-c-struct (ignore c sockaddr_in6)
                (setq sin6_family family sin6_port (htons (getf addr :port))
                         sin6_flowinfo (getf addr :flow) sin6_scope_id (getf addr :scope))
                (lisp-bytes-to-c-bytes (getf addr :ipaddr) sin6_addr))) c))

(defun get-addr-info (host &key (port 0) host-is-name ipv6 errorp)
    "Port: symbol, string or integer. Host-is-name if host string is a host name: nil, :unspec or t. IPv6: nil, :only or t."
    (let ((hint (malloc (sizeof 'addrinfo))) (p (malloc (sizeof 'win:pvoid))) pp)
        (clear-mem hint (sizeof 'addrinfo))
        (with-c-struct (ignore hint addrinfo)
            (setq ai_flags (if (eq host-is-name t) 0 AI_NUMERICHOST)
                     ai_family (if (eq ipv6 t) AF_UNSPEC (if (not ipv6) AF_INET AF_INET6))))
        (if (zerop (getaddrinfo (lisp-string-to-c-string host) (lisp-string-to-c-string (format nil "~a" port)) hint p))
            (setq pp (cref (win:pvoid *) p *))
            (when (and errorp (not (eq host-is-name :unspec))) (error "Winsock error ~A." (WSAGetLastError))))
        (or (and pp (do ((p pp (cref addrinfo p ai_next)) res) ((cpointer-null p) (freeaddrinfo pp) (nreverse res))
                              (push (c-to-addr (cref addrinfo p ai_addr)) res)))
             (and (eq host-is-name :unspec) (get-addr-info host :port port :host-is-name t :ipv6 ipv6 :errorp errorp)))))

(defun get-name-info (addr &key dottedp portp errorp) "Returns host and port names as values."
    (let ((host-text (malloc NI_MAXHOST)) (serv-text (if portp (malloc NI_MAXSERV) null)))
        (if (zerop (getnameinfo (addr-to-c addr) *addr-size* host-text NI_MAXHOST serv-text NI_MAXSERV
                                             (logior (if dottedp NI_NUMERICHOST 0) (if errorp NI_NAMEREQD 0))))
            (values (c-string-to-lisp-string host-text)
                    (when portp (setq serv-text (c-string-to-lisp-string serv-text)) (unless (numberp (parse-integer serv-text :junk-allowed t)) serv-text)))
            ; no error as in Vista and later but lets user know of failure by returning nil as the second value rather than numeric port string
            (let ((error (WSAGetLastError)))
                (if (and portp (eq WSANO_DATA error))
                    (get-name-info addr :dottedp dottedp :errorp errorp)
                    (when errorp (error "Winsock error ~A." error)))))))

(defun address-family-available-p (address-family)
  (let* ((lpdwBufferLength (ct:malloc (ct:sizeof 'win32::DWORD)))
         (lpiErrno (ct:malloc (ct:sizeof 'win32::int))))
    (setf (ct:cref (win32::DWORD *) lpdwBufferLength 0) 0)
    (setf (ct:cref (win32::int *) lpiErrno 0) 0)
    (unwind-protect
        (progn
          (WSCEnumProtocols (ct::int-to-foreign-ptr 0) (ct::int-to-foreign-ptr 0) lpdwBufferLength lpiErrno)
          (let ((protocols-info-array (ct:malloc (ct:cref (win32::DWORD *) lpdwBufferLength 0))))
            (unwind-protect
                (let ((protocols-count (WSCEnumProtocols (ct::int-to-foreign-ptr 0) protocols-info-array lpdwBufferLength lpiErrno)))
                  (block enumeration-loop
                    (dotimes (n protocols-count)
                      (let ((proto (cref (WSAPROTOCOL_INFOW *) protocols-info-array n)))
                        (when (= (cref WSAPROTOCOL_INFOW proto iAddressFamily)
                                 address-family)
                          (return-from enumeration-loop t))))))
              (ct:free protocols-info-array))))
      (ct:free lpdwBufferLength)
      (ct:free lpiErrno))))

(provide "WINSOCK")
