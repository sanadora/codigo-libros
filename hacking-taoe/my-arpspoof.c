//#include <sys/types.h> included by net/ethernet.h
#include <sys/socket.h> 
#include <linux/if_packet.h> 
#include <net/ethernet.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linux/if_arp.h> // for constants


#include <errno.h>

struct my_arphdr {
  __be16		ar_hrd;		/* format of hardware address	*/
  __be16		ar_pro;		/* format of protocol address	*/
  unsigned char	ar_hln;		/* length of hardware address	*/
  unsigned char	ar_pln;		/* length of protocol address	*/
  __be16		ar_op;		/* ARP opcode (command)		*/
  /*
   *	 Ethernet looks like this : This bit is variable sized however...
   */
  unsigned char		ar_sha[ETH_ALEN];	/* sender hardware address	*/
  unsigned char		ar_sip[4];		/* sender IP address		*/
  unsigned char		ar_tha[ETH_ALEN];	/* target hardware address	*/
  unsigned char		ar_tip[4];		/* target IP address		*/
};


void decode_arp(const u_char *header_start) {
  const struct my_arphdr *arp_header;
  int i;

  arp_header = (const struct my_arphdr *)header_start;
  printf("((( Layer 3 ::: ARP Header )))\n");
  printf("hardware address format: 0x%hx", ntohs(arp_header->ar_hrd));
  printf("\tlength: %hhu\n", arp_header->ar_hln);
  printf("protocol address format: 0x%hx", ntohs(arp_header->ar_pro));
  printf("\tlength: %hhu\n", arp_header->ar_pln);
  printf("ARP opcode: 0x%hx\n", ntohs(arp_header->ar_op));

  printf("Sender MAC: ");
  for(i = 0; i < ETH_ALEN; i++) {
    if (i > 0) printf(":");
    printf("%hhX", arp_header->ar_sha[i]);
  }
  printf("\tIP: ");
  for(i = 0; i < 4; i++) {
    if (i > 0) printf(".");
    printf("%hhu", arp_header->ar_sip[i]);
  }

  
  printf("\nTarget MAC: ");
  for(i = 0; i < ETH_ALEN; i++) {
    if (i > 0) printf(":");
    printf("%hhX", arp_header->ar_tha[i]);
  }
  printf("\tIP: ");
  for(i = 0; i < 4; i++) {
    if (i > 0) printf(".");
    printf("%hhu", arp_header->ar_tip[i]);
  }

  printf("\n");
}

void decode_ethernet(const u_char *header_start) {
  int i;
  const struct ether_header *ethernet_header;

  ethernet_header = (const struct ether_header *)header_start;
  printf("[[ Layer 2 :: Ethernet Header ]]\n");
  printf("[ Source: %02x", ethernet_header->ether_shost[0]);
  for (i = 1; i < 6; i++) {
    printf(":%02x", ethernet_header->ether_shost[i]);
  }

  printf("\tDest: %02x", ethernet_header->ether_dhost[0]);
  for (i = 1; i < 6; i++) {
    printf(":%02x", ethernet_header->ether_dhost[i]);
  }
  printf("\tType: %hu ]\n", ethernet_header->ether_type);
}

void build_ether(char *buffer, char *tha, char *sha) {
  int i;
  struct ethhdr *ether_header = (struct ethhdr *) buffer;
  char ether_addr_part[3] = {0, 0, '\n'};

  printf("building ETHERNET frame\n");
  
  for (i = 0; i < ETH_ALEN; i++) {
    // copy target address part into header structure
    strncpy(ether_addr_part, tha + (i * 3), 2);
    ether_header->h_dest[i] = strtol(ether_addr_part, NULL, 16);
    // copy sender address part into header structure
    strncpy(ether_addr_part, sha + (i * 3), 2);
    ether_header->h_source[i] = strtol(ether_addr_part, NULL, 16);
  }

  ether_header->h_proto = htons(ETH_P_ARP);
}

void build_arp(char *buffer, char *tha, char *tpa, char *sha, char *spa) {
  struct my_arphdr *arp_header = (struct my_arphdr *) buffer;
  int i;

  char ether_addr_part[3] = {0, 0, '\n'};
  char ip_addr_part[3] = {0, 0, '\n'};

  struct in_addr sender_ip;
  struct in_addr target_ip;

  unsigned char sender_ip_mask[4];

  printf("building ARP frame\n");
  
  arp_header->ar_hrd = htons(ARPHRD_ETHER);
  arp_header->ar_pro = htons(0x800);
  arp_header->ar_hln = 6;
  arp_header->ar_pln = 4;
  arp_header->ar_op = htons(ARPOP_REPLY);
  
  for (i = 0; i < ETH_ALEN; i++) {
    // copy target address part into header structure
    strncpy(ether_addr_part, tha + (i * 3), 2);
    arp_header->ar_tha[i] = strtol(ether_addr_part, NULL, 16);
    // copy sender address part into header structure
    strncpy(ether_addr_part, sha + (i * 3), 2);
    arp_header->ar_sha[i] = strtol(ether_addr_part, NULL , 16);
  }
  
  inet_aton(spa, (struct in_addr *) &arp_header->ar_sip);
  inet_aton(tpa, (struct in_addr *) &arp_header->ar_tip);
}

// sha = sender's hardware addres, spa = sender's protocol address
// tha = target's hardware addres, tpa = target's protocol address
int arp_reply(char *sha, char* spa, char *tha, char *tpa) {
  int packet_socket, i;
  ssize_t sent_length;
  char buffer[100];
  struct sockaddr_ll *address;
  socklen_t addrlen;
  char ether_addr_part[3] = {0, 0, '\n'};

  if ((address = (struct sockaddr_ll *) malloc(sizeof(struct sockaddr_ll))) == NULL) {
    printf("%s\n", "error on malloc");
    exit(1);
  }

  addrlen = sizeof(struct sockaddr_ll);

  if ((packet_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ARP))) == -1) {
    printf("error creating socket: %s", strerror(errno));
    exit(1);
  }

  build_ether(buffer, tha, sha);
  build_arp(buffer + ETH_HLEN, tha, tpa, sha, spa);

  address->sll_family = AF_PACKET;
  address->sll_protocol = 0;
  address->sll_ifindex = 3;
  address->sll_hatype = 0;
  address->sll_pkttype = 0;
  address->sll_halen = ETH_ALEN;
  for (i = 0; i < ETH_ALEN; i++) {
    strncpy(ether_addr_part, tha + (i * 3), 2);
    address->sll_addr[i] = strtol(ether_addr_part, NULL, 16);
  }

  decode_ethernet(buffer);
  decode_arp(buffer + ETH_HLEN);
  
  if ((sent_length = sendto(packet_socket, buffer, 199, 0,(struct sockaddr *)address, addrlen)) == -1) {
    printf("error sending ARP packet: %s\n", strerror(errno));
    exit(1);
  } else {
    printf("ARP REPLY: sent %d bytes\n", sent_length);
  }
   
}

void capture_arp(int packet_socket) {
  int recv_length, i;
  char buffer[200];
  struct sockaddr_ll *address;
  socklen_t addrlen;
 
  if ((address = (struct sockaddr_ll *) malloc(sizeof(struct sockaddr_ll))) == NULL) {
    printf("%s\n", "error on malloc");
    exit(1);
  }
  
  addrlen = sizeof(struct sockaddr_ll);

  if ((recv_length = recvfrom(packet_socket, buffer, 199, 0,(struct sockaddr *)address, &addrlen)) == -1) {
    printf("error capturing ARP packet: %s", strerror(errno));
    exit(1);
  }
   
  printf("ARP packet... captured. Length: %d\n\n", recv_length);

  decode_ethernet(buffer);
  decode_arp(buffer + ETH_HLEN);
    
  printf("\nssl_family: \t0x%hX\n", address->sll_family);
  printf("ssl_protocol\t0x%hX\n", ntohs(address->sll_protocol));
  printf("ssl_ifindex:\t  %u\n", address->sll_ifindex);
  printf("ssl_hatype: \t0x%hX\n", address->sll_hatype);
  printf("ssl_halen:  \t0x%hhX\n", address->sll_halen);
  printf("ssl_addr:   \t");

  for (i = 0; i < address->sll_halen; i++) {
    if (i > 0) {
      printf(":");
    }
    printf("%02hhX", address->sll_addr[i]);
  }
  printf("\n");

}

int main(int argc, char *argv[]) {
  int packet_socket;

  if (argc < 2) {
    printf("Usage: %s [--capture|reply]\n", argv[0]);
    printf("                   --reply <sender_hw_addr> <sender_ip_addr> <target_hw_addr> <target_ip_addr>\n");
    exit(0);
  }

  if ((packet_socket = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ARP))) == -1) {
    printf("error creating socket: %s\n", strerror(errno));
    exit(1);
  }
  
  if (strcmp("--capture", argv[1]) == 0) {
    capture_arp(packet_socket);    
  } else if (strcmp("--reply", argv[1]) == 0) {
    arp_reply(argv[2], argv[3], argv[4], argv[5]);
  }
  
  exit(0);
}


