# 1 "first.S"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "first.S"
# 12 "first.S"
# 1 "lilo.h" 1
# 63 "lilo.h"
# 1 "/usr/include/linux/version.h" 1 3 4
# 64 "lilo.h" 2
# 77 "lilo.h"
# 1 "version.h" 1
# 78 "lilo.h" 2
# 377 "lilo.h"
BOOTSEG = 0x07C0 ! original address of boot-sector
PARTS_LOAD= 0x0600 ! partition sector load address
PARTS_SCR = 0x0800 ! ditto, for non-boot partitions
PART_TABLE= 0x07BE ! partition table

INITSEG = 0x9000 ! we move boot here - out of the way
SETUPSEG = 0x9020 ! setup starts here
SYSSEG = 0x1000 ! system loaded at 0x10000 (65536).

MAX_DESCR_SECS_asm = 3 ! **
MAX_DESCR_SECTORS_asm = 12 ! **
MAX_IMAGE_NAME_asm = 15 ! **
MAX_PW_CRC_asm = 5 ! **
SECTOR_SIZE_asm = 512 ! **
MAX_MENU_TITLE_asm = 37 ! **
MAX_BIOS_DEVICES_asm = 16 ! **
MAX_RAID_DEVICES_asm = 12 -3 +6 ! **
DEV_MASK_asm = 0x80+16 -1 ! **


STACKSEG = 0x9000 ! MUST == INITSEG for kernel 2.0.36 (and others?)
SETUP_STACKSIZE = 2048 ! stacksize for kernel setup.S
# 408 "lilo.h"
STACK = 2048 ! amount of stack space to reserve
SSDIFF = 0

BOOTSECT = 0x200 ! kernel bootsect.S




KBBEG = 0x41A ! beginning of keyboard buffer
KBEND = 0x41C ! end of keyboard buffer
KBLOW = 0x1e
KBHIGH = 0x3e

!
! Memory layout
!
! 0x007BE-0x007FD 64 B partition table
! 0x07C00-0x07DFF 0.5 kB HD/FD boot load address
! 0x10000-0x8FFFF 512.0 kB kernel (zImage)
! 0x90000-0x901FF 0.5 kB kernel floppy boot sector (bootsect.S)
! 0x90200-0x967FF 25.5 kB kernel setup code (setup.S) and heap
! 0x96800-0x969FF 0.5 kB 0xbb920890 stack
! 0x96A00-0x96BFF 0.5 kB 0xbb920890 first stage loader
! 0x96C00-0x985FF 6.5 kB 0xbb920890 second stage loader
! 0x98600-0x987FF 0.5 kB file map load area
! 0x98800-0x98BFF 1 kB descriptor table load area
! 0x98C00-0x98DFF 0.5 kB default command line load area
! 0x98E00-0x98FFF 0.5 kB keyboard translation table load area
! 0x99000-0x991FF 0.5 kB parameter line construction area
! 0x99200-0x9FFFF 27.5 kB Extended BIOS Data Area

! when 0xbb920890 has loaded the kernel, and control is transfered to
! the kernel setup.S code at 0x9020:0000
!
! 0x007BE-0x007FD 64 B partition table
! 0x07C00-0x07DFF 0.5 kB HD/FD boot load address
! 0x10000-0x8FFFF 512.0 kB kernel (zImage)
! 0x90000-0x901FF 0.5 kB kernel floppy boot sector (bootsect.S)
! 0x90200-0x967FF 25.5 kB kernel setup code (setup.S) and heap
! 0x96800-0x987FF 8.0 kB additional heap space
! 0x98800-0x98FFF 2.0 kB stack created for (setup.S)
! 0x99000-0x991FF 0.5 kB parameter line for kernel
! 0x99200-0x9FFFF 27.5 kB Extended BIOS Data Area

CL_MAGIC_ADDR = 0x20 ! command line magic number
CL_MAGIC = 0xa33f ! very unusual command sequence
CL_OFFSET = 0x22 ! command line offset
CL_LENGTH = 512 ! maximum length = 256-1

! 0x90020-0x90021 2 by command line magic number
! 0x90022-0x90023 2 by command line offset

CL_HEADER_ID = 0x202 ! "HdrS"
CL_HDRS_VERSION = 0x206 ! 0x0201=old; 0x0202=new
NEW_VERSION = 0x202 ! 0x0202 for new cmdline protocol
CL_POINTER = 0x228 ! new pointer is dword address
CL_RAMDISK_MAX = CL_POINTER+4 ! ramdisk_max; header version 0x0203
# 13 "first.S" 2
get common.s
# 40 "first.S"
! VIDEO_ENABLE for those systems that disable the video on boot
! = 0 first stage does not enable video
! = 1 use get vid mode/set vid mode to enable
! = 2 use VGA enable call to enable video
! (cannot use, as code gets too big)
! = 3 use direct mode set (mode 3, CGA, EGA, VGA)
! = 7 use direct mode set (mode 7, MDA)
!
# 56 "first.S"
! do not change the following -- it must correspond to the code in bsect.c



 .text

 .globl _main

 .org 0

zero:
_main: cli ! NT 4 blows up if this is missing
 jmp start

stage: .byte 1
 .org 4
reloc:

 .word theend-zero ! size of the code & params



 .org 6

! Boot device parameters. They are set by the installer.

sig: .ascii "LILO"
vers: .word 256*2 +23
mapstamp: .long 0 ! map timestamp

length = *-sig ! for the stage 1 vs stage 2 comparison

raid: .long 0 ! raid sector offset
tstamp: .long 0 ! timestamp
map_serial_no: .long 0 ! volume S/N containing map file
prompt: .word 0 ! indicates whether to always enter prompt
    ! contains many other flags, too

d_dev: .byte 0x80 ! map file device code
d_flag: .byte 0 ! disk addressing flags
d_addr: .long 0 ! disk addr of second stage index sector


edd_packet = 0
;;; .word 16 ! size of packet
;;; .word 1 ! count of bytes to read

edd_addr = 4
;;; .word map2 ! where to read
;;; .word *-* ! segment where to read

edd_d_addr = 8
;;; .long 1 ! low address or CX,DX (geometric)
    ! start at sector 1 for search in geo mode

;;; .long 0 ! hi address
# 125 "first.S"
! The following instruction MUST be
! first instruction after the CLI/JMP short
! at the start of the file; otherwise
! the boot sector relocation fails.
!
start:
 mov ax,#BOOTSEG ! use DS,ES,SS = 0x07C0


 mov ss,ax
 mov sp,#SETUP_STACKSIZE ! set the stack for First Stage / 스택주소 = 0x07c0:800(2048) = 0x8400 부터 아래로 쌓인다
 sti ! now it is safe / 점프와 스택설정까지 인터럽트 금지

 push dx ! set ext_dl (and ext_dh, which is not used) / first.S가 mbr이면 dl = drive number
 push bx ! WATCH the order of pushes
 push es ! set ext_es
 push si ! set ext_si







 cld ! do not forget to do this !!! 스트링 명령을 위한 df 초기화
 mov ds,ax ! address data area = 0x7c0
 xor bp,bp ! shorted addressing , bp = 0 / 메모리를 읽을때 bp를 참조 하면 1-2바이트를 줄일수 있다. 아래 샘플 참조
!    88 25 64 00 00 00       mov    BYTE PTR ds:0x64,ah
!    67 88 66 64             mov    BYTE PTR [bp+0x64],ah
! a BIOS has been found where the video interrupt (0x10) trashes DX
! so, we had better be very paranoid about DX
! 그래픽 모드를 3 (80x25 16color)으로 세팅한다. (int 0x10 ah=0, al=3)


 pusha ! protect DX
# 168 "first.S"
 mov ax,#0x1200 ! enable video (VGA) / video refresh - AL이 0이면 enable refresh
 mov bl,#0x36 ! (probably a nop on EGA or MDA)

 int 0x10 ! video call / 기본은 ah=0 (set video mode) al=3 (80x25 16color)

 popa ! restore DX




 mov al,#0x0d ! gimme a CR ... / CR, LF를 출력해 아랫줄 처음으로 간다.
 call display
; the suspect call for trashing DX on one BIOS:
 mov al,#0x0a ! ... an LF ...
 call display






 mov al,#0x4c ! ... an 'L' ...
 call display ! LILO 문자열중 화면에 L을 출력

lagain: ; load again??? 
 pusha ! preserve all the registers for restart

 push ds ! 0x7c0
 pop es ! use buffer at end of boot sector / es = ds

 cmp dl,#0xfe ! possible boot command line (chain.S) / 0xfe
 jne boot_in_dl ! dl(drive number)이 0xfe(magic number)인지 확인한다. external parameter는 일반적으로 쓰이지 않는다. 쓰인다면 넘겨주는 dh값을 dl으로 값으로 넘긴다. (drive 번호로 추정)
 mov dl,dh ! code passed in DH instead
boot_in_dl: ; 부팅된 하드디스크(dl)를 체크

 mov bx,#map2 ! buffer for volume search / mbr의 끝주소 map2=512
 mov dh,[d_dev](bp) ! map device to DH / ss=0x7c0, bp=0 [ss:bp+d_dev]=0x80

	! 플로피인지 하드인지 체크 / 부팅딘 하드 제한개수(16 or 2)를 >    넘어가면 분기
 mov ax,dx ! copy device code to AL
 and ah,#0x80 ! AH = 0x80 / AL=drive number
 xor al,ah ! hi-bits must be the same
 js use_installed
 cmp al,#16 ! limit the device code = 16
 jae use_installed ! jump if DL is not valid


! map is on boot device for RAID1, and if so marked; viz.,
! 7번째 비트가 꺼져있으면 use_boot로 분기
 test byte ptr [prompt](bp),#64 ! =64(40)=01000000 FLAG_MAP_ON_BOOT
 jnz use_boot ! as passed in from BIOS or MBR loader

use_installed:
 mov dl,dh ! device code to DL / 0x80
 mov esi,[map_serial_no](bp) ! to search for
 or esi,esi
 jz done_search

 push dx ! save flags

 mov ah,#8 ! get number of hard disks
 mov dl,#0x80 ! drive number 0,1..=floopy 0x80,0x81..=hard drive
 push bx ! paranoia / bx에 drive type이 return됨.
 int 0x13
 pop bx
 jc error

 movzx cx,dl ! extend to word in CX


 mov dx,#0x80-1 ! device 80, flags=0




vagain: ; volume again? 부팅된 디스크가 하드가 아니거나 16번째이상이고 prompt 관련 비트가(RAID1) 0이면 첫번째 하드디스크부터 순차적으로 volume을 찾는다. 찾으면 use_boot로 넘어간다.
 inc dx	! 처음에는 0x80 (첫번째 하드)
 xor eax,eax

 inc ax ! geometric addressing

 call disk_read ! read

 cmp esi,[0x1be -6](bx) ! 446-6 바이트 (MBR 데이터영역) 0x440=disk signature / esi=map_serial_no
 je vol_found	! 볼륨을 찾았다면 use_boot로 점프 아니면 cx=0x80만큼 반복
 loop vagain

 pop dx ! restore specified BIOS code
    ! AX and DX are identical at this point

vol_found:
  ! uses value in DX, stack may have extra value

done_search:
use_boot:
 push bx ! save map2 for later | es:bx 에 한섹터 index를 저장 0x7c0:200

 mov dh,[d_flag](bp) ! get device flags to DH / 설치할때 CHS방식인지 선형섹터주소로 저장하는걸로 추측한다.
 mov si,#d_addr	! second stage index sector
 call pread ! increments BX +512(섹터값)

 mov ah,#0x99 ! possible error code
 cmp dword (bx-4),#0x4f4c494c ! "LILO" / second stage index 마지막 4바이트를 LILO 문자열과 비교
 jne error

 pop si ! point at #map2 | 512


 push #SETUP_STACKSIZE/16 + BOOTSEG + 512/16*2 ! 세그먼트로 계산 0x80 + 0x7c0 + 0x40 = 0x880
 pop es





 xor bx,bx

sload:	! second stage load?
 call pread ! read using map at DS:SI | 0x7c0:200에서 4바이트 섹터 주소를 읽어서
 jnz sload ! into memory at ES:BX (auto increment) / 0x880:0 주소공간에 넣는다.

! Verify second stage loader signature

 mov si,#sig ! pointer to signature area
 mov di,si
 mov cx,#length ! number of bytes to compare
 mov ah,#0x9A ! possible error code | first와 second LILO 문자열, version, map stamp가 같지않으면 에러 출력
 repe
   cmpsb ! check Signature 1 & 2 / 10바이트만큼 체크 도중에 틀리면 중단 -> error
 jne error ! check Signature 2



 mov al,#2 ! do not touch AH (error code)
 scasb		! second.s의 stage: 값과 한바이트 비교. x86 자료형은 컴파일될때 뒤집혀 저장된다.
 jne error


! Start the second stage loader DS=location of Params

 push es ! segment of second stage
 push bp ! BP==0 / 0x880:0  주소를 스택에 넣는다.

 mov al,#0x49 ! display an 'I'
 call display

 retf ! Jump to ES:BP / I를 출력하고 Second Stage로 점프!!!




disk_error2:
 mov ah,#0x40 ; signal seek error

! no return from error
error: ! 에러가 뜨면 스택을 줄이고 다시 시도 (lagain)


	mov al,#32 ! display a space
 call display0

 call bout



 dec byte [zero](bp) ! CLI == 0xFA == 250
 jz zzz


 mov sp,#SETUP_STACKSIZE-4*2-8*2 ! set the stack for First Stage



 popa ! restore registers for restart
 jmp near lagain ! redo from start



zzz:

 hlt	! 인터럽트가 걸릴때까지 대기

 jmp zzz ! spin; wait for Ctrl-Alt-Del




! packet read routine

disk_read:



 pusha






 push bp ! BP==0 ! 확장된 디스크 읽기를 위해 패킷을 스택을 통해 si로 넘겨준다
 push bp ! BP==0

 push eax ! low order disk address






 push es ! memory segment ES
 push bx ! memory offset BX
 push #1 ! sector count
 push #16 ! size of packet = 16 bytes
 mov si,sp ! address of packet DS:SI

 push bx

 test dh,#0x40|0x20 ! d_flag에서 LINEAR나 LBA가 켜져있는지 확인한다.
 jz disk_geometric ! 둘다 꺼져있으면 전통적인 CHS방식으로 점프 (ax=0x201)
 ! Linear만 켜져있으면 CHS방식으로 돌아간다
 test dh,#0x20 ! 둘중 하나 혹은 둘다 켜져있는데 LBA가 꺼져있다면 LINEAR다
 jz disk_convert ; it must be LINEAR

 mov bx,#0x55AA ;magic number
 mov ah,#0x41
 int 0x13
 jc disk_convert	! 에러나면 CHS변환
 cmp bx,#0xAA55 ;changed?
 jne disk_convert	! EDD가 동작하지 않으면 역시 CHS변환
 test cl,#01 ;EDD packet calls supported / bit0  extended disk access functions (AH=42h-44h,47h,48h) supported
 jnz disk_edd	! lba방식으로 점프

disk_convert:
 push dx
 push es ! protect on floppies
 mov ah,#8 ! get geometry
 int 0x13 ! 디스크 정보를 얻어온다.
 pop es
disk_error3: ! transfer through on CF=1
 jc error ! disk_error12


 push cx ! cl 상위 2비트 & ch 8비트 = cylinder
 shr cl,#6 ;;;;
 xchg cl,ch ;CX is max cylinder number / 온전한 10bit cylinder
 mov di,cx ;DI saves it
 pop cx

 shr dx,#8 ! dl = dh의 헤더 개수를 shift
 xchg ax,dx ;AX <- DX
 inc ax ;AX is number of heads (256 allowed)
 ! di는 실린더, ax는 헤더+1
; compensate for Davide BIOS bug
 dec cx ; 1..63 -> 0..62; 0->63
 and cx,#0x003f ;CX is number of sectors = 63 / 섹터만 남긴다
 inc cx ; allow Sectors==0 to mean 64

 mul cx ; kills DX also / 섹터(cx) * 헤더(ax)
 xchg ax,bx ;save in BX
    ; second.S가 위치한 섹터 위치( [si+8], [si+10], eax)
 mov ax,[edd_d_addr](si) ;low part of address / logical block address
 mov dx,[edd_d_addr+2](si) ;hi part of address

 cmp dx,bx	! bx = maximum head
 jae disk_error2 ;prevent division error

 div bx ;AX is cyl, DX is head/sect
	! 몫이 AX, 나머지가 DX cyl*sect?
	  

 cmp ax,di

 ja disk_error2 ;cyl is too big
! 섹터변환
 shl ah,#6 ; save hi 2 bits
 xchg al,ah
 xchg ax,dx
 div cl ;AH = sec-1, AL = head
 or dl,ah ;form Cyl/Sec
 mov cx,dx
 inc cx ; sector is 1 based

 pop dx ! restore device code
 mov dh,al ! set head#
 jmp disk_read2



disk_edd:
 mov ah,#0x42
disk_int13: ! int 13h를 두번 시도해본다
 pop bx

 mov bp,#5 ! bp를 카운터로 두번 시도후 실패시 disk_error3으로 간다.
disk_retry:
 pusha
 int 0x13




 jnc disk_okay

 dec bp ! does not alter CF, already 0
 jz disk_error3 ! go to "jc" with CF=1 & ZF=1

 xor ax,ax ! reset the disk controller
 int 0x13
 popa ! reset AX,BX,CX,DX,SI
 dec bp ! fix up BP count
 jmp disk_retry


disk_geometric: ! CHS방식으로 1섹터를 읽어온다.
 push eax
 pop cx
 pop ax
 mov dh,ah

disk_read2:
 mov ax,#0x201 ;read, count of 1
 jmp disk_int13


disk_okay: ! 디스크 읽기가 성공적이면 스택을 복원
; the pusha block is implicitly removed below
;;; mov (si+2*16-1),ah ! set error code
; the error code is never checked
 lea sp,(si+16) ! do not touch carry;
 popa



 ret



! Pointer Read -- read using pointer in DS:SI
! 2nd stage의 섹터위치가 담긴 DS:SI의 포인터를 이용해 한섹터를 es:bx로 읽는다.SI에 0이 있으면 zero flag를 0으로 하고 리턴한다.
pread:
 lodsd ! get address
 or eax,eax
 jz done
 add eax,[raid](bp) ! reloc is 0 on non-raid 레이드면 eax(second stage sector address)에 더한다.
 call disk_read

 add bh,#512/256 ! next sector / 초기값은 map2 + 512 / 메모리 선형주소값. 코드 크기를 줄이기 위해 bh에 더한다.
done:
 ret





! ah를 4비트씩 2번 출력 + ASCII 변환후 3A-3F를 빼고 변환한다. daa,adc명령으로 단순비교문보다 3바이트 절약한다.
bout: rol ax,#4 ! bring hi-nibble to position
 call nout
 rol ax,#4 ! bring lo-nibble to position
nout: and al,#0x0F ! display one nibble
 daa ! shorter conversion routine
 add al,#0xF0
 adc al,#0x40 ! is now a hex char 0..9A..F / ASCII 0-9, A-F만 출력된다.

; display - write byte in AL to console
; preserves all register contents
;
display0:

display:

 pusha ! make sure no register is changed / 레지스터들을 저장
 mov bx,#7 ! BH=0, BL=07
 mov ah,#14 ! AL의 한글자를 출력
 int 0x10
 popa ! restore all the registers



 ret
# 573 "first.S"
theend:

!
! If 'first' loads as the MBR, then there must be space for the partition
! table. If 'first' loads as the boot record of some partition, then
! the space reserved below is not used. But we must reserve the area
! as a hedge against the first case.
!
!
 .org 0x1b6 !
 .word 0,0,0,0 ! space for NT, DRDOS, and LiLO volume S/N

! .org 0x1be ! spot for the partition table
p_table:
 .blkb 16
 .blkb 16
 .blkb 16
 .blkb 16

 .org *-2
 .long 0x62177489 ! boot block check




! Better be exactly 0x200

map2 equ * ! addressed as ES:[map2]
