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
 mov sp,#SETUP_STACKSIZE ! set the stack for First Stage ! 스택주소 = 0x07c0:800(2048) = 0x8400 부터 아래로 쌓인다
 sti ! now it is safe ! 스택설정후 인터럽트 허용

 push dx ! set ext_dl (and ext_dh, which is not used) ! first.S가 mbr이면 dl = drive number
 push bx ! WATCH the order of pushes
 push es ! set ext_es
 push si ! set ext_si







 cld ! do not forget to do this !!! 스트링 명령을 위한 df 초기화
 mov ds,ax ! address data area = 0x7c0
 xor bp,bp ! shorted addressing , bp = 0 ! bp를 참조해서 메모리를 읽어들이기 위한 세팅. 가까운 곳의 메모리를 읽을때 간접주소 방식을 이용하면 직접 지정방식보다 코드 크기를 줄일수 있다. 직접지정방식도 ax레지스터를 사용하면 크기가 줄어든다. 아래 샘플 참조.
!	890E5000          mov [0x50],cx
!	894E50            mov [bp+0x50],cx
! a BIOS has been found where the video interrupt (0x10) trashes DX
! so, we had better be very paranoid about DX
! 


 pusha ! protect DX
# 168 "first.S"
 mov ax,#0x1200 ! enable video (VGA) ! BL이 0x36이면 enable video refresh ; 비디오 메모리의 내용을 표시한다. disable이면 내용이 있어도 화면에 검은 화면만 나타난다.
 mov bl,#0x36 ! (probably a nop on EGA or MDA)

 int 0x10 ! video call

 popa ! restore DX




 mov al,#0x0d ! gimme a CR ... ! CR, LF를 출력해 아랫줄 처음으로 간다.
 call display
; the suspect call for trashing DX on one BIOS:
 mov al,#0x0a ! ... an LF ...
 call display






 mov al,#0x4c ! ... an 'L' ...
 call display ! LILO 문자열중 첫 L을 출력
 ! load again?
lagain:
 pusha ! preserve all the registers for restart

 push ds ! 0x7c0
 pop es ! use buffer at end of boot sector ! es = 0x7c0

 cmp dl,#0xfe ! possible boot command line (chain.S) ! 0xfe
 jne boot_in_dl ! external parameter는 일반적으로 쓰이지 않는다. 
 mov dl,dh ! code passed in DH instead ; external parameter라면 넘어온 dh를 dl에 넣는다. (external parameter의 드라이브값)
boot_in_dl:

 mov bx,#map2 ! buffer for volume search ! map2는 first 끝. 하드 검색을 위해 MBR을 읽어들일메모리 주소. first 다음(0x7c0:200h)
 mov dh,[d_dev](bp) ! map device to DH ! ss=0x7c0, bp=0. 맵 파일이 저장된 하드 번호. 테스트결과 0x80,0x81등으로 변한다.

 ! 이부분은 second stage로 넘어가기 위한 사전준비다. lilo는 하드 순서가 바뀌더라도 volumeID로 map 파일이 있는 하드를 찾아낸다. map파일에 second와 index sector가 있다.
 mov ax,dx ! copy device code to AL ; 
 and ah,#0x80 ! AH = 0x80 ; al=장치번호
 xor al,ah ! hi-bits must be the same
 js use_installed ; map이 저장된 곳과 부팅된 장치 종류(하드=0x8?)가 일치하지 않으면 검색하러 간다.
 cmp al,#16 ! limit the device code = 16 ; MAX_BIOS_DEVICES
 jae use_installed ! jump if DL is not valid ; 하드 번호가 최대 bios 장치수 보다 크면 봄륨검색

! 
! map is on boot device for RAID1, and if so marked; viz.,
! 
 test byte ptr [prompt](bp),#64 ! FLAG_MAP_ON_BOOT ; 덤프결과 레이드가 아닌 상황에서 켜지지 않았다.
 jnz use_boot ! as passed in from BIOS or MBR loader ; 부팅된 하드에 맵파일이 있다면 하드 검색루틴을 건너뛴다. (레이드시 켜지는걸로 추측)
! 볼륨검색을 위한 사전준비
use_installed: ! 일반적으로 이곳으로 와서 일치하는 볼륨을 검사한다.
 mov dl,dh ! device code to DL ; map이 저장된 곳
 mov esi,[map_serial_no](bp)
 or esi,esi
 jz done_search ! 저장된 volumeID가 없으면 검색하지 않고 저장된 하드번호를 쓴다.

 push dx ! save flags

 mov ah,#8 ! get number of hard disks
 mov dl,#0x80 ! drive number 0,1..=floopy 0x80,0x81..=hard drive ; 연결된 하드 갯수를 구한다.
 push bx ! paranoia ! bx 값이 바뀌기 때문에 저장
 int 0x13
 pop bx
 jc error

 movzx cx,dl ! extend to word in CX ! 하드개수를 cx로 확장


 mov dx,#0x80-1 ! device 80, flags=0 



! volume again? volumeID로 설치된 장치를 첫번째 하드부터 순차적으로 검색한다.
vagain:
 inc dx	! 하드번호++
 xor eax,eax

 inc ax ! geometric addressing ; 플래그(dh)가 0이라 선형주소가 아닌 CHS방식이다. MBR을 나타낸다.

 call disk_read ! read ! es:bx에 MBR을 가져온다. disk_read는 bx가 변경되지 않는다.(pread에서 증가)

 cmp esi,[0x1be -6](bx) ! MBR의 446-6. 0x440=volumeID esi=[map_serial_no] (=lilo가 설치된 하드)
 je vol_found	! 일치하는 volumeID를 찾았다
 loop vagain ! 시스템의 하드수 만큼 반복

 pop dx ! restore specified BIOS code ! 찾지 못했다면 부팅시 넘어온 DL 복구, dl=부팅된 하드 혹은 external parameter 하드번호
    ! AX and DX are identical at this point

vol_found:
  ! uses value in DX, stack may have extra value
  ! DL 디스크의 second index sector를 읽어들인다.
done_search:
use_boot:
 push bx ! save map2 for later ; 0x7c0:200

 mov dh,[d_flag](bp) ! get device flags to DH ! 0x60=EDD, 0x40=disk_convert, 0 = disk_geometry(CHS) ; 디스크 플래그를 넣는다. disk_read가 이 값을 참조한다.
 mov si,#d_addr	! [ds:si]는 second의 섹터주소들이 담긴 인덱스 섹터의 섹터주소다
 call pread ! increments BX +512(섹터값) ; 인덱스 섹터를 읽는다.

 mov ah,#0x99 ! possible error code
 cmp dword (bx-4),#0x4f4c494c ! "LILO" ! second stage index 마지막 4바이트를 LILO 문자열과 비교
 jne error

 pop si ! point at #map2 | 512


 push #SETUP_STACKSIZE/16 + BOOTSEG + 512/16*2 ! 세그먼트로 계산 0x80 + 0x7c0 + 0x40 = 0x88
 pop es	; (0x7c00 + stack(2048) + first sector + 로딩용 섹터)/16 = 0x880





 xor bx,bx
! second stage load? index sector로 second.s를 읽어들인다.
sload:
 call pread ! read using map at DS:SI | 0x7c0:200의 index sector를 기반으로 second stage를 0x880:0 주소공간에 로드한다.
 jnz sload ! into memory at ES:BX (auto increment) ! 끝까지 읽는다.

! Verify second stage loader signature

 mov si,#sig ! pointer to signature area
 mov di,si ; 세그먼트가 다르다.
 mov cx,#length ! number of bytes to compare
 mov ah,#0x9A ! possible error code | first와 second LILO 문자열, version, map stamp가 같지않으면 에러 출력
 repe
   cmpsb ! check Signature 1 & 2 ! 10바이트만큼 체크 도중에 틀리면 중단 -> error
 jne error ! check Signature 2



 mov al,#2 ! do not touch AH (error code) ; stage 정보 (second stage)
 scasb		! second.s의 stage: 값과 한바이트 비교. x86 자료형은 컴파일될때 뒤집혀 저장된다.
 jne error


! Start the second stage loader DS=location of Params

 push es ! segment of second stage
 push bp ! BP==0 ! 0x880:0  주소를 스택에 넣는다.

 mov al,#0x49 ! display an 'I'
 call display

 retf ! Jump to ES:BP ! 두번째 문자 I를 출력하고 Second Stage로 점프!!!




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
! eax=섹터주소, dh=플래그, es:bx=메모리 주소
disk_read:



 pusha






 push bp ! BP==0 ! 확장된 디스크 읽기를 위해 패킷을 스택을 통해 si로 넘겨준다
 push bp ! BP==0

 push eax ! low order disk address ; 섹터주소






 push es ! memory segment ES
 push bx ! memory offset BX
 push #1 ! sector count
 push #16 ! size of packet = 16 bytes
 mov si,sp ! address of packet DS:SI ! 인자들을 si를 통해 읽는다.

 push bx ! disk_int13에서 복원 

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
 test cl,#01 ;EDD packet calls supported ! bit0  extended disk access functions (AH=42h-44h,47h,48h) supported
 jnz disk_edd	! lba방식으로 점프
! LBA 변환 공식 LBA = (( C * HPC ) + H ) * SPT + S -1 
disk_convert:
 push dx
 push es ! protect on floppies
 mov ah,#8 ! get geometry
 int 0x13 ! Linear->CHS로 변환하기위해 디스크 정보를 얻어온다.
 pop es
disk_error3: ! transfer through on CF=1
 jc error ! disk_error12
! CHS 변환 공식 C=LBA / (SPT*HPC)  H=(LBA/SPT) % HPC  S=(LBA % SPT) + 1

 push cx 
 shr cl,#6 ;;;; cl 2bits(high) + ch 8bits(low) = 10bits cylinder(0-1023)
 xchg cl,ch ;CX is max cylinder number 
 mov di,cx ;DI saves it  ; DI=하드 실린더 수
 pop cx

 shr dx,#8 ! dx = Heads Per Cylinder (0-255)
 xchg ax,dx ;AX = HPC
 inc ax ;AX is number of heads (256 allowed)
 ! di=Cylinders, ax=HPC
; compensate for Davide BIOS bug
 dec cx ; 1..63 -> 0..62; 0->63 ! and후 0=64로 만들기 위해 앞뒤에 +-1 추가
 and cx,#0x003f ;CX is number of sectors = 63 ! sectors per track 만 남긴다
 inc cx ; allow Sectors==0 to mean 64

 mul cx ; kills DX also ! HPC * SPT (Secters per track)
 xchg ax,bx ;save in BX ! bx에 저장(최대 256*64=16384)
    ; 섹터 주소 ( [si+8], [si+10], eax)
 mov ax,[edd_d_addr](si) ;low part of address ! DX,AX=LBA
 mov dx,[edd_d_addr+2](si) ;hi part of address

 cmp dx,bx	! bx = maximum head 나눗셈 결과가 ax를 초과하게된다.
 jae disk_error2 ;prevent division error

 div bx ;AX is cyl, DX is head/sect ; C=LBA/(SPT*HPC)
	! AX=몫(Cylinder).
	! DX=나머지. (H * SPT) + S - 1 

 cmp ax,di ; AX=cylinder DI=max cylinders

 ja disk_error2 ;cyl is too big ; 최대 실린더를 초과하면 에러
! 섹터변환
 shl ah,#6 ; save hi 2 bits ; CHS 실린더+섹터 형식에 맞게 실린더의 상위 2비트를 올리고 맞교환한다.(ah<->al)
 xchg al,ah
 xchg ax,dx ; Cylinder는 Low8,High2로 DX에 보존. AX는 아까 계산한 나머지
 div cl ; (( H * SPT ) + S - 1) / SPT ; 나머지:AH=S-1  몫:AL=Head
 or dl,ah ;form Cyl/Sec ! Cyliner(Low8,High2),Sector 형식으로 저장
 mov cx,dx ; cx=실린더,섹터 정보
 inc cx ; sector is 1 based ; CHS 섹터는 1부터 시작

 pop dx ! restore device code
 mov dh,al ! set head#
 jmp disk_read2 ; ax=0x201 CHS로 한섹터를 읽는다.



disk_edd:
 mov ah,#0x42
disk_int13: ! 실제 인터럽트 호출 루틴
 pop bx

 mov bp,#5 ! bp를 카운터로 5번 시도후 실패시 disk_error3으로 간다.
disk_retry:
 pusha
 int 0x13




 jnc disk_okay

 dec bp ! does not alter CF, already 0 ; 종료조건 bp==0
 jz disk_error3 ! go to "jc" with CF=1 & ZF=1

 xor ax,ax ! reset the disk controller
 int 0x13
 popa ! reset AX,BX,CX,DX,SI
 dec bp ! fix up BP count ; 실제 카운트다운
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
! [DS:SI]==섹터주소 es:bx==메모리 공간. 섹터주소가 0이면 리턴 zf=0
pread:
 lodsd ! get address ; 4바이트를 읽어온다. 인덱스는 4바이트 구조체
 or eax,eax
 jz done
 add eax,[raid](bp) ! reloc is 0 on non-raid 레이드를 위해 섹터주소 보정
 call disk_read

 add bh,#512/256 ! next sector ! 디스크를 읽어들일 메모리 주소. 코드 크기를 줄이기 위해 bh에 2를 더한다. = 0x200 = 512
done:
 ret





! ah를 4비트씩 2번 출력 + ASCII 변환후 3A-3F를 빼고 변환한다. daa,adc명령으로 단순비교문보다 몇바이트 절약한다.
bout: rol ax,#4 ! bring hi-nibble to position
 call nout
 rol ax,#4 ! bring lo-nibble to position
nout: and al,#0x0F ! display one nibble
 daa ! shorter conversion routine
 add al,#0xF0
 adc al,#0x40 ! is now a hex char 0..9A..F ! ASCII 0-9, A-F만 출력된다.

; display - write byte in AL to console
; preserves all register contents
;
display0:

display:

 pusha ! make sure no register is changed ! 레지스터들을 저장
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
