# 1 "second.S"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "second.S"
# 21 "second.S"
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
# 22 "second.S" 2
get common.s
# 88 "second.S"
LOADSEG = SYSSEG ; max kernel = 1024 sectors







STAGE_MENU = 0






STAGE_BITMAP = 0





STAGE_SERIAL = 0x0100



X=0x9dd476ec



 .text

 .globl _main
 .org 0

_main: jmp start
# 136 "second.S"
 .org 6

! Boot device parameters. They are set by the installer.

sig: .ascii "LILO"
version: .word 256*2 +23
mapstamp: .long 0

stage: .word 2|STAGE_SERIAL|STAGE_MENU|STAGE_BITMAP

port: .byte 0 ; COM port (0 = unused, 1 = COM1, etc.)
sparam: .byte 0 ; serial port parameters (0 = unused)

timout: .word 0 ; input timeout
delay: .word 0 ; boot delay
ms_len: .word 0 ; initial greeting message


kt_cx: .word 0 ; keyboard translation table
kt_dx: .word 0
kt_al: .byte 0

flag2: .byte 0 ; second stage specific flags


! GDT for "high" loading

 .align 16

gdt: ; space for BIOS
 .blkb 0x10
 ; source
 .word 0xffff ; no limits
 .byte 0
 .word LOADSEG>>4 ; start: 0x10000
 .byte 0x93 ; permissions
 .word 0 ; padding for 80286 mode :-(
 ; destination
 .word 0xffff ; no limits
 .word 0 ; start - filled in by user
 .byte 0
 .byte 0x93 ; permissions
 .word 0 ; padding for 80286 mode :-(
 ; space for BIOS
 .blkb 0x10
! start: 에서 continue:까지는 자기복제 루틴이다.
start: cld ; only CLD in the code; there is no STD

 push ds 
 pop fs ; address parameters from here ; fs = 0x07c0






 seg cs	; segment prefix = cs: = 2Eh
 mov [init_dx],dx ; save DX passed in from first.S ; DX(시작 드라이브)의 정보를 [cs:init_dx]에 저장한다.

 int 0x12 ; get memory available ; 메모리의 값. ax = 640-EBDA(KB) 값을 리턴한다. http://is.gd/YidBqs, http://is.gd/4d0zzT




 shl ax,#6 	; convert to paragraphs ; ax의 KB 단위를 세그먼트 단위로 바꾼다.
 sub ax,#Dataend/16 ; ax=640-EBDA-second길이. EBDA 를 제외한 메모리 끝에 복사준비.
 mov es,ax ; destination address
 push cs ; first의 (0x7c00+스택크기(2048)+first+로딩용섹터) /16 = 0x880 =second 시작부분
 pop ds  ; ds=0x880
 xor si,si
 xor di,di
 xor ax,ax 			; 코드를 복제하기 위해 레지스터 초기화
 mov cx,#max_secondary/2 ; count of words to move ; 워드 단위로 옮기기 위해 cx=second길이(max_secondary)/2 
 rep
   movsw			; 코드/데이터영역 복사 (_main ~ max_secondary) 
 add di,#BSSstart-max_secondary	; 코드끝에서 BSSstart사이는 넘어간다. (7섹터)
 mov cx,#BSSsize/2	; 이 소스에서는 512bytes 를 넘지않을것으로 보인다.
 rep
   stosw	; BSS를 0으로 초기화
 push es	; 점프후 cs는 메모리 끝 second 시작부분의 세그먼트가 들어간다.
 push #continue		; 복제코드의 continue:로 점프하기 위한 push
 retf ; branch to continue address ; 복제코드로 점프
continue:
# 231 "second.S"
 call serial_setup ; set up the COM port, if any ; COM 포트를 세팅하고 LI 문자열을 출력한다.


! drkbd(drain keyboard?)는 키보드 버퍼를 비운다(max:32) BIOS영역에는 41aH, 41cH를 포인터로 하는 보통 41eH부터 32바이트 원형 키보드 큐가 있지만 2바이트씩 증가하기 때문에 max 15회로 추측한다. 여유있게 비워주는걸로 보인다.
 mov cx,#32 ; drain type-ahead buffer ? 
drkbd: mov ah,#1 ; is a key pressed ? ; drain keyboard?
 int 0x16
 jz comcom ; no -> done ; jz==(zf=1) 키가 안눌렸으면 이 루프을 건너뛴다.
 xor ah,ah ; get the key ; 키가 눌렸다면 키값을 받아 키 버퍼를 비운다.
 int 0x16
 loop drkbd


comcom:
; 섹터크기가 9이하면 섹터수를 18로 패치
 mov al,#0x4c ; display an 'L'
 call display	; LILO의 3번째 문자 L을 출력한다.
 push #0 ; get pointer to disk parameter table in DS:SI
 pop ds
 lds si,[0x78] ; 0x78 = 4*0x1E ; 인터럽트 0x1e(Disk Initialization Parameter) 주소를 DS:SI에 가져온다.

 cmp byte ptr (si+4),#9 ; okay ? 3.5 720k, 5.25 360k 이하는 SPT(Sector per track)가 9 이하다. 9를 넘는다면 dskok
 ja dskok ; yes -> do not patch
! SPT가 9 이하라면 복사후 SPT를 18로 바꾸고 lilo의 disk parameter를 사용한다.
 push cs ; get pointer to new area in ES:DI
 pop es
 mov di,#dskprm		; second의 dskprm으로 복사한다.
 mov cx,#6 ; copy 12 bytes	; 파라미터는 12바이트
 rep
 movsw	; second stage의 [dskprm]으로 옮긴다 
 seg es ; patch number of sectors

 mov byte ptr (di-8),#18	; SPT(파라미터+4 == 끝-8)에 18을 넣는다. 
! 3.5인치 720k와 1.44M은 섹터크기가 각각 9, 18이다. 18을 넣어주는것은 이와 관련있지 않을까 추측한다.
! 섹터크기가 9이하라면 디스크 상태가 이상한게 아니냐는 추측도 있었다.

 push #0
 pop ds
 cli ; paranoia
 mov [0x78],#dskprm ; 복사후 lilo의 파라미터를 사용한다.
 mov [0x7a],es		
 sti
dskok:

 seg cs ; clear the break flag
 mov byte ptr break,#0	; break=0, 시리얼출력(serdisp)에서 LSR의 break에 따라 1이되기도 한다.

 call instto ; get timer interrupt ; 타이머 인터럽트 핸들러(0x1c)를 tick레이블에 해당하는 서비스 루틴을 설치한다.

;;; jmp restrt ; get going

! Restart here after a boot error
! 세그먼트,메모리,스택 세팅하고 문자열 및 버전을 확인한다.
restrt: mov bx,cs ; adjust segment registers
 mov ds,bx
 mov es,bx	; ds=es=cs

 sub bx,#63*0x20+0x20 ; segment for setup code & MAX_SETUPSECS=커널에서 SETUP의 최대 크기(63); cs - 0x800 = 2048 (바이트로 바꾸면 32KB) 
      ; bootsect
 mov cx,#INITSEG ; 0x9000
 cmp bx,cx ; bx=second세그먼트-0x800 ; 32k 여유공간이 있는지 체크
 jbe restrt1 ; bx는 0xA000(640kb)-second크기-EBDA-32kb다. 이 값이 0x9000(576kb)이하라면 이 값을 그대로 쓰고 0x9000보다 크다면 [initseg]=0x9000 (second+EBDA+32kb < 64kb 면 0x9000)
 mov bx,cx ; BX is the smaller segment 
restrt1:
 mov word ptr [map],#Map = max_secondary + 512
 mov [initseg],bx ; set up INITSEG (was 0x9000) ; initseg=cs 
 lea cx,(bx+0x20)
 mov [setupseg],cx ; set up SETUPSEG (was 0x9020) ; initseg 한섹터 뒤
 mov cx,cs
 sub cx,bx ; subtract [initseg]	; cs-initseg 세그먼트와 오프셋을 분리한다.
 shl cx,#4 ; get stack size	; 스택의 크기(bytes) = offset
 mov ss,bx ; must lock with move to SP below ; INITSEG (0x9000)
 mov sp,cx ; data on the stack)	; 스택은 initseg 아래 위치한다.
# 392 "second.S"
 cmp dword [sig],#0x4f4c494c ; "LILO" ; 첫부분 문자열 확인. (sig=="LILO")
 jne crshbrn2	; crshbrn로 가서 signature not found 출력후 대기-무한루프
 cmp dword [mcmdbeg+6],#0x4547414d ; "MAGE" from BOOT_IMAGE	; 끝부분 BOOT_IMAGE 문자열에서 MAGE
 jne crshbrn2	
 cmp BYTE [stage],#2	; 스테이지 확인

 jne crshbrn
 cmp WORD [version],#256*2 +23	; 버전 확인

crshbrn2: jne crshbrn ; 문자열이나 버전등이 맞지 않으면 에러출력후 종료
 mov [cmdbeg],#acmdbeg ; probably unattended boot ; acmdbeg="auto "

 mov di,#devmap ; place to store the device map ; BSSstart + 128

 mov ah,[init_dx] ; AH is physical device ; 저장한 드라이브 값 dx(dl) 

 seg fs		; 0x07c0 ; first 데이터는 fs 사용
 mov al,[par1_secondary+0+SSDIFF] ; map device logical ; SSDIFF=0 first의 d_dev = 0x80?






 cmp ah,al
 je end_tt





! 부팅된하드와 first의 d_dev값이 다르면 devmap에 AX를 추가한다. ah=부팅된 하드, al=first에 저장된 하드번호
 stosw ; set up the translation from map -> boot
end_tt:
 xor ax,ax ; devmap 마지막은 Null이다.
 stosw
! ldsc: 키보드 테이블 로드, 볼륨테이블 생성, Default command line, 디스크립터 로드. 키보드 체크
ldsc:
! load descriptor
 seg fs ; first (0x7c0)
 mov eax,[par1_mapstamp]	; map file이 만들어진 시간(first)

 cmp eax,[par2_mapstamp]
 jne timeerr	; first와 second의 mapstamp가 다르면 timestamp mismatch 출력후 무한루프

 call kt_read ; read the Keytable

 call build_vol_tab ; 볼륨 테이블을 만든다.

 mov bx,#Descr ; 인터럽트시 사용되는 버퍼 어드레스(es:bx)
 mov si,#Keytable+256+mt_descr ; 디스크립터의 섹터 주소
descr_more:
 lodsw
 xchg cx,ax
 lodsw
 xchg dx,ax
 lodsb
 call cread ; 섹터 읽는다.
 jc near fdnok ; error -> retry ; 에러나면 ldsc로 점프
 add bh,#2 ; increment address ; 저장할 다음 어드레스 (+0x200==512)
 cmp si,#Keytable+256+mt_descr+sa_size*MAX_DESCR_SECS_asm ; 아직 다 안읽었다면
 jb descr_more ; 더 읽어들인다.

 mov si,#Descr ; compute a checksum of the descriptor table ; es:si는 crc 체크할 번지
 mov di,#512*3 -4 ; di는 크기

 push dword #0x04c11db7 ; crc 32비트에서 많이 쓰이는 키값
 call crc32
 add di,si 
 cmp eax,dword (di) ; eax에 crc값이 리턴되면 저장된 값과 비교
 jz nochkerr
! 이 부분은 chkerr로 가거나 timeerr과 chkerr 레이블을 바꿔줘야 할것 같다.
! 이대로면 time error메세지를 출력한다. 버그 같다.

! Timestamp error
timeerr:
 mov bx,#msg_time
 jmp zz

! Checksum error
chkerr:
 mov bx,#msg_chkerr
 jmp zz ; go wait

crshbrn:
 mov bx,#msg_sigerr ; signature not found
zz: call say
zzz: hlt ; wait for interrupt
 jmp zzz ; sit here forever


nochkerr:
# 495 "second.S"
; remove those items that have "vmdisable", if virtual boot
 call vmtest ; vmware가 동작중인지 테스트
 jnc virtual_done ; vmware가 안돌면 virtual_done으로 점프
 mov di,#DESCR0 ; point at first descriptor ; 디스크립터(커널 이름,패스워드, 램디스크사이즈,vga_mode 등 ) 정보 메모리 주소. 디스크립터의 구조는 common.h의 IMAGE_DESCR 구조체에 정의되어 있다
vir_loop:
 test byte ptr [id_name](di),#0xFF ; test for NUL name ; 디스크립터의 끝이면 다음 루틴으로
 jz virtual_done
 test word ptr [id_flags](di),#512 ; FLAG_VMDISABLE가 꺼져있으면 스킵
 jz vir_skip
; FLAG_VMDISABLE가 켜있는 디스크립터를 덮어쓴다. (지운다.)
 push di
 lea si,[id_size](di) ; 디스크립터 구조체 한개의 크기 [di+id_size]
vir_loop1:
 mov cx,#id_size
 rep
    movsb
 test byte ptr [id_name](di),#0xFF ; 끝까지 복사
 jnz vir_loop1

 pop di
 jmp vir_loop

vir_skip:
 add di,#id_size ; 다음 디스크립터를 지정한다.
 jmp vir_loop

virtual_done:


; remove those items that have "nokbdisable", if nokeyboard boot
 call kbtest ; 키보드가 연결되었는지 테스트
 jc kbd_done ; 정상이면 다음루틴으로 점프
 mov di,#DESCR0 ; point at first descriptor
kbd_loop:
 test byte ptr [id_name](di),#0xFF ; test for NUL name
 jz kbd_done
 test word ptr [id_flags](di),#0x8000 ; #FLAG_NOKBDISABLE ; NOKBDISABLE이 꺼져있으면 스킵.
 jz kbd_skip
; NOKBDISABLE이 켜 있다면 해당 디스크립터는 덮어써서 지운다.
 push di
 lea si,[id_size](di)
kbd_loop1:
 mov cx,#id_size
 rep
    movsb
 test byte ptr [id_name](di),#0xFF
 jnz kbd_loop1

 pop di
 jmp kbd_loop

kbd_skip:
 add di,#id_size
 jmp kbd_loop

kbd_done:
# 560 "second.S"
 mov bx,#Keytable+256 ; Menutable
 mov al,(bx+mt_flag) ; 플래그를 가져온다. FLAG_NOBD 등 (bios data collection)

 seg fs ; get possible 16
 or byte ptr [par1_prompt+SSDIFF],al ; first의 d_flag ; NOBD flag??




 mov bx,#Dflcmd ; Default command line이 위치할 메모리 주소
;
;seg fs
 mov cx,mt_dflcmd+Keytable+256 ;DFCMD_OFF ; 키테이블에 저장된 DFL의 섹터주소(sa_size)
;seg fs
 mov dx,mt_dflcmd+2+Keytable+256
;seg fs
 mov al,mt_dflcmd+4+Keytable+256
;
 call cread ; DFL을 읽어들인다.
 jc fdnok ; error -> retry ; 에러가 나면 키테이블부터 다시 읽어들인다.
 mov bx,#Dflcmd
 cmp word ptr (bx),#0xf4f2 ; okay ?
 jne bdcmag ; no -> do not write
; DC_MAGIC(0xf4f2) 이면 DC_MAGIC에 mk(0x6b6d)를 넣고 DFL 섹터에 저장한다.
 mov word ptr (bx),#0x6b6d ; erase the magic number ; DC_MGOFF=="mk"
 call cmd_write ; write out the command line ; 0x6b6d로 바뀐 내용을 dfl에 쓴다.
# 598 "second.S"
 jmp dokay ; continue
bdcmag: mov byte ptr (bx+2),#0 ; disable the command line ; DC_MAGIC(0xf4f2)가 아니면 DFL 시작부분에 null을 채운다.
 jmp dokay ; go on
fdnok:





 br ldsc ; retry ; 키테이블부터 다시 읽어들인다.

! List all known boot images
! 커널 이미지들을 출력한다.
list: mov byte ptr (bx),#0 ; set EOL marker
 call crlf



 mov si,#DESCR0 ; list all images
 mov cx,#IMAGES
 xor dl,dl ; DL counts the images
lloop: testb (si),#0xff ; done ?
 jz ldone ; yes
 mov bx,si ; display the name
 call say
 add si,#15 +4
 inc dl ; count the image
 test dl,#3 ; inside line -> go on
 jnz fill
 call crlf
 jmp imgdne ; next image
fill: push bx ; fill with spaces
 mov al,#0x20
 call display
 pop bx
 inc bx
 cmp bx,si
 jbe fill
imgdne: add si,#id_size-15 -4
 loop lloop ; next image
ldone: test dl,#3 ; already at BOL ?
 jz atbol ; yes -> no CRLF
 call crlf
atbol:



 br iloop ; done

! Ready to process user input
! 입력받기전 세팅
dokay: mov bx,#ospc ; display 'O ' ; LIL까지 출력되었고 "O 23.2" 를 출력. o+space?
 call say

 xor eax,eax
 mov dword ptr [hma],eax ; HMA=0

 mov ospc,al ; disable the message ; [ospc]=null 한번만 출력한다
 mov word ptr vgaovr,#0x8000 ; disable VGA override ; #VGA_NOCOVR
;;
;; seg fs
 xchg ax,par2_delay ;DSC_OFF-8+SSDIFF ; old_delay=old_delay|delay ; delay=0
;;
 or old_del,ax ; remember delay
 mov nodfl,#iloop ; interactive prompt if falling through ; 다음 분기할 곳=폴링으로 입력받는 루틴(iloop)  기본으로 넣어둔다.

 call kbtest ; keyboard present?

 jc kbd_present ; 정상이면 건너뛴다.
; no PC keyboard on the system, is there a serial port in use?
 cmp byte ptr [par2_port],#0 ; 키보드, 시리얼 연결이 없으면 첫번째 이미지로 부팅할 것이다.
 jz skip_prompt ; no serial keyboard either



kbd_present:
; prompt 옵션이 켜있는지 체크한다.

 seg fs ; enter boot prompt ?
 test byte ptr par1_prompt+SSDIFF,#1 ;DSC_OFF+15+SSDIFF,#0 ; FLAG_PROMPT

 jnz extp ; yes -> check for external parameters ; (first) FLAG_PROMPT가 켜있으면 extp로 넘어간다.
skip_prompt: ; 이 곳은 키보드와 시리얼이 없거나 PROMPT 옵션이 꺼져 있을 때만 온다.
 mov nodfl,#bfirst ; boot first image if falling through ; 다음 분기할 곳=첫번째 이미지로 부팅하는 곳 (bfirst) = 커널 부팅 루틴
 call waitsh ; wait for a shifting key ; 키가 눌러졌거나 시리얼에서 break신호가 오면(cf=1) 바로 iloop로 간다.
 jc iloop ; key pressed -> enter interactive mode 

! Check for external parameters
! extp(external parameter)를 사용하면 extp의 command line 값을 es:bx에 넣고 non-interactive로 처리한다.
extp:
 seg fs ; external parameters ?
 cmp byte ptr SETUP_STACKSIZE-8+SSDIFF+6,#0xfe ; extp의 dl값. ext_dl 
! first의 스택은 SETUP_STACKSIZE(2048)로 세팅되었다.
 jne noex ; no -> go on ; external parameter를 사용하지 않으면 스킵
! external parameter는 lilo가 first에서 넘겨받는 값이다. dx,bx,es,si 순으로 쌓는다. 역순으로 찾으려면 스택의 시작 0x7c00:2048-8 부터 ext_di,ext_es,bx, ext_dx
 seg fs ; first를 참조한다. 0x7c0
 mov bl,SETUP_STACKSIZE-8+SSDIFF+7 ; get drive ; little-endian이라 dh가 온다. == drive  ; first에서 이미 dl에 드라이브 값이 들어갔다.
 seg fs ; clear flag
 mov byte ptr SETUP_STACKSIZE-8+SSDIFF+6,bl ; clear flag ; 매직넘버(dl==0xfe) 초기화
 seg fs ; load the signature pointer
 les bx,SETUP_STACKSIZE-8+SSDIFF ; LILO 문자열이 담긴 주소(ext_es:ext_di)를 가져온다.

 seg es
 cmp dword ptr (bx),#0x4f4c494c ; "LILO" ; 문자열이 일치하지 않으면 extp를 쓰지 않는다.
 jne noex ; no -> go on

 seg fs
 mov si,SETUP_STACKSIZE-8+SSDIFF+4 ; pointer to the command line ; ext_bx는 커맨드라인(옵션) 주소

 seg es
 cmp byte ptr (si),#0 ; empty ?
 je iloop ; yes -> enter interactive mode
 jmp niloop ; enter non-interactive mode ; 커맨드라인에 값이 있으면 그 값을 처리한다.

! No external parameters after timeout -> boot first image
! 입력 처리 전에 세팅해준다.
noex: push cs ; restore ES ; 0x9000 or 0x8xxx
 pop es
 mov si,#Dflcmd+2 ; default command line ? ; 매직넘버를 제외한 커맨드라인 주소
 cmp byte ptr (si),#0
 jne niloop ; yes -> use it ; 저장된 cmdline값이 있으면 우선 처리해준다. (끝이 0이면 끝. 0xff면 키입력을 받는다.)
 mov ax,nodfl ; no idea how to tell as86 to do jmp (addr) :-( ; 스킵되면 bfirst 아니면 iloop로 가서 입력방는다.
 jmp ax ; fall through


; Command input processor
; polling 방식의 입력 처리 부분
iloop:




! message=file 옵션이 정의되었다면 메세지를 저장한다. 이 메세지를 읽어서 출력한다.
;;
;; seg fs ; message disabled ?
 cmp word ptr par2_msg_len,#0 ;MSG_OFF+SSDIFF,#0 ; 메세지가 있는지 체크
;;
 je nomsg ; yes -> skip this ; greeting 메세지가 없으면 출력안함
 call crlf ; 줄넘김
; 메세지의 인덱스 섹터를 읽는다.
;seg fs ; load the message file
 mov cx,mt_msg+Keytable+256 ;MSG_OFF+SSDIFF+2
;seg fs
 mov dx,mt_msg+2+Keytable+256
;seg fs
 mov al,mt_msg+4+Keytable+256
;
 mov bx,[map] ; map은 섹터 인덱스가 위치한다.
 call sread ; 인덱스 섹터를 읽는다.
 call loadfile ; map의 인덱스 섹터를 기반으로 여러번 읽는다. es!=0라 xread(상위메모리)가 아닌 sread로 읽는다. 읽을곳은 Map 다음 Dflcmd

 xor bx,bx ; set the terminating NUL and disable further
    ; messages
 xchg bx,par2_msg_len ;MSG_OFF+SSDIFF

 push #SYSSEG
 pop ds ; ds=0x1000(SYSSEG)
 mov byte ptr (bx),#0
 xor bx,bx ; display the message ; 읽어온 메세지를 출력
 call say

 push cs ; restore segment registers
 pop ds

;es=ds=cs
nomsg: push cs ; disable external parameters
 pop es

 mov cmdbeg,#acmdbeg ; probably unattended boot ; "auto " 문자열 오프셋
 mov si,#usrinpm ; interactive mode ; UI_MAGIC(0xff) si에 이 값이 있으면 input에서 바로 키입력을 받는다.
niloop: ; ES may point to external params ; no input loop? ; si!=0로 넘어오면 키입력을 받지 않는다.
 mov bx,#msg_p ; display boot prompt ; "boot: " 문자열 출력
 call say
 mov bx,#cmdline ; move cursor to the end of the line
clend: mov al,(bx) ; cmdline의 옵션 출력
 or al,al ; at end ?
 jz cledne ; yes -> go on
 push bx ; display the character
 call display
 pop bx
 inc bx ; next one
 jne clend
cledne: mov byte ptr prechr,#32 ; character before command line is a space ; 나중에 사용하기 위해? "BOOT_IMAGE"+" " 문자열에 공백 추가

! Input loop
! [si]가 UI_MAGIC(0xff)면 키입력을 받아 처리하고(kbinp) 아니면 저장된 [si](default command line,external parameter의 command line)을 처리(gotinp)하고 cmdline(bx)에 넣는다. iloop를 통해 왔다면 0xff가 된다.
input: seg es ; interactive mode ?
 cmp byte ptr (si),#0xff
 je kbinp ; yes -> get keyboard input
 seg es ; get non-interactive input
 mov al,(si)
 inc si ; si를 증가시키면서 탐색
 jmp gotinp ; go on

tolist:



 br list ; ...

kbinp:
 mov cx,#brto ; get a key
 call getkey ; 키 입력을 받는다. timeout안에 키입력이 없으면 brto(cx)로 점프한다.
# 812 "second.S"
noNull: or al,al ; keyboard NUL input? ; 저장된 cmdline의 null은 종료를 의미하기 때문에 키입력에서 받은 null은 무시한다.
 je input ; yes, skip Keyboard NUL
; stored command line NUL is handled differently
! 받은 키입력(저장된 cmdline) 처리부분
gotinp: cmp al,#9 ; TAB ?
 je tolist ; yes -> list images ; second는 menu가 아닌 text라 기본으로 커널 이미지들을 보여주지는 않는것 같다. TAB이나 ?을 타이핑 해야 이미지들을 보여준다.
 cmp al,#63 ; "?" ?
 je tolist ; yes -> list images
 or al,al ; NUL ?
 je nul ; yes -> go on
 cmp al,#8 ; BS ? ; 백스페이스는 bx를 지우고 화면(시리얼)출력도 한칸 지운다.
 je todelch ; yes -> erase one character 
 cmp al,#13 ; CR ? 
 je cr ; yes -> go on
 cmp al,#127 ; DEL ?
 je todelch ; yes -> erase one character
 ja input ; non-printable -> ignore it ; 0x80이상은 출력하지 않는 문자들
 cmp al,#21 ; ^U ? ; C-u는 보통 한줄지움
 je todell ; yes -> erase the line
 cmp al,#24 ; ^X ?
 je todell ; yes -> erase the line
 cmp al,#32 ; ignore non-printable characters except space
 jb input ; 32 미만의 제어문자들도 위에서 처리한 것들 빼고는 무시한다.
 ja noblnk ; no space -> go on ; 32이상 글자들 숫자,기호,알파벳
 cmp (bx-1),al ; second space in a row ? ; 키입력이 공백이면 공백이 중복인지 체크한다.
 je input ; yes -> ignore it
noblnk: cmp bx,#cmdline+CL_LENGTH-1 ; at end of buffer ? ; cmdline의 최대크기(끝)이면 무시
 je input ; yes -> ignore
 xor ah,ah ; cmdline is always NUL terminated
 mov (bx),ax ; store in the buffer
 inc bx ; increment pointer
 push bx
 call display ; echo





 pop bx
 cmp bx,#cmdline+1 ; first character ?
 jne input ; no -> next input

 call upcase ; convert to upper case

 mov cx,#IMAGES ; check if we have a single-key entry
 mov di,#DESCR0
 mov ah,al
sklp: test word ptr (di+id_flags),#4096 ; single-key entry ? ; 싱글키 옵션. a-z등 한글자만 누르면 일치하는 이미지로 부팅
 jz sknext ; no -> try next
 mov al,(di) ; get first character

 call upcase ; convert to upper case ; al을 소문자 -> 대문자

 cmp al,ah ; do we have a match ?
 jne sknext ; no -> try next
 cmp byte ptr (di+1),#0 ; at end ?
 je cr ; yes -> run it
sknext: add di,#id_size ; test next entry
 loop sklp ; next one
 br input ; done -> get more input

todelch: br delch ; ...
todell: br delline ; ...

! End of input, process the command line
! cmdline의 끝(null)이면 저장된 옵션 처리를 끝낸다. 키가 눌려있다면 대화식 처리-iloop로 돌아가 키입력을 받는다.
nul: push bx ; automatic boot - wait for timeout ; 키부분의 Null(0)이 아닌 저장된 null값의 처리
 mov ax,old_del
 call waitsh
 pop bx
 jnc crnul ; no key pressed -> continue ; 키가 눌렸다면 대화식 처리
 mov bx,#msg_int ; interrupted -> display a message
 call say
 mov byte ptr cmdline,#0 ; clear the command line
 br iloop ; return to interactive prompt

cr:
;;22.7 mov word par2_timeout,#0xffff ; kill timeout
# 899 "second.S"
 mov cmdbeg,#mcmdbeg ; probably manual boot
crnul:

 mov byte ptr break,#0 ; clear the break flag

 push cs ; set ES to CS
 pop es
 xor al,al ; mark end
 mov (bx),al
 mov si,#cmdline ; copy command line to save buffer
 mov di,#lkcbuf
 mov byte ptr dolock,#0 ; disable locking

cpsav: lodsb ; copy one byte
 stosb
 or al,al ; at end ?
 jnz cpsav ; no -> go on ; cmdline에서 lkcbuf로 복사 (null end)

 cmp bx,#cmdline ; empty line ? ; bx는 command line(DFL,ext DFL)에서 bx(cmdline)로 입력값을 저장한 크기
 je notrspc ; yes -> boot first image ; 입력받은게 없으면 notrspc
 cmp byte ptr (bx-1),#32 ; trailing space ?
 jne notrspc ; no -> go on
 dec bx ; remove the space ; 마지막 값이 공백인가?
 mov byte ptr (bx),al
notrspc: mov si,#cmdline ; scan the command line for "vga=", "kbd=",
 mov di,si ; "lock" or "mem="
chkvga:
! lilo의 옵션들을  해석/처리한다.

vsktnbd:
 cmp dword ptr (si),#0x64626f6e ; "nobd" ; 바이오스 데이터를 수집하지 않는다. (빠른 부팅)
 jne vsktv
 cmp byte (si+4),#32 ; terminated with SP or NUL?
 jnbe vsktv
! nobd면서 다음글자가 32이하라면 nobd 플래그를 켠다.
 seg fs ; enter boot prompt ?
 or byte ptr par1_prompt+SSDIFF,#16 ; suppress BIOS data collection ; FLAG_NOBD

 jmp vskwd ; skip word ; 4바이트 넘김

vsktv:
 cmp dword ptr (si),#0x3d616776 ; "vga="
 jne vsktk ; "vga=" 이 아니면 skip
 call setvga ; set VGA mode ; "vga=" 뒤의 값을 해석해서 vgaovr에 넣는다.
 jc near iloop ; error -> get next command ; 에러나면 복귀
 jmp vskdb ; proceed by discarding last blank ; 이미 si를 증가시켰기 때문에 si증가없이 다음 문자 처리
vsktk:
 cmp dword ptr (si),#0x3d64626b ; "kbd="
 jne vsktl
 call putkbd ; pre-load keyboard buffer ; 16진수 키코드(스캔코드,아스키)를 키보드 버퍼에 넣는다.
 jmp vskdb ; proceed by discarding last blank
vsktl:
 cmp dword ptr (si),#0x6b636f6c ; "lock"
 jne vsktm
 cmp byte (si+4),#32 ; space?
 jnbe vsktm
 mov byte ptr dolock,#1 ; enable locking ; lock이고 뒷글자가 32이하면 dolock=1
vskwd: add si,#4 ; skip word
vskdb: dec di ; discard last blank ; 공백 쓰기용
 jmp vsknb ; continue
vsktm:



 cmp dword ptr (si),#0x3d6d656d ; "mem="

 jne vsknb
 call getmem ; get the user-provided memory limit ; 메모리 상한선 설정
vsknb:
 lodsb ; copy one byte
 stosb
 cmp al,#32 ; space ?
 je chkvga ; yes -> look for options again ; 하나 끝. 재탐색
 or al,al ; at end ?
 jnz vsknb ; no -> go on ; (NUL,공백이 아닌) 일반 문자열 계속 복사
 call crlf ; write CR/LF
 cmp di,#cmdline+1 ; empty line ?
emptyl: je bfirst ; yes -> boot first image ; 옵션이 없으면 bfirst
 jmp bcmd ; boot the specified image

! Find the boot image and start it

bcmd:
 call find_image ; 이미지를 찾는다. bx에는 포인터, ax는 넘버
 jc near boot ; eureka, it was found ; 일치하는 이미지를 찾았다.

 mov bx,#msg_nf ; not found -> display a message
 call say
 br iloop ; get more input

! Delete one character
! 한글자 지운다.
delch: cmp bx,#cmdline ; at the beginning ?
 je toinput ; yes -> do nothing
 dec bx ; move the pointer
 push bx ; display[B BS,SPC,BS
 mov bx,#bs
 call say
# 1020 "second.S"
 pop bx
toinput: br input ; go on

! Delete the entire line
! 한줄 지우는 루틴
delline:

 cmp bx,#cmdline ; done ?
 je toinput ; yes -> go on
 push bx ; display BS,SPC,BS
 mov bx,#bs ; backspace, space, backspace 출력
 call say
 pop bx
 dec bx ; move the pointer
 jmp delline ; next one ; 첫글자까지 지우는걸 반복한다.







! Boot first after timeout

brto: call crlf ; display a CRLF
 jmp brfrst ; boot

! Boot the first image

bfirst: mov byte ptr lkcbuf,#0 ; clear default
 cmp byte ptr cmdline,#0 ; is there a default ?
 jne bcmd ; yes -> boot that image
brfrst:
 mov bx,#DESCR0 ; boot the first image


 xor ax,ax ; mask = 0 
 call vmtest
 jnc brfrst0v ; not virtual
 mov ax,#2048 ; FLAG_VMDEFAULT
brfrst0v:     ;vm test 완료
 call kbtest
 jc brfrst0k
 mov ax,#0x4000 ; FLAG_NOKBDEFAULT
brfrst0k:	; kb test 완료

 mov cx,#IMAGES
brfrst1: test word ptr (bx+id_flags),ax ; 같은 플래그의 커널 이미지를 찾는다.
 jnz brfrst3 ; flag가 같다면, brfrst3으로 넘어간다
 add bx,#id_size
 loop brfrst1

 mov bx,#DESCR0 ; restore default
brfrst3:
# 1104 "second.S"
 mov si,bx ; copy the name to the command line ; 찾은 디스크립터 위치(이름)을 복사
 mov di,#cmdline
bfcpl: lodsb ; copy one character ; 디스크립터 이름 하나만 복사?
 mov (di),al
 inc di 
 or al,al ; NUL ?
 jnz bfcpl ; no -> next one

! Boot the image BX points to (with password check)

boot:
 mov word par2_timeout,#0xffff ; kill timeout (22.7) ; timeout 중지 (카운트다운 되지 않음)
 mov si,#cmdline ; locate start of options
locopt: lodsb ; 마지막이 Space,NUL이면 공백제거
 or al,al ; NUL ?
 je optfnd ; yes -> no options
 cmp al,#32 ; space ?
 jne locopt ; no -> continue searching ; 공백이 나올때까지 반복
 cmp byte ptr (si),#0 ; followed by NUL ?
 jne optfnd ; no -> go on
 mov byte ptr (si-1),#0 ; discard trailing space ; cmdline끝 공백 제거
optfnd: dec si ; adjust pointer
 mov options,si ; store pointer for later use
# 1141 "second.S"
 test word ptr [id_flags](bx),#1024 ; #FLAG_VMWARN
 jz boot9
 call vmtest ; 'vmwarn' there, is it actually virt. boot
 jnc boot9
; VMWARN set, and is virtual boot, so issue comment
;; FLAG_VMWARE가 켜있고 vmware안에서 동작중
;; seg fs
 mov word ptr par2_timeout,#0xffff ; cancel timeout
;;

 push bx ; save image descriptor ptr
 mov bx,#msg_vmwarn
 call say
 mov cx,#vmwto ; timeout exit
 call getkey

 push ax
 cmp al,#0x20 ; compare to Space
 jb boot3 ; no echo if ctrl char
 call display ; echo
boot3: call crlf
 pop ax

 pop bx ; restore image descriptor ptr
 cmp al,#0x79 ; y is yes
 je boot9
 cmp al,#0x59 ; Y is yes
 je boot9

vmwto:
 br iloop

boot9:
; 패스워드 사용 체크
 test byte ptr (bx+id_flags),#128 ; use a password FLAG_PASSWORD
 jz toboot ; no -> boot
 test byte ptr (bx+id_flags),#2 ; restricted ? FLAG_RESTR
 jz dopw ; no -> get the password ; FLAG_PASSWORD & FLAG_RESTR가 켜있으면 패스워드 사용
 cmp byte ptr (si),#0 ; are there any options ?
 jne dopw ; yes -> password required ; FLAG_RESTR이 꺼있어도 si에 내용이 있다면 패스워드 사용?
toboot: br doboot ; ...  부팅합시다~ ^^
dopw:	;password 입력

 push bx ; save the image descriptor
;;
;; seg fs
 mov word ptr par2_timeout,#0xffff ; cancel timeout
;;
 mov bx,#msg_pw ; display a prompt
 call say

 push bp ; save BP
 mov bp,sp ; save SP in BP
 sub sp,#CL_LENGTH ; allocate space for PW string
 mov si,sp ; si points at string
 xor di,di ; di counts characters
pwloop:	   
# 1209 "second.S"
 mov cx,#pwtime ; get timeout exit
 call getkey

 cmp al,#13 ; CR ?
 je pwcr ; yes -> handle it
 cmp al,#21 ; ^U ?
 je pwdell ; yes -> erase line
 cmp al,#24 ; ^X
 je pwdell
 cmp al,#8 ; BS ?
 je pwdelch ; yes -> erase one character
 cmp al,#127 ; DEL
 je pwdelch
 ja pwloop ; ignore other non-printable characters
 cmp al,#32
 jb pwloop

 cmp di,#CL_LENGTH ; check for buffer overflow
 jae pwloop ; ingnore further input
 seg ss
 mov (si),al ; store char in buffer
 inc si
 inc di
 mov al,#42 ; echo '*'
 call display
 jmp pwloop ; loop back for more

pwdelch: or di,di
 jz pwloop
 call pwbs
 dec si
 dec di
 jmp pwloop

pwdell: inc di
pwdel: dec di
 jz pwloop
 call pwbs
 dec si
 jmp pwdel

pwbs: mov bx,#bs
 call say
 ret

pwcr:
 xor cx,cx ; signal okay
pwtime: ; CX != 0 if enter here
 inc cx
 call crlf

 sub si,di ; point SI at start of buffer
 push es ; save ES
# 1321 "second.S"
; DI is the count
; SS:SI is the password string
 push di
 push si
 call _shsInit
 call _shsUpdate
 call _shsFinal
 mov bx,(bp+2) ; restore image descriptor pointer
 lea di,(bx+id_password_crc)
 mov si,#shs_digest
 mov cx,#5*4
; ES==DS
 repe
     cmpsb

 pop si ; restore buffer ptr
 pop di ; clear stack
 push ss
 pop es ; ES=SS

 je pwcleanup ; CX will be 0
 inc cx ; CX is > 0


pwcleanup:
 push cx
 mov cx,#CL_LENGTH
 mov di,si
 xor ax,ax
 rep ; wipe out password in memory
     stosb
 pop cx
 pop es ; restore the saved ES

 mov sp,bp
 pop bp
 pop bx
 or cx,cx ; test CX==0 means all okay
 jz doboot
; fall into pwfail
# 1419 "second.S"
pwfail: mov bx,#msg_pf ; display an error message
 call say
 br iloop ; get next input

! Boot the image BX points to

doboot: mov byte ptr prechr,#61 ; switch to equal sign "="
 push bx ; save image descr
 mov bx,#msg_l ; say hi "Loading "
 call say
 pop bx ; display the image name
 push bx
 call say ; 리눅스 이미지 이름 출력
 pop si

 push si
 add si,#id_start ; form address ; [si+id_start]==커널 이미지 섹터 주소

; Now load the kernel sectors
 xor ax,ax
 mov word ptr (gdt+0x1b),ax ; set GDT to "load low" ; gdt 일부분 초기화
 mov byte ptr (gdt+0x1f),al
 mov moff,ax ; map is not loaded yet ; map offset ; map에 쓰이는 오프셋값 초기화

 lodsw ; address of the first map sector  ; 커널 섹터 주소 세팅
 xchg cx,ax
 lodsw
 xchg dx,ax
 lodsb

 push si ; save SI







 mov bx,[map] ; load the first map sector
 call sread ; 커널의 첫 맵섹터(인덱스)를 읽어온다.




 mov bx,#Dflcmd ; load the default command line
;
;seg fs
 mov cx,mt_dflcmd+Keytable+256
;seg fs
 mov dx,mt_dflcmd+2+Keytable+256
;seg fs
 mov al,mt_dflcmd+4+Keytable+256
;
 call cread ; DFLcmd를 읽어온다.
 push word ptr (Dflcmd) ; push magic number
 mov bx,#Dflcmd ; load the fallback sector
 call load1 ; 폴백 섹터(1sector)를 읽어온다.
 pop ax ; valid magic number ?

 cmp ax,#0xf4f2 ; default command line 매직넘버
 je dclok ; yes -> can write
 cmp ax,#0x6b6d ; mk
 jne nofbck ; invalid -> must not write
dclok: mov bx,#Dflcmd ; fallback data present ? ; 6b6d면 bx=dflcmd
 cmp word ptr (bx),#0xf4f2
 jne nofbck ; no -> go on
 call cmd_write ; write out the command line ; 0xf4f2면 로드한 폴백을 dflcmd 섹터에 쓴다.
nofbck:





 mov bx,#Dflcmd ; load the options sector
 call load1 ; [map]의 두번째 섹터(옵션섹터)를 읽는다.
 mov si,cmdbeg ; copy non-options part of command line
 mov di,#Parmline
 mov cx,#CL_LENGTH-1 ; max number of characters to copy ; cmdline의 최대값

cpnocl:

 cmp si,#cmdline ; parmline에 cmdbeg(auto... or BOOT_IMAGE....)부터 cmdline위치 까지 복사



 je cpnodn ; yes -> go on
 movsb ; copy one byte
 loop cpnocl ; next one
 jmp cpovfl ; signal overflow

cpnodn:

 pop ax ; get saved pointer
 pop si ; get saved descriptor
 push si
 push ax
cpdname:
 lodsb
 or al,al
 jz cpdname9 ; cmdline에서 끝(null)까지 parmline에 복사
 stosb
 dec cx
 jmp cpdname
cpdname9:

 mov si,#Dflcmd ; constant options ? ; Dflcmd에서 자료 처리를 위해 사전준비.
 cmp byte ptr (si),#0
 je nocopt ; no -> go on
 mov al,#32 ; add a space ; Dflcmd에 값이 있으면 parmline에 공백을 한칸 넣는다.
 stosb
 dec cx ; count character
 jz cpovfl ; cx가 0이면 패스
cpcodsp:



 cmp dword ptr (si),#0x3d6d656d ; "mem="

 jne cpnotmem
 call getmem ; get the user-provided memory limit ; Dflcmd가 "mem="이면 mem 뒤에 오는 값을 계산해서 hma에 넣는다.
cpnotmem:
 lodsb ; fetch next byte
 cmp al,#32 ; space ?
 je cpcodsp ; yes -> discard it ; 다음글자가 공백이면 위로 올라간다.
cpcolp: or al,al ; NUL ?
 jz cpcodn ; yes -> done
 stosb ; store byte ; 끝이 아니면 parmline에 공백을 넣어주고 다음 처리 준비
 dec cx ; count character
 jz cpovfl
 cmp al,#32 ; a space ?
 je cpcodsp ; yes -> discard next
 lodsb ; get next byte
 jmp cpcolp

cpcodn: seg es
 cmp byte ptr (di-1),#32 ; last was space ?
 jne nocopt ; no -> go on
 dec di ; discard it
 inc cx ; **
nocopt: mov si,options ; append variable options
cpvalp: lodsb ; copy one byte
 stosb
 or al,al ; NUL ?
 jz cpdone ; done?
 loop cpvalp ; count and loop back
cpovfl: mov (di),cl ; CX is zero


! parmline에 복사 완료
cpdone:
# 1586 "second.S"
 mov es,[initseg] ; load the original boot sector
 xor bx,bx ; load now
 call load1 ; initseg:0에 1섹터 로드 bootsect.S
 pop si ; restore SI
 lodsw ; get flags bit map
 xchg bx,ax ; move to BX
 lodsw ; copy parameters ... VGA mode ... (done)
 cmp word ptr vgaovr,#0x8000 ; VGA mode not overridden on ; VGA_NOCOVR
    ; command line ?
 je vganorm ; no -> go on
 mov ax,vgaovr ; use that value
 jmp vgaset
vganorm: test bx,#1
 jz novga
vgaset: seg es
  mov [506],ax ; magic offset in the boot sector
novga: push bx ; use flags (BX) later
 test bx,#4 ; ... lock target ?
 jnz lockit ; yup -> do it
 cmp byte ptr dolock,#0 ; did user ask to lock new target ?
 je nolock ; no -> go on
lockit:

 mov bx,#lkwbuf ; save the command line
 mov word (bx),#0xf4f2 ;
 push es
 push si

        push ds ;
        pop es ;
 call cmd_write ; write out the command line

 pop si
 pop es

nolock:




 xor cx,cx
 seg es
   add cl,[497]
;;; or cx,cx
 jnz lsetup
 mov cl,#4 ; default is to load four sectors
lsetup:
 mov es,[setupseg] ; load the setup codes ; 일반적으로 0x9020 initseg 다음 섹터


 mov ax,cx ; number of sectors to AX ; 섹터수는(/512(==2^9))
 shl ax,#5 ; convert to paragraphs (9-4) ; <<5번하면 세그먼트 단위가 된다.
 mov bx,es
 add bx,ax ; setupseg(0x9020) + (setup) 세그먼트 크기
 add bx,#STACK>>4 ; allow for stack space in paragraphs ; 스택의 세그먼트 단위를 위 결과에 더해준다.
 mov ax,cs
 cmp bx,ax
 jbe enough_mem ; 로드한 크기가 현재 코드를 침범하지 않으면 enough_mem
 mov bx,#msg_mem ; we are very short on memory
 call say ; 스택이 오버랩됐다고 메세지 출력

enough_mem:


 xor bx,bx ; other operating system)
lsloop: push cx
 call loadopt ; 0x9020에 섹터수만큼 로드하고 launch ; 실제 리눅스???
 pop cx
 loop lsloop




 pop bx ; get flags
 test bx,#8 ; "modern" kernel ? FLAG_MODKRN	; bzimage?
 jz loadlow ; no -> avoid all patching and such
 seg es ; set loader version
 mov byte ptr (16),#0x02 ;LOADER_VERSION ; setupseg:16에 로더 버전을 넣어준다.

 test bx,#256 ; load kernel high ; FLAG_LOADHI
 jz nohigh ; loadhi가 꺼져있으면 gdt 설정 않음.
! gdt 세팅
 seg es
 mov ax,word ptr (20+1) ; get start address 00 1000 00
 mov (gdt+0x1b),ax
 seg es
 mov al,byte ptr (20+3) ; get hi-byte of address
 mov (gdt+0x1f),al
nohigh:

 seg es ; version >= 1 ?
 cmp word ptr (6),#0x200 ; NEW_HDR_VERSION 버전이 0x200 이하면 노힙!
 jbe noheap ; no -> do not patch heap
 mov ax,cs
 sub ax,[initseg] ; find no. of paragraphs available
 shl ax,4 ; init부터 second 시작부분까지 바이트 크기
 add ax,#Parmline-SETUP_STACKSIZE-BOOTSECT ; parmline-2048-512 ; SLA_SIZE_DYN ; setup load area size dynamic
 seg es
 mov word ptr (36),ax ; 힙을 구해서 넣어준다.
 seg es ; patch flags
 or byte ptr (17),#0x80 ; LFLAG_USE_HEAP
noheap:
 pop si ; restore pointer to Descr to load

 push [gdt+0x1b]
 mov al,[gdt+0x1f]
 push ax

 call load_initrd ; load the initrd & patch header

 pop ax
 mov [gdt+0x1f],al
 pop bx
 mov [gdt+0x1b],bx

 cbw
 or ax,bx ; load low ?

 je loadlow ; yes -> do it
 xor ax,ax ; GDT is already set up ...
 mov es,ax
 mov bx,#gdt






 call lfile ; load the system ... ; 반복해서 읽는다.
 jmp launch2 ; ... and run it
loadlow:






 call loadfile ; load the system ; map을 다 읽어들인다.
launch2:

 jmp launch ; go !
! loadfile은 [map]의 섹터주소 배열을 기반으로 sread를 이용해 SYSSEG(0x1000):0에 파일을 모두 읽어들인다. map의 마지막은 다음 map의 섹터주소다.
loadfile:
 push #SYSSEG ; load a file at SYSSEG:0000 ; 0x1000:0에 로드한다.
 pop es
 xor bx,bx
lfile: call load
 jmp lfile

! Load one sector. Issue an error at EOF.
! load1을 호출하면 sa_size 구조체 하나(1섹터)만 읽고 리턴한다. 
load1: call loadit ; load the sector
 mov bx,#msg_eof ; we only get here at EOF
 call say
 br restrt

loadit: call load ; load it
 pop ax ; drop return address of load1 ; 윗부분 call을 건너뛴다.
 ret

! Load one sector. Start the system at EOF.

loadopt: call loadit ; load the sector
 jmp launch ; go

! Load one sequence of sectors. Leave outer function at EOF.
! [map]의 인덱스를 기반으로 sread를 이용해 순차적으로 섹터들을 읽어들인다.
load: push es ; save ES:BX
 push bx
lfetch: mov si,moff ; get map offset
 mov bx,[map] ; 읽어들인 맵섹터(sa_size구조체의 집합)
 mov cx,(bx+si) ; get address [Map+moff]
 mov dx,(bx+si+2)
 mov al,(bx+si+4)
 or cx,cx ; at EOF ? ; 섹터주소(혹은+실린더)가 0이 아니면 읽는다.
 jnz noteof ; no -> go on
 or dx,dx ; 상,하위 섹터주소(혹은CHS)와 드라이브가 null이면 복구하고 리턴한다.
 jnz noteof
 pop bx ; restore ES:BX
 pop es
 pop ax ; pop return address ; 디스크 읽기를 완전히 (한단계 더) 리턴한다.
 ret ; return to outer function
noteof: add si,#sa_size ; increment pointer ; cx,dx,al 5바이트 ; 다음 섹터 위치
 mov moff,si
 cmp si,#512 - sa_size + 1 ; page end ?
 jb near doload  ; 맵의 끝이 아니면 상위메모리 혹은 일반으로 읽어들인다.
! 맵섹터의 마지막 sa_size(5bytes)는 다음 맵 섹터다.
 mov moff,#0 ; reset pointer
 push cs ; adjust ES
 pop es

 mov bl,hinib ; this might get clobbered ; cread 내부에서 값이 변경되기에  저장해둔다.
 push bx ; so save it
 mov bx,[map] ; load map page
 call sread
        pop ax ; restore the hi-nibble
 mov hinib,al ;

 mov al,#0x2e ; print a dot ; 인덱스를 읽을때마다 '.' 을 출력한다.
 call display
 jmp lfetch ; try again

! Start the kernel

launch:
; terminate emulation if CD boot
 test byte ptr [par2_flag2],#2 ; a CD? FLAG2_EL_TORITO	
 jz not_el_torito
 mov si,#Map ; empty command packet
 mov byte ptr (si),#0x13 ; size of command packet
 mov ax,#0x4b00 ; terminate emulation ; Bootable CD-ROM - TERMINATE DISK EMULATION
;;;; mov dl,al ; DL is 0
 mov dl,[init_dx] ; terminate boot device
 int 0x13
not_el_torito:



 call crlf ; display a CRLF


 mov dx,#0x3f2 ; stop the floppy motor ; 모터를 꺼줍니다.
 xor ax,ax
 out dx,al ; outb
 mov dl,al
 int 0x13 ; reset the FDC (AH=0) ; ah=0 dl=드라이브. 플로피 디스크 리셋

 mov es,[initseg] ; adjust segment registers
 mov di,#Parmline ; set parameter line offset
 mov ax,cs ; find where we are loaded
 sub ax,[initseg] ; find no. of paragraphs available
 shl ax,4 ; convert para. to bytes
 add di,ax
 seg es
 cmp dword ptr CL_HEADER_ID,#0x53726448 ; "HdrS" (reversed)
 je chkver ; go check header version
mbchain:

! it must be the chain loader



 seg fs ; suppress BIOS data collection
 or byte ptr par1_prompt+SSDIFF,#16 ; suppress BIOS data collection
! 바이오스 수집

   ; ES:DI will point at param line (chain.b)
 push ds ; save DS
 mov ds,[setupseg] ; point at chain loader(?) header
; DS points at chain loader
 cmp dword [parC_signature],#0x4f4c494c ;
 jne not_chain
 cmp word [parC_stage],#0x10
 jne not_chain
 cmp word [parC_version],#256*2 +23
 jne not_chain
 mov dx,[parC_drive] ; get drive
;;; call map_device ; map drive -- uses CS to address "devmap"
 mov [parC_drive],dl ; store mapped drive
 mov [parC_devmap],#devmap ; save our drive mapping
 mov [parC_devmap+2],cs ; our DS register
not_chain:
 pop ds

 seg fs
 mov dx,[SETUP_STACKSIZE-8+SSDIFF+6] ; pass DX from first stage






 br start_setup2


chkver:
 mov bh,[gdt+0x1f] ; check for kernel/initrd conflict
 shl ebx,#8
 mov bx,[gdt+0x1b] ; form kernel final load address
 shl ebx,#8
 mov eax,[rdbeg] ; initrd beg address (0 if none)
 or eax,eax
 jz no_overwrite
 sub eax,ebx
 jae no_overwrite
 mov bx,#msg_confl
 br zz

no_overwrite:
# 1885 "second.S"
 seg es
 cmp word ptr CL_HDRS_VERSION,#NEW_VERSION ; check for
    ; new cmdline protocol
 jb protocol201

! and now the new protocol

 mov ax,es ; form long address
 movzx edx,ax ; zero extend segment part to EDX
 movzx edi,di ; zero extend offset
 shl edx,4 ; make segment into address
 add edx,edi ; form long absolute address
 seg es
 mov CL_POINTER,edx ; and pass the address





 jmp start_setup


! the old command line passing protocol

protocol201:
 seg es
 mov CL_MAGIC_ADDR,#CL_MAGIC ; set magic number
 seg es
 mov word ptr CL_OFFSET,di
# 1937 "second.S"
start_setup: ; kernel boot comes here
# 1955 "second.S"
 mov bx,#msg_bc
 call say

 seg fs ; suppress BIOS data collection?
 test byte ptr par1_prompt+SSDIFF,#16 ; suppress?

 jz start_setup3
 mov bx,#msg_by
 call say
 jmp start_setup2
start_setup3:


 or byte ptr [Keytable+256+mt_flag],#16 ; suppress



 call kt_write
# 1984 "second.S"

 seg fs
 mov dx,[SETUP_STACKSIZE-8+SSDIFF+6] ; pass in DX from first stage


 push es ; save ES
 call is_prev_mapper ; is there a previous mapper
 jz no_remove
 seg es
   mov word (di),#0 ; sterilize it
no_remove:
 pop es ; and restore ES

 call io_biosdata

 mov bx,#msg_s
 call say

; if the BIOS data collection was successful, do not suppress it on future boots
 and byte ptr [Keytable+256+mt_flag],#~16 ; no suppress
 call kt_write



start_setup2: ; chain loader boot comes here



 mov ax,#1500/55 ; about 1.5 second
 call setto ; set timeout
vpaus1: test byte ptr timeout,#-1
 jz vpaus1 ; 시간 대기

 call remto ; free timer interrupt ; 타이머 인터럽트 복구

 push es ; is initseg
 pop ds ; DS = 0x9000 (initseg)







 add sp,#Parmline ; increase stack size over this code
if ~*&1 ; align to an odd memory location
 nop
endif
 jmpi 0,SETUPSEG ; segment part is a variable ; 커널로 점프
setupseg = *-2 ; setupseg is filled in now
initseg: .word INITSEG


! Load one sector (called from load)

doload: pop bx ; restore ES:BX ; 메인에서 호출될때 bx 값
 pop es

! Load a sequence of sectors, possibly moving into "high memory" (> 1 MB)
! afterwards.
! es가 0이면 상위메모리에 읽고 아니면 그냥 읽는다.
xread: push ax ; ES == 0 ?
 mov ax,es
 or ax,ax
 pop ax
 jz rdhigh ; yes -> read into high memory



 jmp sread

rdhigh: push bx ; okay - DS:BX points to GDT in this case ; sread로 읽어서 1M이상 메모리로 옮긴다.
 mov bx,#LOADSEG ; adjust ES:BX ; 0x1000
 mov es,bx
 xor bx,bx
 call sread ; load the sector(s) ; 일단 LOADSEG로 읽는다.
        mov tempal,al ; al 보존
 pop bx ; get pointer to GDT
 push ax ; just in case ...
 push cx
 push si
 mov si,bx ; turn ES:SI into pointer to GDT
 push ds
 pop es
 xor cx,cx ; number of words to move
 mov ch,tempal
# 2095 "second.S"
 push [gdt+0x1e]
 push bx ; do the transfer. (save BX, CX and SI because
 push cx ; we are paranoid)
 push si
 mov ah,#0x87 ; Move Extended Memory Block
 int 0x15
 pop si
 pop cx
 pop bx
 jc badmov ; failed ...
 pop ax ; check the GDT
 cmp ah,[gdt+0x1f] ; catch a BIOS that does not handle 386
    ; addresses (>16Mb)
 jne badmov+1 ; AH error code will be hi byte of address
 shr cx,#8-1 ; convert words to bytes/256
 sub ax,ax ; put ES back to 0
 add (si+0x1b),cx
 adc (si+0x1f),al
 mov es,ax ; put ES back to 0
 pop si
 pop cx
 pop ax
 ret ; done

badmov: pop bx ; discard GDT
 push ax ; save the error code
 mov bx,#msg_bm ; tell the user ...
 jmp reset ; (standard procedure calls say & bout)

! Load a sequence of sectors
! 섹터수만큼 읽어들이고 읽은만큼 메모리 주소(es:bx)를 증가시킨다.
sread: push bx ; save registers
 push cx
 push dx
 call cread
 mov di,ax ; save AL return count ; 읽은 섹터수 저장
 jc rerror ; error -> complain
 pop dx ; restore registers
 pop cx
rokay: pop bx
        shl ax,8 ; convert sectors to bytes
        add ah,ah ; 섹터수*(2^9). 읽은 섹터를 바이트로 변환한다. 
 jc dowrap ; loaded an entire segment -> advance ES ; 섹터수의 최대값은 CHS는 128(세그먼트가 64kb), EDD는 127이기 때문에 넘치면 128로 생각해서 세그먼트만 더해준다. 하지만 cread에서처리할때 al은 상위 바이트값으로 쓰여서 EDD는 1섹터씩 읽는다.
 add bx,ax ; move BX ; 오프셋에 크기를 더한다.
 jnc nowrap ; same segment -> go on
dowrap: mov ax,es ; move ES ; 넘친만큼 세그먼트에 더한다.
 add ax,#0x1000
 mov es,ax
nowrap:
 mov ax,di ; restore the block count in AL
aret: ret ; done

! Read error - try a second time and give up if that fails too

rerror:
 push ax
 mov bx,#msg_re ; say something
reset: call say ; 에러메세지와 에러코드 출력후 restrt부터 다시 시작한다.
 pop ax ; display the error code
 mov al,ah
 call bout
 call crlf ; a CR/LF
 mov moff,#0 ; restore initial state
# 2167 "second.S"
 br restrt

! Convert character in AL to upper case
! al이 소문자일때만 대문자로 만든다.
upcase: cmp al,#0x61 ; lower case character ? ('a')
 jb nolower ; no -> go on
 cmp al,#0x7a ; 'z'
 ja nolower
 sub al,#0x20 ; convert to upper case ; 소문자 a-z일때만 0x20('a'-'A')를 뺀다.
nolower: ret ; done

pause:

 pusha
 mov ax,#3200/55 ; delay 3+ seconds
 call setto
delay1: test byte ptr timeout,#-1
 jz delay1
 popa







 ret
# 2231 "second.S"
bout: push ax ; save byte
 shr al,#4 ; display upper nibble
 call nout
 pop ax
nout: and al,#0x0F ; lower nible only
 daa ; smaller conversion routine
 add al,#0xF0
 adc al,#0x40 ; AL is hex char [0..9A..F]
 jmp display ; display it

! part of the 'say' routine
! actual entry point is below at 'say:'

say_loop: 
 cmp al,#10 ; \n ?
 jne nonl ; no -> go on
 mov al,#13 ; display a CRLF ; LF면 CRLF를 찍어준다. LF=10 CR=13
 call display
 mov al,#10
nonl:
 cmp al,#12 ; ^L ? ! 화면 지움
 jne nocls ; no -> go on







 push bx
 mov ah,#0xf ; clear the local screen
 int 0x10
 xor ah,ah
 int 0x10	; get video state, set video mode / 화면 상태를 얻어오는 함수와 설정을 얻어오는 인터럽트로 화면을 지운다.
 pop bx

tosnext: jmp snext ; next character
nocls: call display ; display, tty-style
snext:
 inc bx ; next one
! fall into say ; process next character

! Display a NUL-terminated string on the console
! bx에 문자열의 포인터를 받아 특수문자들을 처리해주고 null(0)값이 나올때까지 출력한다.
say: mov al,(bx) ; get byte
 or al,al ; NUL ? 
 jnz say_loop ; not the end
 ret


! Display CR/LF
! 아랫줄 앞으로 가는 루틴
crlf: mov al,#13 ; CR
 call display
 mov al,#10 ; LF
;;; jmp display
; fall into display

! Display one character on the console

display:
 push bx ; save BX


 call serdisp	; serial display?
# 2353 "second.S"
;;; xor bh,bh ; display on screen
 mov bx,#7 ; set color for 0x9dd476ec interface	; bh = page number, bl = foreground color, 7은 밝은회색
 mov ah,#14
 int 0x10	; al의 한글자를 쓴다.

dispret:
 pop bx ; restore BX
 ret



serdisp: push dx ; wait for space in the send buffer
 seg cs
 mov dx,slbase	; 3F8h, 2F8h등 base는 receiver buffer/transmitter holding register
 or dx,dx
 jz serret	; slbase가 0이면 리턴한다.
 add dx,#5	; slbase + 5
 push ax
serwait: in al,dx	; LSR(line status register)에서 1byte 읽어온다.
 test al,#0x10 ; break -> set break flag	; bit 4 = break interrupt
 jz nobrk	;	break 비트가 켜있다면 [break]=1
 seg cs
 mov byte ptr break,#1 ; break condition은 읽으면 리셋되기에 표시해둔다.
nobrk: test al,#0x20 ; ready to send ?	; bit 5 = emptry transmitter holding register
 jz serwait ; no -> wait	; 데이터를 보내도 될 때까지 대기
 sub dx,#5 ; send the character	
 pop ax
 out dx,al ; 준비가 됐다면 시리얼로 출력한다.
serret: pop dx ; done
 ret


! Get a key (CX = timeout exit)
! timeout 시간까지 키입력과 시리얼 입력을 기다린다. 키가 눌리거나, 시간이 다되면 cx로 들어온 Handler 점프한다.
getkey: ;;
;; seg fs ; set the timeout
 mov ax,par2_timeout ;DSC_OFF-10+SSDIFF
;;
 call setto ; timeout 설정
gwtkey: mov ah,#1 ; is a key pressed ? ; 키가 눌렸으면 zf=0
 int 0x16
 jnz gotkey ; yes -> get it ; 입력된 키가 있으면 키를 받아 리턴한다.
! 입력된 키가 없으면 시리얼 포트 확인
 mov dx,slbase ; using a serial port ?
 or dx,dx
 jz gnokey ; no -> wait
 add dx,#5 ; character ready ? ; LSR register
 in al,dx
 test al,#1 ; Receiver buffer register에 데이터가 있는가?
 jz gnokey ; no -> wait ; 시리얼에도 없으면 gnokey
 sub dx,#5 ; get it
 in al,dx
 and al,#0x7f ; strip 8th bit ; 7번 비트는 0으로 예약
 jnz gotch ; ignore NULs ; 시리얼에서 온 데이터는 키테일블로 변환하지 않는다. 입력받은 al 값으로 리턴

gnokey:
# 2417 "second.S" ; 들어온 키가 없으면 timeout 체크 0 or 0xff(timed out)
 test byte ptr timeout,#1 ; timed out ?
 jz gwtkey ; no -> wait ; 타임아웃이 안됐다. -> 다시 키입력 체크
 pop ax ; discard return address ; 원래 call에서 리턴할 주소
 jmp cx ; jump to timeout handler ; timeout시 cx로 점프
gotkey: xor ah,ah ; read a key
 int 0x16
 push bx ; keyboard translation (preserve BX)
 mov bx,#Keytable
 xlatb ; 받은 아스키 키값을 키테이블에서 변환한다.
 pop bx
gotch:


 seg fs ; always enter prompt ?
 test byte ptr par1_prompt+SSDIFF,#1 ; FLAG_PROMPT

 jz noosht ; yes -> do not disable timeout

; disable timeout ; 키 입력을 받으면 timeout을 해제할건지 체크한다.
 test byte ptr par2_flag2,#4 ; FLAG2_UNATTENDED 이 옵션이 켜있다면 키입력시마다 timeout을 재설정한다.
 jnz nocancel
 mov word ptr par2_timeout,#0xffff ; unattend 옵션이 꺼졌다면 한번 키를 입력하면 timeout은 없어진다.
nocancel:
noosht:
 ret ; done

! Shift wait loop (AX = timeout, returns CY set if interrupred)
! 상태키 혹은 일반키가 눌려지거나 시리얼에서 break 신호가 오면 CF=1을 리턴한다.
waitsh: call setto ; set timeout
actlp: mov ah,#2 ; get shift keys
 int 0x16



 and al,#0x5f ; anything set ? (except NumLock) ; 01011111 ; 키보드 상태비트에서 numlock,insert제외

 jnz shpress ; yes -> return with CY set ; 상태키(alt,shift,ctrl...)가 눌려(켜져)있으면 CF=1과 리턴
; 22.7.1 begin
 mov ah,#1 ; get status
 int 0x16 ; 키가 눌려졌으면 zf=0, jnz는 zf=0일때 분기한다.
 jnz shpress ; key pressed ; 키가 눌려졌다면 cf=1과 리턴
; 22.7.1 end

 mov dx,slbase ; using a serial port ?
 or dx,dx
 jz acnosp ; no -> go on ; 시리얼을 사용하지 않으면 시리얼쪽 체크는 안한다.
 cmp byte ptr break,#0 ; break received ? ; 이전에 break를 받았는가?
 jnz shpress ; yes -> return with CY set
 add dx,#5 ; check for pending break ; LSR register
 in al,dx
 test al,#0x10 ; 현재 break condition을 체크한다.
 jnz shpress ; break received -> return with CY set

acnosp: test byte ptr timeout,#1 ; timed out ?
 jz actlp ; no -> wait
 clc ; clear carry
 ret ; done
shpress: stc ; set carry
 ret ; done

! Timeout handling

instto: push ds ; install the timeout handler ; 타이머 인터럽트 (0x1c)에 타임아웃 핸들러 설치
 push #0
 pop ds

 cli ; no interrupts
 mov eax,[0x1c*4] ; get the old vector
 seg cs
 mov [int1c_l],eax ; save H & L parts	; int1c_l에 원래 루틴 주소를 저장하고
 mov [0x1c*4],#tick ; install new vector	; 인터럽트 핸들러를 가로챈다
 mov [0x1c*4+2],cs
 sti ; done
 pop ds
 ret

remto: push es ; remove the interrupt handler ; 원래의 타이머 인터럽트 루틴을 복원한다.
 push #0
 pop es

 mov eax,[int1c_l] ; restore the old vector
 seg es
 mov [0x1c*4],eax ; **
 pop es
 ret

! AX = ticks, 0xffff = no timeout
! ax를 타이머 countdown에 넣는다.
setto: or ax,ax ; time out immediately ?
 jz toimmed ; yes -> do it
 cli ; set timeout value
 mov cntdown,ax
 mov byte ptr timeout,#0 ; clear timed-out flag
 sti ; done
 ret
toimmed: mov byte ptr timeout,#0xff ; set the timed-out flag
 ret ; done

tick: pushf ; save flags ; 대체되는 0x1c 타이머 인터럽트 루틴
 seg cs ; no timeout ?
 cmp word ptr cntdown,#0xffff	; 0xffff면 timeout 되지 않는다.
 je notzro ; yes -> go on
 seg cs ; decrement counter
 dec word ptr cntdown
 jnz notzro ; not zero -> go on	; cntdown--가 0이 아니면 리턴
 seg cs ; set timeout flag
 mov byte ptr timeout,#0xff	; cntdown이 0이되면 timeout= 0xff
notzro:
 seg cs
 push dword [int1c_l]	; 0x1c의 원래 인터럽트 루틴으로 점프 (플래그는 복구된다)
 iret ; continue with old interrupt

kt_set:
;; keyboard table set?? 키보드 테이블의 섹터주소(5byte) 를 가져온다.
;; seg fs ; load the keyboard translation table
 mov cx,par2_keytab ;MSG_OFF+SSDIFF+7 ; SSDIFF=0 ; second 처음의 kt_cx
;; seg fs
 mov dx,par2_keytab+2 ;MSG_OFF+SSDIFF+9 ; kt_dx
;; seg fs
 mov al,par2_keytab+4 ;MSG_OFF+SSDIFF+11 ; kt_al
;;
 mov bx,#Keytable ; max_secondary+1536 오프셋
 ret



! Sector write; used for the keytable only

kt_write:
 push es
 push ds
 pop es
 call kt_set ; set for Keytable i/o


 seg fs ; BIOS data collection worked before?
 test byte ptr par1_prompt+SSDIFF,#128

 jnz kt_nowrite

 test byte ptr [par2_flag2],#2 ; a CD?
 jnz kt_nowrite

 call cwrite
kt_nowrite:
 pop es
 ret

! Sector write; used for the stored command line only
! 버퍼의 내용을 Default command line 섹터에 기록한다.
cmd_write:
;
;seg fs
 mov cx,mt_dflcmd+Keytable+256
;seg fs
 mov dx,mt_dflcmd+2+Keytable+256
;seg fs
 mov al,mt_dflcmd+4+Keytable+256
;
; fall into cwrite
;
; General sector write
;
cwrite:








 seg fs
 test byte ptr par1_prompt+SSDIFF,#2 ; is it a RAID write

 jnz cmd_raid_wrt

        mov byte ptr (dsk_wrflag),#1 ; flag write operation
        call cread
        mov byte ptr (dsk_wrflag),#0 ; flag read operation

 jnc cwok ; no error - return
cnok:
 pusha
 cmp ah,#3 ; write protect error
 je cnok3
 push ax ; save error code in AH
 mov bx,#msg_wrerr
 call say
 pop ax
 mov al,ah ; error code
 call bout
 call crlf ; leave space
 jmp cnok5
cnok3:
 mov bx,#msg_wrerr3 ; write protect
 call say
cnok5:
 popa
        stc ; flag error JRC
cwok:





 ret ; done

cmd_raid_wrt:
 test dl,#0x10 ; relocation called for?
 jnz crw1
 mov ah,#0x99 ; flag error
 jmp cnok
crw1:
 push si
 mov si,[rmask] ; get raid physical device mask
cwrm = 0x40|0x20|0x40|0x10|0X80
 and dl,#cwrm ; save flags, set device to 80

cwr2: shr si,#1
 jnc cwr3
 pusha
        mov byte ptr (dsk_wrflag),#1 ; flag write operation
        call cread_physical ; read the PHYSICAL device #
        mov byte ptr (dsk_wrflag),#0 ; flag read operation
 popa

cwr3: inc dx
 or si,si ; clears the carry
 jnz cwr2

 pop si
;;; clc ; signal no error
 jmp cwok

cwr_cnt: .byte 0 ; device code count
cwr_flags: .byte 0 ; saved flags



kt_read: ; Keytable read
 call kt_set ; set for Keytable i/o ; 키보드 테이블 섹터주소 세팅하고 bx엔 Keytable 메모리 주소를 넣는다.
 call cread  ; keytable 섹터를 읽어들인다.
 jc keyerr
 mov si,#Keytable ; compute a checksum of the keytable
 mov di,#512 - 8 ; skip the last 4+4 bytes
 push dword #0x04c11db7
 call crc32
 add di,si
 cmp eax,dword (di)
 jz nokeyerr

! Checksum error ; crc 에러나면 무한루프
keyerr:
 mov bx,#msg_chkkey
 br zz ; go wait
nokeyerr:
 ret

! Sector read
! enter with AL, CX, DX, ES, BX set for read
! trashes CX and DI
!
cread: ; entry point for mapped device r/w
 call map_device ; DL (logical) -> DL (physical) ; VolumeID기반으로 하드 번호를 매핑(변경)한다. 

cread_physical: ; same entry, device is not mapped

        test dl,#0x40|0x20	; 0x40=LINEAR, 0x20=LBA32
        jnz use_linear	; LBA or LINEAR가 켜져있으면 분기
! LINEAR, LBA32 둘다 꺼져있으면 CHS 방식으로 읽는다.
        push ax ;save the count
		mov ah,#2 ;read command
        call dsk_do_rw ; int 0x13 with retries ; 5번 시도
        pop cx ;Carry Set means error on read
        mov al,cl ;count in AL, error code in AH
        ret
! LINEAR나 LBA 하나만 켜있다면 DI=(hinib<<8)|dh 둘다 켜있다면 DI=(al<<8)|dh 가 된다. EDD라면 읽을 섹터수는 1이고 al은 상위 주소로 쓰인다.
use_linear:
        mov ah,hinib ;will be zero for LINEAR ; hinib==선형섹터주소의 24-31 bits. LINEAR라면 계속 0이다.
        xchg al,dh ;AX is possible address ; AX에 선형주소의 상위 16비트를 넣는다. hinib(31-24):dh(23-16). LBA32&LINEAR는 al:dh가 상위16비트. 하위16비트는 CX
		test dl,#0x20 ;test for LBA32/LINEAR *****
		jz lnread ;pure LINEAR ***** ; lba32나 linear 둘중 하나만 켜있으면 lnread
        test dl,#0x40
        jz lnread
        mov ah,dh	;former count is really hi-nibble ; linear, lba 둘다 켜지면 count에 1을 넣고 기존 al값을 상위8비트로 쓴다.
        mov hinib,ah
        mov dh,#1 ;set count to 1
lnread:
        xchg di,ax ;hi-address to DI ; lnread = linear read? ; di,cx=섹터 주소
        mov al,dh ;count to AL

 test dl,#0x10 ; ******
 jz ln_do_read ; ******
! 레이드 비트가 켜지면 섹터주소 변환(+raid_offset)
 call translate ; in volume.S 

ln_do_read:
        call lba_read
        mov al,cl ;count returned in AL, error code in AH
        ret ;Carry Set means error on read


; vmtest -- return Carry=1 if in virtual (VMware) mode
; return Carry=0 if in real mode
; 보호모드가 켜져있을 경우 virtual machine 에서 돌아간다고 가정.
vmtest:

 pushad ; save all extended registers
 smsw ax			;ax에 machine 상태를 저장한다.
 rcr al,1 ; PE bit in AL to Carry ; 오른쪽으로 PE비트값을 carry로 밀어낸다.
 jc vm_ret ; exit if virtual mode ; 보호모드에서 실행중이면 리턴 cf=1







;
; If no vmdefault, vmdisable, or vmwarn keywords were used, then we do not
; care about virtual mode. Do not touch the hardware, and always return
; Carry=0.
;
 test byte ptr [par2_flag2],#8 ; any vmXXX keywords?
 jz vm_ret ; TEST clears the carry, always
;
; VMware(R) test for virtual mode
;Hypervisor port 참조 : http://is.gd/RAiIvh
; vmware backdoor io port(0x5658)으로 vmware에서 실행중인지 확인하는 코드
 mov eax,#0x564D5868 ; EAX: in = 'VMXh' out = version ; io port에서 읽어들이기 전에 eax에 매직넘버를 넣어줘야한다.
 xor ebx,ebx ; EBX: out = 'VMXh' under vmware ; in을 통해 값을 읽으면, ebx
 mov edi,eax
 mov dx,#0x5658 ; DX: in = 'VX'
 mov ecx,#10 ; ECX: in = VMXGetVersion ; ECX에는 명령어가 온다. 10은 getversion
 in eax,dx	; 레지스터를 세팅후 0x5658 포트를 읽으면 eax,ebx,ecx의 값이 변한다.
 cmp ebx,edi ; test for vmware ; vmware라면 ebx=0x564d5868 (vmware 매직넘버)
 clc ; NOT vmware if Carry==0 ; 도중에 분기할때를 위한 리턴값 cf=0
 jne vm_ret ; not vmware ; vmware가 없다면 cf=0

 inc eax ; carry is not affected by INC
 jz vm_ret ; invalid version number == 0xFFFFFFFF ; eax에는 버전정보가 리턴된다. 잘못된 버전이면 cf=0인채 리턴

vm_vir:
 stc ; signal virtual mode ; vmware가 있으면 cf=1

vm_ret: popad ; restore all the extended registers
 ret
# 2809 "second.S"
; kbtest -- return Carry=1 if IBM PC/AT keyboard is present
; -- return Carry=0 if no IBM keyboard is present
;
kbtest:
 push ax
;
; If neither nokbdefault nor nokbdisable was used, we do not touch
; the keyboard hardware. Always report Carry=1 (keyboard present).
; 에코 커맨드(EE!)를 날려서 키보드가 연결되었는지 체크한다.
 test byte ptr [par2_flag2],#16 ; FLAG2_NOKBD
 jz kbtest8 ; NOKBD가 꺼져있으면 cf=1 리턴
# 2828 "second.S"
 cli ; added 5/17/2006
 mov al,#0xee ; echo command = 0xEE
 out #0x60,al
wait_kbd_ctrl_ready:
 in al,#0x64 ; 읽기:상태레지스터, 쓰기:커맨드레지스터
 and al,#0x01 ; output 레지스터에 데이터가 있는지 체크한다.
 jz wait_kbd_ctrl_ready ; read status port while it is not ready
 in al,#0x60 ; 포트가 준비되면 읽는다.
 sti ; added 5/17/2006
 xor al,#0xee ; XOR clears the carry ; 에코 커맨드가 정상적으로 돌아오면 0이 된다. 
 jne kbtest9 ; xor에서 캐리 플래그는 초기화된다. 에코가 안오면 cf=0인채 리턴
  ; if we got the same byte, the keyboard is attached







kbtest8:
 stc ; flag keyboard present ; 에코가 오면 cf=1 (키보드 정상)
kbtest9:
 pop ax
 ret




; crc32 -- calculate CRC-32 checksum
; shift연산으로 CRC32를 빠르게 구하는 루틴이다.
; call:
; push dword #POLYNOMIAL ; 나누기 위한 다항식(젯수:divisor)
; 많이 쓰는 다항식으로는 0x8005 (CRC16) 0x04C11DB7(CRC32)등이 있다.
; ES:SI char string pointer ; es:si=데이터주소
; DI count of characters ; di=크기
;
; call crc32
;
; CRC-32 is returned in EAX or DX:AX
; the arguments are popped from the stack
; crc32 표준 값인 다항식=0x04C11DB7, 초기값=0xFFFFFFFF, 최종XOR=0xFFFFFFFF(==not) 을 사용하고 있다.
crc32:
  push bp
  mov bp,sp

  push si
  push di
  push bx
  push cx

  xor eax,eax ; initialize CRC
  dec eax ; EAX = 0xFFFFFFFF ; shift register의 초기값
  inc di
crc32a:
  dec di
  jz crc32d ; 데이터가 더 없으면 종료
  mov cx,#8 ; count 8 bits ; 바이트 단위로 계산한다.
  seg es
  mov bl,(si) ; get next character ; 1byte 비트열을 가져온다.
  inc si 
crc32b: shl bx,#1 ; get hi bit of char in BH ; 데이터의 1bit 
  shl eax,#1 ; shift hi bit out of CRC ; shift register의 최상위비트(MSB)
  adc bh,#0 ; add carry to BH ; 위의 두 비트를 xor한다.
  shr bh,#1 ; put bit in carry 
  jnc crc32c ; skip the xor
  xor eax,(bp+4) ; xor in the polynomial ; crc32 값을 의미한다.
crc32c:
  loop crc32b ; loop back for 8 bits ; 8비트짜리 작은 루프
  jmp crc32a ; 다음 데이터 1byte

crc32d:
  not eax ; finialize CRC

  pop cx
  pop bx
  pop di
  pop si

  leave 			; enter 와 반대, 설정된 스택프레임을 해제화한다.
  ret 4				; return 시 sp-=4를 한다. stdcall 규약시에도 ret 를 통해 sp 조정을 한다고 함.


; enter with BX == Ramdisk size (in 4k pages)
;
rd_setup:
 push bx ; save Ramdisk size in pages
 mov eax,[hma] ; user specified?
 or eax,eax



 jnz near rd_have_hma

 seg fs
 test byte ptr par1_prompt+SSDIFF,#32

 jz near no_e801

; try the E820 memory map first
 xor edx,edx ; flag nothing found
 xor esi,esi ; flag size==0
 xor ebx,ebx
 jmp e8go
e8go2: or ebx,ebx ; test for end
 jz e8go5
e8go: push edx ; save best prospect
 mov eax,#0xe820
 mov edx,#0x534d4150 ;'SMAP'
 mov ecx,#20
 mov di,#memmap
 int 0x15 ; get memory map
 pop edx ; restore what we have found so far
 jc no_e820
 cmp eax,#0x534d4150 ;'SMAP'
 jne no_e820
 cmp ecx,#20
 jne no_e820
# 2970 "second.S"
 cmp word memmap+16,#1 ; available?
 jne e8go2
 mov eax,memmap+4 ; hi part of start
 shrd memmap,eax,#10 ; convert start to 1k
 mov eax,memmap+12 ; hi part of size
 shrd memmap+8,eax,#10 ; convert to 1k
 cmp dword memmap,#1024 ; below 1M
 jb e8go2 ; below 1M, no interest
 cmp esi,memmap+8 ; check size
 ja e8go2 ; want largest
 mov edx,memmap ; start (in 1k)
 mov esi,memmap+8 ; size (in 1k)
 add edx,esi ; HMA in 1k
 jmp e8go2

e8go5: or edx,edx ; find anything?
 jz no_e820
 xchg eax,edx
 jmp rd_have_hma
no_e820:
; above failed, try the older E801 block count interface
 xor cx,cx ; some BIOSs are buggy
 xor dx,dx
 mov ax,#0xe801 ; call
 stc
 int 0x15
 jc no_e801
 or cx,cx
 jz e801cx
 mov ax,cx
e801cx: or dx,dx
 jz e801dx
 mov bx,dx
e801dx:
 movzx ebx,bx
 movzx eax,ax
 shl ebx,#6 ; convert 64k to 1k
 mov ecx,#16*1024
 cmp eax,ebx ; compare sizes
 ja e801eax
 add ebx,ecx ; add in 16M
 mov eax,ebx ; and use this value
 jmp rd_have_hma
e801eax:
 add eax,#1024 ; add 1M
 cmp eax,ecx ; is it 16M
 jne rd_have_hma
 add eax,ebx ; add in ebx
 jmp rd_have_hma

no_e801:
; above two methods failed, try the old 0x88 function
 mov ah,#0x88 ; get count of extended memory blocks
 int 0x15
 movzx eax,ax ; extend to dword
 add eax,#1024 ; add in base 1M
;
rd_have_hma: ; have the HMA / 1k in EAX
# 3038 "second.S"
 mov ebx,#15*1024 ; 15Mb
 cmp eax,ebx ; compare to 15M
 jbe rd_use_eax ; use lower value

 seg fs
 test byte ptr par1_prompt+SSDIFF,#32

 jnz large_okay
 xchg eax,ebx ; limit to 15Mb
large_okay:
 mov ebx,#0x38000000/1024

 push ds
 mov ds,[initseg] ; load the original boot sector
 cmp word ptr [CL_HDRS_VERSION],#0X203
 jb not203
 mov ebx,[CL_RAMDISK_MAX]
# 3067 "second.S"
 dec ebx
 shr ebx,#10 ; divide by 1024
 inc ebx
not203:
 pop ds

 cmp eax,ebx
 jb rd_use_eax
;;;rd_use_smaller:
 xchg eax,ebx ; must use the smaller
rd_use_eax:
 pop bx ; get size in pages
 shr eax,2 ; convert to pages
 movzx ebx,bx ; zero high part of size
 sub eax,ebx ; start address of ramdisk to EAX
# 3098 "second.S"
 cmp eax,#4*256 ; Ramdisk loaded below 4Mb
 jae rd_okay ; kernel to be useful ...
 mov bx,#msg_rd4M ; complain
 call say ; is at zz

rd_okay:
 shl eax,4 ; shift (12-8) -> 4
 mov [rdbeg+1],ax ; set up beginning address
 mov [gdt+0x1b],ax ; set the GDT for the moves
 shr eax,16 ; get hi-byte of address
 mov [rdbeg+3],al ; set rest of address
 mov [gdt+0x1f],al ; and in the GDT, too
 ret
# 3119 "second.S"
load_initrd:
 push [map]
 push [moff]
 mov word ptr [map],#Map2
 push ds
 pop es
 mov ax,(si+id_flags) ; get 32, if any



 and al,#32 ; separate flag

 seg fs
 or byte ptr par1_prompt+SSDIFF, al ; set 32

 add si,#id_rd_size ; point at ramdisk size long
! take care of the RAM disk first
 xor eax,eax
 mov (rdbeg),eax ; clear address
 lodsd
 mov (rdszl),eax ; set rdszl+rdszh
 add eax,#4095 ; round up &
 shr eax,#12 ; convert to pages
 xchg bx,ax ; copy to BX
 lodsw ; address of the first map sector
 xchg cx,ax
 lodsw
 xchg dx,ax
        lodsb
 or bx,bx ; no RAM disk ?
 jz noramd ; yes -> skip it 2

 push si ; save SI, ES, and BX (RD size)
 push es
 push bx
 mov bx,[map] ; load the first map sector
 call sread
 mov moff,#0





 pop bx
 call rd_setup

 cmp dword ptr (rdbeg),#0
 je nordpt ; no -> no need to patch header for that
; setrdp:







 mov es,[setupseg] ; load the setup codes
 mov eax,(rdbeg) ; get RAM disk start address
 seg es
 mov (24),eax ; store in header
 mov eax,rdszl
 seg es
 mov (28),eax ; set RAM disk size
nordpt:
 push #0 ; ES=0 is our secret code to load via GDT
 pop es
 mov bx,#gdt
 call lfile ; load it

 mov al,#0x20 ; print a space
 call display

 pop es ; restore ES and SI
 pop si
noramd:
 pop [moff]
 pop [map]
 ret


serLI: .byte 13,10,0x4c,0x49 ; cr,lf,"LI"

BAUD_BASE = 115200 ; divisor == 1
divisor:
 .byte BAUD_BASE / 19200 ; must be same as bsect.c table
 .byte BAUD_BASE / 38400
 .byte BAUD_BASE / 57600 
 .byte BAUD_BASE / 115200
 .byte BAUD_BASE / 2400
 .byte BAUD_BASE / 2400
 .byte BAUD_BASE / 2400
 .byte BAUD_BASE / 2400

; serial_setup -- do the setup for the serial line communications
;
; No registers are saved
;
serial_setup:

;; seg fs
 mov dx,par2_port ; use a COM port ? ; BOOT_PARAM2 구조체의 par2_port와 par2_ser_param를 가져온다. sparam: ?
   ; watch out, loads par2_ser_param
;;
 dec dl
 js nocom ; no -> go on		; 1 감소시켰을때 MSB(최상위비트)가 켜져있다면 리턴
 xor ax,ax ; initialize the serial port
 xchg al,dh	; al = par2_ser_param, dh = 0

 push ax
 push dx

;;; or al,#0x06 ; stop bits = 2, nbits = 7 or 8
    ; this OR is not needed yet (21.7)
 int 0x14 ; Communications Port INIT ; ah=0 serial port 초기화, al=파라미터 비트(par2_ser_param, DX=0기반 시리얼포트 번호 COM1-4(0-3) = par2_port

 push #0x40
 pop ds	; ds = 0x40	; BDA(BIOS data area)

 pop bx ; was DX

 shl bx,#1	; *2
 mov dx,(bx) ; get the port address from the BIOS	; dx=COM?의 port address

 seg cs ; keep it
 mov slbase,dx	; slbase=COM port

 pop bx ; special baud rate test -- was AX ; par2_ser_param

 test bl,#0x04 ; stop bits == 2? ; 2번 비트(stop bit)가 0이면 1 stop bit, 1이면 2 stop bits
 cli ; do not disturb any code below
 jz stdbps ; standard BPS ; stop bit가 1이면 bps를 조절하지 않는다.

 shr bx,#5 ; index divisor array ; 상위 3비트만 남긴다. (baud rate)
 seg cs
 mov bl,divisor(bx) ; 배열에서 divisor를 얻는다.

spcbps: ; CLI: do not disturb ...
 push dx ; save base address
 add dx,#3 ; enable divisor latch access ; line control register
 in al,dx
 or al,#0x80 ; baud rate divisor = on
 out dx,al
 pop dx ; set new divisor
 push dx
 xchg ax,bx ; al=divisor, bx=LCR포트값
 out dx,al
 inc dx ; Modem control register
 mov al,ah ; 0?
 out dx,al
 inc dx ; disable divisor latch access
 inc dx ; Modem status register
 xchg ax,bx
 and al,#0x7f ; LCR포트의 값에 baud rate divisor bit를 끈다.(원상복귀)
 out dx,al
 pop dx ; restore base address

stdbps: ; CLI: redundant if fell in from above
 push dx
 add dx,#4 ; address Modem Control Reg.




 mov al,#3 ; turn on DTR and RTS

 out dx,al ; MCR의 0,1비트를 켠다. activate DTR, RTS
 pop dx
 sti ; done

 mov cx,#32 ; drain the queue (if any)
drain: in al,dx
 loop drain ; 버퍼를 비운다.
 add dx,#5 ; clear the status register ; line status register
 in al,dx

    ; send "\r\nLI" to the serial port

 mov si,#serLI
 mov cx,#4
ser1: seg cs
 lodsb
 call serdisp ; cr, lf, "LI" 출력(first에서 한것)
 loop ser1

nocom:
 ret






# 1 "shs3.S" 1
; shs3.S
# 10 "shs3.S"
;
;;; group dgroup _data

;;; segment _data public align=16 class=data

; global _shsInfo
# 36 "shs3.S"
;;; segment _text public align=16 class=code
# 45 "shs3.S"
shsTransform:
 push bp
 push di
 push si

 mov di,#shs_digest ;##
 mov eax,dword (di)
 mov esi,dword (di+4)
 mov ecx,dword (di+8)
 mov edx,dword (di+12)
 mov edi,dword (di+16)
 sub bx,bx ; count = 0

; align 4 ;align
shs_F356:
 mov ebp,esi
 and ebp,ecx
 push esi
 not esi
 and esi,edx
 or ebp,esi
 pop esi
 add ebp,#0x5a827999 ;##

 call shsTransCommon

 cmp bx,#20*4 ;##
 jb shs_F356

; align 4 ;align
shs_F359:
 mov ebp,edx
 xor ebp,ecx
 xor ebp,esi
 add ebp,#0x6ed9eba1

 call shsTransCommon

 cmp bx,#40*4
 jb shs_F359

; align 4 ;align
shs_F362:
 mov ebp,ecx
 and ebp,esi
 push ecx
 or ecx,esi
 and ecx,edx
 or ebp,ecx
 pop ecx
 sub ebp,#0x70e44324

 call shsTransCommon

 cmp bx,#60*4
 jb shs_F362

; align 4 ;align
shs_F365:
 mov ebp,edx
 xor ebp,ecx
 xor ebp,esi
 sub ebp,#0x359d3e2a

 call shsTransCommon

 cmp bx,#80*4
 jb shs_F365

 mov bx,#shs_digest ;##
 add dword (bx),eax
 add dword (bx+4),esi
 add dword (bx+8),ecx
 add dword (bx+12),edx
 add dword (bx+16),edi

 pop si
 pop di
 pop bp
 ret

; align 4
shsTransCommon:
 add ebp,edi
 mov edi,edx
 mov edx,ecx
 ror esi,2
 mov ecx,esi
 mov esi,eax
 rol eax,5
 add ebp,eax
 cmp bx,#16*4
 jae shsJ1
 mov eax,dword Wshs(bx)
 jmp shsJ2
shsJ1:
 push bx
 add bx,#13*4
 and bx,#15*4
 mov eax,dword Wshs(bx)
 sub bx,#5*4
 and bx,#15*4
 xor eax,dword Wshs(bx)
 sub bx,#6*4
 and bx,#15*4
 xor eax,dword Wshs(bx)
 sub bx,#2*4
 and bx,#15*4
 xor eax,dword Wshs(bx)
 rol eax,1
 mov dword Wshs(bx),eax
 pop bx
shsJ2:
 add eax,ebp
 add bx,#4
 ret

; align 4
byteReverse:
 push di
 mov cx,#16 ;##
 mov di,#Wshs ;##

; align 4 ;align
shs_F376:
 mov eax,dword (di)
;;; bswap eax
 xchg ah,al
 rol eax,16
 xchg ah,al

 stosd
 loop shs_F376

 pop di
 ret




; align 4

; global _shsInit, _shsUpdate, _shsFinal
_shsInit:
 push bp
 mov bp,sp
; push ds
 push di

 mov di,#shs_digest ;##
 mov dword (di),#0x67452301 ;##
 mov dword (di+4),#0xefcdab89
 mov dword (di+8),#0x98badcfe
 mov dword (di+12),#0x10325476
 mov dword (di+16),#0xc3d2e1f0 ;##
 sub eax,eax
 mov dword (di+20),eax
 mov dword (di+24),eax

 pop di
; pop ds
 leave
 ret


; align 4

_shsUpdate:
 push bp
 mov bp,sp

; buffer [bp+4]
; count [bp+6]

 push si
 push di
; push ds

 push ds
 pop es

; remain = shsInfo.countLo & (SHS_BLOCKSIZE-1);
 mov di,[shs_count]
 and di,#63 ;##

 movzx eax,word (bp+6) ;count
 add [shs_count],eax
 adc dword [shs_count+4],#0 ;##

 mov si,(bp+4) ;buffer

shs_J4:
 mov cx,#64 ;##
 sub cx,di ;CX = SHS_BLOCKSIZE-remain
 cmp ax,cx ; count >= SHS_BLOCKSIZE-remain
 jb shs_J6

 add di,#Wshs ;##
 sub ax,cx ; count -= SHS_BLOCKSIZE-remain
 push ax
 rep

   seg ss

      movsb ;memcpy

 call byteReverse
 call shsTransform

 pop ax
 sub di,di ;remain
 jmp shs_J4
shs_J6:
 add di,#Wshs ;##
 mov cx,ax
 rep

   seg ss

      movsb

; pop ds
 pop di
 pop si
 leave
 ret

; align 4
_shsFinal:
 push bp
 mov bp,sp
 push si
 push di
; push ds

 push ds
 pop es

 mov di,[shs_count]
 and di,#63 ;##
 mov byte Wshs(di),#0x80 ;##
 inc di
 sub ax,ax
 cmp di,#56 ;##
 jbe shs_J10
; count > 56
 mov cx,#64
 sub cx,di ;SHS_BLOCKSIZE - count
 add di,#Wshs ;##
 rep
      stosb

 call byteReverse
 call shsTransform

 mov cx,#56 ;##
 mov di,#Wshs ;##
 sub ax,ax
 jmp shs_J11
shs_J10:
 mov cx,#56 ;##
 sub cx,di
 add di,#Wshs ;##
shs_J11:
 rep
      stosb
 call byteReverse
 mov eax,[shs_count]
 mov ebx,[shs_count+4]
 shld ebx,eax,3
 shl eax,3
 mov [Wshs+14*4],ebx
 mov [Wshs+15*4],eax
 call shsTransform

; pop ds
 pop di
 pop si
 leave
 ret
# 335 "shs3.S"
; end shs3.S
# 3312 "second.S" 2

# 1 "read.S" 1
# 11 "read.S"
;
; lba_read: read using LBA
;
; Enter with:
; AL actual count of blocks to transfer
; DL device (0x00=A:, 0x80=C:, 0x81=D:) and LINEAR/LBA32 flags
; ES:BX buffer pointer
; DI:CX LBA to read
;
; Exit with:
; No Error: carry clear
; CX count of blocks read
; ES:BX unchanged
; DX unchanged
; AH error status if CF=1
; DI trashed
;
; count(al)만큼 읽어들인다. 문제가 있으면 CHS로 변환해 읽어들인다.
lba_read: push si ;save some registers

                push bx
                push dx
                xor ah,ah ;convert count to word
                push ax

                push cx ;gotta ask about 32-bit addressing
                push bx

  mov dh,dl ;use BL for flag test

  and dl,#DEV_MASK_asm ;remove spurious flags (0x8F)

  test dh,#0x20 
  jz no_lba ;linear will never use EDD calls ; lba32가 꺼졌다면 no_lba로 가서 변환한다.


         cmp al,#127 ;test for LINEAR transfer too big ; 읽을 섹터 수가 0x7f이상이면 CHS로 변환
  ja no_lba ; for LBA mode (127 is max)
                push ax

                mov bx,#0x55AA ;magic number
                mov ah,#0x41 ;function call
                int 0x13 ; EDD 지원여부 테스트

                pop ax

                jc no_lba ;  에러있으면 CHS로 변환
                cmp bx,#0xAA55 ;magic return
                jne no_lba ; magic number가 이상해도 CHS
                test cl,#01 ;packet calls supported?
                jz no_lba


; LBA mode is to be used

lba_avail:
                pop bx
                pop cx


                pop ax
                push ax


                push ds ;save DS

  push dword #0 ; 0L is pushed ; 섹터주소 주소
                push di ;LBA hi word ; LBA 주소 di:cx
                push cx ; lo word
                push es ;ES:BX
                push bx ; 읽어올 메모리 주소

                push ax ; 읽을 섹터 갯수



                push #16 ;size of parameter area ;# ; 패킷 사이즈
                           ;actually pushes a word
                mov si,sp ;DS:SI is param block pointer

                push ss
                pop ds ;DS:SI points at param block


                mov ax,#0x4200 ;read function -- must be AX ; EDD 읽기 함수
     ; as AL has meaning on WRITE
                call dsk_do_rw





                lea sp,word ptr (si+16) ;use lea so flags are not changed ; EDD 패킷 반환

                pop ds ;restore DS

                jmp lba_read_exit1 ; 리턴


; linear주소 -> CHS로 변환
no_lba:
                pop bx
                pop cx


lba_small: ;must get the disk geometry

lba_more_small:
                push bx
                push di
                push cx
                push ax

                push bx
                push dx
                push di
                push cx

                push es
                mov ah,#8 ; DL is set to device

                call dsk_do_int13



                pop es
                jc lba_geom_error

                push cx
                shr cl,#6 ;;;;
                xchg cl,ch ;CX is max cylinder number
                mov di,cx ;DI saves it
                pop cx

  shr dx,#8
  xchg ax,dx ;AX <- DX
  inc ax ;AX is number of heads (256 allowed)

; compensate for Davide BIOS bug
  dec cx ; 1..63 -> 0..62; 0->63
  and cx,#0x003f ;CX is number of sectors
  inc cx ; allow Sectors==0 to mean 64

                mul cx ; kills DX
                xchg ax,si ;save in SI

                pop ax ;was CX
                pop dx ;was DI
                cmp dx,si
                jae lba_geom_error2 ;prevent division error
                div si ;AX is cyl, DX is head/sect
                cmp ax,di
                ja lba_geom_error2 ;cyl is too big

                shl ah,#6 ;;;;
                xchg al,ah
                xchg ax,dx
                div cl ;AH = sec-1, AL = head

                sub cl,ah ;CX = max count possible
                mov si,cx ;save in SI

                inc ah
                add dl,ah ;form Cyl/Sec
                mov cx,dx
                pop dx ;get device
                pop bx
                xchg al,dh ;


                pop ax ;restore the count
                push ax ;keep in the stack
                cmp ax,si ;
                jb lba_cntltmax
                mov ax,si ;smaller is in AX
lba_cntltmax: push ax
                mov ah,#2 ;READ
                call dsk_do_rw




        ; carry is set or clear

                pop bx ;actual count read (was AX)
                pop si ;count remaining
                pop cx
                pop di
                jc lba_read_exit_e
                add cx,bx ;update lba address
                adc di,#0 ;the # was omitted in rev 3
                xchg ax,bx
                pop bx ;buffer address
                add bh,al ;update ES:BX
                add bh,al ;0xbb920890 has already checked for seg update
                xchg si,ax
                sub ax,si ;AX is remaining count after transfer
                jnz lba_more_small
                         ; after the sub yields 0, the carry is clear

lba_read_exit1: jmp lba_read_exit







dsk_do_rw: or ah,#0 ; 0=read, 1=write, 2=read-only test
dsk_wrflag equ *-1 ; byte data area is the immediate
# 237 "read.S"
dsk_do_int13:
                push bp ; 디스크 읽기를 5번 시도하고 캐리를 보존한다.
                mov bp,#5 ;number of tries
dsk_do_int13a: pusha
                int 0x13
                jnc dsk_io_exit ; 에러가 안나면 리턴
                dec bp ;does not affect the carry ; 에러나면 카운트(5)를 감소
                jz dsk_io_exit ; 5번 실패시 리턴 cf=1
                xor ax,ax ;reset disk controllers
                int 0x13
                popa	; 재시도시 레지스터들 복구. 카운트(bp)도 복구된다.
                dec bp
                jmp dsk_do_int13a

dsk_io_exit: mov bp,sp ;do not touch any flags ; add명령을 쓰면 3줄을 한줄로 바꿀수 있지만 플래그를 건드리기 때문에 lea를 쓴다.
                lea sp,(bp+16) ;an ADD would touch flags  ; 레지스터들을 복구시키지 않는다.상태코드 ah==0이면 노에러
                pop bp ;do not touch any flags
                ret

lba_geom_error:
                pop cx
                pop di
                jmp lba_g3
lba_geom_error2:
                mov ah,#0x40 ;seek failure error code
lba_g3: pop dx
                pop bx

                pop cx ;was AX
                pop cx
                pop di
lba_read_exit_e:
                pop bx

                stc
lba_read_exit:

                pop cx ;return count in CX
                pop dx
                pop bx

                pop si

                ret
# 3314 "second.S" 2
# 1 "volume.S" 1
# 40 "volume.S"
rmask: .word 0 ; physical raid mask

; build_vol_tab -- Build the table of volume IDs
; and fill in the device translate table
;
; Enter with:
; DS=ES=CS
;
; Exit with:
; Nothing
;
; Side effects: The volume ID table is built
; The from:to device translate table is filled in
;
; 볼륨테이블을 만든다. 볼륨테이블은 논리적번호:물리적번호를 가지며 논리번호는 리눅스에 lilo가 설치될때의 하드번호, 물리번호는 부팅되서 연결된 실제 하드번호를 가진다. volumeID로 매핑한다.
build_vol_tab:
 pusha

 xor cx,cx ; depend on this being preserved
 xor dx,dx
 xchg [devmap],dx ; clear our First Stage mapping

 call is_prev_mapper ; is there a previous mapper ; 체인로더로 인터럽트 0x13이 매핑되었는지 확인한다.
 jz bvt0 ; 매핑이 안되어있다면 점프

; have previous mapper active

; ****** 22.5.7
 push di
 or dx,dx ; any translation?
 jz bvt003
bvt001:
 seg es
 mov ax,(di) ; get previous translation
 inc di
 inc di
 or ax,ax
 jz bvt003
 cmp al,dh ; does it affect us?
 jne bvt001
 mov [init_dx],ah ; update physical device
bvt003:
 pop di
; ****** 22.5.7

 seg es
   mov (di),cx ; sterilize it
bvt0:
 push cs
 pop es ; restore ES

; ****** 22.5.8
 mov di,#Keytable+256+mt_serial_no ; 키테이블 크기는 256, 뒤에 Menutable 구조체가 있다.
 mov cx,#MAX_BIOS_DEVICES_asm ; = 16
 xor eax,eax
 repe	 ; mt_serial_no부터 0이 아닌값을 찾는다.
   scasd ; scan for any serial nos in table
 je bvt90 ; if none, skip reading vol_ids ; 메뉴테이블 값이 전부 0이면 bvt90(레이드 테이블 생성)으로 점프한다.
    ; as there will be no translations
; ****** 22.5.8


 xor cx,cx ; start at hard drive 0 (0x80)
 mov di,#vtab ; place to put IDs ; volume table
bvt1:
 call read_vol_id ; get VolumeID in EAX ; MBR을 읽어서 eax에 volumeID를 리턴한다.
 stosd ; store in table ; 읽어온 볼륨ID를 vtab에 저장한다.
 or eax,eax ; test for zero
 jz bvt9 ; end, or no volume ID ; 볼륨ID가 없으면 끝나거나 레이드 비교

; now see if there will be a translation
 push di ; 연산하기 위해 값을 보존
 push cx
; vtab에 저장한 값이 eax에 읽어들인 volumeID와 중복이면 0을 넣는다.
; ****** 22.5.9
 mov cx,di ; 4*table count to CX ; CX=device code , 첫번째는 cx=vtab+4
 mov di,#vtab 
 sub cx,di ; 4*count ; 저장된 vtab의 길이 - 첫vtab offset = 저장된 볼륨ID 길이
 shr cx,#2 ; table count ; 저장된 offset / 4 해서 갯수가 나온다.
 dec cx ; 갯수 - 1
 jz bvt1.5 ; table empty ; 처음이면 bvt1.5로 점프
 repne ; repeat while no match ; MBR에서 읽어온 볼륨ID(eax)값과 vtab(di)값을 비교한다. 
   scasd
 jne bvt1.5 ; 중단이 되었거나 끝까지 중복되는 값이 없으면 점프
;;; bvt1은 연결된 하드 볼륨을 읽어와서 vtab에 쓰고 연결된 하드에 중복값이 있으면 에러출력하는 루틴
 mov bx,#msg_dupl ; duplicate message
 call say ; 에러메세지 출력

 call pause ; 딜레이

 pop cx
 pop di

 mov dword (di-4),#0 ; zero the duplicated volumeID ; 중복 volumid를 0으로
 jmp bvt9 ; skip to next on duplication ; 다음 루프를 돈다.

bvt1.5:
; ****** 22.5.9
 mov si,#Keytable+256+mt_serial_no ; 시리얼 넘버가 저장된 배열의 주소
 mov cx,#MAX_BIOS_DEVICES_asm
 mov di,si
bvt2: jcxz bvt7
 repne ; repeat while not matching
   scasd ; 논려volume과 물리volume과 일치하는 값을 찾는다.
 jne bvt7 ; jump if no match ; 일치하는 값이 없다면 스킵
# 153 "volume.S"
 lea dx,(di-4) ; DX is address of match ; 일치하는 logical volumeID 위치
 sub dx,si ; DX is 4*index 
 shr dx,#2 ; DX is input device # ; 몇번째인지 구한다.
 pop bx ; BX is real device # ; bx에 스택에 있는cx(하드디스크 숫자0-15)를 가져온다.
 push bx
 cmp bx,dx ; bx는 물리 번호, dx는 논리 번호다. 둘이 일치하면(ex 01:01) 볼륨테이블에 쓰지 않는다.
; ****** 22.5.9
;;; je bvt2 ; equal means no translation
 je bvt7 ; equal means no translation 
; ****** 22.5.9
 mov dh,bl ; dx에 물리적 논리적 테이블을 몰아넣는다.
 or dx,#0x8080 ; make into HD bios codes ; 하드디스크를 표시하는 최상위비트를 켠다.
# 173 "volume.S"
 push si
 mov bx,#devmap ; scan the device translation table
bvt4:
 mov si,(bx) ; get from(low):to(high) pair ; bx는 devmap의 포인터값
 inc bx
 inc bx ; bump pointer by 2
 cmp si,dx ; duplicate?
 je bvt5 ; 일치하는 값이 있으면 넘어간다.
;;; bvt4는 from:to의 값을 devmap에 실제로 넣는 루틴이다.
 or si,si ; not duplicate; at end? ; devmap의 끝(0)이 아니면 bvt4로 루프
 jnz bvt4 ; 끝부분 까지 증가시킨다.

 mov (bx-2),dx ; put at end of table ; 물리,논리값
 mov (bx),si ; and mark new end ; 0
bvt5:
 pop si
; ****** 22.5.9
;;; jmp bvt2
; ****** 22.5.9

bvt7:
 pop cx
 pop di
bvt9:
 inc cx
 cmp cx,#MAX_BIOS_DEVICES_asm
 jb bvt1

bvt90:
; now build the RAID offset table
; MAX_BIOS_DEVICES만큼 루프를 돌면서 해당 하드가 레이드라면 물리하드 레이드비트(rmask)에 해당 비트를 켜고 연결된 하드 순서에 맞춰 rtab에 raid_offset 테이블을 만든다.
 mov si,#Keytable+256+mt_raid_offset
 mov dx,[Keytable+256+mt_raid_dev_mask] ; 16비트짜리 하드별 레이드 비트
 xor bx,bx ; count thru devices ; n번째 * 4 의 오프셋
bvt91:
 xor eax,eax ; may store 0
 shr dx,#1 ; is it raid? ; 이 하드가 레이드가 아니면 bvt92로 스킵한다. (1bit를 shift하면 바이트 크기가 절약된다.)
 jnc bvt92 ; not a raid device 

 lodsd ; get raid offset
 push eax ; save value in stack ; raid 오프셋을 읽어내 스택에 저장

 mov eax,[Keytable+256+mt_serial_no](bx)  ; 이 하드의 volumeID를 읽어온다.
 mov di,#vtab ; physical table address ; vtab==볼륨테이블에서 물리 번호를 저장할 위치
 mov cx,#MAX_BIOS_DEVICES_asm ; 16번
 repne
   scasd ; scan for a match ; eax는 인스톨러로 저장된 볼륨 vtab은 연결된 하드 볼륨
 jne bvt_not_found ; the logical volume is not there ; 일치하는 볼륨이 없다면 스킵
 lea di,(di-4-vtab) ; DI is 4*index into table ; di를 4로 나누면 일치하는 n번째 하드가 나온다.
 mov cx,di
 shr cx,#2 ; make 0..15 ; 일치하는 하드 번호
 mov ax,#1
 shl ax,cl ; mask bit in right position ; n번 하드에 해당하는 rmask 비트를 켠다.
 or [rmask],ax
 pop dword ptr rtab(di) ; store RAID offset ; rtab에 raid_offset을 넣는다.
 jmp bvt92
bvt_not_found:
 pop eax ; clean up the stack
bvt92:
 add bx,#4 ; for(bx=0;bx<16*4;bx+=4) ; 오프셋을 4 증가
 cmp bx,#MAX_BIOS_DEVICES_asm*4
 jb bvt91
# 294 "volume.S"
 popa ; restore all the regs
 ret

msg_dupl:
 .ascii "O\nError: Duplicated Volume ID\n"
 .byte 0
# 376 "volume.S"
; read_vol_id -- Read the volume ID off of a drive
;
; Enter with:
; CX = drive number to read (hard disk 0..15)
; ES=DS=CS
;
; Return:
; Carry Clear on success
; EAX = volume ID (0 means no ID)
;
; Carry set on error
; EAX = 0
;
;
read_vol_id:
 push bx
 push dx
 push cx
 push di

 push cx
 push es ; paranoia (floppies touch it)

 mov ah,#8 ; get number of drives in DL 
 mov dl,#0x80
 call dsk_do_int13 ; retry 5 times

 pop es
 pop cx ; restore device code

 jc rvi_9 ; 디스크 읽기가 실패하면 rvi_9로 점프
 cmp cl,dl ; dl = 드라이브 갯수
 jae rvi_9 ; cl이 드라이브 갯수보다 더 커지면 종료

 mov dl,cl ; cl(드라이브) 값을 dl에 저장
 mov cx,#1 ; cx = cylinder , sector
 mov bx,#Map ; 쓸 메모리 주소
 or dl,#0x80 ; 드라이브 값에 0x80을 더해준다.
 mov dh,ch ; head, ch=0
; ****** 22.6.1


; MBR을 읽어온다.
; ****** 22.6.1
 mov ax,#0x201 ; read
 call dsk_do_int13
 jc rvi_9

 seg es
   mov eax,(bx+0x1be -6) ; fetch return ; eax에 volumeID를 읽어온다.
 jmp rvi_exit
rvi_9:
 xor eax,eax ; rvi_9는 에러나면 eax를 0으로 하고 CF=1로 한다.
 stc
rvi_exit:
 pop di
 pop cx
 pop dx
 pop bx
 ret


; map_device -- Take the logical device code in DL and map it
; into the physical device code preserving all flags
; 22.5.6 Any RAID relocated device code maps to the boot device code
;
; Enter with:
; DL containing logical device code & flags
; DS register not guaranteed
;
; Exit with:
; DL containing physical device code & same flags
;
;
map_device:
 push si ; save working registers
 push ax ; 쓰이는 레지스터들 보존
 push bx
 mov si,#devmap ; point at translation table ; devmap은 BSS+[128]의 36바이트 공간이다.
 mov bl,dl
 and bl,#DEV_MASK_asm ; from device code in BL ; 상태 비트(4-6)를 제외한다.

; ****** 22.5.6
 seg cs
 mov ah,[init_dx] ; get boot device code ; ah=부팅된 하드 bl=읽어들일 하드
 test dl,#0x10
 jnz bios_tt_match ; it is RAID, go use the boot device code ; 읽을 하드의 레이드 비트가 켜져 있으면 부팅된 하드번호를 넘긴다..
; ***** 22.5.6
! bios translation table next
bios_tt_next:
 seg cs ; DS may be bad ; devmap에서 순차적으로 값을 읽어와 비교한다.
   lodsw ; get from/to pair
 or ax,ax ; end of list?
 jz bios_tt_done ; 끝(0)이면 종료한다.
 cmp al,bl
 jne bios_tt_next
; got a match ; logical 값이 일치하면 physical로 바꾼다.
bios_tt_match:
 and dl,#0xFF-DEV_MASK_asm ; save flags =0x70 ==01110000 
 or dl,ah ; put on the TO device code ; 상태 비트(4-6) 보존
bios_tt_done:
 pop bx
 pop ax
 pop si
 ret
# 519 "volume.S"
; translate -- test for a raid device, and do the offsetting
;
; Enter with:
; DI:CX LBA32 or LINEAR address
; DL physical device code & flags (0x10 is set)
; AL sector count
; ES:BX buffer pointer for R/W
;
; Exit with:
; DI:CX updated if RAID translation takes place
; All other registers are unchanged
;
; raid 하드 섹터 주소 변환. rtab의 raid_offset만큼 섹터주소에 더해준다.
translate:
 push bp
 mov bp,sp

 cmp word [rmask],#0 ; any translate bits set?
 jnz trans_1

; this special cases the initial Keytable read, when no setup has been done
; dl의 레이드 비트가 켜져야 넘어오기 때문에 볼륨테이블이  생성되기 전 키테이블을 읽어들일때를 위한 예외루틴. first에 저장된 raid_offset을 더한다.
 seg fs
 add cx,par1_raid_offset+SSDIFF ; ***** RAID ******
 seg fs
 adc di,par1_raid_offset+2+SSDIFF ; ***** RAID ******

 jmp trans_ret

trans_1:
 push di
 push cx ; form dword (bp-4)

 mov di,dx ; DI gets full device code
 and di,#DEV_MASK_asm & 0x7F ; 하위 4비트만 남는다.
# 571 "volume.S"
 shl di,#2 ; index into array ; n번째 하드*4 == 오프셋

 mov cx,[rtab](di) ; get low relocation value
 mov di,[rtab+2](di) ; get high relocation value
# 590 "volume.S"
 add (bp-4),cx ; relocate ; bp-4==cx(stack) 32비트 raid_offset을 더해준다.
 adc (bp-4+2),di ; ** ; bp-2== di

 pop cx
 pop di
# 607 "volume.S"
trans_ret:
 pop bp
 ret
# 3315 "second.S" 2

# 1 "mapper.S" 1
; mapper.S - 0xbb920890 chain loader subroutine
;
; Copyright 2003-2004 John Coffman.
; All rights reserved.
;
; Licensed under the terms contained in the file 'COPYING' in the
; source directory.
;
; Check for presence of existing drive mapper
;
; Enter with DS == CS, SS == 0000 (chain loader)
; Enter with DS == CS == ES, SS != 0000 (second stage)
;
; If a previous drive mapper exists, ES:DI points at the drvmap
; and ZF=0 (DI != 0)
;
; If no recognizable drive map exists, DI == 0 and ZF==1
; ES is indeterminate
;
;
;
is_prev_mapper:
 push cx
 push si


 push #0
 pop es
 seg es	; 세그먼트는 0



 les di,[4*0x13] ; vector to int 0x13
 or di,di
 jnz is_p_no_mapper ; our mappers start at offset 0 ; 할당하는 코드는 0으로 하는걸로 추측한다.
! 인터럽트 0x13 코드 오프셋이 0이 아니면 점프. 오프셋이 0이면서 세그먼트가 0xA000이상 이거나 0x60미만이면 is_p_no_mapper로 점프한다. 가로채려는 의미로 추측
 mov di,es		; 세그먼트를 비교한다.
 cmp di,#0xA000 ; start of system reserved locations
 jae is_p_no_mapper
 cmp di,#0x0060 ; VERY conservative
 jb is_p_no_mapper
! 이미 매핑이 되어있으면
; first test for new mapper
 xor di,di
 mov cx,#new13_length ; 길이값
 mov si,#new13		; 0x13 인터럽트의 코드의 어드레스
 repe
   cmpsb		; new13코드 시작부터 LILO스트링 등을 비교
 jne is_p_try_old ; new13코드와 다르면 is_p_try_old로 분기
; 후킹할 코드가 같다면
; found new (v.22) mapper
 seg es
   mov di,[new13_drvmap_offset]	; new13 코드 앞부분 일부의 길이



! es = int13h 인터럽트의 세그먼트
 jmp is_prev_ret ; 이미 매핑되어 있고 new13 코드와 같다면 리턴한다.
! 이미 매핑이 되어있다면 코드가 같은지 비교한다
is_p_try_old:
 xor di,di
 mov cx,#new13_old_length ; 길이-2로 mov di,에서 멈춘다. 
 mov si,#new13_old 
 repe
   cmpsb
 jne is_p_no_mapper ; 
; new13_old와 매핑값이 같으면 그 다음
; likely old (<=v.21) mapper
 seg es
   mov di,(di) ; new13_old 코드에서 di에 들어가는 값과 비교한다.
 cmp di,#new13_old_min_offs ; validate the range of values ; 0x46 = 70 보다 작으면 매핑이 안되어있다.
 jb is_p_no_mapper
 cmp di,#new13_old_max_offs ; validate the range of values ; 0x50 = 80
; 70이상 80이하면 매핑이 되어있다는 의미로 추측





 jbe is_prev_ret

; 매핑이 안돼있으면 DI=0(ZF=1)과 함께 리턴한다. 매핑이 되어있다면 di는 0이 아니다. (ZF=0)
is_p_no_mapper:
 xor di,di ; set DI = 0, ZF=1
is_prev_ret:
 or di,di ; set ZF by DI
 pop si
 pop cx
 ret


; 비교하기 위한 루틴
new13_old:
 push ax ! save AX (contains function code in AH)
 push bp ! need BP to mess with stack
 mov bp,sp
 pushf ! push flags (to act like interrupt)
 push si
 mov si,#drvmap-new13 

new13_old_drvmap_offs = * - new13_old - 2
new13_old_length = new13_old_drvmap_offs
new13_old_min_offs = 0x46 ; min seen in old code is 0x49
new13_old_max_offs = 0x50 ; maxed out at 21.7.5 at 0x4d

 .even ! this is very important

new13: push ax ! save AX (contains function code in AH)
 push bp ! need BP to mess with stack
 mov bp,sp
 jmp new13a ! make space for signature

 .org new13+6
 .ascii "LILO"
 .word 0x11
new13_length = *-new13 ; max compare length
 .word 256*2 +23
new13_drvmap_offset = * - new13
 .word drvmap-new13 ! relative pointer to drive map
new13a:
# 180 "mapper.S"
drvmap:
# 3317 "second.S" 2


# 1 "bdata.h" 1
# 3320 "second.S" 2
# 1 "biosdata.S" 1
; biosdata.S is
;
; Copyright 2002-2004 John Coffman.
; All rights reserved.
;
; Licensed under the terms contained in the file 'COPYING' in the
; source directory.
;
;


io_sig: .long 0 ; space for CRC
 .ascii "LiLo" ; "LiLo"
 .word 6 ; sanity check
io_lth: .word 0 ; byte count overall
io_good_disk:
 .byte 0 ; last good drive (in low byte)
 .byte 3
 .byte 2
 .byte 16
io_flp: .word 0
io_hrd: .word 0
io_pt: .word 0


io_eqp: .word 0
io_vid: .word 0


io_l_sig = *-io_sig

; read partition table of device in DL
;
; save PT
;
io_get_pt:
 push ds
 push es

 push #0
 pop es
 mov bx,#BOOTSEG*16+512
 mov cx,#1
 mov dh,#0
 mov ax,#0x201 ; read 1 sector







 call dsk_do_rw ; make 5 tries
 push es
 pop ds

 pop es




 lea si,(bx+0x1be -8)
 mov cx,#4*16+8

 jc io_get_pt_err
 rep
   movsb
io_get_pt_ret:
 pop ds
 ret

io_get_pt_err:
 mov al,#-1
 rep
   stosb
 jmp io_get_pt_ret



; check a hard drive for EDD support
;
; device code is in DL
;
;
io_do_edd_check:
 push dx
 mov ah,#0x41
 mov bx,#0x55AA







 int 0x13

 xchg al,ah
 lahf
 pop dx

 stosw
 mov ax,bx
 stosw
 xchg ax,cx
 stosw

 jc io_do_edd_check_ret
 cmp bx,#0xAA55
 jne io_do_edd_check_ret





; get the EDD parameters

 push dx ; paranoia, protect DL
 push es ; more paranoia
 push ds

 push es
 pop ds
 mov si,di ; DS:SI points at return area
 push di
 mov ah,#0x48
 mov word (si),#30 ; set max count to return







 int 0x13
 pop di

 xchg al,ah
 lahf

 add di,#30
 stosw ; save the return flags

 pop ds
 pop es
 pop dx

io_do_edd_check_ret:
 ret



;
; io_biosdata: examine hard disk BIOS devices
; and video state
;
; Enter with:
; DS == CS
; direction flag clear
;
;
; Exit with:
; All registers preserved
;
; Side effect is to write the low memory disk data area
;
;
;

io_biosdata:
 pusha ;save all registers
 push es

 push #0x60 ;save area is at 0060:0000 (0x000600)
 pop es
 mov di,#io_l_sig ;skip over header area

; get the equipment configuration flags


 mov io_eqp,di ;save equipment pointer
 push dx ; protect this register
# 191 "biosdata.S"
 int 0x11
 stosw

; get the conventional memory size
# 203 "biosdata.S"
 int 0x12
 stosw ; save the number
# 214 "biosdata.S"
 pop ax ; get saved DX register
 stosw


; collect the video information


 mov io_vid,di ; set the pointer


 mov ah,#0x0F ; get video mode







 int 0x10

 push ds
 push #0x40
 pop ds
 mov bl,[0x84] ; get rows, too
 pop ds

 stosw ; save AX
 xchg ax,bx
 stosw ; save BX
 cmp bl,#7 ; is it MDA
 beq io_floppies ; yup, skip it all
 cmp bh,#80 ; number of columns on screen
 jb io_floppies1 ; probably CGA




 mov ah,#0x12
 mov bx,#0xFF10 ; get configuration information







 int 0x10

 stosw ; save AX
 xchg ax,bx
 stosw ; save BX
 cmp ah,#1
 ja io_floppies1


 mov ax,#0x1A00 ; get display combination code







 int 0x10

 stosw ; save AX
 xchg ax,bx
 stosw ; save BX
 cmp bl,#0x1A ; is function supported?

 jne io_floppies




 cmp al,#4
 jb io_floppies1



; test to see if registers get trashed (some geforce video bios trashes dx)
 push ds
 push es
 push di

 mov cx,#0x1234
 mov dx,#0x5680
 mov bp,#0x4321

 mov ax,#0x1200
 mov bx,#0x0036







 int 0x10 ; enable screen refresh

 pop di
 pop es
 pop ds
 stosw ; AX
 xchg ax,cx
 stosw ; CX
 xchg ax,dx
 stosw ; DX
 xchg ax,bp
 stosw ; BP





 mov ax,#0x4F00 ; check VESA present
; ES:DI is already set







 int 0x10

 seg es
   mov bx,(di) ; possible "VE"
 seg es
   mov cx,(di+2) ; possible "SA"
 stosw ; save AX
 xchg ax,bx
 stosw ; possible "VE"
 xchg ax,cx ; possible "SA"
 stosw
 cmp bx,#0x004F ; good return
 jne io_floppies
 cmp cx,#0x4556 ; "VE"
 jne io_floppies
 cmp ax,#0x4153 ; "SA"
 jne io_floppies

 mov ax,#0x4F01
 mov cx,#0x0101 ; get mode information







 int 0x10

 seg es
   mov bx,(di) ; get bits
 stosw ; save AX
 xchg ax,bx
 stosw ; save bits

 mov ax,#0x4F01
 mov cx,#0x0103 ; get mode information







 int 0x10

 seg es
   mov bx,(di) ; get bits
 stosw ; save AX
 xchg ax,bx
 stosw ; save bits
# 397 "biosdata.S"
; now go after the disk drive information

io_floppies:

io_floppies1:

 mov cx,#2 ;test 4 floppies
 xor dx,dx
 mov io_flp,di ;set pointer to floppy info

io_next_drive:
 push cx

; get the drive type

 mov ah,#0x15
 push dx
# 429 "biosdata.S"
 int 0x13

 xchg al,ah ;code to AL
 lahf ;flags to AH

 stosw ; save AX
 xchg ax,dx
 stosw ; save DX (low order)
 xchg ax,cx
 stosw ; save CX (high order)
 xchg ax,dx ; restore code to AL
 pop dx

 jc io_no_disk ; error means no disk present
 dec al ; AL==0 means no disk present
 jns io_get_param ; if S=0, some disk type is present

io_no_disk:
 or dl,dl
 jns io_get_param ;do it all on floppies

 pop cx ;premature loop termination
 jmp io_loop_end ;skip the rest on fixed disks

io_get_param:

; get drive parameters

 push dx
 push es ; supposedly clobbered for floppies only
 push di ; do not trust anyone

 mov ah,#0x08







 int 0x13
 xchg al,ah
 lahf

 mov bp,di ; save floppy param pointer
 mov bx,es

 pop di
 pop es

 stosw ; save return code & flags
 xchg ax,cx
 stosw
 xchg ax,dx
 stosw
 pop dx

 jc io_fh_check ; bad return above
;;; mov [io_good_disk],dl ; save DL

 cmp dl,#0x80 ; check for first HD
 jne io_fh_check
 cbw ; former DL has disk count
 pop cx ; get HD count
 push ax ; set new HD count

io_fh_check:
 or dl,dl ; check floppy/hard disk
 js io_check_edd
 xchg ax,bp ; was DI
 stosw
 xchg ax,bx ; was ES
 stosw
 jmp io_skip_edd

; check EDD extensions present

io_check_edd:

 call io_do_edd_check

io_skip_edd:

 mov [io_good_disk],dl ;save last disk checked
 pop cx
 inc dl

 loop io_next_drive




io_loop_end:
 or dl,dl ; set the S flag
 push di ;
 mov cx,#16 ; do not touch flags ***
 mov dl,#0x80 ; do not touch flags ***

 jns io_next_drive ; do the hard drives if not done



 pop word [io_pt] ; save pt pointer
 pop word [io_hrd] ; save hd pointer

; now save the partition tables

io_get:
 cmp dl,io_good_disk
 ja io_got
 call io_get_pt
 inc dx
 jmp io_get
io_got:

io_checksum_it:

; now must record and checksum the results

 mov io_lth,di ; address of end is overall count

 push di
 xor di,di ;move to here
 mov si,#io_sig ;move from here
 mov cx,#io_l_sig ;this number of bytes
 rep
   movsb ;
 pop di
 mov si,#4 ; skip long at beginning
 sub di,si

 push dword #0x04c11db7
 call crc32

 seg es
 mov [0],eax ; save that crc


; restore the registers and return

 pop es
 popa
 ret

; end biosdata.S
# 3321 "second.S" 2
# 3336 "second.S"
! Put tokens into keyboard buffer
! "kbd=" 다음에 오는 값을 처리한다. 이 값은 16진수 키코드이며 콤마(,)로 구분된다. 이 값은 bios 키보드 버퍼(41x)에 저장된다. 다음에 키보드 인터럽트를 통해 키입력을 받아올때 이 값이 읽혀진다.
putkbd: add si,#4 ; skip over "kbd="
 push es
 xor ax,ax ; set ES to zero
 mov es,ax ; es=0
pknext: lodsb ; get next byte
 or al,al ; NUL ?
 jz pkdone ; yes -> done ; NUL이면 종료
 cmp al,#32 ; blank ? ; 공백이어도 종료
 jne pkrd ; no -> read scan code
pkdone: dec si ; return last character
 pop es ; done
 ret
pkrd: xor cx,cx ; clear accumulator
pkrdlp: cmp al,#97 ; lower case character ?
 jb pknol ; no -> go on
 sub al,#32 ; make upper case ; 대문자화
pknol: sub al,#48 ; normalize ; 0x30 ='0'
 cmp al,#10 ; >"9" ?
 jb pkok ; no -> okay ; 10보다 작으면 ok. -> pkok
 cmp al,#17 ; <"A" ?
 jb pksyn ; yes -> syntax error ; 10이상 A미만이면 에러출력후 정리하고 리턴
 sub al,#7 ; adjust ; 16진수화
 cmp al,#16 ; >"F" ?
 jae pksyn ; yes -> syntax error ; 16진수 초과시 역시 에러출력후 종료
pkok: shl cx,1 ; shift CX
 jc pksyn ; carry means trouble
 shl cx,1
 jc pksyn
 shl cx,1
 jc pksyn
 shl cx,1 ; 4비트 shift (16진수 한글자)
 jc pksyn ; 도중에 에러가 있으면 에러출력후 리턴
 add cl,al ; put in lowest nibble
 lodsb ; get next byte
 or al,al ; NUL ?
 jz pkend ; yes -> at end ; 끝(Null)이면 키보드 버퍼에 저장하고 리턴
 cmp al,#32 ; space ?
 je pkend ; yes -> at end ; space도 마찬가지
 cmp al,#44 ; comma ?
 je pkmore ; yes -> end of token ; 콤마면 저장하고 다음 값을 계속 읽는다.
 jmp pkrdlp ; token continues ; 
pksyn: mov bx,#msg_pks ; complain
 call say
pkfls: lodsb ; flush to end of option
 or al,al
 jz pkdone
 cmp al,#32
 je pkdone ; 다음 파싱할것(NUL,SP)까지 si를 증가한후 리턴한다. 
 jmp pkfls
pkend: call pkput ; store token
 jmp pkdone ; ... and return
pkmore: call pkput ; store token
 jmp pknext ; handle next token
pkput: seg es ; get buffer pointer
 mov bx,[KBEND] ; 0x41c ==tail
 mov dx,bx
 add dx,#2 ; increment it
 cmp dx,#KBHIGH ; (wrap around end) ; KBHIGH==0x3E. 키보드버퍼의 끝은 0x3D다. 0x3E보다 작아야한다.
 jb pknadj
 mov dx,#KBLOW ; 초과하면 버퍼의 시작(KBLOW==0x1E)
pknadj: seg es ; buffer full ?
 cmp dx,[KBBEG] ; 0x41a head. 가장 오래된 키를 가지고 있다.
 je pkfull ; yes -> error ; 이 값이 tail과 같으면 현재 키버퍼에 키가 없는걸 나타낸다. 꽉찼으면 에러출력, 비우고 리턴
 seg es ; store scan code
 mov (bx+0x400),cx ; 키 코드 저장
 seg es ; store new pointer
 mov [KBEND],dx ; tail을 증가시킨다.
 ret ; done
pkfull: mov bx,#msg_pkf ; complain ; 꽉찼다고 출력
 call say
 pop ax ; discard return address
 jmp pkfls ; abort

! Set VGA mode
! lilo는 normal(80x25), extended(ext:80x50), ask(부팅시 입력),숫자입력 모드를 지원한다.
setvga: add si,#4 ; skip over "vga="
 push si ; save SI
 mov bx,#vgatab ; scan VGA table
svgatb: pop si ; get pointer to option value ; si는 vga 다음값
 push si
 mov cx,(bx) ; get VGA code
 or cx,cx ; at end ?
 jz vganum ; yes -> must be numeric ; 끝이면(매치되는 문자열이 없으면) 숫자입력
 inc bx ; compare the strings ; vga코드는 건너뛴다.
 inc bx
vgacmp: lodsb
 call upcase ; (case-insensitive)
 mov ah,(bx)
 inc bx
 or ah,ah ; at end ?
 jnz vgamore ; no -> go on ; VGA table entry의 문자열 끝이 아니면 vgamore로 계속된다.
 or al,al ; at end of line ?
 jz vgafnd ; yes -> found it ; command line 끝이면 리턴
 cmp al,#32 ; space ?
 je vgafnd ; yes -> found it ; vga=xxx처리중 공백을 만났으면 리턴
 jmp svgatb ; try next entry otherwise ; VGA table의 한 entry이면 다음 entry를 읽는다.
vgamore: cmp al,ah
 je vgacmp ; equal -> next character ; command line의 vga=<xxxx>와 VGA table의 문자열값이 같으면 계속 비교
vgaskp: mov al,(bx) ; skip to end of reference string
 inc bx
 or al,al
 jnz vgaskp ; 문자열이 다르다. 이 entry는 넘긴다.
 jmp svgatb ; try next entry ; 다음 entry 검사
vgafnd: pop ax ; drop SI

vgaput: dec si ; read last character again
vgaput1: mov vgaovr,cx ; set VGA mode
 clc ; okay, done
 ret

vganum: pop si ; get SI
! SI에서 문자열을 읽어서 바이너리 값으로 변환하고 결과는 cx에 들어간다.
 call strtoul
 jc vgaerr ; 변환 에러
 mov cx,ax
 or dx,dx
 jnz vgaerr ; 결과값이 ax를 초과하면 에러 출력
 jmp vgaput1 ; 변환된 값을 vgaovr에 넣고 리턴한다.
# 3476 "second.S"
vgaerr: mov bx,#msg_v ; display an error message
 call say

 xor eax,eax
 mov dword ptr [hma],eax ; vga에러면 hma=0

 stc ; return an error
 ret

vgatab:

 .word 0xfffd
 .ascii "ASK"
 .byte 0
 .word 0xfffe
 .ascii "EXTENDED"
 .byte 0
 .word 0xfffe
 .ascii "EXT"
 .byte 0
 .word 0xffff
 .ascii "NORMAL"
 .byte 0

 .word 0


! get numeric string suffixed with "KkMmGg"
! updates SI
! M,G등을 바꿔준다. 기본단위는 kilo
get_K:
 push cx ; save CX

 call strtoull ; get number in DX:AX ; 문자열을 바이너리 숫자로 변환
 jc gmthis2 ; signal conversion error ; 에러 & 리턴

 mov bl,(si) ; get next character
 or bl,#0x20 ; convert to lower case
 cmp bl,#0x6b ; 'K' or 'k' ?
 je gmthis ; yes -> do not change ; 노 에러 & 리턴

 mov cx,#20 ; divide or multiply by 2^20
 cmp bl,#0x67 ; 'G' or 'g' ?
 je gmmul

 mov cx,#10 ; divide or multiply by 2^10
 cmp bl,#0x6d ; 'M' or 'm' ?
 je gmmul

! no Suffix
 dec si ; will increment later

gmdivl:
 shr eax,cl ; shift by CL ; 언급이 없으면 byte라 kilo에서 10번 >>
 jmp gmthis ; done
gmmul:
gmmull:
 shl eax,1 ; shift by 1 each time
 jc gmvbig ; very big if overflow
 loop gmmull ; ten times ; 단위만큼 shift해서 올린다.

! exit with no error

gmthis:
 inc si
 clc ; signal no error
gmthis2:
 mov bl,(si) ; next character to BL
# 3554 "second.S"
 pop cx ; restore register
 ret

gmvbig:
 mov eax,#0x38000000/1024 ; high memory max
 jmp gmthis


! Set memory limit
! 메모리 상한선을 파싱해서 hma에 세팅하고 커널 파라미터에도 복사한다. 이미 세팅되어 있다면 복사하지 않는다.
getmem:
 push si ; save SI for copying
 add si,#4 ; advance to number? ; "mem=" 다음 문자
 call get_K ; bl=다음값 ; 숫자를 분석해서 단위까지 계산한다. eax에 size가 들어간다.
 jc gmcopy ; error, just copy it

 cmp bl,#0x40 ; is it '@'
 jne gm22
! <size>@<start> format (2.4 kernels)
 push eax ; save size ; 값 저장
 inc si ; skip '@' ; @면 다음 숫자를 한번 더 해석한다.
 call get_K ; eax=start
 pop edx ; restore size
 jc memerr
 cmp eax,#1024 ; start : 1meg ; start가 1M보다 크면 복사
 ja gmcopy ; just copy if above
 add eax,edx ; EAX = hma/1024
 cmp eax,#2048 ; high : 2meg ; size+start가 2048이하면 복사
 jbe gmcopy
gm22: ; start가 1024 이하면서 size+start는 2048보다 크다.
 or bl,#0x20
 cmp bl,#0x20 ; NUL or SPACE



 jne gmcopy ; allow <size>#<start> and <size>$<start>

 cmp dword ptr [hma],#0 ; set already?
 jne gmnocopy ; 이미 값이 세팅됐으면 복사안함
 mov dword ptr [hma],eax ; set it
gmcopy: pop si ; kernel쪽 파라미터에 복사
gmret: ret
gmnocopy: pop bx ; 파싱한mem=xxx부분을 복사하지 않는다.
 ret


memerr:
 mov bx,#msg_me ; numeric conversion error
 call say
 br restrt

strtoull: ; numeric conversion to EAX
 call strtoul
 push dx
 push ax
 pop eax
 ret
! 10진수 or 16진수 ascii 문자열을 binary값으로 변환. 에러나면 carry=1로 리턴한다. 결과값은 dx와 ax에 저장된다.
strtoul:
 xor ax,ax
 xor dx,dx
 mov cx,#10 ; default radix is decimal ; 10의 기수는 10
 cmp byte ptr (si),#0x39
 ja s2lbad ; error if > '9'
 cmp byte ptr (si),#0x30 ; == '0'?
 jb s2lbad ; error if < '0'
 jne s2lnext ; 1~9면 다음으로 넘어간다.
 inc si ; 0으로 시작하면 다음글자
 dec cx
 dec cx ; assume octal : CX = 8 ; 0으로 시작하면 8진수

 cmp byte ptr (si),#0x58 ; == 'X'?
 je s2lhex
 cmp byte ptr (si),#0x78 ; == 'x'?
 jne s2lnext ; 그냥 0이면 8진수로 s2lnext로 넘어간다.
s2lhex: add cx,cx ; it is hexadecimal ; 0x or 0X면 16진수. 기수=16
 inc si ; x 넘김
s2lnext:
 xor bx,bx
 mov bl,(si) ; get next character

 or bl,#0x20 ; convert to lower case ; 0x20을 켜면 무조건 소문자 a(0x41)->A(0x61)
 sub bl,#0x30 ; - '0'
 jb s2ldone ; 0x30보다 작은 수면 기호나 특수문자(NUL,space 포함). 변환완료
 cmp bl,cl ; compare to radix
 jb s2lmul ; 기수보다 작으면 패스
 add bl,#0x30-0x61+10 ; 기수보다 크다. 16진수면서 A-F다. 더해서 10-15로 변환
 cmp bl,cl ; compare to radix
 jnb s2ldone ; F가 넘어가는 문자면 변환종료(carry=0)
s2lmul:
 push dx ; save high order
 mul cx ; multiply by radix ; ax에 기수를 곱해 한자리 올린다.
 add ax,bx ; 맞는 자리에 수를 더한다.
 adc dx,#0 ; carry possible only in radix 10
 pop bx
 push dx
 xchg ax,bx ; 하위 자리는 bx에 상위는 ax에 들어간다.
 mul cx ; 상위 자릿수에 기수를 곱한다.
 or dx,dx
 jnz s2lbad ; 상위자리까지 초과했으면 error
 pop dx
 add dx,ax ; 상위 자리끼리 더한다.
 jc s2lbad ; 넘치면 error
 xchg ax,bx ; 아랫자리 복구
 inc si
 jmp s2lnext ; 다음글자를 읽는다.

s2lbad:
 stc
 ret

s2ldone:
 clc
 ret


; find_image
; if there is something on the command line
; return the image number it selects
;
; enter with:
; nothing
; exit with:
; If nothing selected:
; Carry Clear
; AX==0
; If an image is selected:
; (fuzzy selection or exact selection)
; Carry SET
; AX==#image
; BX==pointer to descriptor
! IMAGES_numerator = SECTOR_SIZE_asm*MAX_DESCR_SECS_asm - 4 - 1
! IMAGES = IMAGES_numerator / id_size ; id_size==54??
; side effect:
; The selected image is hi-lited if the menu is displayed
; n번째 디스크립터 포인터를 push/pop해서 부팅할 곳 기억. ax에는 디스크립터 넘버. bx에 부팅할 디스크립터를 리턴한다.
find_image:
 push cx
 push si
 push di

 mov cx,#IMAGES ; test all names ; 이미지가 들어갈수 있는 최대 숫자 (카운터)
 mov si,#DESCR0 ; 디스크립터 영역
 xor bx,bx ; clear BX
 push si

fi_nextn:
 mov di,#cmdline
 test byte ptr (si),#0xFF ; null descriptor at end ; 디스크립터 끝이면 리턴
 jz fi_nomore

fi_nextc:
 mov al,(si) ; get next character in descr
 inc si

 call upcase

 mov ah,al
 mov al,(di) ; get next char in cmdline
 inc di

 call upcase
! ah=디스크립터 이름 al=cmdline 커널이름
 or al,al ; NUL in command line
 je fi_pmat
 cmp al,#32 ; SPACE in command line
 jne fi_cmp

; have partial match, set BX conditionally
fi_pmat: ; cmdline 이름이 NUL 혹은 공백
 or ah,ah ; NUL in descriptor name
 jz fi_found ; EXACT match found ; 디스크립터의 이름도 끝이면 찾았다.
; 부분 일치할때 처리
 test byte ptr par2_flag2,#4 ; (22.7) ; FLAG2_UNATTENDED
 jnz fi_skipn ; no partial match if unattended ; unattended가 켜있으면 패스

 or bx,bx
 jnz fi_skipn ; already set ; 디스크립터 포인터가 세팅돼있으면 패스
 pop bx
 push bx ; 부분 일치하는 부분을 기억한다.
 jmp fi_skipn ; go to next

fi_cmp:
 cmp al,ah ; character equal ?
 je fi_nextc ; compare next character

; advance to next descriptor
fi_skipn:
 pop si
 add si,#id_size ; test next name ; 다음 디스크립터 포인터를 push
 push si
 loop fi_nextn

fi_nomore:
 pop si
 or bx,bx ; fuzzy match?
 jnz fi_fuzzy






 xor ax,ax ; clears the carry
fi_exit:
 pop di
 pop si
 pop cx
 ret

fi_found:
 pop bx ; BX is matched descriptor
fi_fuzzy:
 mov ax,bx
 sub ax,#DESCR0
 mov cl,#id_size
 div cl
 cbw ; ax=n번째
# 3786 "second.S"
 stc
 jmp fi_exit
# 3835 "second.S"
! Some messages

msg_p: .ascii "boot: "
 .byte 0

msg_l: .ascii "Loading "
 .byte 0

msg_bc: .ascii "BIOS data check "
 .byte 0

msg_s: .ascii "successful\n"
 .byte 0

msg_by: .ascii "bypassed\n"
 .byte 0

msg_re: .byte 10
 .ascii "Error 0x"
 .byte 0

msg_nf: .ascii "No such image. [Tab] shows a list."
 .byte 10,0

msg_time:
 .ascii "O - Timestamp mismatch\n"
 .byte 0

msg_chkerr:
 .ascii "O - Descriptor checksum error\n"
 .byte 0

msg_chkkey:
 .ascii "O - Keytable read/checksum error\n"
 .byte 0

msg_confl:
 .ascii "Kernel and Initrd memory conflict\n"
 .byte 0

msg_sigerr:
 .ascii "O - Signature not found\n"
 .byte 0

msg_me: .byte 10
 .ascii "vga/mem=  requires a numeric value"
 .byte 10,0

msg_wrerr: .ascii "\nMap file write; BIOS error code = 0x"
 .byte 0

msg_wrerr3: .ascii "\nMap file: WRITE PROTECT\n"
 .byte 0


msg_mem: .ascii "EBDA is big; kernel setup stack overlaps LILO second stage"
 .byte 10,0



msg_vmwarn:
 .ascii "WARNING:  Booting in Virtual environment\n"
 .ascii "Do you wish to continue? [y/n] "
 .byte 0
# 3967 "second.S"
msg_int: .byte 10
 .ascii "*Interrupted*"
 .byte 10,0

msg_eof: .byte 10
 .ascii "Unexpected EOF"
 .byte 10,0

msg_pw: .ascii "Password: "
 .byte 0

msg_pf: .ascii "Sorry."
 .byte 10,0

msg_v: .byte 10
 .ascii "Valid vga values are ASK, NORMAL, EXTENDED or a "
 .ascii "decimal number."
 .byte 10,0

msg_pks: .byte 10
 .ascii "Invalid hexadecimal number. - Ignoring remaining items."
 .byte 10,0

msg_pkf: .byte 10
 .ascii "Keyboard buffer is full. - Ignoring remaining items."
 .byte 10,0

msg_bm: .byte 10
 .ascii "Block move error 0x"
 .byte 0






msg_rd4M: .byte 10
 .ascii "Initial ramdisk loads below 4Mb; "
 .ascii "kernel overwrite is possible."
 .byte 10,0


ospc: .ascii "O"




 .ascii " "
 .ascii "23"
 .ascii "."
 .ascii "2"
 .ascii ""

 .byte 32,0

bs: .byte 8,32,8,0
# 4059 "second.S"
 .even

init_dx: .word 0




hinib: .byte 0 ; hi-nibble of address
tempal: .byte 0
moff: .word 0 ; map offset
map: .word Map ; map to use ; 덤프결과 0x2800

cntdown: .word 0 ; count-down
timeout: .byte 0 ; timed out

dolock: .byte 0

int1c_l: .word 0 ; old timer interrupt
int1c_h: .word 0

old_del: .word 0 ; delay before booting

nodfl: .word 0 ; action if no defaults are present


slbase: .word 0 ; serial port base (or 0 if unused)
break: .byte 0 ; break received flag


usrinpm: .byte 0xff

cmdbeg: .word 0
options: .word 0

rdbeg: .word 0,0 ; RAM dist begin address (dword)

rdszl: .word 0 ; RAM disk size
rdszh: .word 0

vgaovr: .word 0 ; VGA mode overwrite


hma: .word 0,0 ; Highest Memory Address
memmap: .word 0,0,0,0,0,0,0,0,0,0

dskprm: .word 0,0,0,0,0,0

 .even ; control alignment from here down
acmdbeg: .ascii "auto "
mcmdbeg: .ascii "BOOT_IMAGE"
prechr: .byte 32 ; space: guard double blank supression
    ; equal sign: variable assignment
cmdline: .byte 0 ; Map을 사용하지 않을때는 Map의 공간을 cmdline으로 사용할 수 있다. 최소 512bytes(섹터크기)이상. lkwbuf와 lkcbuf는 Map과 Dflcmd와 겹칠수 있다.




 .org *+4
theend:
; 커맨드라인 영역으로 1024bytes 할당?
lkwbuf = cmdline+CL_LENGTH+2 ; this is a word ; CL_LENGTH=512
lkcbuf = lkwbuf+2
theend2 = lkcbuf+CL_LENGTH ; lkcbuf is 256

the_end1 = theend+511	! theend + 1섹터 (bytes) ; 코드크기/섹터크기 결과값에 +1해준다.
theends = the_end1/512	! theends = theend의 섹터크기 ; 코드끝까지 섹터수를 구한다.
 .org theends*512-4
 .long X
 .align 512		! 섹터단위 정렬 ; max_secondary는 코드&데이터부분 다음 섹터에 위치한다.
max_secondary:	
# 4140 "second.S"	! max_seocndary = theend 다음 섹터
Map = max_secondary + 512	! Map = max_secondary + 1 (sector) menu일때 20번째 섹터 ; 섹터주소로 이루어진 배열(map)을 읽을 용도로 쓰인다.
Dflcmd = Map + 512			! Dflcmd = max_secondary + 2 ; Default command line 버퍼. 다른 섹터 로드용으로도 쓰인다.
Map2 = Dflcmd				! Map2 = max_secondary + 2
Keytable = Dflcmd + 512 	! Keytable = max_secondary + 3 ; 맨 처음 읽는 Keytable 섹터. 256바이트 이후엔 Menutable영역이 있다.
Descr = Keytable + 512		! Descr = max_secondary + 4
ParmBSS = Descr + 512*MAX_DESCR_SECS_asm ! ParmBSS = max_secondary + 7



BSSstart = ParmBSS			! BSSstart = max_secondary + 7
Parmline = BSSstart + 512	! Parmline = max_secondary + 8





!************************************
! BSS data:
! moved from volume.S
! BSS크기 = 512 bytes

 .org BSSstart
# 1 "volume.S" 1
# 25 "volume.S"
vtab = *
 .org *+MAX_BIOS_DEVICES_asm*4 ; volume IDs indexed by ; *+16*4
     ; REAL bios device code

rtab = *
 .org *+MAX_BIOS_DEVICES_asm*4 ; raid offsets indexed the same

; devmap은 로지컬 device code를 피지컬 코드로 변환하는 정보를 가지고 있다.
devmap = *
 .org *+16*2+4 ; device code map
     ; logical -> physical  
     ; (lo-byte::hi-byte)
# 4163 "second.S" 2

# 1 "shs3.S" 1
; shs3.S
# 10 "shs3.S"
;
;;; group dgroup _data

;;; segment _data public align=16 class=data

; global _shsInfo
# 25 "shs3.S"
_shsInfo = *
shs_digest = *
  .org *+5*4
shs_count = *
  .org *+2*4

Wshs = *
  .org *+16*4
# 335 "shs3.S"
; end shs3.S
# 4165 "second.S" 2


 .align 512
BSSend = *
BSSsize = BSSend-BSSstart	! 1섹터


Dataend = Parmline + 512	! Dataend = max_secondary + 9 sector (4608 bytes)

 .org max_secondary






DESCR0 = Descr
