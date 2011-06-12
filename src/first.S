#if 0
;  first.S  -  LILO first stage boot loader with LBA32 support */
Copyright 1992-1998 Werner Almesberger.
Copyright 1999-2005 John Coffman.
All rights reserved.

Licensed under the terms contained in the file 'COPYING' in the 
source directory.

#endif
#define LILO_ASM
#include "lilo.h"
get common.s		/* as86 "include" will bypass the CPP */

#define DEBUG 0

#if VERSION_MINOR>=50
# define DEBUG_NEW

# undef VIDEO_ENABLE
# define VIDEO_ENABLE 3

# define VALIDATE !DEBUG	/* adds 0Dh bytes */
# define SECOND_CHECK !DEBUG	/* adds  5h bytes */
# define CYL1023 DEBUG		/* subs  8h bytes */
# define GEOMETRIC !DEBUG	/* adds  1h byte  */

# if DEBUG
#  define DEBUG_LARGE
# endif
#else

# define VALIDATE 1		/* adds 0Dh bytes */
# define SECOND_CHECK 1		/* adds  5h bytes */
# define CYL1023 0		/* subs  8h bytes */
# define GEOMETRIC 1		/* adds  1h byte  */
#endif


!  VIDEO_ENABLE	for those systems that disable the video on boot
!	= 0		first stage does not enable video
!	= 1		use get vid mode/set vid mode to enable
!	= 2		use VGA enable call to enable video
!			(cannot use, as code gets too big)
!	= 3		use direct mode set (mode 3, CGA, EGA, VGA)
!	= 7		use direct mode set (mode 7, MDA)
!
#ifndef VIDEO_ENABLE
# if VALIDATE==0
#  define VIDEO_ENABLE 2
# else
#  define VIDEO_ENABLE 2
# endif
#endif

! do not change the following -- it must correspond to the code in bsect.c
#define RELOCATABLE -1


	.text

	.globl	_main

	.org	0

zero:
_main:	cli			! NT 4 blows up if this is missing
	jmp	start

stage:	.byte	STAGE_FIRST
	.org	4
reloc:
#if RELOCATABLE
	.word	theend-zero	! size of the code & params
#else
	.word	0		! no size indication
#endif
	.org	6

! Boot device parameters. They are set by the installer.

sig:	.ascii	"LILO"
vers:	.word	VERSION
mapstamp: .long 0		! map timestamp

length	=  *-sig		! for the stage 1 vs stage 2 comparison

raid:	.long	0		! raid sector offset
tstamp:	.long	0		! timestamp
map_serial_no:	.long	0	! volume S/N containing map file
prompt:	.word	0		! indicates whether to always enter prompt
				! contains many other flags, too

d_dev:	.byte	0x80		! map file device code
d_flag:	.byte	0		! disk addressing flags
d_addr:	.long	0		! disk addr of second stage index sector


edd_packet	=  0
;;;	.word	16		! size of packet
;;;	.word	1		! count of bytes to read

edd_addr	=  4
;;;	.word	map2		! where to read
;;;	.word	*-*		! segment where to read

edd_d_addr	=  8
;;;	.long	1		! low address or CX,DX (geometric)
				! start at sector 1 for search in geo mode

;;;	.long	0		! hi address

#if 0
!  These locations are referenced as EX_OFF 
!					(they used to be at CODE_START_1)
ext_si:	.word	0		! external interface
ext_es:	.word	0		! these locations are referenced in second.S
ext_bx:	.word	0		! do not disturb the ordering
ext_dl:	.byte	0		! second.S will check this magic number
ext_dh:	.byte	0		! not referenced, but must align stack
ext_stack:
#endif
	
/***************************************************/
!	The following instruction MUST be
!	first instruction after the CLI/JMP short
!	at the start of the file; otherwise
!	the boot sector relocation fails.
!
start:
	mov	ax,#BOOTSEG	! use DS,ES,SS = 0x07C0
/***************************************************/

	mov	ss,ax
	mov	sp,#SETUP_STACKSIZE	! set the stack for First Stage / 스택주소 = 0x07c0:800(2048) = 0x8400 부터 아래로 쌓인다
	sti			! now it is safe	/ 점프와 스택설정까지 인터럽트 금지

	push	dx		! set ext_dl (and ext_dh, which is not used) / first.S가 mbr이면 dl = drive number
	push	bx		! WATCH the order of pushes
	push	es		! set ext_es
	push	si		! set ext_si

#ifdef DEBUG_NEW	/* 안들어감 */
	push	ds
	push	es	! just not enough space with debug turned on
#endif

#define JRC_DS_EQ_SS
	cld			! do not forget to do this !!!  스트링 명령을 위한 df 초기화
	mov	ds,ax		! address data area = 0x7c0
	xor	bp,bp		! shorted addressing , bp = 0

#if VIDEO_ENABLE
! a BIOS has been found where the video interrupt (0x10) trashes DX
!   so, we had better be very paranoid about DX
! 그래픽 모드를 3 (80x25 16color)으로 세팅한다. (int 0x10 ah=0, al=3)

# if VIDEO_ENABLE==2
	pusha			! protect DX
# endif
# if VIDEO_ENABLE > 2
	mov	ax,#VIDEO_ENABLE	!	VIDEO_ENABLE = 3
# elif VIDEO_ENABLE==1		/* 1	-	use get vid mode/set vid mode to enable */
	mov	ah,#15		! get video mode
	int	0x10		! AH = number of columns / AL = mode / BH = display page
	cbw				! 부호 확장 ax = al 
# else  /* VIDEO_ENABLE==2 */	! = 2		use VGA enable call to enable video
	mov	ax,#0x1200	! enable video (VGA) / video refresh - AL= 0 - enable refresh
	mov	bl,#0x36	! (probably a nop on EGA or MDA)
# endif
	int	0x10		! video call / 기본은 ah=0 (set video mode) al=3 (80x25 16color)
# if VIDEO_ENABLE==2
	popa			! restore DX
# endif
#endif

#if (VIDEO_ENABLE&1) == 0	/* 그래픽모드 최하위비트가 0일때 줄넘김 */
	mov	al,#0x0d	! gimme a CR ...
	call	display
; the suspect call for trashing DX on one BIOS:
	mov	al,#0x0a	! ... an LF ...
	call	display
#endif

#if defined(DEBUG_NEW)
	mov	ah,dl
	call	bout		! code in AH / 디버그시 4비트씩 출력
#endif
	mov	al,#0x4c	! ... an 'L' ...
	call	display ! 화면에 L을 출력

lagain:
	pusha			! preserve all the registers for restart

	push	ds	! 0x7c0
	pop	es		! use buffer at end of boot sector / es = ds

	cmp	dl,#EX_DL_MAG	! possible boot command line (chain.S) / 0xfe
	jne	boot_in_dl	! dl(drive number)이 magic number(0xfe)와 같은지 확인한다. 일반적으로 쓰이지 않음
	mov	dl,dh		! code passed in DH instead
boot_in_dl:

	mov	bx,#map2	! buffer for volume search / mbr의 끝주소 map2=512
	mov	dh,[d_dev](bp)  ! map device to DH / ss=0x7c0, bp=0 [ss:bp+d_dev]=0x80

#if VALIDATE /* 플로피인지 하드인지 체크 / 부팅딘 하드 제한개수(16 or 2)를 넘어가면 분기 */
	mov	ax,dx		! copy device code to AL 
	and	ah,#0x80	! AH = 0x80 / AL=drive number
	xor	al,ah		! hi-bits must be the same
	js	use_installed
	cmp	al,#MAX_BIOS_DEVICES	! limit the device code = 16
	jae	use_installed	! jump if DL is not valid
#endif

! map is on boot device for RAID1, and if so marked; viz.,
!	7번째 비트가 꺼져있으면 use_boot로 분기
	test	byte ptr [prompt](bp),#FLAG_MAP_ON_BOOT ! =64(40)=01000000
	jnz	use_boot	! as passed in from BIOS or MBR loader

use_installed:
	mov	dl,dh		! device code to DL / 0x80
	mov	esi,[map_serial_no](bp)	! to search for
	or	esi,esi
	jz	done_search

	push	dx		! save flags

	mov	ah,#8		! get number of hard disks
	mov	dl,#0x80	! drive number 0,1..=floopy 0x80,0x81..=hard drive
	push	bx		! paranoia / bx에 drive type이 retune됨.
	int	0x13
	pop	bx
	jc	error

	movzx	cx,dl		! extend to word in CX

#if GEOMETRIC
	mov	dx,#0x80-1	! device 80, flags=0
#else
	mov	dx,#LBA32_FLAG*256+0x80-1	! device 80, flags=LBA32
#endif

vagain:
	inc	dx
	xor	eax,eax
#if GEOMETRIC
	inc	ax		! geometric addressing
#endif
	call	disk_read	! read 

	cmp	esi,[PART_TABLE_OFFSET-6](bx) ! 446 바이트 (MBR 데이터영역)
	je	vol_found
	loop	vagain

	pop	dx		! restore specified BIOS code
				! AX and DX are identical at this point

vol_found:
		! uses value in DX, stack may have extra value

done_search:	
use_boot:
	push	bx		! save map2 for later

	mov	dh,[d_flag](bp)	! get device flags to DH
	mov	si,#d_addr
	call	pread		! increments BX

	mov	ah,#0x99	! possible error code
	cmp	dword (bx-4),#EX_MAG_HL	! "LILO"
	jne	error

	pop	si		! point at #map2

#if 1
	push	#SETUP_STACKSIZE/16 + BOOTSEG + SECTOR_SIZE/16*2
	pop	es
#else
	mov	ax,ds		! get segment
	add	ax,#SETUP_STACKSIZE/16    !   + SECTOR_SIZE/16*2
	mov	es,ax
#endif
	xor	bx,bx

sload:
	call	pread		! read using map at DS:SI
	jnz	sload		! into memory at ES:BX (auto increment)

! Verify second stage loader signature
	
	mov	si,#sig		! pointer to signature area
	mov	di,si
	mov	cx,#length	! number of bytes to compare
	mov	ah,#0x9A	! possible error code
	repe
	  cmpsb			! check Signature 1 & 2
	jne	error	! check Signature 2

#if SECOND_CHECK
/* it would be nice to re-incorporate this check */
	mov	al,#STAGE_SECOND	! do not touch AH (error code)
	scasb
	jne	error
#endif

! Start the second stage loader     DS=location of Params

	push	es		! segment of second stage
	push	bp		! BP==0

	mov	al,#0x49	! display an 'I'
	call	display

	retf			! Jump to ES:BP




disk_error2:
	mov	ah,#0x40	; signal seek error

! no return from error
error:	! 에러가 뜨면 스택을 줄이고 다시 시도 (lagain)

#ifndef LCF_NO1STDIAG
        mov	al,#32          ! display a space
	call	display0

	call	bout
#endif

#ifndef DEBUG_LARGE
	dec	byte [zero](bp)		!  CLI == 0xFA == 250
	jz	zzz

#ifndef DEBUG_NEW
	mov	sp,#SETUP_STACKSIZE-4*2-8*2	! set the stack for First Stage
#else
	mov	sp,#SETUP_STACKSIZE-4*2-2*2-8*2	! set the stack for First Stage
#endif
	popa				! restore registers for restart
	jmp	near lagain		! redo from start
#endif


zzz:
#ifndef DEBUG_NEW
	hlt
#endif
	jmp	zzz		! spin; wait for Ctrl-Alt-Del




! packet read routine

disk_read:
#ifndef JRC_DS_EQ_SS
	push	ds
#endif
	pusha

#ifndef JRC_DS_EQ_SS
	push	ss
	pop	ds
#endif

	push	bp		! BP==0  ! 확장된 디스크 읽기를 위해 패킷을 스택을 통해 si로 넘겨준다
	push	bp		! BP==0

	push	eax		! low order disk address
#ifdef DEBUG_LARGE	/* 디버그가 켜져있으면 d_flag와 하드디스크 넘버가 출력된다 */
	xchg	ax,dx
	call	wout
	xchg	ax,dx
	call	dout		! print out disk address
#endif
	push	es		! memory segment ES
	push	bx		! memory offset BX
	push	#1		! sector count
	push	#16		! size of packet = 16 bytes
	mov	si,sp		! address of packet  DS:SI

	push	bx

	test	dh,#LINEAR_FLAG|LBA32_FLAG	! d_flag에서 LINEAR나 LBA가 켜져있는지 확인한다.
	jz	disk_geometric ! 둘다 꺼져있으면 전통적인 CHS방식으로 점프 (ax=0x201)
	! Linear만 켜져있으면 CHS방식으로 돌아간다
	test	dh,#LBA32_FLAG	! 둘중 하나 혹은 둘다 켜져있는데 LBA가 꺼져있다면 LINEAR다
	jz	disk_convert	; it must be LINEAR

	mov	bx,#0x55AA	;magic number
	mov	ah,#0x41
	int	0x13
	jc	disk_convert
	cmp	bx,#0xAA55	;changed?
	jne	disk_convert
	test	cl,#EDD_PACKET	;EDD packet calls supported
	jnz	disk_edd

disk_convert:
	push	dx
	push	es		! protect on floppies
	mov	ah,#8		! get geometry
	int	0x13		! 디스크 정보를 얻어온다.
	pop	es
disk_error3:			! transfer through on CF=1
	jc	error		! disk_error12

#if !CYL1023 
	push	cx		! cl 상위 2비트 & ch 8비트 = cylinder 
	shr	cl,#6		;;;;
	xchg	cl,ch	   ;CX is max cylinder number / 온전한 10bit cylinder
	mov	di,cx	   ;DI saves it
	pop	cx 
#endif 
	shr	dx,#8		! dl = dh의 헤더 개수를 shift 
	xchg	ax,dx		;AX <- DX
	inc	ax		;AX is number of heads (256 allowed)
	! di는 실린더, ax는 헤더+1
; compensate for Davide BIOS bug
	dec	cx		; 1..63 -> 0..62;  0->63
	and	cx,#0x003f	;CX is number of sectors = 63 / 섹터만 남긴다
	inc	cx		; allow Sectors==0 to mean 64

	mul	cx		; kills DX also / 섹터(cx) * 헤더(ax)
	xchg	ax,bx	   	;save in BX
				; second.S가 위치한 섹터 위치( [si+8], [si+10], eax)
	mov	ax,[edd_d_addr](si)	;low part of address / logical block address
	mov	dx,[edd_d_addr+2](si)	;hi part of address

	cmp	dx,bx
	jae	disk_error2	;prevent division error

	div	bx		;AX is cyl, DX is head/sect
#if CYL1023
	cmp	ax,#1023
#else
	cmp	ax,di
#endif
	ja	disk_error2	;cyl is too big

	shl	ah,#6		; save hi 2 bits
	xchg	al,ah
	xchg	ax,dx
	div	cl		;AH = sec-1, AL = head
	or	dl,ah	   ;form Cyl/Sec
	mov	cx,dx
	inc	cx		; sector is 1 based

	pop	dx		! restore device code
	mov	dh,al		! set head#
	jmp	disk_read2



disk_edd:
	mov	ah,#0x42
disk_int13:		! int 13h를 두번 시도해본다
	pop	bx

	mov	bp,#5	! bp를 카운터로 두번 시도후 실패시 disk_error3으로 간다.
disk_retry:
	pusha
	int	0x13
#if 0
	stc
	mov	ah,#0x9C
#endif
	jnc	disk_okay

	dec	bp		! does not alter CF, already 0
	jz	disk_error3	! go to "jc" with CF=1 & ZF=1

	xor	ax,ax		! reset the disk controller
	int	0x13
	popa			! reset AX,BX,CX,DX,SI
	dec	bp		! fix up BP count
	jmp	disk_retry


disk_geometric:		! CHS방식으로 1섹터를 읽어온다.
	push	eax
	pop	cx
	pop	ax
	mov	dh,ah

disk_read2:
	mov	ax,#0x201	;read, count of 1
	jmp	disk_int13


disk_okay:	! 디스크 읽기가 성공적이면 스택을 복원
; the pusha block is implicitly removed below
;;;	mov	(si+2*16-1),ah	! set error code
;   the error code is never checked
	lea	sp,(si+16)	! do not touch carry; 
	popa
#ifndef JRC_DS_EQ_SS
	pop	ds
#endif
	ret



! Pointer Read -- read using pointer in DS:SI

pread:
	lodsd			! get address
	or	eax,eax
	jz	done
	add	eax,[raid](bp)	! reloc is 0 on non-raid  레이드면 eax(second stage sector address)에 더한다.
	call	disk_read	

	add	bh,#SECTOR_SIZE/256    	! next sector
done:
	ret




#if !defined(LCF_NO1STDIAG) || defined(DEBUG_NEW)
! ah를 4비트씩 2번 출력 + ASCII 변환후 3A-3F를 빼고 변환한다.
bout:	rol     ax,#4		! bring hi-nibble to position
	call	nout
	rol     ax,#4		! bring lo-nibble to position
nout:	and	al,#0x0F	! display one nibble
	daa			! shorter conversion routine
	add	al,#0xF0  
	adc	al,#0x40	! is now a hex char 0..9A..F / ASCII 0-9, A-F만 출력된다.
#endif
; display - write byte in AL to console
;	preserves all register contents
; 
display0:
#ifndef LCF_NOVGA
display:
#endif
	pusha		! make sure no register is changed / 레지스터들을 저장
	mov	bx,#7		!  BH=0, BL=07
	mov	ah,#14	! AL의 한글자를 출력
	int	0x10	
	popa		! restore all the registers
#ifdef LCF_NOVGA
display:
#endif
	ret

#ifdef DEBUG_LARGE

! eax 더블워드를 출력
dout:	pushad
	ror	eax,#16
	call	wout
	ror	eax,#16
	call	wout
	mov	al,#0x20	! space
	call	display
	popad
	ret

! ax레지스터를 화면에 출력
wout:	push	ax
	call	bout	! put out AH
	pop	ax
	push	ax
	xchg	al,ah
	call	bout	! put out AL (now in AH)
	pop	ax
	ret
#endif

theend:

!
!   If 'first' loads as the MBR, then there must be space for the partition
!   table.  If 'first' loads as the boot record of some partition, then
!   the space reserved below is not used.  But we must reserve the area
!   as a hedge against the first case.
!
!
	.org	MAX_BOOT_SIZE	!
	.word	0,0,0,0		! space for NT, DRDOS, and LiLO volume S/N

!	.org	0x1be		! spot for the partition table
p_table:
	.blkb	16
	.blkb	16
	.blkb	16
	.blkb	16
#ifdef FIRST
	.org	*-2
	.long	FIRST		! boot block check
#else
	.word	0xAA55		! boot block signature
#endif

! Better be exactly 0x200

map2	equ	*		! addressed as ES:[map2]
