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

start: cld ; only CLD in the code; there is no STD

 push ds
 pop fs ; address parameters from here






 seg cs
 mov [init_dx],dx ; save DX passed in from first.S

 int 0x12 ; get memory available




 shl ax,#6 ; convert to paragraphs
 sub ax,#Dataend/16
 mov es,ax ; destination address
 push cs
 pop ds
 xor si,si
 xor di,di
 xor ax,ax
 mov cx,#max_secondary/2 ; count of words to move
 rep
   movsw
 add di,#BSSstart-max_secondary
 mov cx,#BSSsize/2
 rep
   stosw
 push es
 push #continue
 retf ; branch to continue address
continue:
# 231 "second.S"
 call serial_setup ; set up the COM port, if any



 mov cx,#32 ; drain type-ahead buffer ?
drkbd: mov ah,#1 ; is a key pressed ?
 int 0x16
 jz comcom ; no -> done
 xor ah,ah ; get the key
 int 0x16
 loop drkbd


comcom:

 mov al,#0x4c ; display an 'L'
 call display
 push #0 ; get pointer to disk parameter table in DS:SI
 pop ds
 lds si,[0x78] ; 0x78 = 4*0x1E

 cmp byte ptr (si+4),#9 ; okay ?
 ja dskok ; yes -> do not patch

 push cs ; get pointer to new area in ES:DI
 pop es
 mov di,#dskprm
 mov cx,#6 ; copy 12 bytes
 rep
 movsw
 seg es ; patch number of sectors

 mov byte ptr (di-8),#18



 push #0
 pop ds
 cli ; paranoia
 mov [0x78],#dskprm
 mov [0x7a],es
 sti
dskok:

 seg cs ; clear the break flag
 mov byte ptr break,#0

 call instto ; get timer interrupt

;;; jmp restrt ; get going

! Restart here after a boot error

restrt: mov bx,cs ; adjust segment registers
 mov ds,bx
 mov es,bx

 sub bx,#63*0x20+0x20 ; segment for setup code &
      ; bootsect
 mov cx,#INITSEG
 cmp bx,cx
 jbe restrt1
 mov bx,cx ; BX is the smaller segment #
restrt1:
 mov word ptr [map],#Map
 mov [initseg],bx ; set up INITSEG (was 0x9000)
 lea cx,(bx+0x20)
 mov [setupseg],cx ; set up SETUPSEG (was 0x9020)
 mov cx,cs
 sub cx,bx ; subtract [initseg]
 shl cx,#4 ; get stack size
 mov ss,bx ; must lock with move to SP below
 mov sp,cx ; data on the stack)
# 392 "second.S"
 cmp dword [sig],#0x4f4c494c ; "LILO"
 jne crshbrn2
 cmp dword [mcmdbeg+6],#0x4547414d ; "MAGE" from BOOT_IMAGE
 jne crshbrn2
 cmp BYTE [stage],#2

 jne crshbrn
 cmp WORD [version],#256*2 +23

crshbrn2: jne crshbrn
 mov [cmdbeg],#acmdbeg ; probably unattended boot

 mov di,#devmap ; place to store the device map

 mov ah,[init_dx] ; AH is physical device

 seg fs
 mov al,[par1_secondary+0+SSDIFF] ; map device logical






 cmp ah,al
 je end_tt






 stosw ; set up the translation from map -> boot
end_tt:
 xor ax,ax
 stosw

ldsc:

 seg fs
 mov eax,[par1_mapstamp]

 cmp eax,[par2_mapstamp]
 jne timeerr

 call kt_read ; read the Keytable

 call build_vol_tab

 mov bx,#Descr
 mov si,#Keytable+256+mt_descr
descr_more:
 lodsw
 xchg cx,ax
 lodsw
 xchg dx,ax
 lodsb
 call cread
 jc near fdnok ; error -> retry
 add bh,#2 ; increment address
 cmp si,#Keytable+256+mt_descr+sa_size*MAX_DESCR_SECS_asm
 jb descr_more

 mov si,#Descr ; compute a checksum of the descriptor table
 mov di,#512*3 -4

 push dword #0x04c11db7
 call crc32
 add di,si
 cmp eax,dword (di)
 jz nochkerr


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
 call vmtest
 jnc virtual_done
 mov di,#DESCR0 ; point at first descriptor
vir_loop:
 test byte ptr [id_name](di),#0xFF ; test for NUL name
 jz virtual_done
 test word ptr [id_flags](di),#512
 jz vir_skip

 push di
 lea si,[id_size](di)
vir_loop1:
 mov cx,#id_size
 rep
    movsb
 test byte ptr [id_name](di),#0xFF
 jnz vir_loop1

 pop di
 jmp vir_loop

vir_skip:
 add di,#id_size
 jmp vir_loop

virtual_done:


; remove those items that have "nokbdisable", if nokeyboard boot
 call kbtest
 jc kbd_done
 mov di,#DESCR0 ; point at first descriptor
kbd_loop:
 test byte ptr [id_name](di),#0xFF ; test for NUL name
 jz kbd_done
 test word ptr [id_flags](di),#0x8000
 jz kbd_skip

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
 mov bx,#Keytable+256
 mov al,(bx+mt_flag)

 seg fs ; get possible 16
 or byte ptr [par1_prompt+SSDIFF],al




 mov bx,#Dflcmd
;
;seg fs
 mov cx,mt_dflcmd+Keytable+256 ;DFCMD_OFF
;seg fs
 mov dx,mt_dflcmd+2+Keytable+256
;seg fs
 mov al,mt_dflcmd+4+Keytable+256
;
 call cread
 jc fdnok ; error -> retry
 mov bx,#Dflcmd
 cmp word ptr (bx),#0xf4f2 ; okay ?
 jne bdcmag ; no -> do not write

 mov word ptr (bx),#0x6b6d ; erase the magic number
 call cmd_write ; write out the command line
# 598 "second.S"
 jmp dokay ; continue
bdcmag: mov byte ptr (bx+2),#0 ; disable the command line
 jmp dokay ; go on
fdnok:





 br ldsc ; retry

! List all known boot images

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

dokay: mov bx,#ospc ; display 'O '
 call say

 xor eax,eax
 mov dword ptr [hma],eax

 mov ospc,al ; disable the message
 mov word ptr vgaovr,#0x8000 ; disable VGA override
;;
;; seg fs
 xchg ax,par2_delay ;DSC_OFF-8+SSDIFF
;;
 or old_del,ax ; remember delay
 mov nodfl,#iloop ; interactive prompt if falling through

 call kbtest ; keyboard present?

 jc kbd_present
; no PC keyboard on the system, is there a serial port in use?
 cmp byte ptr [par2_port],#0
 jz skip_prompt ; no serial keyboard either



kbd_present:


 seg fs ; enter boot prompt ?
 test byte ptr par1_prompt+SSDIFF,#1 ;DSC_OFF+15+SSDIFF,#0

 jnz extp ; yes -> check for external parameters
skip_prompt:
 mov nodfl,#bfirst ; boot first image if falling through
 call waitsh ; wait for a shifting key
 jc iloop ; key pressed -> enter interactive mode

! Check for external parameters

extp:
 seg fs ; external parameters ?
 cmp byte ptr SETUP_STACKSIZE-8+SSDIFF+6,#0xfe

 jne noex ; no -> go on

 seg fs
 mov bl,SETUP_STACKSIZE-8+SSDIFF+7 ; get drive
 seg fs ; clear flag
 mov byte ptr SETUP_STACKSIZE-8+SSDIFF+6,bl ; clear flag
 seg fs ; load the signature pointer
 les bx,SETUP_STACKSIZE-8+SSDIFF

 seg es
 cmp dword ptr (bx),#0x4f4c494c ; "LILO"
 jne noex ; no -> go on

 seg fs
 mov si,SETUP_STACKSIZE-8+SSDIFF+4 ; pointer to the command line

 seg es
 cmp byte ptr (si),#0 ; empty ?
 je iloop ; yes -> enter interactive mode
 jmp niloop ; enter non-interactive mode

! No external parameters after timeout -> boot first image

noex: push cs ; restore ES
 pop es
 mov si,#Dflcmd+2 ; default command line ?
 cmp byte ptr (si),#0
 jne niloop ; yes -> use it
 mov ax,nodfl ; no idea how to tell as86 to do jmp (addr) :-(
 jmp ax ; fall through


; Command input processor

iloop:





;;
;; seg fs ; message disabled ?
 cmp word ptr par2_msg_len,#0 ;MSG_OFF+SSDIFF,#0
;;
 je nomsg ; yes -> skip this
 call crlf
;
;seg fs ; load the message file
 mov cx,mt_msg+Keytable+256 ;MSG_OFF+SSDIFF+2
;seg fs
 mov dx,mt_msg+2+Keytable+256
;seg fs
 mov al,mt_msg+4+Keytable+256
;
 mov bx,[map]
 call sread
 call loadfile

 xor bx,bx ; set the terminating NUL and disable further
    ; messages
 xchg bx,par2_msg_len ;MSG_OFF+SSDIFF

 push #SYSSEG
 pop ds
 mov byte ptr (bx),#0
 xor bx,bx ; display the message
 call say

 push cs ; restore segment registers
 pop ds


nomsg: push cs ; disable external parameters
 pop es

 mov cmdbeg,#acmdbeg ; probably unattended boot
 mov si,#usrinpm ; interactive mode
niloop: ; ES may point to external params
 mov bx,#msg_p ; display boot prompt
 call say
 mov bx,#cmdline ; move cursor to the end of the line
clend: mov al,(bx)
 or al,al ; at end ?
 jz cledne ; yes -> go on
 push bx ; display the character
 call display
 pop bx
 inc bx ; next one
 jne clend
cledne: mov byte ptr prechr,#32 ; character before command line is a space

! Input loop

input: seg es ; interactive mode ?
 cmp byte ptr (si),#0xff
 je kbinp ; yes -> get keyboard input
 seg es ; get non-interactive input
 mov al,(si)
 inc si
 jmp gotinp ; go on

tolist:



 br list ; ...

kbinp:
 mov cx,#brto ; get a key
 call getkey
# 812 "second.S"
noNull: or al,al ; keyboard NUL input?
 je input ; yes, skip Keyboard NUL
; stored command line NUL is handled differently

gotinp: cmp al,#9 ; TAB ?
 je tolist ; yes -> list images
 cmp al,#63 ; "?" ?
 je tolist ; yes -> list images
 or al,al ; NUL ?
 je nul ; yes -> go on
 cmp al,#8 ; BS ?
 je todelch ; yes -> erase one character
 cmp al,#13 ; CR ?
 je cr ; yes -> go on
 cmp al,#127 ; DEL ?
 je todelch ; yes -> erase one character
 ja input ; non-printable -> ignore it
 cmp al,#21 ; ^U ?
 je todell ; yes -> erase the line
 cmp al,#24 ; ^X ?
 je todell ; yes -> erase the line
 cmp al,#32 ; ignore non-printable characters except space
 jb input
 ja noblnk ; no space -> go on
 cmp (bx-1),al ; second space in a row ?
 je input ; yes -> ignore it
noblnk: cmp bx,#cmdline+CL_LENGTH-1 ; at end of buffer ?
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
sklp: test word ptr (di+id_flags),#4096 ; single-key entry ?
 jz sknext ; no -> try next
 mov al,(di) ; get first character

 call upcase ; convert to upper case

 cmp al,ah ; do we have a match ?
 jne sknext ; no -> try next
 cmp byte ptr (di+1),#0 ; at end ?
 je cr ; yes -> run it
sknext: add di,#id_size ; test next entry
 loop sklp ; next one
 br input ; done -> get more input

todelch:br delch ; ...
todell: br delline ; ...

! End of input, process the command line

nul: push bx ; automatic boot - wait for timeout
 mov ax,old_del
 call waitsh
 pop bx
 jnc crnul ; no key pressed -> continue
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
 jnz cpsav ; no -> go on

 cmp bx,#cmdline ; empty line ?
 je notrspc ; yes -> boot first image
 cmp byte ptr (bx-1),#32 ; trailing space ?
 jne notrspc ; no -> go on
 dec bx ; remove the space
 mov byte ptr (bx),al
notrspc:mov si,#cmdline ; scan the command line for "vga=", "kbd=",
 mov di,si ; "lock" or "mem="
chkvga:


vsktnbd:
 cmp dword ptr (si),#0x64626f6e ; "nobd"
 jne vsktv
 cmp byte (si+4),#32 ; terminated with SP or NUL?
 jnbe vsktv

 seg fs ; enter boot prompt ?
 or byte ptr par1_prompt+SSDIFF,#16 ; suppress BIOS data collection

 jmp vskwd ; skip word

vsktv:
 cmp dword ptr (si),#0x3d616776 ; "vga="
 jne vsktk
 call setvga ; set VGA mode
 jc near iloop ; error -> get next command
 jmp vskdb ; proceed by discarding last blank
vsktk:
 cmp dword ptr (si),#0x3d64626b ; "kbd="
 jne vsktl
 call putkbd ; pre-load keyboard buffer
 jmp vskdb ; proceed by discarding last blank
vsktl:
 cmp dword ptr (si),#0x6b636f6c ; "lock"
 jne vsktm
 cmp byte (si+4),#32 ; space?
 jnbe vsktm
 mov byte ptr dolock,#1 ; enable locking
vskwd: add si,#4 ; skip word
vskdb: dec di ; discard last blank
 jmp vsknb ; continue
vsktm:



 cmp dword ptr (si),#0x3d6d656d ; "mem="

 jne vsknb
 call getmem ; get the user-provided memory limit
vsknb:
 lodsb ; copy one byte
 stosb
 cmp al,#32 ; space ?
 je chkvga ; yes -> look for options again
 or al,al ; at end ?
 jnz vsknb ; no -> go on
 call crlf ; write CR/LF
 cmp di,#cmdline+1 ; empty line ?
emptyl: je bfirst ; yes -> boot first image
 jmp bcmd ; boot the specified image

! Find the boot image and start it

bcmd:
 call find_image
 jc near boot ; eureka, it was found

 mov bx,#msg_nf ; not found -> display a message
 call say
 br iloop ; get more input

! Delete one character

delch: cmp bx,#cmdline ; at the beginning ?
 je toinput ; yes -> do nothing
 dec bx ; move the pointer
 push bx ; display[B BS,SPC,BS
 mov bx,#bs
 call say
# 1020 "second.S"
 pop bx
toinput:br input ; go on

! Delete the entire line

delline:

 cmp bx,#cmdline ; done ?
 je toinput ; yes -> go on
 push bx ; display BS,SPC,BS
 mov bx,#bs
 call say
 pop bx
 dec bx ; move the pointer
 jmp delline ; next one







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
 mov ax,#2048
brfrst0v:
 call kbtest
 jc brfrst0k
 mov ax,#0x4000
brfrst0k:

 mov cx,#IMAGES
brfrst1: test word ptr (bx+id_flags),ax
 jnz brfrst3
 add bx,#id_size
 loop brfrst1

 mov bx,#DESCR0 ; restore default
brfrst3:
# 1104 "second.S"
 mov si,bx ; copy the name to the command line
 mov di,#cmdline
bfcpl: lodsb ; copy one character
 mov (di),al
 inc di
 or al,al ; NUL ?
 jnz bfcpl ; no -> next one

! Boot the image BX points to (with password check)

boot:
 mov word par2_timeout,#0xffff ; kill timeout (22.7)
 mov si,#cmdline ; locate start of options
locopt: lodsb
 or al,al ; NUL ?
 je optfnd ; yes -> no options
 cmp al,#32 ; space ?
 jne locopt ; no -> continue searching
 cmp byte ptr (si),#0 ; followed by NUL ?
 jne optfnd ; no -> go on
 mov byte ptr (si-1),#0 ; discard trailing space
optfnd: dec si ; adjust pointer
 mov options,si ; store pointer for later use
# 1141 "second.S"
 test word ptr [id_flags](bx),#1024
 jz boot9
 call vmtest ; 'vmwarn' there, is it actually virt. boot
 jnc boot9
; VMWARN set, and is virtual boot, so issue comment
;;
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

 test byte ptr (bx+id_flags),#128 ; use a password
 jz toboot ; no -> boot
 test byte ptr (bx+id_flags),#2 ; restricted ?
 jz dopw ; no -> get the password
 cmp byte ptr (si),#0 ; are there any options ?
 jne dopw ; yes -> password required
toboot: br doboot ; ...
dopw:

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

doboot: mov byte ptr prechr,#61 ; switch to equal sign
 push bx ; save image descr
 mov bx,#msg_l ; say hi
 call say
 pop bx ; display the image name
 push bx
 call say
 pop si

 push si
 add si,#id_start ; form address

; Now load the kernel sectors
 xor ax,ax
 mov word ptr (gdt+0x1b),ax ; set GDT to "load low"
 mov byte ptr (gdt+0x1f),al
 mov moff,ax ; map is not loaded yet

 lodsw ; address of the first map sector
 xchg cx,ax
 lodsw
 xchg dx,ax
        lodsb

 push si ; save SI







 mov bx,[map] ; load the first map sector
 call sread




 mov bx,#Dflcmd ; load the default command line
;
;seg fs
 mov cx,mt_dflcmd+Keytable+256
;seg fs
 mov dx,mt_dflcmd+2+Keytable+256
;seg fs
 mov al,mt_dflcmd+4+Keytable+256
;
 call cread
 push word ptr (Dflcmd) ; push magic number
 mov bx,#Dflcmd ; load the fallback sector
 call load1
 pop ax ; valid magic number ?

 cmp ax,#0xf4f2
 je dclok ; yes -> can write
 cmp ax,#0x6b6d
 jne nofbck ; invalid -> must not write
dclok: mov bx,#Dflcmd ; fallback data present ?
 cmp word ptr (bx),#0xf4f2
 jne nofbck ; no -> go on
 call cmd_write ; write out the command line
nofbck:





 mov bx,#Dflcmd ; load the options sector
 call load1
 mov si,cmdbeg ; copy non-options part of command line
 mov di,#Parmline
 mov cx,#CL_LENGTH-1 ; max number of characters to copy

cpnocl:

 cmp si,#cmdline



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
 jz cpdname9
 stosb
 dec cx
 jmp cpdname
cpdname9:

 mov si,#Dflcmd ; constant options ?
 cmp byte ptr (si),#0
 je nocopt ; no -> go on
 mov al,#32 ; add a space
 stosb
 dec cx ; count character
 jz cpovfl
cpcodsp:



 cmp dword ptr (si),#0x3d6d656d ; "mem="

 jne cpnotmem
 call getmem ; get the user-provided memory limit
cpnotmem:
 lodsb ; fetch next byte
 cmp al,#32 ; space ?
 je cpcodsp ; yes -> discard it
cpcolp: or al,al ; NUL ?
 jz cpcodn ; yes -> done
 stosb ; store byte
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



cpdone:
# 1586 "second.S"
 mov es,[initseg] ; load the original boot sector
 xor bx,bx ; load now
 call load1
 pop si ; restore SI
 lodsw ; get flags bit map
 xchg bx,ax ; move to BX
 lodsw ; copy parameters ... VGA mode ... (done)
 cmp word ptr vgaovr,#0x8000 ; VGA mode not overridden on
    ; command line ?
 je vganorm ; no -> go on
 mov ax,vgaovr ; use that value
 jmp vgaset
vganorm:test bx,#1
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
 mov es,[setupseg] ; load the setup codes


 mov ax,cx ; number of sectors to AX
 shl ax,#5 ; convert to paragraphs (9-4)
 mov bx,es
 add bx,ax
 add bx,#STACK>>4 ; allow for stack space in paragraphs
 mov ax,cs ;
 cmp bx,ax
 jbe enough_mem
 mov bx,#msg_mem ; we are very short on memory
 call say

enough_mem:


 xor bx,bx ; other operating system)
lsloop: push cx
 call loadopt
 pop cx
 loop lsloop




 pop bx ; get flags
 test bx,#8 ; "modern" kernel ?
 jz loadlow ; no -> avoid all patching and such
 seg es ; set loader version
 mov byte ptr (16),#0x02

 test bx,#256 ; load kernel high
 jz nohigh

 seg es
 mov ax,word ptr (20+1) ; get start address 00 1000 00
 mov (gdt+0x1b),ax
 seg es
 mov al,byte ptr (20+3) ; get hi-byte of address
 mov (gdt+0x1f),al
nohigh:

 seg es ; version >= 1 ?
 cmp word ptr (6),#0x200
 jbe noheap ; no -> do not patch heap
 mov ax,cs
 sub ax,[initseg] ; find no. of paragraphs available
 shl ax,4
 add ax,#Parmline-SETUP_STACKSIZE-BOOTSECT
 seg es
 mov word ptr (36),ax
 seg es ; patch flags
 or byte ptr (17),#0x80
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






 call lfile ; load the system ...
 jmp launch2 ; ... and run it
loadlow:






 call loadfile ; load the system
launch2:

 jmp launch ; go !

loadfile:
 push #SYSSEG ; load a file at SYSSEG:0000
 pop es
 xor bx,bx
lfile: call load
 jmp lfile

! Load one sector. Issue an error at EOF.

load1: call loadit ; load the sector
 mov bx,#msg_eof ; we only get here at EOF
 call say
 br restrt

loadit: call load ; load it
 pop ax ; drop return address of load1
 ret

! Load one sector. Start the system at EOF.

loadopt:call loadit ; load the sector
 jmp launch ; go

! Load one sequence of sectors. Leave outer function at EOF.

load: push es ; save ES:BX
 push bx
lfetch: mov si,moff ; get map offset
 mov bx,[map]
 mov cx,(bx+si) ; get address
 mov dx,(bx+si+2)
 mov al,(bx+si+4)
 or cx,cx ; at EOF ?
 jnz noteof ; no -> go on
 or dx,dx
 jnz noteof
 pop bx ; restore ES:BX
 pop es
 pop ax ; pop return address
 ret ; return to outer function
noteof: add si,#sa_size ; increment pointer
 mov moff,si
 cmp si,#512 - sa_size + 1 ; page end ?
 jb near doload

 mov moff,#0 ; reset pointer
 push cs ; adjust ES
 pop es

        mov bl,hinib ; this might get clobbered
        push bx ; so save it
 mov bx,[map] ; load map page
 call sread
        pop ax ; restore the hi-nibble
 mov hinib,al ;

 mov al,#0x2e ; print a dot
 call display
 jmp lfetch ; try again

! Start the kernel

launch:
; terminate emulation if CD boot
 test byte ptr [par2_flag2],#2 ; a CD?
 jz not_el_torito
 mov si,#Map ; empty command packet
 mov byte ptr (si),#0x13 ; size of command packet
 mov ax,#0x4b00 ; terminate emulation
;;;; mov dl,al ; DL is 0
 mov dl,[init_dx] ; terminate boot device
 int 0x13
not_el_torito:



 call crlf ; display a CRLF


 mov dx,#0x3f2 ; stop the floppy motor
 xor ax,ax
 out dx,al ; outb
 mov dl,al
 int 0x13 ; reset the FDC (AH=0)

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
 jz vpaus1

 call remto ; free timer interrupt

 push es ; is initseg
 pop ds ; DS = 0x9000 (initseg)







 add sp,#Parmline ; increase stack size over this code
if ~*&1 ; align to an odd memory location
 nop
endif
 jmpi 0,SETUPSEG ; segment part is a variable
setupseg = *-2 ; setupseg is filled in now
initseg: .word INITSEG


! Load one sector (called from load)

doload: pop bx ; restore ES:BX
 pop es

! Load a sequence of sectors, possibly moving into "high memory" (> 1 MB)
! afterwards.

xread: push ax ; ES == 0 ?
 mov ax,es
 or ax,ax
 pop ax
 jz rdhigh ; yes -> read into high memory



 jmp sread

rdhigh: push bx ; okay - DS:BX points to GDT in this case
 mov bx,#LOADSEG ; adjust ES:BX
 mov es,bx
 xor bx,bx
 call sread ; load the sector(s)
        mov tempal,al
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

sread: push bx ; save registers
 push cx
 push dx
 call cread
 mov di,ax ; save AL return count
 jc rerror ; error -> complain
 pop dx ; restore registers
 pop cx
rokay: pop bx
        shl ax,8 ; convert sectors to bytes
        add ah,ah
 jc dowrap ; loaded an entire segment -> advance ES
 add bx,ax ; move BX
 jnc nowrap ; same segment -> go on
dowrap: mov ax,es ; move ES
 add ax,#0x1000
 mov es,ax
nowrap:
 mov ax,di ; restore the block count in AL
aret: ret ; done

! Read error - try a second time and give up if that fails too

rerror:
 push ax
 mov bx,#msg_re ; say something
reset: call say
 pop ax ; display the error code
 mov al,ah
 call bout
 call crlf ; a CR/LF
 mov moff,#0 ; restore initial state
# 2167 "second.S"
 br restrt

! Convert character in AL to upper case

upcase: cmp al,#0x61 ; lower case character ? ('a')
 jb nolower ; no -> go on
 cmp al,#0x7a ; 'z'
 ja nolower
 sub al,#0x20 ; convert to upper case
nolower:ret ; done

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
 mov al,#13 ; display a CRLF
 call display
 mov al,#10
nonl:
 cmp al,#12 ; ^L ?
 jne nocls ; no -> go on







 push bx
 mov ah,#0xf ; clear the local screen
 int 0x10
 xor ah,ah
 int 0x10
 pop bx

tosnext: jmp snext ; next character
nocls: call display ; display, tty-style
snext:
 inc bx ; next one
! fall into say ; process next character

! Display a NUL-terminated string on the console

say: mov al,(bx) ; get byte
 or al,al ; NUL ?
 jnz say_loop ; not the end
 ret


! Display CR/LF

crlf: mov al,#13 ; CR
 call display
 mov al,#10 ; LF
;;; jmp display
; fall into display

! Display one character on the console

display:
 push bx ; save BX


 call serdisp
# 2353 "second.S"
;;; xor bh,bh ; display on screen
 mov bx,#7 ; set color for 0x9dd476ec interface
 mov ah,#14
 int 0x10

dispret:
 pop bx ; restore BX
 ret



serdisp:push dx ; wait for space in the send buffer
 seg cs
 mov dx,slbase
 or dx,dx
 jz serret
 add dx,#5
 push ax
serwait:in al,dx
 test al,#0x10 ; break -> set break flag
 jz nobrk
 seg cs
 mov byte ptr break,#1
nobrk: test al,#0x20 ; ready to send ?
 jz serwait ; no -> wait
 sub dx,#5 ; send the character
 pop ax
 out dx,al
serret: pop dx ; done
 ret


! Get a key (CX = timeout exit)

getkey: ;;
;; seg fs ; set the timeout
 mov ax,par2_timeout ;DSC_OFF-10+SSDIFF
;;
 call setto
gwtkey: mov ah,#1 ; is a key pressed ?
 int 0x16
 jnz gotkey ; yes -> get it

 mov dx,slbase ; using a serial port ?
 or dx,dx
 jz gnokey ; no -> wait
 add dx,#5 ; character ready ?
 in al,dx
 test al,#1
 jz gnokey ; no -> wait
 sub dx,#5 ; get it
 in al,dx
 and al,#0x7f ; strip 8th bit
 jnz gotch ; ignore NULs

gnokey:
# 2417 "second.S"
 test byte ptr timeout,#1 ; timed out ?
 jz gwtkey ; no -> wait
 pop ax ; discard return address
 jmp cx ; jump to timeout handler
gotkey: xor ah,ah ; read a key
 int 0x16
 push bx ; keyboard translation (preserve BX)
 mov bx,#Keytable
 xlatb
 pop bx
gotch:


 seg fs ; always enter prompt ?
 test byte ptr par1_prompt+SSDIFF,#1

 jz noosht ; yes -> do not disable timeout

; disable timeout
 test byte ptr par2_flag2,#4
 jnz nocancel
 mov word ptr par2_timeout,#0xffff
nocancel:
noosht:
 ret ; done

! Shift wait loop (AX = timeout, returns CY set if interrupred)

waitsh: call setto ; set timeout
actlp: mov ah,#2 ; get shift keys
 int 0x16



 and al,#0x5f ; anything set ? (except NumLock)

 jnz shpress ; yes -> return with CY set
; 22.7.1 begin
 mov ah,#1 ; get status
 int 0x16
 jnz shpress ; key pressed
; 22.7.1 end

 mov dx,slbase ; using a serial port ?
 or dx,dx
 jz acnosp ; no -> go on
 cmp byte ptr break,#0 ; break received ?
 jnz shpress ; yes -> return with CY set
 add dx,#5 ; check for pending break
 in al,dx
 test al,#0x10
 jnz shpress ; break received -> return with CY set

acnosp: test byte ptr timeout,#1 ; timed out ?
 jz actlp ; no -> wait
 clc ; clear carry
 ret ; done
shpress:stc ; set carry
 ret ; done

! Timeout handling

instto: push ds ; install the timeout handler
 push #0
 pop ds

 cli ; no interrupts
 mov eax,[0x1c*4] ; get the old vector
 seg cs
 mov [int1c_l],eax ; save H & L parts
 mov [0x1c*4],#tick ; install new vector
 mov [0x1c*4+2],cs
 sti ; done
 pop ds
 ret

remto: push es ; remove the interrupt handler
 push #0
 pop es

 mov eax,[int1c_l] ; restore the old vector
 seg es
 mov [0x1c*4],eax ; **
 pop es
 ret

! AX = ticks, 0xffff = no timeout

setto: or ax,ax ; time out immediately ?
 jz toimmed ; yes -> do it
 cli ; set timeout value
 mov cntdown,ax
 mov byte ptr timeout,#0 ; clear timed-out flag
 sti ; done
 ret
toimmed:mov byte ptr timeout,#0xff ; set the timed-out flag
 ret ; done

tick: pushf ; save flags
 seg cs ; no timeout ?
 cmp word ptr cntdown,#0xffff
 je notzro ; yes -> go on
 seg cs ; decrement counter
 dec word ptr cntdown
 jnz notzro ; not zero -> go on
 seg cs ; set timeout flag
 mov byte ptr timeout,#0xff
notzro:
 seg cs
 push dword [int1c_l]
 iret ; continue with old interrupt

kt_set:
;;
;; seg fs ; load the keyboard translation table
 mov cx,par2_keytab ;MSG_OFF+SSDIFF+7
;; seg fs
 mov dx,par2_keytab+2 ;MSG_OFF+SSDIFF+9
;; seg fs
 mov al,par2_keytab+4 ;MSG_OFF+SSDIFF+11
;;
 mov bx,#Keytable
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
 call kt_set ; set for Keytable i/o
 call cread
 jc keyerr
 mov si,#Keytable ; compute a checksum of the keytable
 mov di,#512 - 8 ; skip the last 4+4 bytes
 push dword #0x04c11db7
 call crc32
 add di,si
 cmp eax,dword (di)
 jz nokeyerr

! Checksum error
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
 call map_device ; DL (logical) -> DL (physical)

cread_physical: ; same entry, device is not mapped

        test dl,#0x40|0x20
        jnz use_linear

        push ax ;save the count
 mov ah,#2 ;read command
        call dsk_do_rw ; int 0x13 with retries
        pop cx ;Carry Set means error on read
        mov al,cl ;count in AL, error code in AH
        ret

use_linear:
        mov ah,hinib ;will be zero for LINEAR
        xchg al,dh ;AX is possible address
 test dl,#0x20 ;test for LBA32/LINEAR *****
 jz lnread ;pure LINEAR *****
        test dl,#0x40
        jz lnread
        mov ah,dh ;former count is really hi-nibble
        mov hinib,ah
        mov dh,#1 ;set count to 1
lnread:
        xchg di,ax ;hi-address to DI
        mov al,dh ;count to AL

 test dl,#0x10 ; ******
 jz ln_do_read ; ******

 call translate ; in volume.S

ln_do_read:
        call lba_read
        mov al,cl ;count returned in AL, error code in AH
        ret ;Carry Set means error on read


; vmtest -- return Carry=1 if in virtual (VMware) mode
; return Carry=0 if in real mode
;
vmtest:

 pushad ; save all extended registers
 smsw ax
 rcr al,1 ; PE bit in AL to Carry
 jc vm_ret ; exit if virtual mode







;
; If no vmdefault, vmdisable, or vmwarn keywords were used, then we do not
; care about virtual mode. Do not touch the hardware, and always return
; Carry=0.
;
 test byte ptr [par2_flag2],#8 ; any vmXXX keywords?
 jz vm_ret ; TEST clears the carry, always
;
; VMware(R) test for virtual mode
;
 mov eax,#0x564D5868 ; EAX: in = 'VMXh' out = version
 xor ebx,ebx ; EBX: out = 'VMXh' under vmware
 mov edi,eax
 mov dx,#0x5658 ; DX: in = 'VX'
 mov ecx,#10 ; ECX: in = VMXGetVersion
 in eax,dx
 cmp ebx,edi ; test for vmware
 clc ; NOT vmware if Carry==0
 jne vm_ret ; not vmware

 inc eax ; carry is not affected by INC
 jz vm_ret ; invalid version number == 0xFFFFFFFF

vm_vir:
 stc ; signal virtual mode

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
;
 test byte ptr [par2_flag2],#16
 jz kbtest8
# 2828 "second.S"
 cli ; added 5/17/2006
 mov al,#0xee ; echo command
 out #0x60,al
wait_kbd_ctrl_ready:
 in al,#0x64
 and al,#0x01
 jz wait_kbd_ctrl_ready ; read status port while it is not ready
 in al,#0x60
 sti ; added 5/17/2006
 xor al,#0xee ; XOR clears the carry
 jne kbtest9
  ; if we got the same byte, the keyboard is attached







kbtest8:
 stc ; flag keyboard present
kbtest9:
 pop ax
 ret




; crc32 -- calculate CRC-32 checksum
;
; call:
; push dword #POLYNOMIAL
;
; ES:SI char string pointer
; DI count of characters
;
; call crc32
;
; CRC-32 is returned in EAX or DX:AX
; the arguments are popped from the stack
;
crc32:
  push bp
  mov bp,sp

  push si
  push di
  push bx
  push cx

  xor eax,eax ; initialize CRC
  dec eax ; EAX = 0xFFFFFFFF
  inc di
crc32a:
  dec di
  jz crc32d
  mov cx,#8 ; count 8 bits
  seg es
  mov bl,(si) ; get next character
  inc si
crc32b: shl bx,#1 ; get hi bit of char in BH
  shl eax,#1 ; shift hi bit out of CRC
  adc bh,#0 ; add carry to BH
  shr bh,#1 ; put bit in carry
  jnc crc32c ; skip the xor
  xor eax,(bp+4) ; xor in the polynomial
crc32c:
  loop crc32b ; loop back for 8 bits
  jmp crc32a

crc32d:
  not eax ; finialize CRC

  pop cx
  pop bx
  pop di
  pop si

  leave
  ret 4


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
;;
;; seg fs
 mov dx,par2_port ; use a COM port ?
   ; watch out, loads par2_ser_param
;;
 dec dl
 js nocom ; no -> go on
 xor ax,ax ; initialize the serial port
 xchg al,dh

 push ax
 push dx

;;; or al,#0x06 ; stop bits = 2, nbits = 7 or 8
    ; this OR is not needed yet (21.7)
 int 0x14 ; Communications Port INIT

 push #0x40
 pop ds

 pop bx ; was DX

 shl bx,#1
 mov dx,(bx) ; get the port address from the BIOS

 seg cs ; keep it
 mov slbase,dx

 pop bx ; special baud rate test -- was AX

 test bl,#0x04 ; stop bits == 2?
 cli ; do not disturb any code below
 jz stdbps ; standard BPS

 shr bx,#5 ; index divisor array
 seg cs
 mov bl,divisor(bx)

spcbps: ; CLI: do not disturb ...
 push dx ; save base address
 add dx,#3 ; enable divisor latch access
 in al,dx
 or al,#0x80
 out dx,al
 pop dx ; set new divisor
 push dx
 xchg ax,bx
 out dx,al
 inc dx
 mov al,ah
 out dx,al
 inc dx ; disable divisor latch access
 inc dx
 xchg ax,bx
 and al,#0x7f
 out dx,al
 pop dx ; restore base address

stdbps: ; CLI: redundant if fell in from above
 push dx
 add dx,#4 ; address Modem Control Reg.




 mov al,#3 ; turn on DTR and RTS

 out dx,al
 pop dx
 sti ; done

 mov cx,#32 ; drain the queue (if any)
drain: in al,dx
 loop drain
 add dx,#5 ; clear the status register
 in al,dx

    ; send "\r\nLI" to the serial port

 mov si,#serLI
 mov cx,#4
ser1: seg cs
 lodsb
 call serdisp
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
  jz no_lba ;linear will never use EDD calls


         cmp al,#127 ;test for LINEAR transfer too big
  ja no_lba ; for LBA mode (127 is max)
                push ax

                mov bx,#0x55AA ;magic number
                mov ah,#0x41 ;function call
                int 0x13

                pop ax

                jc no_lba
                cmp bx,#0xAA55 ;magic return
                jne no_lba
                test cl,#01 ;packet calls supported?
                jz no_lba


; LBA mode is to be used

lba_avail:
                pop bx
                pop cx


                pop ax
                push ax


                push ds ;save DS

  push dword #0 ; 0L is pushed
                push di ;LBA hi word
                push cx ; lo word
                push es ;ES:BX
                push bx

                push ax



                push #16 ;size of parameter area ;#
                           ;actually pushes a word
                mov si,sp ;DS:SI is param block pointer

                push ss
                pop ds ;DS:SI points at param block


                mov ax,#0x4200 ;read function -- must be AX
     ; as AL has meaning on WRITE
                call dsk_do_rw





                lea sp,word ptr (si+16) ;use lea so flags are not changed

                pop ds ;restore DS

                jmp lba_read_exit1



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
                push bp
                mov bp,#5 ;number of tries
dsk_do_int13a: pusha
                int 0x13
                jnc dsk_io_exit
                dec bp ;does not affect the carry
                jz dsk_io_exit
                xor ax,ax ;reset disk controllers
                int 0x13
                popa
                dec bp
                jmp dsk_do_int13a

dsk_io_exit: mov bp,sp ;do not touch any flags
                lea sp,(bp+16) ;an ADD would touch flags
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
;
build_vol_tab:
 pusha

 xor cx,cx ; depend on this being preserved
 xor dx,dx
 xchg [devmap],dx ; clear our First Stage mapping

 call is_prev_mapper ; is there a previous mapper
 jz bvt0

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
 mov di,#Keytable+256+mt_serial_no
 mov cx,#MAX_BIOS_DEVICES_asm
 xor eax,eax
 repe
   scasd ; scan for any serial nos in table
 je bvt90 ; if none, skip reading vol_ids
    ; as there will be no translations
; ****** 22.5.8


 xor cx,cx ; start at hard drive 0 (0x80)
 mov di,#vtab ; place to put IDs
bvt1:
 call read_vol_id ; get VolumeID in EAX
 stosd ; store in table
 or eax,eax ; test for zero
 jz bvt9 ; end, or no volume ID

; now see if there will be a translation
 push di
 push cx

; ****** 22.5.9
 mov cx,di ; 4*table count to CX
 mov di,#vtab
 sub cx,di ; 4*count
 shr cx,#2 ; table count
 dec cx
 jz bvt1.5 ; table empty
 repne ; repeat while no match
   scasd
 jne bvt1.5

 mov bx,#msg_dupl ; duplicate message
 call say

 call pause

 pop cx
 pop di

 mov dword (di-4),#0 ; zero the duplicated volumeID
 jmp bvt9 ; skip to next on duplication

bvt1.5:
; ****** 22.5.9
 mov si,#Keytable+256+mt_serial_no
 mov cx,#MAX_BIOS_DEVICES_asm
 mov di,si
bvt2: jcxz bvt7
 repne ; repeat while not matching
   scasd
 jne bvt7 ; jump if no match
# 153 "volume.S"
 lea dx,(di-4) ; DX is address of match
 sub dx,si ; DX is 4*index
 shr dx,#2 ; DX is input device #
 pop bx ; BX is real device #
 push bx
 cmp bx,dx
; ****** 22.5.9
;;; je bvt2 ; equal means no translation
 je bvt7 ; equal means no translation
; ****** 22.5.9
 mov dh,bl ;
 or dx,#0x8080 ; make into HD bios codes
# 173 "volume.S"
 push si
 mov bx,#devmap ; scan the device translation table
bvt4:
 mov si,(bx) ; get from(low):to(high) pair
 inc bx
 inc bx ; bump pointer by 2
 cmp si,dx ; duplicate?
 je bvt5

 or si,si ; not duplicate; at end?
 jnz bvt4

 mov (bx-2),dx ; put at end of table
 mov (bx),si ; and mark new end
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

 mov si,#Keytable+256+mt_raid_offset
 mov dx,[Keytable+256+mt_raid_dev_mask]
 xor bx,bx ; count thru devices
bvt91:
 xor eax,eax ; may store 0
 shr dx,#1 ; is it raid?
 jnc bvt92 ; not a raid device

 lodsd ; get raid offset
 push eax ; save value in stack

 mov eax,[Keytable+256+mt_serial_no](bx)
 mov di,#vtab ; physical table address
 mov cx,#MAX_BIOS_DEVICES_asm
 repne
   scasd ; scan for a match
 jne bvt_not_found ; the logical volume is not there
 lea di,(di-4-vtab) ; DI is 4*index into table
 mov cx,di
 shr cx,#2 ; make 0..15
 mov ax,#1
 shl ax,cl ; mask bit in right position
 or [rmask],ax
 pop dword ptr rtab(di) ; store RAID offset
 jmp bvt92
bvt_not_found:
 pop eax ; clean up the stack
bvt92:
 add bx,#4 ;
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

 jc rvi_9
 cmp cl,dl
 jae rvi_9

 mov dl,cl
 mov cx,#1
 mov bx,#Map
 or dl,#0x80
 mov dh,ch
; ****** 22.6.1



; ****** 22.6.1
 mov ax,#0x201 ; read
 call dsk_do_int13
 jc rvi_9

 seg es
   mov eax,(bx+0x1be -6) ; fetch return
 jmp rvi_exit
rvi_9:
 xor eax,eax
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
 push ax
 push bx
 mov si,#devmap ; point at translation table
 mov bl,dl
 and bl,#DEV_MASK_asm ; from device code in BL

; ****** 22.5.6
 seg cs
 mov ah,[init_dx] ; get boot device code
 test dl,#0x10
 jnz bios_tt_match ; it is RAID, go use the boot device code
; ***** 22.5.6

bios_tt_next:
 seg cs ; DS may be bad
   lodsw ; get from/to pair
 or ax,ax ; end of list?
 jz bios_tt_done
 cmp al,bl
 jne bios_tt_next
; got a match
bios_tt_match:
 and dl,#0xFF-DEV_MASK_asm ; save flags
 or dl,ah ; put on the TO device code
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
;
translate:
 push bp
 mov bp,sp

 cmp word [rmask],#0 ; any translate bits set?
 jnz trans_1

; this special cases the initial Keytable read, when no setup has been done

 seg fs
 add cx,par1_raid_offset+SSDIFF ; ***** RAID ******
 seg fs
 adc di,par1_raid_offset+2+SSDIFF ; ***** RAID ******

 jmp trans_ret

trans_1:
 push di
 push cx ; form dword (bp-4)

 mov di,dx ; DI gets full device code
 and di,#DEV_MASK_asm & 0x7F
# 571 "volume.S"
 shl di,#2 ; index into array

 mov cx,[rtab](di) ; get low relocation value
 mov di,[rtab+2](di) ; get high relocation value
# 590 "volume.S"
 add (bp-4),cx ; relocate
 adc (bp-4+2),di ; **

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
 seg es



   les di,[4*0x13] ; vector to int 0x13
 or di,di
 jnz is_p_no_mapper ; our mappers start at offset 0

 mov di,es
 cmp di,#0xA000 ; start of system reserved locations
 jae is_p_no_mapper
 cmp di,#0x0060 ; VERY conservative
 jb is_p_no_mapper

; first test for new mapper
 xor di,di
 mov cx,#new13_length
 mov si,#new13
 repe
   cmpsb
 jne is_p_try_old

; found new (v.22) mapper
 seg es
   mov di,[new13_drvmap_offset]




 jmp is_prev_ret

is_p_try_old:
 xor di,di
 mov cx,#new13_old_length
 mov si,#new13_old
 repe
   cmpsb
 jne is_p_no_mapper

; likely old (<=v.21) mapper
 seg es
   mov di,(di)
 cmp di,#new13_old_min_offs ; validate the range of values
 jb is_p_no_mapper
 cmp di,#new13_old_max_offs ; validate the range of values






 jbe is_prev_ret


is_p_no_mapper:
 xor di,di ; set DI = 0, ZF=1
is_prev_ret:
 or di,di ; set ZF by DI
 pop si
 pop cx
 ret



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

putkbd: add si,#4 ; skip over "kbd="
 push es
 xor ax,ax ; set ES to zero
 mov es,ax
pknext: lodsb ; get next byte
 or al,al ; NUL ?
 jz pkdone ; yes -> done
 cmp al,#32 ; blank ?
 jne pkrd ; no -> read scan code
pkdone: dec si ; return last character
 pop es ; done
 ret
pkrd: xor cx,cx ; clear accumulator
pkrdlp: cmp al,#97 ; lower case character ?
 jb pknol ; no -> go on
 sub al,#32 ; make upper case
pknol: sub al,#48 ; normalize
 cmp al,#10 ; >"9" ?
 jb pkok ; no -> okay
 cmp al,#17 ; <"A" ?
 jb pksyn ; yes -> syntax error
 sub al,#7 ; adjust
 cmp al,#16 ; >"F" ?
 jae pksyn ; yes -> syntax error
pkok: shl cx,1 ; shift CX
 jc pksyn ; carry means trouble
 shl cx,1
 jc pksyn
 shl cx,1
 jc pksyn
 shl cx,1
 jc pksyn
 add cl,al ; put in lowest nibble
 lodsb ; get next byte
 or al,al ; NUL ?
 jz pkend ; yes -> at end
 cmp al,#32 ; space ?
 je pkend ; yes -> at end
 cmp al,#44 ; comma ?
 je pkmore ; yes -> end of token
 jmp pkrdlp ; token continues
pksyn: mov bx,#msg_pks ; complain
 call say
pkfls: lodsb ; flush to end of option
 or al,al
 jz pkdone
 cmp al,#32
 je pkdone
 jmp pkfls
pkend: call pkput ; store token
 jmp pkdone ; ... and return
pkmore: call pkput ; store token
 jmp pknext ; handle next token
pkput: seg es ; get buffer pointer
 mov bx,[KBEND]
 mov dx,bx
 add dx,#2 ; increment it
 cmp dx,#KBHIGH ; (wrap around end)
 jb pknadj
 mov dx,#KBLOW
pknadj: seg es ; buffer full ?
 cmp dx,[KBBEG]
 je pkfull ; yes -> error
 seg es ; store scan code
 mov (bx+0x400),cx
 seg es ; store new pointer
 mov [KBEND],dx
 ret ; done
pkfull: mov bx,#msg_pkf ; complain
 call say
 pop ax ; discard return address
 jmp pkfls ; abort

! Set VGA mode

setvga: add si,#4 ; skip over "vga="
 push si ; save SI
 mov bx,#vgatab ; scan VGA table
svgatb: pop si ; get pointer to option value
 push si
 mov cx,(bx) ; get VGA code
 or cx,cx ; at end ?
 jz vganum ; yes -> must be numeric
 inc bx ; compare the strings
 inc bx
vgacmp: lodsb
 call upcase ; (case-insensitive)
 mov ah,(bx)
 inc bx
 or ah,ah ; at end ?
 jnz vgamore ; no -> go on
 or al,al ; at end of line ?
 jz vgafnd ; yes -> found it
 cmp al,#32 ; space ?
 je vgafnd ; yes -> found it
 jmp svgatb ; try next entry otherwise
vgamore:cmp al,ah
 je vgacmp ; equal -> next character
vgaskp: mov al,(bx) ; skip to end of reference string
 inc bx
 or al,al
 jnz vgaskp
 jmp svgatb ; try next entry
vgafnd: pop ax ; drop SI

vgaput: dec si ; read last character again
vgaput1: mov vgaovr,cx ; set VGA mode
 clc ; okay, done
 ret

vganum: pop si ; get SI

 call strtoul
 jc vgaerr
 mov cx,ax
 or dx,dx
 jnz vgaerr
 jmp vgaput1
# 3476 "second.S"
vgaerr: mov bx,#msg_v ; display an error message
 call say

 xor eax,eax
 mov dword ptr [hma],eax

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

get_K:
 push cx ; save CX

 call strtoull ; get number in DX:AX
 jc gmthis2 ; signal conversion error

 mov bl,(si) ; get next character
 or bl,#0x20 ; convert to lower case
 cmp bl,#0x6b ; 'K' or 'k' ?
 je gmthis ; yes -> do not change

 mov cx,#20 ; divide or multiply by 2^20
 cmp bl,#0x67 ; 'G' or 'g' ?
 je gmmul

 mov cx,#10 ; divide or multiply by 2^10
 cmp bl,#0x6d ; 'M' or 'm' ?
 je gmmul

! no Suffix
 dec si ; will increment later

gmdivl:
 shr eax,cl ; shift by CL
 jmp gmthis ; done
gmmul:
gmmull:
 shl eax,1 ; shift by 1 each time
 jc gmvbig ; very big if overflow
 loop gmmull ; ten times

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
 mov eax,#0x38000000/1024
 jmp gmthis


! Set memory limit

getmem:
 push si ; save SI for copying
 add si,#4 ; advance to number?
 call get_K
 jc gmcopy ; error, just copy it

 cmp bl,#0x40 ; is it '@'
 jne gm22
! <size>@<start> format (2.4 kernels)
 push eax ; save size
 inc si ; skip '@'
 call get_K
 pop edx ; restore size
 jc memerr
 cmp eax,#1024 ; start : 1meg
 ja gmcopy ; just copy if above
 add eax,edx ; EAX = hma/1024
 cmp eax,#2048 ; high : 2meg
 jbe gmcopy
gm22:
 or bl,#0x20
 cmp bl,#0x20 ; NUL or SPACE



 jne gmcopy ; allow <size>#<start> and <size>$<start>

 cmp dword ptr [hma],#0 ; set already?
 jne gmnocopy
 mov dword ptr [hma],eax ; set it
gmcopy: pop si
gmret: ret
gmnocopy: pop bx
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

strtoul:
 xor ax,ax
 xor dx,dx
 mov cx,#10 ; default radix is decimal
 cmp byte ptr (si),#0x39
 ja s2lbad ; error if > '9'
 cmp byte ptr (si),#0x30 ; == '0'?
 jb s2lbad ; error if < '0'
 jne s2lnext
 inc si
 dec cx
 dec cx ; assume octal : CX = 8

 cmp byte ptr (si),#0x58 ; == 'X'?
 je s2lhex
 cmp byte ptr (si),#0x78 ; == 'x'?
 jne s2lnext
s2lhex: add cx,cx ; it is hexadecimal
 inc si
s2lnext:
        xor bx,bx
        mov bl,(si) ; get next character

 or bl,#0x20 ; convert to lower case
 sub bl,#0x30 ; - '0'
 jb s2ldone
 cmp bl,cl ; compare to radix
 jb s2lmul
 add bl,#0x30-0x61+10
 cmp bl,cl ; compare to radix
 jnb s2ldone
s2lmul:
        push dx ; save high order
 mul cx ; multiply by radix
 add ax,bx
 adc dx,#0 ; carry possible only in radix 10
 pop bx
 push dx
 xchg ax,bx
 mul cx
 or dx,dx
 jnz s2lbad
 pop dx
 add dx,ax
 jc s2lbad
 xchg ax,bx
 inc si
 jmp s2lnext

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
;
;
; side effect:
; The selected image is hi-lited if the menu is displayed
;
find_image:
 push cx
 push si
 push di

 mov cx,#IMAGES ; test all names
 mov si,#DESCR0
 xor bx,bx ; clear BX
 push si

fi_nextn:
 mov di,#cmdline
 test byte ptr (si),#0xFF ; null descriptor at end
 jz fi_nomore

fi_nextc:
 mov al,(si) ; get next character in descr
 inc si

 call upcase

 mov ah,al
 mov al,(di) ; get next char in cmdline
 inc di

 call upcase

 or al,al ; NUL in command line
 je fi_pmat
 cmp al,#32 ; SPACE in command line
 jne fi_cmp

; have partial match, set BX conditionally
fi_pmat:
 or ah,ah ; NUL in descriptor name
 jz fi_found ; EXACT match found

 test byte ptr par2_flag2,#4 ; (22.7)
 jnz fi_skipn ; no partial match if unattended

 or bx,bx
 jnz fi_skipn ; already set
 pop bx
 push bx
 jmp fi_skipn ; go to next

fi_cmp:
 cmp al,ah ; character equal ?
 je fi_nextc ; compare next character

; advance to next descriptor
fi_skipn:
 pop si
 add si,#id_size ; test next name
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
 cbw
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
msg_int:.byte 10
 .ascii "*Interrupted*"
 .byte 10,0

msg_eof:.byte 10
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

msg_pks:.byte 10
 .ascii "Invalid hexadecimal number. - Ignoring remaining items."
 .byte 10,0

msg_pkf:.byte 10
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
map: .word Map ; map to use

cntdown:.word 0 ; count-down
timeout:.byte 0 ; timed out

dolock: .byte 0

int1c_l:.word 0 ; old timer interrupt
int1c_h:.word 0

old_del:.word 0 ; delay before booting

nodfl: .word 0 ; action if no defaults are present


slbase: .word 0 ; serial port base (or 0 if unused)
break: .byte 0 ; break received flag


usrinpm:.byte 0xff

cmdbeg: .word 0
options:.word 0

rdbeg: .word 0,0 ; RAM dist begin address (dword)

rdszl: .word 0 ; RAM disk size
rdszh: .word 0

vgaovr: .word 0 ; VGA mode overwrite


hma: .word 0,0 ; Highest Memory Address
memmap: .word 0,0,0,0,0,0,0,0,0,0

dskprm: .word 0,0,0,0,0,0

 .even ; control alignment from here down
acmdbeg:.ascii "auto "
mcmdbeg:.ascii "BOOT_IMAGE"
prechr: .byte 32 ; space: guard double blank supression
    ; equal sign: variable assignment
cmdline:.byte 0




 .org *+4
theend:

lkwbuf = cmdline+CL_LENGTH+2 ; this is a word
lkcbuf = lkwbuf+2
theend2 = lkcbuf+CL_LENGTH ; lkcbuf is 256

the_end1 = theend+511
theends = the_end1/512
 .org theends*512-4
 .long X
 .align 512
max_secondary:
# 4140 "second.S"
Map = max_secondary + 512
Dflcmd = Map + 512
Map2 = Dflcmd
Keytable = Dflcmd + 512
Descr = Keytable + 512
ParmBSS = Descr + 512*MAX_DESCR_SECS_asm



BSSstart = ParmBSS
Parmline = BSSstart + 512





!************************************
! BSS data:
! moved from volume.S


 .org BSSstart
# 1 "volume.S" 1
# 25 "volume.S"
vtab = *
 .org *+MAX_BIOS_DEVICES_asm*4 ; volume IDs indexed by
     ; REAL bios device code

rtab = *
 .org *+MAX_BIOS_DEVICES_asm*4 ; raid offsets indexed the same


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
BSSsize = BSSend-BSSstart


Dataend = Parmline + 512

 .org max_secondary






DESCR0 = Descr
