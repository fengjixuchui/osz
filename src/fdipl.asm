;;	-*- coding: utf-8 -*-
;;	OSZ FAST BOOT SECTOR for Floppy
;;	WTFPL/PUBLIC DOMAIN

%define	IPL_SIGN	0x1eaf

[bits 16]

_HEAD:
	jmp short main
	nop
	db "IPL4OSZF"

incbin PATH_BPB

;;  Variables
fat2    dw 0	; 2
_arch   db 0	; 1
__PDA   db 0	; 1
__N     db 0	; 1
_clust_sft	db 0	; 1

	times 0x26 - ($-$$) db 0

	db 0x29
	dd 0xFFFFFFFF
	;;  123456789AB
	db "OSZ        "
	db "FAT12   "

main:

	;	setup register
	xor si, si
	push si
	popf
	mov ss, si
	mov sp, 0x0400

	;	select architecture
	mov di, _arch
	mov ax, cs
	mov cx, 0x07C0
	push cx
	cmp ah, 0x1F
	ja short initFMT
	jz short init98

	;	IBM PC
initAT:
	mov ax,(main0-_HEAD)
	push ax
	retf
	
main0:
	push cs
	pop ds
	inc byte [di]
	xor ax, ax
	int 0x13
	jmp short _next

	;	FM TOWNS
initFMT:
	push cs
	pop ds
	mov ax, 0x2002
	or  ah, bh
	mov [di], ax
	jmp short init2

	;	NEC PC-98
init98:
	push cs
	pop ds
	mov al, [ss:0x0584]
	mov [__PDA], al
init2:
	mov al, [0x000C]
	shr al, 1
	inc al
	mov [__N], al
	pop es
	xor di, di
	mov cx, 256
	rep movsw
	push es
	call _retf
	push cs
	pop ds

_next:
	mov ax, 0x1000
	mov es, ax
	;push es

	mov al, [0x000D]
	xor dx, dx
.loop_clst_sft:
	shr al, 1
	jz .end
	inc dx
	jmp .loop_clst_sft
.end:
	mov [_clust_sft], dl

	;; FAT2
	mov ax, [0x0011]
	mov cl, 5
	shl ax, cl
	mov si, [0x0016]
	add si, si
	inc si
	xor dx, dx
	div word [0x000B]
	add ax, si
	mov [fat2], ax

	mov ax, 0x0E2E
	int 0x10

	;; READ FIRST CLUSTER
	push es
	xor bp, bp
	push bp
	mov si, [fat2]
	mov cx, [0x000B]
	call diskread

	cmp word [es:0x0000], "EM"
	jz sys_ok
	jmp forever

sys_ok:
	mov ax, 0x0E2E
	int 0x10

	mov cx, [es:0x0006]
	sub cx, [0x000B]
	call diskread

	mov ax, 0x0E2E
	int 0x10

	mov ax, IPL_SIGN
	mov cx, [_arch]
_retf:
	retf




	;; disk read
	;;	IN cx:size si:LBA es:bp:buffer
	;;	USES ax cx bx dx
diskread:
	xchg ax, cx
	xor dx, dx
	mov bx, [0x000B]
	dec bx
	add ax, bx
	adc dx, byte 0
	inc bx
	div bx
	xchg ax, cx
drloop:
	push cx

	xor dx, dx
	mov ax, si
	div word [0x0018]
	inc dx
	shr ax, 1
	adc dh, 0
	cmp byte [_arch], 0
	jz short dr98
	cmp byte [_arch], 2
	jz short drFMT
	mov ch, al
	mov cl, dl
	mov dl, [__PDA]
	xchg bx, bp
	mov ax, 0x0201
	int 0x13
	xchg bx, bp
	jmp short drmde
dr98:
	mov cl, al
	mov ch, [__N]
	mov al, [__PDA]
	mov ah, 0x56
	int 0x1B
drmde:
	jnc nohalt
	jmp forever
nohalt:
	add bp, bx
	inc si
	pop cx
	loop drloop
	ret

drFMT:
	push bx
	push ds
	push di
	mov cl, al
	mov al, [__PDA]
	mov ah, 0x05
	mov bx, 0x0001
	push es
	pop ds
	mov di, bp
	db 0x9A
	dd 0xFFFB0014
	pop di
	pop ds
	pop bx
	jmp short drmde


forever:
	jmp short $


	times 0x01FE - ($-$$) db 0
	db 0x55, 0xAA

end
