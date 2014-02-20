;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Boot Sector
;;
;;	Copyright (c) 1998-2014, MEG-OS project
;;	All rights reserved.
;;	
;;	Redistribution and use in source and binary forms, with or without modification, 
;;	are permitted provided that the following conditions are met:
;;	
;;	* Redistributions of source code must retain the above copyright notice, this
;;	  list of conditions and the following disclaimer.
;;	
;;	* Redistributions in binary form must reproduce the above copyright notice, this
;;	  list of conditions and the following disclaimer in the documentation and/or
;;	  other materials provided with the distribution.
;;	
;;	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;;	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;	ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

[bits 16]

_HEAD:
	jmp short main
	nop
	db "IPL4MEG "
	dw 0x0200
	db 1
	dw 1
	db 2
	dw 0x00E0
	dw 2880
	db 0xF0
	dw 9
	dw 18
	dw 2
	times 10 db 0
	db 0x29
	dd 0xFFFFFFFF
	;;  123456789AB
	db "MEGOS      "
	db "FAT12   "

        ;;  FILENAMEEXT
sysname db "OSZ     SYS"

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

	;	PC
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

	;	TOWNS
initFMT:
	push cs
	pop ds
	mov ax, 0x2002
	or  ah, bh
	mov [di], ax
	jmp short init2

	;	PC98
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
	push es

	;; read Root DIR
	mov ax, [0x0011]
	mov cl, 5
	shl ax, cl
	mov cx, ax
	mov si, [0x0016]
	add si, si
	inc si
	xor dx, dx
	div word [0x000B]
	add ax, si
	mov [fat2], ax
	xor bp, bp
	call diskread

	;; Read FAT
	mov ax, [0x0016]
	mul word [0x000B]
	xchg ax, cx
	mov si, 1
	push cs
	pop es
	mov bp, 0x0400
	call diskread
	pop es

	;; Find System
	mov cx, [0x0011]
	xor di, di
cfnl:
	push cx
	mov si, sysname
	mov cx, 11
	rep cmpsb
	pop cx
	jz ff
	or di, byte 0x1F
	inc di
	loop cfnl
	jmp forever
ff:
	and di, byte 0xE0
	mov bx, [0x000B]
	mov si, [es:di+0x001A]
	xor bp, bp
	push es
	push bp
lfl:
	cmp si, 0x0FF7
	jae force
	push si
	sub si, byte 2
	add si, [fat2]
	mov cx, [0x000B]
	call diskread
noread:
	pop ax
	mov bx, ax
	add bx, bx
	add bx, ax
	shr bx, 1
	mov si, [cs:bx+0x0400]
	jnc nfl0
	mov cl, 4
	shr si, cl
nfl0:
	and si, 0x0FFF
	jmp short lfl
force:

	;; jump system
	mov ax, 0x1eaf	; IPL signature
	mov cx, [_arch]
_retf:
	retf

	;; disk read
diskread:
	xchg ax, cx
	xor dx, dx
	div word [0x000B]
	xchg ax, cx
drl:
	push cx

	xor dx, dx
	mov ax, si
	div word [0x0018]
	inc dx
	shr ax, 1
	adc dh, 0
	mov bx, [0x000B]
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
	mov cl, 4
	shr bx, cl
	mov ax, es
	add ax, bx
	mov es, ax
	inc si
	pop cx
	loop drl
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


;;  Variables
fat2    dw 0
_arch   db 0
__PDA   db 0
__N     db 0

	times 0x01FE-($-$$) db 0
	db 0x55, 0xAA

end
