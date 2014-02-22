;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Zero - BIOS for NEC PC-9800 Series Personal Computer
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

%include "osz.inc"


[bits 16]

_HEAD:
	db 0xC3, 0x5A
	dw (_END-_HEAD)/16

	jmp near crt

	alignb 2
saved_imr	dw 0

cons_cursor	dw 0, 0xA000
cons_scroll	dw 160*24

_bios_table:
	dw _bios_const
	dw _bios_conin
	dw _bios_conout
	dw _bios_cls
	dw _bios_power
	dw _bios_dispose
	dw _bios_init_disk
	dw _bios_fd_read
	dw _bios_fd_write


__int28:
	sti
	hlt
	iret


__int29:
	push ax
	mov ah, BIOS_CONOUT
	push cs
	call _bios_entry
	pop ax
	iret


_bios_entry:
	push es
	push ds
	push bp
	push di
	push si
	push bx
	push dx
	push cx
	;push ax
	;mov bp,sp

	mov bl, ah
	mov bh, 0x00
	add bx, bx
	call [cs:_bios_table + bx]

	;lea sp,[bp+2]
	pop cx
	pop dx
	pop bx
	pop si
	pop di
	pop bp
	pop ds
	pop es
_retf:
	retf


_bios_const:
	mov ah, 0x01
	int 0x18
	or bh, bh
	jz short .empty
	mov al, 0xFF
	ret
.empty:
	xor ax, ax
	ret


_bios_conin:
.loop:
	mov ah,0x01
	int 0x18
	or bh, bh
	jz short .wait
	xor ah, ah
	int 0x18
	xor ah, ah
	ret
.wait:
	int 0x28
	jmp short .loop


_bios_conout:
	les di, [cs:cons_cursor]
	call _check_scroll
	cmp al, 0x08
	jz short .bs
	cmp al, 0x0A
	jz short .crlf
	cmp al, 0x0D
	jz short .cr
	cmp al, 0x7F
	jbe short .ascii
	cmp al, 0xC0
	jb short .end
	mov al, '?'
.ascii:
	cmp al, 0x5C
	jnz .no_5C
	mov al, 0xFC
.no_5C:
	xor ah, ah
	stosw
	jmp short .end
.bs:
	mov ax, di
	mov cx, 160
	cwd
	div cx
	mov bx, dx
	mul cx
	or bx, bx
	jz .bs_leftend
	dec bx
	dec bx
.bs_leftend:
	add ax, bx
	mov di, ax
	jmp short .end
.cr:
	mov ax, di
	mov cx, 160
	cwd
	div cx
	mul cx
	mov di, ax
	jmp short .end
.crlf:
	mov ax, di
	mov cx, 160
	cwd
	div cx
	inc ax
	mul cx
	mov di, ax
	;jmp short .end
.end:
	mov [cs:cons_cursor], di
	mov dx, di
	mov ah, 0x13
	int 0x18
	ret


_check_scroll:
	cmp di, [cs:cons_scroll]
	jae short .do
	ret
.do:
	push ax
	push ds
	push es
	pop ds
	mov si, 160
	xor di, di
	mov cx, [cs:cons_scroll]
	sub cx, si
	shr cx, 1
	rep movsw
	mov ax, 0x0020
	mov cx, 80
	rep stosw
	pop ds
	pop ax
	sub di, 160
	ret


_bios_cls:
	mov dx, 0x0E120
	mov ah, 0x16
	int 0x18
	xor dx, dx
	mov [cs:cons_cursor], dx
	mov ah, 0x13
	int 0x18
	mov ah, 0x11
	int 0x18
	ret


_bios_power:
	push cs
	pop ds

	; np2 shutdown
	mov si, np2_shutdown_cmd
	mov dx, 0x07EF
.loop_np2:
	lodsb
	or al, al
	jz short .end_np2
	out dx, al
	jmp short .loop_np2
.end_np2:

	;;	TODO: APM Shutdown

	;;	REBOOT
	mov al, 0x0F
	out 0x37, al
	mov al, 0x0B
	out 0x37, al
	xor al, al
	out 0xF0, al

forever:
	hlt
	jmp forever



_bios_dispose:
	cli
	mov al, [cs:saved_imr]
	out 0x02, al
	mov al, [cs:saved_imr+1]
	out 0x0A, al
	;; TODO:
	ret


_bios_init_disk:
	mov ax, 0x0330
	int 0x1B
	mov ax, 0x0730
	int 0x1B
	ret


_bios_fd_read:
	xor di,di
	
	mov ax,[si+0x08]
	les bp,[si+0x04]
	mov cx,[si+0x02]
.loop:	
	push ax
	push cx
	
	mov cl, 18
	div cl
	mov dl, ah
	xor dh, dh
	inc dx
	mov cl, al
	shr cl, 1
	adc dh, 0
	mov ch, 0x02
	mov bx, 0x0200
	mov ax, 0x5630
	int 0x1B
	
	pop cx
	pop ax
	jc short .end
	
	dec cx
	jz short .end
	inc di
	mov dx, es
	add dx, 0x20
	mov es, dx
	inc ax
	jmp short .loop

.end:
	mov ax, di
	ret


_bios_fd_write:
	xor ax, ax
	ret


np2_shutdown_cmd db "poweroff", 0

	alignb 16
_END_RESIDENT:


crt:
	mov al, [es:bx + OSZ_SYSTBL_ARCH]
	or al, al
	jnz .dont_install
	mov ax, [es:bx + OSZ_SYSTBL_BIOS]
	or ax, [es:bx + OSZ_SYSTBL_BIOS+2]
	jz .install_ok
.dont_install:
	xor ax,ax
	retf
.install_ok:

	; setup
	mov [es:bx + OSZ_SYSTBL_BIOS], word _bios_entry
	mov [es:bx + OSZ_SYSTBL_BIOS+2], cs

	in al, 0x02
	mov [saved_imr], al
	in al, 0x0A
	mov [saved_imr+1], al

	mov di, 0x28*4
	mov ax, __int28
	stosw
	mov ax, cs
	stosw
	mov ax, __int29
	stosw
	mov ax, cs
	stosw

	mov al, [es:0x0501]
	and ax, byte 0x07
	inc ax
	mov cl, 13
	shl ax, cl
	mov [es:bx + OSZ_SYSTBL_MEMSZ], ax
	
	cmp [es:bx + OSZ_SYSTBL_CPUID], byte 2
	jb .no_extmem
	mov al, [es:0x0401]
	xor ah, ah
	mov cl, 7
	shl ax, cl
	mov [es:bx + OSZ_SYSTBL_MEMPROT], ax
.no_extmem:

	;;  SETUP 640x480
	mov al, [es:0x045C]
	test al, 0x40
	jz short .no_pegc

	mov ax, 0x300C
	mov bx, 0x3200
	int 0x18
	mov ax, 0x4D00
	mov cx, 0x0100
	int 0x18
	mov ah, 0x0C
	int 0x18
	mov ah, 0x40
	int 0x18

	mov cx, 0xE000
	mov es, cx
	mov ax,0x0001
	mov [es:0x0100],al
	mov [es:0x0102],ax
	
	mov [cons_scroll], word 160*29

.no_pegc:

	call _bios_cls

	call _bios_init_disk
	
	mov ax, (_END_RESIDENT-_HEAD)/16
	retf

	alignb 16
_END:




