;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Zero - BIOS for IBM PC
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
;_osz_systbl	dd 0

lba_enabled	db 0

_bios_table:
	dw _bios_const
	dw _bios_conin
	dw _bios_conout
	dw _puts
	dw _bios_power
	dw _bios_dispose
	dw _bios_init_disk
	dw _bios_fd_read
	dw _bios_fd_write


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
	int 0x16
	jz short .empty
	mov al, 0xFF
	ret
.empty:
	xor ax, ax
	ret


_bios_conin:
.loop:
	mov ah,0x01
	int 0x16
	jz short .wait
	xor ax,ax
	int 0x16
	xor ah, ah
	ret
.wait:
	sti
	hlt
	jmp short .loop
	
	
_bios_conout:
	mov bx, 0x0007
	cmp al, 0x0A
	jz short .crlf
	cmp al, 0x7F
	jbe short .ascii
	cmp al, 0xC0
	jb short .end
	cmp al, 0xFE
	jae short .end
	mov al, '?'
.ascii:
	mov ah, 0x0E
	int 0x10
	jmp short .end
.crlf:
	mov ax,0x0E0D
	int 0x10
	mov ax,0x0E0A
	int 0x10
.end:
	ret


_puts:
	mov si, dx
.loop:
	lodsb
	or al,al
	jz short .end
	call _bios_conout
	jmp short .loop
.end:
	ret


_bios_power:

	; exit cable3 (8086tiny http://www.megalith.co.uk/8086tiny/)
	xor bx, bx
	mov ds, bx
	mov [bx], byte 0xCB
	push cs
	call _exit_cable3
	
	; APM shutdown
	mov ax, 0x5301
	xor bx, bx
	int 0x15
	
	mov ax, 0x530E
	xor bx, bx
	mov cx, 0x0102
	int 0x15
	
	mov ax, 0x5307
	mov bx, 0x0001
	mov cx, 0x0003
	int 0x15
	
	; then reboot
	;mov al, 0xFF
	;out 0x21, al
	;out 0xA1, al
	cli
	mov al, 0xFE
	out 0x64 ,al
	mov al, 0x01
	out 0x92, al
	
	int 0x19
_forever:
	hlt
	jmp _forever

_exit_cable3:
	push bx
	push bx
	retf


_bios_dispose:
	cli
	mov al, [cs:saved_imr]
	out 0x21, al
	mov al, [cs:saved_imr+1]
	out 0xA1, al
	;; TODO:
	ret


_bios_init_disk:
	xor ax,ax
	xor dx,dx
	int 0x13
	
	mov dl, 0x80
	mov bx, 0x55AA
	mov ah, 0x41
	int 0x13
	jc .no_lba
	cmp bx, 0xAA55
	jnz .no_lba
	test cx, 0x01
	jz .no_lba
	mov [cs:lba_enabled], bl
.no_lba:
	
	ret


_bios_fd_read:
	xor di,di
	
	mov ax,[si+0x08]
	les bx,[si+0x04]
	mov cx,[si+0x02]
.loop:	
	push ax
	push cx
	mov cl, 18
	div cl
	mov cl, ah
	mov ch, al
	xor dh, dh
	shr ch, 1
	adc dh, dh
	inc cl
	mov dl, 0x00
	mov ax,0x0201
	int 0x13
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
	ret


	alignb 16
_END_RESIDENT:


crt:
	mov al, [es:bx + OSZ_SYSTBL_ARCH]
	cmp al, 0x01
	jnz .dont_install
	mov ax, [es:bx + OSZ_SYSTBL_BIOS]
	or ax, [es:bx + OSZ_SYSTBL_BIOS+2]
	jz .install_ok
.dont_install:
	xor ax,ax
	retf
.install_ok:

	; setup
	;mov [_osz_systbl], bx
	;mov [_osz_systbl+2], es
	mov [es:bx + OSZ_SYSTBL_BIOS], word _bios_entry
	mov [es:bx + OSZ_SYSTBL_BIOS+2], cs

	in al, 0x21
	mov [saved_imr], al
	in al, 0xA1
	mov [saved_imr+1], al

	int 0x12
	mov cl, 6
	shl ax, cl
	mov [es:bx + OSZ_SYSTBL_MEMSZ], ax
	
	;mov ax,0x0013
	;int 0x10
	
	call _bios_init_disk
	
	mov ax, (_END_RESIDENT-_HEAD)/16
	retf
	

	alignb 16
_END:
