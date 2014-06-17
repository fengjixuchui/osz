;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Z - Runtime Environment
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

%define	SIZE_BSS	0

[bits 16]
_HEAD:
	db 0xC3, 0x5A
	dw (_END-_HEAD)/16

	jmp _crt

_call_bios:
	db 0x9A
_osz_systbl	dd 0
	ret

_call_bdos:
	db 0x9A
_bdos_ptr	dd 0
	ret


_int3F:
	cmp ah, OSZ_I3F_HANDLE_MAGIC
	jz short _MAGIC_WORD_HANDLER
	db 0xEA
_int3F_old	dd 0

%define	LOCAL_APP_SRC_SEG	-2
%define	LOCAL_APP_SRC_SIZE	-4
%define	LOCAL_APP_JIT_SEG	-6
%define	LOCAL_APP_JIT_SIZE	-8
_MAGIC_WORD_HANDLER:
	push bp
	mov bp, sp
	sub sp, 16
	mov [bp+LOCAL_APP_SRC_SEG], ds
	mov [bp+LOCAL_APP_SRC_SIZE], cx

	mov [cs:_bdos_ptr], bx
	mov [cs:_bdos_ptr+2], es

	mov ax, ds
	mov dx, cx
	mov cl, 4
	shr dx, cl
	inc dx
	add ax, dx
	mov [bp+LOCAL_APP_JIT_SEG], ax
	xor cx, cx
	mov [bp+LOCAL_APP_JIT_SIZE], cx

%if 0
	mov dx, [bp+LOCAL_APP_SRC_SEG]
	call _disp_hex_16
	mov al, ':'
	int 0x29
	mov dx, [bp+LOCAL_APP_SRC_SIZE]
	call _disp_hex_16
%endif

	lds cx, [bp+LOCAL_APP_SRC_SIZE]
	mov es, [bp+LOCAL_APP_JIT_SEG]
	mov si, 0x0100
	add cx, si
	xor di, di

	lodsw

.inst_loop:
	lodsb

	; cout
	cmp al, 0x0E
	jnz .no_op0E
	mov ax, 0x29CD
	stosw
	jmp short .inst_end
.no_op0E:

	; ldc rni, i8
	cmp al, 0x10
	jb .no_op10
	cmp al, 0x13
	ja .no_op10
	and al, 0x03
	or al, 0xB8
	stosb
	lodsb
	cbw
	stosw
	jmp short .inst_end
.no_op10:
	; ldc rni, i16
	cmp al, 0x14
	jb .no_op14
	cmp al, 0x17
	ja .no_op14
	and al, 0x03
	or al, 0xB8
	stosb
	lodsw
	stosw
	jmp short .inst_end
.no_op14:

	; for rn, rn
	cmp al, 0x20
	jnz .no_op20
	mov [cs:_for_addr], di
	lodsb
	mov [cs:_for_regs], al
	jmp short .inst_end
.no_op20:

	; end for
	cmp al, 0x21
	jnz .no_op21

	mov al, [cs:_for_regs]
	and ax, 0x0007
	or ax, 0xCE40
	stosw
	mov al, 0x3B ; CMP
	stosb
	mov al, [cs:_for_regs]
	or al, 0xC0
	stosb

	mov bx, [cs:_for_addr]
	lea dx, [di+2]
	sub bx, dx

	mov al, 0x7D
	stosb
	mov al, bl
	stosb

	jmp short .inst_end
.no_op21:

	jmp .inv_op
.inst_end:
	cmp cx, si
	je short _end_jit
	jb .inv_op
	jmp .inst_loop

.inv_op:
	push cs
	pop ds
	push ax
	mov dx, inv_op_msg
	mov ah, OSZ_DOS_PUTS
	call _call_bdos
	lea dx, [si-0x0101]
	call _disp_hex_16
	mov al, ':'
	int 0x29
	pop dx
	call _disp_hex_8

	int 0x20

_end_jit:

	mov [bp+ LOCAL_APP_JIT_SIZE], di
	mov ax, 0x20CD
	stosw

	push cs
	pop ds
	mov dx, src_msg
	mov ah, OSZ_DOS_PUTS
	call _call_bdos

	lds cx, [bp+LOCAL_APP_SRC_SIZE]
	mov si, 0x0100
.loop1
	push cx
	mov al, ' '
	int 0x29
	lodsb
	mov dl, al
	call _disp_hex_8
	pop cx
	loop .loop1
	mov al, 10
	int 0x29

	push cs
	pop ds
	mov dx, jit_msg
	mov ah, OSZ_DOS_PUTS
	call _call_bdos

	lds cx, [bp+LOCAL_APP_JIT_SIZE]
	xor si, si
.loop2
	push cx
	mov al, ' '
	int 0x29
	lodsb
	mov dl, al
	call _disp_hex_8
	pop cx
	loop .loop2
	mov al, 10
	int 0x29

	xor ax, ax
	mov bx, [bp+LOCAL_APP_JIT_SEG]
	push bx
	push ax
	retf

	int 0x20


_disp_hex_16:
	mov cl, 4
	jmp short _disp_hex
_disp_hex_8:
	mov dh, dl
	mov cl, 2
	jmp short _disp_hex
_disp_hex:
	mov ch, 0
	push bx
.loop:
	push cx
	mov cl, 4
	rol dx, cl
	mov bx, dx
	and bx, byte 0x000F
	mov al, [cs:_hex_tbl+bx]
	int 0x29
	pop cx
	loop .loop
	pop bx
	ret


_for_addr	dw 0
_for_regs	db 0, 0


_hex_tbl	db "0123456789abcdef"

src_msg		db "SRC: ",0
jit_msg		db "JIT: ",0

inv_op_msg	db "JIT ERROR: Invalid opcode ", 0

_crt:
	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

	xor ax, ax
	mov es, ax
	mov bx, 0x3F*4
	
	mov ax, [es:bx]
	mov cx, [es:bx+2]
	mov [_int3F_old], ax
	mov [_int3F_old+2], cx
	mov [es:bx], word _int3F
	mov [es:bx+2], cs

	mov ax,(SIZE_BSS + _END-_HEAD)/16
	retf


	alignb 16
_END:


