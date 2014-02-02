;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Zero - Shell interface
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


%define	INT_DOS_VERSION	0x5005

%define OSZ_BDOS	0x0005

%define	MAX_CMDLINE	127
%define	MAX_CMD		127

;;	256
%define	str_buff	_BSS
;;	256
%define	cmdline		_BSS + 0x0100
;;	128
%define	cmd_buffer	_BSS + 0x0200
;;	128
%define	arg_buffer	_BSS + 0x0280
;;	256
%define	dir_buff	_BSS + 0x0300
;;	16
%define	numbuff		_BSS + 0x0400

%define	_END		_BSS + 0x0400

%define	SIZE_MAINSTACK	16
%define	LOCAL_SAVED_HANDLE		-2
%define	LOCAL_SIZE_CMDLINE		-4
%define	LOCAL_SIZE_CMD			-6
%define	LOCAL_SIZE_ARG			-8

%define	STK_AX					0
%define	STK_CX					2
%define	STK_DX					4
%define	STK_BX					6
%define	STK_SI					8
%define	STK_DI					10
%define	STK_BP					12
%define	STK_DS					14
%define	STK_ES					16


[bits 16]

_HEAD:
	db 0xC3, 0x5A
	dw (_BSS-_HEAD)/16

	jmp _crt


	alignb 4
_saved_sssp	dd 0

_BDOS_function_table:
	dw _BDOS_00-_HEAD
	dw _BDOS_01-_HEAD
	dw _BDOS_02-_HEAD
	dw _BDOS_03-_HEAD
	dw _BDOS_04-_HEAD
	dw _BDOS_05-_HEAD
	dw _BDOS_06-_HEAD
	dw _BDOS_07-_HEAD
	dw _BDOS_08-_HEAD
	dw _BDOS_09-_HEAD
	dw _BDOS_0A-_HEAD
	dw _BDOS_0B-_HEAD
	dw _BDOS_0C-_HEAD
	dw _BDOS_0D-_HEAD
	dw _BDOS_0E-_HEAD
	dw _BDOS_0F-_HEAD
	dw _BDOS_10-_HEAD
	dw _BDOS_11-_HEAD
	dw _BDOS_12-_HEAD
	dw _BDOS_13-_HEAD
	dw _BDOS_14-_HEAD
	dw _BDOS_15-_HEAD
	dw _BDOS_16-_HEAD
	dw _BDOS_17-_HEAD
	dw _BDOS_18-_HEAD
	dw _BDOS_19-_HEAD
	dw _BDOS_1A-_HEAD
	dw _BDOS_unknown-_HEAD
_END_BDOS_function:


_BDOS_over:
	mov ax, 0xFFFF
	ret

_BDOS_entry:
	cmp cl, (_END_BDOS_function-_BDOS_function_table)/2
	jae short _BDOS_over
	push es
	push ds
	push bp
	push di
	push si
	push bx
	push dx
	push cx
	push ax
	mov bp,sp

	mov bl, cl
	mov bh, 0x00
	add bx, bx
	call [cs:_BDOS_function_table + bx]

_BDOS_return:
	lea sp,[bp+2]
	pop cx
	pop dx
	pop bx
	pop si
	pop di
	pop bp
	pop ds
	pop es
	ret


	; DEFAULT BDOS FUNCTION
_BDOS_unknown:
_BDOS_06:
_BDOS_07:
_BDOS_09:
_BDOS_0A:
_BDOS_0B:
_BDOS_0C:
_BDOS_0D:
_BDOS_0E:
_BDOS_0F:
_BDOS_18:
_BDOS_19:
_BDOS_1A:
	mov ax, 0xFFFF
	ret

_int00: ; INTEGER DIVIDE BY ZERO
	push cs
	pop ds
	mov dx, int00_msg
	mov ah, BIOS_PUTS
	call _call_bios
	;jmp _BDOS_00

_BDOS_00: ; EXIT
	mov cx, cs
	mov ds, cx
	mov es, cx
	cli
	cld
	mov ss,[_saved_sssp+2]
	mov sp,[_saved_sssp]
	mov bp, sp
	sub sp, SIZE_MAINSTACK
	call _crlf
	jmp _loop


_psp_bdos:
	call _BDOS_entry
	retf


_crlf:
	mov al, 10
_fast_conout:
	mov ah, BIOS_CONOUT
_call_bios:
	db 0x9A
_osz_systbl	dd 0
	ret


	; CONIN
_BDOS_01:
	mov ah, BIOS_CONIN
	jmp short _call_bios

	; CONOUT
_BDOS_02:
	mov al, dl
	jmp short _fast_conout
	ret

	; CONST
_BDOS_03:
	mov ah, BIOS_CONST
	jmp short _call_bios

	; CONOUT STRING
_BDOS_04:
	mov ah, BIOS_PUTS
	call _call_bios
	ret

	; CONIN BUFFERED
	; IN ax = limit ds:dx = buffer
	; OUT ax = length
_BDOS_05:
	push bx
	push si
	push di

	mov di, ax
	mov si, dx
	xor bx, bx

.main_loop:
	mov ah, BIOS_CONST
	call _call_bios
	or al,al
	jnz .has_key
	sti
	hlt
	jmp short .main_loop

.has_key:
	mov ah, BIOS_CONIN
	call _call_bios
	
	cmp al, 0x03
	jnz short .no_break

	mov al, '^'
	call _fast_conout
	mov al, 'C'
	call _fast_conout

	jmp short _BDOS_00
	;xor bx,bx
	;jmp short .end
	
.no_break:

	cmp al, 0x0D
	jz short .crlf
	cmp al, 0x0A
	jnz short .no_crlf

.crlf:
	call _crlf

	jmp short .end
		
.no_crlf:

	cmp al, 0x1B
	jnz short .no_esc

.loop_esc:
	or bx, bx
	jz short .main_loop

	dec bx
	
	mov al, 8
	call _fast_conout
	mov al, ' '
	call _fast_conout
	mov al, 8
	call _fast_conout
	
	jmp short .loop_esc

.no_esc:

	cmp al, 0x08
	jnz short .no_bs
	
	or bx, bx
	jz short .main_loop
	
	dec bx
	
	mov al, 8
	call _fast_conout
	mov al, ' '
	call _fast_conout
	mov al, 8
	call _fast_conout
	
	jmp short .main_loop

.no_bs:

	cmp bx, di
	jnc .main_loop

	mov [si+bx], al
	inc bx
	
	call _fast_conout

	jmp short .main_loop


.end:
	cmp bx, di
	jae .no_last_nul
	xor cl, cl
	mov [si+bx],cl
.no_last_nul:
	mov ax,bx

	pop di
	pop si
	pop bx
	
	ret


_BDOS_08: ; SYSINFO
	les bx,[cs:_osz_systbl]
	mov ax, [es:bx + OSZ_SYSTBL_VERSION]
	mov [bp+STK_BX], ax
	mov cl, [es:bx + OSZ_SYSTBL_CPUID]
	xor ch, ch
	mov [bp+STK_CX], cx
	xor dx, dx
	mov [bp+STK_DX], dx
	mov ax, INT_DOS_VERSION
	ret


_BDOS_IFS:
_BDOS_10:
_BDOS_11:
_BDOS_12:
_BDOS_13:
_BDOS_14:
_BDOS_15:
_BDOS_16:
_BDOS_17:
	les bx,[cs:_osz_systbl]
	call far [es:bx+OSZ_SYSTBL_IFS]
	ret




_crt:
	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

	mov ax, cs
	add ax, (_END)
	mov [es:bx+OSZ_SYSTBL_LASTMEM], ax
	
	xor bx, bx
	mov es, bx
	mov word [es:bx], _int00
	mov [es:bx+2], cs

	push cs
	pop es
	mov cx, (_END-_BSS)/2
	mov di, _BSS
	xor ax, ax
	rep stosw

	mov [_saved_sssp], sp
	mov [_saved_sssp+2], ss

%if 0
	push es
	mov dx, cs
	les bx, [_osz_systbl]
	call _disp_hex_16
	mov al, ' '
	call _fast_conout
	mov dx, [es:bx+OSZ_SYSTBL_MEMSZ]
	call _disp_hex_16
	mov al, ' '
	call _fast_conout
	mov dx, [es:bx+OSZ_SYSTBL_LASTMEM]
	call _disp_hex_16
	mov al, 10
	call _fast_conout
	pop es
%endif
	
	mov bp, sp
	sub sp, SIZE_MAINSTACK

	call _crlf
	;call _cmd_ver
_loop:
	mov si, str_buff
	mov cl, OSZ_DOS_GET_CWD
	call _BDOS_entry
	lea dx, [si+2]
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
	mov al, '>'
	call _fast_conout

	mov ax, MAX_CMDLINE
	mov cl, OSZ_DOS_GETS
	mov dx, cmdline
	call _BDOS_entry
	or ax, ax
	jz short _loop
	mov [bp+LOCAL_SIZE_CMDLINE], ax

	mov si, cmdline
.loop_pre_cmd:
	mov al, [si]
	or al, al
	jz short _loop
	cmp al, 0x20
	ja short .end_pre_cmd
	inc si
	jmp short .loop_pre_cmd
.end_pre_cmd:
	mov di, cmd_buffer
	xor bx, bx	
.loop_cmd_token:
	mov al, [si+bx]
	cmp al, 0x20
	jbe short .end_cmd_token
	mov [di+bx], al
	inc bx
	cmp bx, MAX_CMD
	jb short .loop_cmd_token
	jmp short .bad_cmd
.end_cmd_token:
	or bx, bx
	jz _loop
	xor al, al
	mov [di+bx], al
	mov [bp+LOCAL_SIZE_CMD], bx

.loop_post_cmd:
	mov al, [si+bx]
	or al,al
	jz .end_post_cmd
	cmp al, 0x20
	ja .end_post_cmd
	inc bx
	jmp short .loop_post_cmd
.end_post_cmd:
	add si, bx
	mov di, arg_buffer
	mov dx, -1
.loop_copy_arg:
	lodsb
	stosb
	inc dx
	or al,al
	jnz .loop_copy_arg
	mov [bp+LOCAL_SIZE_ARG], dx

	mov bx, [bp+LOCAL_SIZE_CMD]

	mov si, cmd_table
	mov di, cmd_buffer
.loop_cmd_tbl:
	lodsb
	or al, al
	jz .end_cmd_tbl
	cmp al, bl
	jnz .not_much
	push si
	push di
	mov cx, bx
	rep cmpsb
	pop di
	pop si
	jnz .not_much
	add si, bx
	lodsw
	call ax
	jmp _loop
.not_much:
	xor ah, ah
	add si, ax
	lodsw
	jmp short .loop_cmd_tbl
.end_cmd_tbl:

	mov si, app_ext
	mov di, cmd_buffer
	add di, [bp+LOCAL_SIZE_CMD]
	mov dx, si
	call _strlen
	inc ax
	mov cx, ax
	rep movsb

	mov dx, cmd_buffer
	mov cl, OSZ_DOS_OPEN
	call _BDOS_entry
	test ax, ax
	jns .load_exec

.bad_cmd:
	mov dx, bad_cmd_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry

	jmp _loop

.load_exec:

	mov [bp+LOCAL_SAVED_HANDLE], ax

	les bx,[cs:_osz_systbl]
	mov es, [es:bx+OSZ_SYSTBL_LASTMEM]
	xor di, di
	mov ax, '_='
	stosw

	mov si, cmdline
	mov cx, [bp+LOCAL_SIZE_CMDLINE]
	rep movsb
	xor ax, ax
	stosw

	mov cl, 4
	add di, byte 0x0F
	shr di, cl
	inc di
	mov bx, es
	add di, bx
	mov es, di
	
	xor di, di
	xor ax, ax
	mov cx, 0x8000
	rep stosw
	
	mov [es:0x2C], bx

	;xor di, di
	mov al, 0xEA
	stosb
	mov ax, _BDOS_00
	stosw
	mov ax, cs
	stosw
	mov al, 0x9A
	stosb
	mov ax, _psp_bdos
	stosw
	mov ax, cs
	stosw
	mov al, 0xC3
	stosw
	
	mov di, 0x0080
	mov si, arg_buffer
	mov ax, [bp+LOCAL_SIZE_ARG]
	stosb
	mov cx, ax
	rep movsb
	xor al,al
	stosb

	mov si, [bp+LOCAL_SAVED_HANDLE]

	push es
	pop ds

	mov dx, 0x0100
	mov cl, OSZ_DOS_READ
	call _BDOS_entry

	mov cl, OSZ_DOS_CLOSE
	call _BDOS_entry

	mov dx, ds
	cli
	mov ss, dx
	xor sp, sp
	mov cx, 0x0100
	xor ax, ax
	push ax
	push ss
	push cx
	xor cx, cx
	xor dx, dx
	xor bx, bx
	mov bp, OSZ_BDOS
	xor si, si
	xor di, di
	sti
	retf


_cmd_exit:
	mov ax, BIOS_POWER * 0x100
	call _call_bios
	ret


_cmd_ver:
	mov dx, ver_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
	ret


_cmd_echo:
	mov dx,arg_buffer
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
	call _crlf
	ret


_cmd_cd:
	mov dx,arg_buffer
	mov cl, OSZ_DOS_SET_CWD
	call _BDOS_entry
	ret


_cmd_dir:
	push cs
	pop es

	xor ax, ax
.loop:
	mov dx, dir_buff
	mov cl, OSZ_DOS_ENUM_FILE
	call _BDOS_entry
	or ax, ax
	jz short .end
	js short .end
	push ax

	mov al, ' '
	call _fast_conout

	mov cx, 4
	mov di,numbuff
	mov ax, 0x2020
	rep stosw
	xor al,al
	stosb
	mov bx, numbuff+7
	mov dx, [dir_buff+0x1E]
	mov ax, [dir_buff+0x1C]
	mov cx, 10
.loop_sz:
	div cx
	add dl, '0'
	mov [bx], dl
	dec bx
	jz .end_sz
	or ax, ax
	jz .end_sz
	xor dx, dx
	jmp short .loop_sz
.end_sz:
	mov cl, OSZ_DOS_PUTS
	mov dx, numbuff
	call _BDOS_entry

	mov al, ' '
	call _fast_conout

	mov cl, OSZ_DOS_PUTS	
	mov dx, dir_buff + 64
	call _BDOS_entry
	
	call _crlf
	
	pop ax
	jmp short .loop
.end:
	ret


_cmd_type:
	push ds
	push es

	mov dx, arg_buffer
	mov cl, OSZ_DOS_OPEN
	call _BDOS_entry
	test ax, ax
	jns .file_ok

	mov dx, nofile_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry

	jmp .end

.file_ok:

	mov si, ax

	lds bx,[cs:_osz_systbl]
	mov ax, [ds:bx+OSZ_SYSTBL_LASTMEM]
	mov ds, ax
	mov es, ax
	
	xor di,di
	mov cx, 0x8000
	xor ax, ax
	rep stosw
	
	xor dx, dx
	mov cl, OSZ_DOS_READ
	call _BDOS_entry

	mov cl, OSZ_DOS_CLOSE
	call _BDOS_entry

	xor dx, dx
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry

	call _crlf

.end:
	pop es
	pop ds
	ret


%if 0
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
	mov al, [_hex_tbl+bx]
	call _fast_conout
	pop cx
	loop .loop
	pop bx
	ret

_hex_tbl	db "0123456789abcdef"
%endif


_strlen:
	push cx
	push si
	mov si, dx
	xor cx, cx
.loop:
	lodsb
	or al, al
	jz short .end
	inc cx
	jmp short .loop
.end:
	mov ax, cx
	pop si
	pop cx
	ret




app_ext:
	db ".bin",0

cmd_table:
	db 2,"cd"
	dw _cmd_cd
	db 3,"ver"
	dw _cmd_ver
	db 3,"dir"
	dw _cmd_dir
	db 4,"type"
	dw _cmd_type
	db 4,"echo"
	dw _cmd_echo
	db 4,"exit"
	dw _cmd_exit
	db 0


ver_msg:
	db "MEG-OS OSZ ver 0.0.1.alpha", 10, 0

bad_cmd_msg:
	db "Bad command or file name", 10, 0

nofile_msg:
	db "No such file or directory", 10, 0

int00_msg	db "#DIV/0!", 10, 0

	alignb 16
_BSS:
