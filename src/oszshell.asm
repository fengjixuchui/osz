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


%define	INT_DOS_VERSION		0x3205

%define OSZ_BDOS	0x000A

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

%define	STK_AX					0
%define	STK_CX					2
%define	STK_DX					4
%define	STK_BX					6
%define	STK_SI					8
%define	STK_DI					10
%define	STK_BP					12
%define	STK_DS					14
%define	STK_ES					16
%define	STK_IP					18
%define	STK_CS					20
%define	STK_FLAGS				22


[bits 16]

_HEAD:
	db 0xC3, 0x5A
	dw (_BSS-_HEAD)/16

	jmp _crt


	alignb 4
_saved_sssp	dd 0

_BDOS_function_table:
	dw _BDOS_00
	dw _BDOS_01
	dw _BDOS_02
	dw _BDOS_03
	dw _BDOS_04
	dw _BDOS_05
	dw _BDOS_06
	dw _BDOS_07
	dw _BDOS_08
	dw _BDOS_09
	dw _BDOS_0A
	dw _BDOS_0B
	dw _BDOS_0C
	dw _BDOS_0D
	dw _BDOS_0E
	dw _BDOS_0F
	dw _BDOS_10
	dw _BDOS_11
	dw _BDOS_12
	dw _BDOS_13
	dw _BDOS_14
	dw _BDOS_15
	dw _BDOS_16
	dw _BDOS_17
	dw _BDOS_18
	dw _BDOS_19
	dw _BDOS_1A
	dw _BDOS_unknown-_HEAD
_END_BDOS_function:

int21_function_table:
	dw _BDOS_00
	dw int21_01
	dw int21_02
	dw int21_03
	dw int21_04
	dw int21_05
	dw int21_06
	dw int21_07
	dw int21_08
	dw int21_09
	dw int21_0A
	dw int21_0B
	dw int21_0C
end_int21_function:


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
	xor bh, bh
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


_int21_over:
	mov al, 0xFF
	iret

_int21:
	cmp ah, (end_int21_function-int21_function_table)/2
	jae short _int21_over
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

	mov bl, ah
	xor bh, bh
	add bx, bx
	call [cs:int21_function_table + bx]

	lea sp,[bp+2]
	pop cx
	pop dx
	pop bx
	pop si
	pop di
	pop bp
	pop ds
	pop es
_int_nop:
	iret


	; DUMMY DOS CALLS
int21_03:
int21_04:
int21_05:
	xor al, al
	ret


_int2526: ; DOS1+ ABSOLUTE DISK I/O (DUMMY)
	mov ax, 0x0207
	stc
	retf 2


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


_psp_bdos:
	call _BDOS_entry
	retf


_crlf:
	mov al, 10
	int 0x29
	ret


_call_bios:
	db 0x9A
_osz_systbl	dd 0
	ret


	; CONIN WITH ECHO
int21_01:
	call _BDOS_01
	int 0x29
	ret

	; CONIN
int21_07:
int21_08:
_BDOS_01:
	mov ah, BIOS_CONIN
	jmp short _call_bios


	; CONOUT
int21_02:
_BDOS_02:
	mov al, dl
	int 0x29
	ret

	; DIRECT CONSOLE IO
int21_06:
	cmp dl, 0xFF
	jnz _BDOS_02
	; TODO: NOT SUPPORTED
	xor al, al
	ret

	; CONST
_BDOS_03:
	mov ah, BIOS_CONST
	jmp short _call_bios


	; CONOUT STRING
_BDOS_04:
	mov si, dx
.loop:
	lodsb
	or al,al
	jz short .end
	int 0x29
	jmp short .loop
.end:
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
	int 0x28
	jmp short .main_loop

.has_key:
	mov ah, BIOS_CONIN
	call _call_bios
	
	cmp al, 0x03
	jnz short .no_break

	mov al, '^'
	int 0x29
	mov al, 'C'
	int 0x29

	jmp _BDOS_00
	;xor bx,bx
	;jmp short .end
	
.no_break:

	cmp al, 0x0D ; cr
	jz short .crlf
	cmp al, 0x0A ; lf
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
	int 0x29
	mov al, ' '
	int 0x29
	mov al, 8
	int 0x29
	
	jmp short .loop_esc

.no_esc:

	cmp al, 0x08 ; ascii backspace
	jz short .backspace
	cmp al, 0x7F ; some unicses standard backspace
	jnz short .no_bs

.backspace:
	or bx, bx
	jz short .main_loop

	dec bx
	mov al, [si+bx]
	cmp al, 0x20
	jnc .bs_printchar
	
	mov al, 8
	int 0x29
	mov al, ' '
	int 0x29
	mov al, 8
	int 0x29

.bs_printchar:

	mov al, 8
	int 0x29
	mov al, ' '
	int 0x29
	mov al, 8
	int 0x29
	
	jmp short .main_loop

.no_bs:

	cmp bx, di
	jnc .main_loop

	mov [si+bx], al
	inc bx
	cmp al, 0x20
	jc .print_ctrl
	int 0x29
	jmp .main_loop

.print_ctrl:
	xchg ax, cx
	mov al, '^'
	int 0x29
	xchg ax, cx
	add al, 0x40
	int 0x29
	jmp .main_loop

.end:
	cmp bx, di
	jae short .no_last_nul
	xor cl, cl
	mov [si+bx],cl
.no_last_nul:
	mov ax,bx

	pop di
	pop si
	pop bx
	
	ret




	; SYSINFO
_BDOS_08:
	les bx,[cs:_osz_systbl]
	mov ax, [es:bx + OSZ_SYSTBL_VERSION]
	mov [bp+STK_BX], ax
	mov cl, [es:bx + OSZ_SYSTBL_CPUID]
	mov ch, [es:bx + OSZ_SYSTBL_ARCH]
	mov [bp+STK_CX], cx
	xor dx, dx
	mov [bp+STK_DX], dx
	mov ax, INT_DOS_VERSION
	ret




	; CONST
int21_0B:
	call _BDOS_03
	and al, al
	jz .no_char
	mov al, 0xFF
.no_char:
	ret


	; FLUSH KEYBOARD
int21_0C:
	; TODO: NOT SUPPORTED
	ret

	; DOS PUTS
int21_09:
	mov si, dx
.loop:
	lodsb
	cmp al, '$'
	jz short .end
	int 0x29
	jmp short .loop
.end:
	ret

	; DOS GETS
int21_0A:
	mov bx, dx
	mov al, [bx]
	xor ah, ah
	inc dx
	inc dx
	call _BDOS_05
	mov [bx+1], al
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






%define	SIZE_MAINSTACK	16
%define	LOCAL_SAVED_HANDLE		-2
%define	LOCAL_SIZE_CMDLINE		-4
%define	LOCAL_SIZE_CMD			-6
%define	LOCAL_SIZE_ARG			-8
%define	LOCAL_MEMSZ				-10

_int27: ; DOS1+ TERMINATE AND STAY RESIDENT
	or dx, dx
	jz short _BDOS_00
	add dx, byte 0x000F
	sbb ax, ax
	mov cl, 4
	shr dx, cl
	sub dx, ax
	les bx, [cs:_osz_systbl]
	add [es:bx + OSZ_SYSTBL_LASTMEM], dx
	jmp short _BDOS_00


_int00: ; INTEGER DIVIDE BY ZERO
	push cs
	pop ds
	mov dx, int00_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
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
	jmp short _loop

_crt:
	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

	mov ax, cs
	add ax, (_END-_HEAD)/16
	mov [es:bx+OSZ_SYSTBL_LASTMEM], ax

	xor di, di
	mov ax, _int00
	stosw
	mov ax, cs
	stosw
	mov di, 0x20*4
	mov ax, _BDOS_00
	stosw
	mov ax, cs
	stosw
	mov ax, _int21
	stosw
	mov ax, cs
	stosw
	
	mov cx, 3 ; 22 23 24
.loop_int_nop:
	mov ax, _int_nop
	stosw
	mov ax, cs
	stosw
	loop .loop_int_nop

	mov ax, _int2526
	stosw
	mov ax, cs
	stosw
	mov ax, _int2526
	stosw
	mov ax, cs
	stosw
	mov ax, _int27
	stosw
	mov ax, cs
	stosw

	push cs
	pop es
	mov cx, (_END-_BSS)/2
	mov di, _BSS
	xor ax, ax
	rep stosw

	mov [_saved_sssp], sp
	mov [_saved_sssp+2], ss

	mov bp, sp
	sub sp, SIZE_MAINSTACK

	call _cmd_ver
	call _cmd_mem
_loop:
	mov si, str_buff
	mov cl, OSZ_DOS_GET_CWD
	call _BDOS_entry
	lea dx, [si+2]
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
	mov al, '>'
	int 0x29

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
	mov ax, [es:bx+OSZ_SYSTBL_MEMSZ]
	mov [bp+LOCAL_MEMSZ], ax

	mov es, [es:bx+OSZ_SYSTBL_LASTMEM]
%if 0
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
%endif
	
	; create psp
	xor di, di
	xor ax, ax
	mov cx, 0x8000
	rep stosw
	
	;mov [es:0x2C], bx

	;xor di, di
	mov ax, 0x20CD ; INT 20
	stosw
	mov ax, [bp+LOCAL_MEMSZ]
	dec ax
	stosw
	
	xor al,al
	stosb
	mov ax, 0xFFFF
	stosb
	stosw
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


_cmd_cls:
	mov ah, BIOS_CLS
	call _call_bios
	ret


_cmd_mem:
	push es
	les bx, [_osz_systbl]

	mov dx, mem_1_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
	mov dx, [es:bx+OSZ_SYSTBL_MEMSZ]
	sub dx, [es:bx+OSZ_SYSTBL_LASTMEM]
	mov cl, 6
	shr dx, cl
	call _disp_dec

	mov dx, mem_2_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
	mov dx, [es:bx+OSZ_SYSTBL_MEMSZ]
	mov cl, 6
	shr dx, cl
	call _disp_dec
	mov dx, mem_kb_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry

	mov ax, [es:bx+OSZ_SYSTBL_MEMPROT]
	or ax, ax
	jz .no_protmem
	mov dx, mem_prot_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
	mov dx, [es:bx+OSZ_SYSTBL_MEMPROT]
	call _disp_dec
	mov dx, mem_kb_msg
	mov cl, OSZ_DOS_PUTS
	call _BDOS_entry
.no_protmem:

	pop es
	ret


_disp_dec:
	push es
	push ds
	pop es
	push cx
	push bx
	push di
	
	mov di,numbuff
	mov ax, 0x2020
	stosw
	stosw
	xor ah, ah
	stosw
	mov ax, dx
	xor dx, dx
	mov cx, 10
	lea bx, [di-2]
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
	
	pop di
	pop bx
	pop cx
	pop es
	ret


mem_1_msg		db "Memory: ", 0
mem_2_msg		db " KB / ", 0
mem_prot_msg	db "Extend: ", 0
mem_kb_msg		db " KB", 10, 0




_cmd_dir:
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
	int 0x29

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
	int 0x29

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


%if 1
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
	int 0x29
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
	db ".com",0

cmd_table:
	db 2,"cd"
	dw _cmd_cd
	db 3,"cls"
	dw _cmd_cls
	db 3,"dir"
	dw _cmd_dir
	db 3,"mem"
	dw _cmd_mem
	db 3,"ver"
	dw _cmd_ver
	db 4,"echo"
	dw _cmd_echo
	db 4,"exit"
	dw _cmd_exit
	db 4,"type"
	dw _cmd_type
	db 0


ver_msg:
	db "MEG-OS Z ver 0.0.2", 10, 0

bad_cmd_msg:
	db "Bad command or file name", 10, 0

nofile_msg:
	db "No such file or directory", 10, 0

int00_msg	db 10, "#DIV/0!", 10, 0


	alignb 16
_BSS:
