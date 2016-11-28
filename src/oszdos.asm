;;	-*- coding: utf-8 -*-
;;
;;	MEG OSZ - BDOS Interface
;;
;;	Copyright (c) 2014,2015 MEG-OS project
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


%define OSZ_BDOS	0x004A

%define	FAKE_DOSVER	0x0005
%define	PSP_PARENT	0x0016
%define	PSP_SETVER	0x0040

%define	MAX_CMDLINE	127
%define	MAX_CMD		127

%define	COM_ORG_100		0x0100
%define	MAX_COM_FILE	0xFF00

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

%define	_END		_BSS + 0x0500

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


CPU 8086
[bits 16]

_HEAD:
	db 0xCB, 0x1A
	dw _init


	alignb 4
_saved_sssp	dd 0

int21_function_table:
	dw int21_00, int21_01, int21_02, int21_03, int21_04, int21_05, int21_06,
	dw int21_07, int21_08, int21_09, int21_0A, int21_0B, int21_0C, int21_0D,
	dw int21_0E, int21_0F, int21_10, int21_11, int21_12, int21_13, int21_14,
	dw int21_15, int21_16, int21_17, int21_18, int21_19, int21_1A, int21_1B,
	dw int21_1C, int21_1D, int21_1E, int21_1F, int21_20, int21_21, int21_22,
	dw int21_23, int21_24, int21_25, int21_26, int21_27, int21_28, int21_29,
	dw int21_2A, int21_2B, int21_2C, int21_2D, int21_2E, int21_2F, int21_30,
	dw int21_31, int21_32, int21_33, int21_34, int21_35, int21_36, int21_37,
	dw int21_38, int21_39, int21_3A, int21_3B, int21_3C, int21_3D, int21_3E,
	dw int21_3F, int21_40, int21_41, int21_42, int21_43, int21_44, int21_45,
	dw int21_46, int21_47, int21_48, int21_49, int21_4A, int21_4B, int21_4C,
	dw int21_4D, int21_4E, int21_4F, int21_50, int21_51, int21_52, int21_53,
	dw int21_54, int21_55, int21_56, int21_57, int21_58, int21_59, int21_5A,
	dw int21_5B, int21_5C, int21_5D, int21_5E, int21_5F, int21_60, int21_61,
	dw int21_62, int21_63, int21_64, int21_65, int21_66, int21_67, int21_68,
	dw int21_69, int21_6A, int21_6B, int21_6C, int21_6D, int21_6E, int21_6F,
end_int21_function:


_BDOS_function_table:
	dw _BDOS_00, _BDOS_01, _BDOS_02, _BDOS_03, _BDOS_04, _BDOS_05, _BDOS_06, _BDOS_07
	dw _BDOS_08, _BDOS_09, _BDOS_0A, _BDOS_0B, _BDOS_0C, _BDOS_0D, _BDOS_0E, _BDOS_0F
_END_BDOS_function:

_BDOS_over:
	mov ax, 0xFFFF
	ret

_BDOS_entry:
	cmp ah, (_END_BDOS_function-_BDOS_function_table)/2
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

	mov bl, ah
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
	xor al, al
	iret

_int21:
	cmp ah, (end_int21_function-int21_function_table)/2
	jae short _int21_over
	cmp ah, 0x62
	ja short .out_of_5052
	jz short .int21_5162
	cmp ah, 0x52
	ja short .out_of_5052
	jnz short .no_52
.int21_52: ; GET LOL (DUMMY)
	xor bx, bx
	mov es, bx
	mov bx, 0xFFFF
	iret
.no_52:
	cmp ah, 0x50
	jb short .out_of_5052
	jnz short .int21_5162

.int21_50: ; SET CURRENT PSP
	mov [cs:_CURRENT_PSP], bx
	iret

.int21_5162: ; GET CURRENT PSP
	mov bx,[cs:_CURRENT_PSP]
	iret

.out_of_5052:

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
	clc

	call [cs:int21_function_table + bx]

	mov [bp+STK_AX], ax
	sbb al, al
	and al, 0x01
	mov [bp+STK_FLAGS], al

	pop ax
	pop cx
	pop dx
	pop bx
	pop si
	pop di
	pop bp
	pop ds
	pop es
int21_50:
int21_51:
int21_52:
int21_62:
_int_nop:
	iret


	; DUMMY DOS CALLS
int21_03:
int21_04:
int21_05:
int21_2E:	; SET VERIFY
int21_54:	; GET VERIFY
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
_BDOS_0C:
_BDOS_0D:
_BDOS_0E:
_BDOS_0F:
	mov ax, 0xFFFF
	ret


_psp_bdos:
	call _BDOS_entry
	retf


_crlf:
	mov al, 10
	int 0x29
	ret


_force_bs:
	mov al, 8
	int 0x29
	mov al, ' '
	int 0x29
	mov al, 8
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


	; BEEP
_BDOS_09:
	mov ah, BIOS_BEEP
	jmp short _call_bios




	; CONIN BUFFERED
	; IN cx = limit ds:dx = buffer
	; OUT ax = length
_BDOS_05:
	push bx
	push si
	push di

	mov di, cx
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
	call _force_bs
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
	call _force_bs
.bs_printchar:
	call _force_bs
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




	; SYSINFO
_BDOS_08:
	les bx,[cs:_osz_systbl]
	mov ax, [es:bx + OSZ_SYSTBL_REVISION]
	mov [bp+STK_BX], ax
	mov cl, [es:bx + OSZ_SYSTBL_CPUID]
	mov ch, [es:bx + OSZ_SYSTBL_ARCH]
	mov [bp+STK_CX], cx
	xor dx, dx
	mov [bp+STK_DX], dx
	mov ax, [es:bx + OSZ_SYSTBL_VERSION]
	ret


	; GET TICK
_BDOS_0A:
	mov ah, BIOS_GET_TICK
	call _call_bios
	mov [bp+STK_CX], cx
	mov [bp+STK_DX], dx
	ret




	; WAIT TICK
_BDOS_0B:
	mov bx, cx

	mov ah, BIOS_GET_TICK
	call _call_bios
	mov si, ax
	mov di, dx

.loop:
	int 0x28
	mov ah, BIOS_GET_TICK
	call _call_bios

	sub ax, si
	sbb dx, di
	or dx, dx
	jnz .end

	mul cx
	or dx, dx
	jnz .end
	cmp ax, bx
	jb .loop

.end:
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
	mov cl, [bx]
	xor ch, ch
	inc dx
	inc dx
	call _BDOS_05
	mov [bx+1], al
	ret


	; SET IVT
int21_25:
	xor bx, bx
	mov es, bx
	mov bl, al
	add bx, bx
	add bx, bx
	mov [es:bx], dx
	mov [es:bx+2], ds
	ret


	; GET IVT
int21_35:
	xor bx, bx
	mov es, bx
	mov bl, al
	add bx, bx
	add bx, bx
	les bx, [es:bx]
	mov [bp+STK_BX], bx
	mov [bp+STK_ES], es
	ret


	; GET FAKED DOS VER
int21_30:
	mov es, [cs:_CURRENT_PSP]
	mov ax, [es:PSP_SETVER]
	xor cx, cx
	mov [bp+STK_BX], cx
	mov [bp+STK_CX], cx
	ret


	; CREATE NEW PSP
int21_26:
	push es
	push ds

	mov es, dx

	xor di, di
	mov ax, 0x20CD ; INT 20
	stosw
	xor ax,ax ; TODO: MEMSZ
	stosw

	xor al,al
	stosb
	mov ax, 0xFFFF
	stosb
	stosw
	stosw

	xor ax, ax
	mov ds, ax
	mov si, 0x22*4
	mov cx, 6
	rep movsw

	;xor ax, ax
	mov cx, 0x0075
	rep stosw

	mov di, OSZ_BDOS
	mov al, 0x9A
	stosb
	mov ax, _psp_bdos
	stosw
	mov ax, cs
	stosw
	mov al, 0xC3
	stosb

	mov di, PSP_SETVER
	mov ax, FAKE_DOSVER
	stosw

	pop ds
	pop es
	ret


	; FCB FUNCTIONS
int21_0F:	; FCB OPEN
int21_10:	; FCB CLOSE
int21_11:	; FCB FIND FIRST
int21_12:	; FCB FIND NEXT
int21_13:	; FCB DELETE
int21_14:	; FCB READ SEQUENTIAL
int21_15:	; FCB WRITE SEQUENTIAL
int21_16:	; FCB CREATE OR TRUNCATE
int21_17:	; FCB RENAME
int21_21:	; FCB READ RANDOM
int21_22:	; FCB WRITE RANDOM
int21_23:	; FCB GET FILE SIZE
int21_24:	; FCB SET RANDOM RECORD
int21_27:	; FCB RANDOM BLOCK READ
int21_28:	; FCB RANDOM BLOCK WRITE
	mov al, 0xFF
	ret


int21_39:	; mkdir
int21_3A:	; rmdir
int21_41:	; unlink

int21_3B:	; set cwd

int21_42:	; seek
int21_43:	; get/set file attrs
int21_44:	; ioctl
int21_45:	; dup
int21_46:	; dup2
int21_47:	; get cwd
int21_60:	; truename
	mov al, 0x05
	stc
	ret


int21_3C:	; creat
	mov si, 0x0012
	jmp short _open

int21_3D:	; open
	mov si, 0x0001
	jmp short _open

int21_5B:	; create new file
	mov si, 0x0010
	jmp short _open

int21_6C:	; CreateFileEx
	xchg dx, si

_open:
	mov al, OSZ_IFS_OPEN
	mov ah, OSZ_I3F_IFS
	int 0x3F
	test ax, ax
	js .not_exist
	clc
	ret

.not_exist:
	mov al, 0x02
	stc
	ret

int21_3E:	; CloseHandle
	mov si, [bp+STK_BX]
	mov al, OSZ_IFS_CLOSE
	mov ah, OSZ_I3F_IFS
	int 0x3F
	xor ax, ax
	ret

int21_3F:	; ReadFile
	sub sp, 4

	mov si, [bp+STK_BX]
	mov al, OSZ_IFS_READ
	mov ah, OSZ_I3F_IFS
	int 0x3F
	or ax, ax
	js .error

	push ss
	pop es
	mov di, sp
	mov si, [bp+STK_BX]
	mov cx, OSZ_IFS_IOCTL_GET_FILE_SIZE
	mov al, OSZ_IFS_IOCTL
	mov ah, OSZ_I3F_IFS
	int 0x3F
	or ax, ax
	js .error

	mov ax, [es:di]
	add sp, 4
	ret

.error:
	xor ax, ax
	add sp, 4
	ret

int21_40:	; WriteFile
	mov al, 0x05
	stc
	ret

int21_5A:	; create temp file
	mov al, 0x02
	stc
	ret



	; TODO: DOS
int21_0D:
int21_0E:
int21_18:	; NULL FOR CP/M
int21_19:	; GET CURRENT DRIVE
int21_1A:	; GET DTA
int21_1B:
int21_1C:
int21_1D:	; NULL FOR CP/M
int21_1E:	; NULL FOR CP/M
int21_1F:
int21_20:	; NULL FOR CP/M
int21_29:	; PARSE FCB FILENAME
int21_2A:	; GET SYSTEM DATE
int21_2B:	; SET SYSTEM DATE
int21_2C:	; GET SYETEM TIME
int21_2D:	; SET SYSTEM TIME
int21_2F:
int21_31:
int21_32:
int21_33:
int21_34:
int21_36:
int21_37:
int21_38:
int21_48:	; malloc
int21_49:	; free
int21_4A:	; realloc
int21_4B:
int21_4D:
int21_4E:
int21_4F:
int21_53:
int21_55:
int21_56:
int21_57:
int21_58:
int21_59:

int21_5C:
int21_5D:
int21_5E:
int21_5F:
int21_61:
int21_63:
int21_64:
int21_65:
int21_66:
int21_67:
int21_68:	; commit file
int21_69:
int21_6A:	; commit file
int21_6B:
int21_6D:
int21_6E:
int21_6F:
	mov ax, 0xFFFF
	stc
	ret


	; FAST CONSOLE OUTPUT
_int29:
	push ax
	mov ah, BIOS_CONOUT
	call _call_bios
	pop ax
	iret


%define	SIZE_MAINSTACK	16
%define	LOCAL_SAVED_HANDLE		-2
%define	LOCAL_SIZE_CMDLINE		-4
%define	LOCAL_SIZE_CMD			-6
%define	LOCAL_SIZE_ARG			-8
%define	LOCAL_SIZE_APP_BIN		-10
%define	LOCAL_MEMSZ				-12

_init:
	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

	mov ax, cs
	add ax, (_END-_HEAD)/16
	mov [_DOS_LASTMEM], ax

	xor di, di
	mov ax, _int00
	stosw
	mov ax, cs
	stosw
	mov ax, _int01
	stosw
	mov ax, cs
	stosw
	mov di, 0x03*4
	mov ax, _int03
	stosw
	mov ax, cs
	stosw
	mov ax, _int04
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

	mov di, 0x29*4
	mov ax, _int29
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

	mov dx, [cs:_DOS_LASTMEM]
	mov [_ROOT_PSP], dx
	mov [_CURRENT_PSP], dx
	mov ax, dx
	add ax, 0x0011
	mov [cs:_DOS_LASTMEM], ax
	call int21_26

	push cs
	pop es

	mov [_saved_sssp], sp
	mov [_saved_sssp+2], ss

	mov bp, sp
	sub sp, SIZE_MAINSTACK

	call _cmd_mem

	mov cx, 2000
	call _beep
	mov cx, 1000
	call _beep

	call _cmd_ver

	jmp short _loop

_beep:
	mov ah, BIOS_BEEP
	call _call_bios

	;mov ah, OSZ_DOS_WAIT_TICK
	mov cx, 200
	call _BDOS_0B

	xor cx, cx
	mov ah, BIOS_BEEP
	call _call_bios
	ret

_int27: ; DOS1+ TERMINATE AND STAY RESIDENT
	or dx, dx
	jz short _BDOS_00
	add dx, byte 0x000F
	sbb ax, ax
	mov cl, 4
	shr dx, cl
	sub dx, ax
	add [cs:_DOS_LASTMEM], dx
	jmp short _BDOS_00


_int00: ; INTEGER DIVIDE BY ZERO
	mov dx, int00_msg
	jmp short _abort_w_msg

_int01: ; DEBUG FAULT
	mov dx, int01_msg
	jmp short _abort_w_msg

_int03: ; BREAK POINT
	mov dx, int03_msg
	jmp short _abort_w_msg

_int04: ; INTO DETECTED OVERFLOW
	mov dx, int04_msg
_abort_w_msg:
	push cs
	pop ds
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	;jmp short _BDOS_00

int21_00: ; exit
int21_4C: ; DOS2+ exit with return code
_BDOS_00: ; EXIT
	cli
	cld

	mov ds, [cs:_CURRENT_PSP]
	mov dx, [ds:PSP_PARENT]
	mov [cs:_CURRENT_PSP], dx

	mov cx, cs
	mov ds, cx
	mov es, cx
	mov ss,[_saved_sssp+2]
	mov sp,[_saved_sssp]
	mov bp, sp
	sub sp, SIZE_MAINSTACK
	call _crlf

	;jmp short _loop

_loop:
	mov si, str_buff
	mov al, OSZ_IFS_GET_CWD
	mov ah, OSZ_I3F_IFS
	int 0x3F
	lea dx, [si+2]
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	mov al, '>'
	int 0x29

	mov cx, MAX_CMDLINE
	mov dx, cmdline
	mov ah, OSZ_DOS_GETS
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
	mov ah, 0x3D
	int 0x21
	jnc .load_exec

.bad_cmd:
	mov dx, bad_cmd_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry

	jmp _loop

.load_exec:

	mov [bp+LOCAL_SAVED_HANDLE], ax

	les bx,[cs:_osz_systbl]
	mov ax, [es:bx+OSZ_SYSTBL_MEMSZ]
	mov [bp+LOCAL_MEMSZ], ax

	mov dx, [cs:_DOS_LASTMEM]
	call int21_26

	mov ax, [cs:_CURRENT_PSP]
	mov es, dx
	mov [cs:_CURRENT_PSP], dx
	mov [es:PSP_PARENT], ax

	mov di, 0x0080
	mov si, arg_buffer
	mov ax, [bp+LOCAL_SIZE_ARG]
	or ax, ax
	jnz .arg_nosp
	stosb
	jmp short .arg_end

.arg_nosp:
	mov cx, ax
	inc al
	stosb
	mov al, ' '
	stosb
	rep movsb

.arg_end:
	mov al, 0x0D
	stosb

	push es
	pop ds

	mov dx, COM_ORG_100
	mov cx, MAX_COM_FILE
	mov bx, [bp+LOCAL_SAVED_HANDLE]
	mov ah, 0x3F
	int 0x21
	;mov [bp+LOCAL_SIZE_APP_BIN], ax
	mov di, ax

	mov ah, 0x3E
	int 0x21

	push cs
	pop es
	mov cx, di
	mov si, COM_ORG_100
	mov bx, _psp_bdos
	mov ax, [si]
	cmp ax, 0xED31 ; XOR BP, BP - format1
	jz short .magic_ok
	cmp ax, 0xED33 ; XOR BP, BP - format2
	jz short .magic_ok
	mov dx, ax
	mov ax, OSZ_I3F_HANDLE_MAGIC
	int 0x3F

.bad_magic:
	push cs
	pop ds
	mov dx, bad_magic_found_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	jmp _BDOS_00

.magic_ok:
	mov dx, ds
	cli
	mov ss, dx
	xor sp, sp

	xor ax, ax
	;xor cx, cx
	xor dx, dx
	xor bx, bx
	push ax
	push ds
	inc si
	inc si
	push si
	push ds
	pop es

	mov bp, OSZ_BDOS
	xor si, si
	xor di, di
	sti
	retf




_cmd_exit:

	les bx, [_osz_systbl]
	mov ax, [es:bx+OSZ_SYSTBL_ACPI]
	or ax, ax
	jz .no_acpi
	mov ah, 5
	call far [es:bx+OSZ_SYSTBL_ACPI]
.no_acpi:

	mov ax, BIOS_POWER * 0x100
	call _call_bios
	ret


_cmd_ver:
	push es

	mov dx, ver_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry

	les bx,[cs:_osz_systbl]
	mov si, [es:bx + OSZ_SYSTBL_REVISION]
	mov bx, [es:bx + OSZ_SYSTBL_VERSION]

	mov dl, bl
	xor dh, dh
	call _disp_dec
	mov al, '.'
	int 0x29

	mov dl, bh
	xor dh, dh
	call _disp_dec
	mov al, '.'
	int 0x29

	mov dx, si
	call _disp_dec
	mov al, 10
	int 0x29

	pop es
	ret


_cmd_reserved:
	mov dx, cmd_reserved_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	ret


_cmd_echo:
	mov dx,arg_buffer
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	call _crlf
	ret


_cmd_cd:
	mov dx,arg_buffer
	mov al, OSZ_IFS_SET_CWD
	mov ah, OSZ_I3F_IFS
	int 0x3F
	ret


_cmd_cls:
	mov ah, BIOS_CLS
	call _call_bios
	ret


_cmd_mem:
	push es
	les bx, [_osz_systbl]

	mov dx, mem_1_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	mov dx, [es:bx+OSZ_SYSTBL_MEMSZ]
	sub dx, [cs:_DOS_LASTMEM]
	mov cl, 6
	shr dx, cl
	call _disp_dec

	mov dx, mem_2_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	mov dx, [es:bx+OSZ_SYSTBL_MEMSZ]
	mov cl, 6
	shr dx, cl
	call _disp_dec
	mov dx, mem_kb_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry

	mov ax, [es:bx+OSZ_SYSTBL_MEMPROT]
	or ax, ax
	jz .no_protmem
	mov dx, mem_prot_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry
	mov dx, [es:bx+OSZ_SYSTBL_MEMPROT]
	call _disp_dec
	mov dx, mem_kb_msg
	mov ah, OSZ_DOS_PUTS
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

	mov di, numbuff
	mov ax, 0x2020
	stosw
	stosw
	xor ah, ah
	stosw

	lea bx, [di-2]
	mov ax, dx
	mov cx, 10
.loop_sz:
	xor dx, dx
	div cx
	add dl, '0'
	mov [bx], dl
	dec bx
	jz .end_sz
	or ax, ax
	jz .end_sz
	jmp short .loop_sz
.end_sz:

	mov bx, numbuff
	mov al, ' '
.loop_sp:
	cmp [bx], al
	jnz short .end_sp
	inc bx
	jmp short .loop_sp

.end_sp:
	mov dx, bx
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry

	pop di
	pop bx
	pop cx
	pop es
	ret




_cmd_dir:
	xor bx, bx
.loop:
	mov dx, dir_buff
	mov al, OSZ_IFS_ENUM_FILE
	mov ah, OSZ_I3F_IFS
	int 0x3F
	cmp ax, 0
	jg short .continue
	jmp .end
.continue:
	xchg ax, bx
	cmp byte [dir_buff+64], '.'
	jz short .loop
	push bx

	mov al, ' '
	int 0x29

	mov si, dir_buff
	mov cx, 8
.loop_fn8:
	lodsb
	int 0x29
	loop .loop_fn8
	mov al, '.'
	int 0x29
	mov cx, 3
.loop_fn3:
	lodsb
	int 0x29
	loop .loop_fn3
	mov al, ' '
	int 0x29
	int 0x29

	mov dx, [dir_buff+0x18]

	mov ax, dx
	mov cl, 9
	shr ax, cl
	add ax, 1980
	call _disp_dec_04

	mov al, '-'
	int 0x29

	mov ax, dx
	mov cl, 5
	shr ax, cl
	db 0x25, 0x0F, 0x00 ; and ax, 0x000F
	call _disp_dec_02

	mov al, '-'
	int 0x29

	mov ax, dx
	db 0x25,  0x1F, 0x00 ; and ax, 0x001F
	call _disp_dec_02

	mov al, ' '
	int 0x29

	mov dx, [dir_buff+0x16]

	mov ax, dx
	mov cl, 11
	shr ax, cl
	call _disp_dec_02

	mov al, ':'
	int 0x29

	mov ax, dx
	mov cl, 5
	shr ax, cl
	db 0x25, 0x3F, 0x00 ; and ax, 0x003F
	call _disp_dec_02

	mov al, ' '
	int 0x29
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
	mov dx, numbuff
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry

	mov al, ' '
	int 0x29

	mov dx, dir_buff + 64
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry

	call _crlf

	pop bx
	jmp .loop
.end:
	ret

_disp_dec_02:
	mov cl, 10
	div cl
	or ax, '00'
	int 0x29
	xchg al, ah
	int 0x29
	ret

_disp_dec_04:
	push dx

	xor dx, dx
	mov cx, 1000
	div cx
	or al, '0'
	int 0x29

	mov ax, dx
	xor dx, dx
	mov cx, 100
	div cx
	or al, '0'
	int 0x29

	mov ax, dx
	xor dx, dx
	mov cl, 10
	div cl
	or ax, '00'
	int 0x29
	mov al, ah
	int 0x29

	pop dx
	ret




_cmd_type:
	push ds
	push es

	mov dx, arg_buffer
	mov ah, 0x3D
	int 0x21
	jnc .file_ok

	mov dx, nofile_msg
	mov ah, OSZ_DOS_PUTS
	call _BDOS_entry

	jmp .end

.file_ok:

	mov bx, ax

	mov ax, [cs:_DOS_LASTMEM]
	mov ds, ax
	mov es, ax

	xor di,di
	mov cx, 0x8000
	xor ax, ax
	rep stosw

	xor dx, dx
	mov cx, 0xFFFF
	mov ah, 0x3F
	int 0x21

	mov cx, ax
	xor si, si
.loop:
	jcxz .loop_end
	lodsb
	int 0x29
	dec cx
	jmp .loop
.loop_end:

	mov ah, 0x3E
	int 0x21

	call _crlf

.end:
	pop es
	pop ds
	ret


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
	db 2, "cd"
	dw _cmd_cd
	db 3, "cls"
	dw _cmd_cls
	db 3, "dir"
	dw _cmd_dir
	db 3, "mem"
	dw _cmd_mem
	db 3, "ver"
	dw _cmd_ver
	db 4, "echo"
	dw _cmd_echo
	db 4, "exit"
	dw _cmd_exit
	db 4, "type"
	dw _cmd_type
	db 4, "date"
	dw _cmd_reserved
	db 4, "time"
	dw _cmd_reserved
	db 3, "del"
	dw _cmd_reserved
	db 3, "ren"
	dw _cmd_reserved
	db 0


ver_msg:
	db "MEG-OS Z ver ", 0

bad_cmd_msg:
	db "Bad command or file name", 10, 0

bad_magic_found_msg:
	db "Bad MAGIC found", 10, 0

nofile_msg:
	db "No such file or directory", 10, 0

cmd_reserved_msg:
	db "Feature not available", 10, 0

uname_msg:
	db "osz", 10, 0

mem_1_msg		db "MEMORY: ", 0
mem_2_msg		db "/", 0
mem_prot_msg	db "EXTEND: ", 0
mem_kb_msg		db " KB", 10, 0

int00_msg	db 10, "#DIV/0!", 10, 0
int01_msg	db 10, "#DEBUG", 10, 0
int03_msg	db 10, "#BREAKPOINT", 10, 0
int04_msg	db 10, "#OVERFLOW", 10, 0

_DOS_LASTMEM	dw 0
_ROOT_PSP		dw 0
_CURRENT_PSP	dw 0

	alignb 16
_BSS:
