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

%define	MAGIC_WORD			0xE2C3

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
	cmp ax, OSZ_I3F_HANDLE_MAGIC
	jnz short .no_handle_magic
	cmp dx, MAGIC_WORD
	;jnz .no_handle_magic
	jz short _MAGIC_WORD_HANDLER
.no_handle_magic:
	db 0xEA
_int3F_old	dd 0

	; AX OSZ_I3F_HANDLE_MAGIC
	; CX SIZE OF BIN
	; DX MAGIC_WORD
	; ES:BX PTR_TO_BDOS_CALL
	; DS:SI PTR_TO_BIN typically SI=0100

%define	LOCAL_APP_SRC_SEG	-2
%define	LOCAL_APP_SRC_SIZE	-4
%define	LOCAL_APP_JIT_SEG	-6
%define	LOCAL_APP_JIT_SIZE	-8
%define	LOCAL_APP_LAST_ADDR	-10
%define	LOCAL_FOR_NEST_MAX	-12
%define	LOCAL_FOR_NEST_COUNT	-14
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

	xor ax, ax
	mov [bp+LOCAL_FOR_NEST_MAX], ax
	mov [bp+LOCAL_FOR_NEST_COUNT], ax

	lds cx, [bp+LOCAL_APP_SRC_SIZE]
	mov es, [bp+LOCAL_APP_JIT_SEG]
	mov si, 0x0100
	add cx, si
	xor di, di

	lodsw

_inst_loop:
	mov [bp+ LOCAL_APP_LAST_ADDR], si
	lodsb
	mov bl, al
	mov bh, 0x00
	add bx, bx
	call [cs:_op_table+bx]
	cmp cx, si
	je short _end_jit
	jb short _bytecode_overflow
	jmp short _inst_loop

	; noop
_op00:
	ret


_bytecode_overflow:
	push cs
	pop ds
	mov dx, bc_of_msg
	mov ah, OSZ_DOS_PUTS
	call _call_bdos
	mov dx, [bp+LOCAL_APP_LAST_ADDR]
	sub dx, 0x100
	call _disp_hex_16

	int 0x20


_end_jit:

	mov cx, [bp+LOCAL_FOR_NEST_COUNT]
	jcxz .no_more_end_for
.loop_end_for:
	call _op07
	loop .loop_end_for
.no_more_end_for:

	mov [bp+ LOCAL_APP_JIT_SIZE], di
	mov ax, 0x20CD
	stosw

	push cs
	pop ds
	mov dx, src_msg
	mov ah, OSZ_DOS_PUTS
	call _call_bdos

	lds cx, [bp+LOCAL_APP_SRC_SIZE]
	sub cx, 2
	mov si, 0x0102
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
	cli
	mov ss, bx
	xor sp, sp
	push bx
	push ax
	sti
	retf




	; for r0, r1
_op06:
	mov [cs:_for_addr], di
	mov al, 0xC8
	mov [cs:_for_regs], al
	inc word [bp+LOCAL_FOR_NEST_COUNT]
	ret


	; end for
_op07:
	mov al, [cs:_for_regs]
	and al, 0x03
	or al, 0x40 ; INC Rn
	stosb
	mov al, 0x3B ; CMP
	stosb
	mov al, [cs:_for_regs]
	stosb

	mov bx, [cs:_for_addr]
	lea dx, [di+2]
	sub bx, dx

	mov al, 0x7F ; JG
	stosb
	mov al, bl
	stosb

	dec word [bp+LOCAL_FOR_NEST_COUNT]

	ret


	; cout
_op0E:
	mov ax, 0x29CD
	stosw
	ret


	; ldc rni, i8
_op10:
_op11:
_op12:
_op13:
	and al, 0x03
	or al, 0xB8
	stosb
	lodsb
	cbw
	stosw
	ret


	; ldc rni, i16
_op14:
_op15:
_op16:
_op17:
	and al, 0x03
	or al, 0xB8
	stosb
	lodsw
	stosw
	ret


_op01:
_op02:
_op03:
_op04:
_op05:
_op08:
_op09:
_op0A:
_op0B:
_op0C:
_op0D:
_op0F:
_op18:
_op19:
_op1A:
_op1B:
_op1C:
_op1D:
_op1E:
_op1F:
_op20:
_op21:
_op22:
_op23:
_op24:
_op25:
_op26:
_op27:
_op28:
_op29:
_op2A:
_op2B:
_op2C:
_op2D:
_op2E:
_op2F:
_op30:
_op31:
_op32:
_op33:
_op34:
_op35:
_op36:
_op37:
_op38:
_op39:
_op3A:
_op3B:
_op3C:
_op3D:
_op3E:
_op3F:
_op40:
_op41:
_op42:
_op43:
_op44:
_op45:
_op46:
_op47:
_op48:
_op49:
_op4A:
_op4B:
_op4C:
_op4D:
_op4E:
_op4F:
_op50:
_op51:
_op52:
_op53:
_op54:
_op55:
_op56:
_op57:
_op58:
_op59:
_op5A:
_op5B:
_op5C:
_op5D:
_op5E:
_op5F:
_op60:
_op61:
_op62:
_op63:
_op64:
_op65:
_op66:
_op67:
_op68:
_op69:
_op6A:
_op6B:
_op6C:
_op6D:
_op6E:
_op6F:
_op70:
_op71:
_op72:
_op73:
_op74:
_op75:
_op76:
_op77:
_op78:
_op79:
_op7A:
_op7B:
_op7C:
_op7D:
_op7E:
_op7F:
_op80:
_op81:
_op82:
_op83:
_op84:
_op85:
_op86:
_op87:
_op88:
_op89:
_op8A:
_op8B:
_op8C:
_op8D:
_op8E:
_op8F:
_op90:
_op91:
_op92:
_op93:
_op94:
_op95:
_op96:
_op97:
_op98:
_op99:
_op9A:
_op9B:
_op9C:
_op9D:
_op9E:
_op9F:
_opA0:
_opA1:
_opA2:
_opA3:
_opA4:
_opA5:
_opA6:
_opA7:
_opA8:
_opA9:
_opAA:
_opAB:
_opAC:
_opAD:
_opAE:
_opAF:
_opB0:
_opB1:
_opB2:
_opB3:
_opB4:
_opB5:
_opB6:
_opB7:
_opB8:
_opB9:
_opBA:
_opBB:
_opBC:
_opBD:
_opBE:
_opBF:
_opC0:
_opC1:
_opC2:
_opC3:
_opC4:
_opC5:
_opC6:
_opC7:
_opC8:
_opC9:
_opCA:
_opCB:
_opCC:
_opCD:
_opCE:
_opCF:
_opD0:
_opD1:
_opD2:
_opD3:
_opD4:
_opD5:
_opD6:
_opD7:
_opD8:
_opD9:
_opDA:
_opDB:
_opDC:
_opDD:
_opDE:
_opDF:
_opE0:
_opE1:
_opE2:
_opE3:
_opE4:
_opE5:
_opE6:
_opE7:
_opE8:
_opE9:
_opEA:
_opEB:
_opEC:
_opED:
_opEE:
_opEF:
_opF0:
_opF1:
_opF2:
_opF3:
_opF4:
_opF5:
_opF6:
_opF7:
_opF8:
_opF9:
_opFA:
_opFB:
_opFC:
_opFD:
_opFE:
_opFF:
_inv_bc:
	push cs
	pop ds
	push ax
	mov dx, inv_bc_msg
	mov ah, OSZ_DOS_PUTS
	call _call_bdos
	mov dx, [bp+LOCAL_APP_LAST_ADDR]
	sub dx, 0x100
	call _disp_hex_16
	mov al, ':'
	int 0x29
	pop dx
	call _disp_hex_8

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


	alignb 16
_op_table:
	dw _op00,_op01,_op02,_op03,_op04,_op05,_op06,_op07,_op08,_op09,_op0A,_op0B,_op0C,_op0D,_op0E,_op0F,
	dw _op10,_op11,_op12,_op13,_op14,_op15,_op16,_op17,_op18,_op19,_op1A,_op1B,_op1C,_op1D,_op1E,_op1F,
	dw _op20,_op21,_op22,_op23,_op24,_op25,_op26,_op27,_op28,_op29,_op2A,_op2B,_op2C,_op2D,_op2E,_op2F,
	dw _op30,_op31,_op32,_op33,_op34,_op35,_op36,_op37,_op38,_op39,_op3A,_op3B,_op3C,_op3D,_op3E,_op3F,
	dw _op40,_op41,_op42,_op43,_op44,_op45,_op46,_op47,_op48,_op49,_op4A,_op4B,_op4C,_op4D,_op4E,_op4F,
	dw _op50,_op51,_op52,_op53,_op54,_op55,_op56,_op57,_op58,_op59,_op5A,_op5B,_op5C,_op5D,_op5E,_op5F,
	dw _op60,_op61,_op62,_op63,_op64,_op65,_op66,_op67,_op68,_op69,_op6A,_op6B,_op6C,_op6D,_op6E,_op6F,
	dw _op70,_op71,_op72,_op73,_op74,_op75,_op76,_op77,_op78,_op79,_op7A,_op7B,_op7C,_op7D,_op7E,_op7F,
	dw _op80,_op81,_op82,_op83,_op84,_op85,_op86,_op87,_op88,_op89,_op8A,_op8B,_op8C,_op8D,_op8E,_op8F,
	dw _op90,_op91,_op92,_op93,_op94,_op95,_op96,_op97,_op98,_op99,_op9A,_op9B,_op9C,_op9D,_op9E,_op9F,
	dw _opA0,_opA1,_opA2,_opA3,_opA4,_opA5,_opA6,_opA7,_opA8,_opA9,_opAA,_opAB,_opAC,_opAD,_opAE,_opAF,
	dw _opB0,_opB1,_opB2,_opB3,_opB4,_opB5,_opB6,_opB7,_opB8,_opB9,_opBA,_opBB,_opBC,_opBD,_opBE,_opBF,
	dw _opC0,_opC1,_opC2,_opC3,_opC4,_opC5,_opC6,_opC7,_opC8,_opC9,_opCA,_opCB,_opCC,_opCD,_opCE,_opCF,
	dw _opD0,_opD1,_opD2,_opD3,_opD4,_opD5,_opD6,_opD7,_opD8,_opD9,_opDA,_opDB,_opDC,_opDD,_opDE,_opDF,
	dw _opE0,_opE1,_opE2,_opE3,_opE4,_opE5,_opE6,_opE7,_opE8,_opE9,_opEA,_opEB,_opEC,_opED,_opEE,_opEF,
	dw _opF0,_opF1,_opF2,_opF3,_opF4,_opF5,_opF6,_opF7,_opF8,_opF9,_opFA,_opFB,_opFC,_opFD,_opFE,_opFF,


_for_addr	dw 0
_for_regs	db 0, 0


_hex_tbl	db "0123456789abcdef"

src_msg		db "SRC:",0
jit_msg		db "JIT:",0

inv_bc_msg	db "JIT ERROR: Invalid bytecode ", 0
bc_of_msg	db "JIT ERROR: Bytecode overflow at ", 0

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


