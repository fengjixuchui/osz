;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Zero - Second boot loader
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

%define	ORG_BASE			0x0C00
%define	_osz_system_table	0x0800

[bits 16]
[org ORG_BASE]

_HEAD:
	xor ax,0x1eaf
	jz short _crt

forever:
	jmp short forever

_crt:

	cli
	cld
	mov es, ax
	mov ss, ax
	mov sp, ORG_BASE
	push cx
	
	push cs
	pop ds

	xor si, si
	mov di, ORG_BASE
	mov cx,  (_END - _HEAD)/2
	rep movsw
	mov di, _osz_system_table
	mov cx, OSZ_SYSTBL_SIZE/2
	rep stosw
		
	pop ax
	mov [es:_osz_system_table + OSZ_SYSTBL_ARCH], ax
	mov al, 0xEA
	mov [es:_osz_system_table + OSZ_SYSTBL_CALLBIOS], al
	
	mov ax, _next
	push es
	push ax
	

	; DETECT CPU
_DETECT_CPUID:
	mov di, _osz_system_table + OSZ_SYSTBL_CPUID

	mov dx,0xF000
	pushf
	pop ax
	mov cx, ax
	and ax, 0x0FFF
	push ax
	popf
	pushf
	pop ax
	and ax,dx
	cmp ax,dx
	jz short .end_cpu
	
	or cx,dx
	push cx
	popf
	pushf
	pop ax
	and ax,dx
	mov byte [es:di], 2
	jz short .end_cpu

	pushfd
	pop eax
	mov ecx,eax
	xor eax,0x00040000
	push eax
	popfd
	xor eax,ecx
	mov byte [es:di], 3
	jz short .end_cpu
	push ecx
	popfd
	
	mov eax,ecx
	xor eax,0x00200000
	push eax
	popfd
	pushfd
	pop eax
	xor eax,ecx
	mov byte [es:di], 4
	jz short .env_no_cpuid

	mov byte [es:di], 5

	mov eax,0x80000000
	cpuid
	cmp eax,0x80000000
	jbe .env_no_cpuid80000000
	mov eax,0x80000001
	cpuid
	;bt edx,20
	;jnc short .env_no_nx
	;or byte [ebp],ENV_NX
;.env_no_nx:
	bt edx,29
	jnc short .env_no_amd64
	mov byte [es:di], 6
.env_no_amd64:
.env_no_cpuid80000000:
.env_no_cpuid:

.end_cpu:

%if 0
	;;  A20 Check
_A20chk:
	cmp byte [es:di], 2
	jb short .ng
	xor ax,ax
	mov ds,ax
	dec ax
	mov es,ax
	xor si,si
	mov di,16
	mov cx,512
	rep cmpsw
	push cs
	pop ds
	jnz short .ok
.ng:
	jmp short .last
.ok:
	mov dx, _A20_ok_msg
	push cs
	pop ds
	mov cl, OSZ_DOS_PUTS
	push cs
	call _BDOS_entry
.last:
	push cs
	pop ds
%endif

	retf


_next:
	mov ax, ds
	add ax, (_END-_HEAD)/16
	mov ds, ax
	mov dx, (ORG_BASE + _END - _HEAD)/16
	mov es, dx

.load_loop:
	push es
	xor si, si
	xor di, di
	
	mov cx, [si+0x02]
	mov dx, ds
	add dx, cx
	push dx
	add cx, cx
	add cx, cx
	add cx, cx
	rep movsw

	mov ax, es	
	push cs
	pop es
	mov bx, _osz_system_table

	push cs
	call _invoke
	
	pop ds
	pop bx
	add bx, ax
	mov es, bx
	jmp short .load_loop

_invoke:
	mov ds, ax
	mov cx, 0x0004
	push ax
	push cx
	retf


	alignb 16
_END:

