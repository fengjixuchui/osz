;;	-*- coding: utf-8 -*-
;;
;;	ACPI BIOS for OSZ
;;
;;	Copyright (c) 1998-2016 MEG-OS project
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

;%define	DEBUG
%include "osz.inc"

[bits 16]

_HEAD:
	db 0xCB, 0x1A
	dw _init


_rsdt_ptr		dd 0
_facp_ptr		dd 0
_dsdt_ptr		dd 0

_SMI_CMD		dw 0
_ACPI_ENABLE	db 0
_ACPI_DISABLE	db 0
_PM1a_CONTROL	dw 0
_PM1b_CONTROL	dw 0
_SLP_TYPa		dw 0
_SLP_TYPb		dw 0


_call_bios:
	db 0x9A
_osz_systbl	dd 0
	ret

_acpi_power:
	cmp ah, 5
	jz _to_S5
	xor ax, ax
	retf

_to_S5:
	mov dx, [cs:_PM1a_CONTROL]
	mov ax, [cs:_SLP_TYPa]
	out dx, ax

	mov dx, [cs:_PM1b_CONTROL]
	or dx, dx
	jz .skip
	mov ax, [cs:_SLP_TYPb]
	out dx, ax
.skip:
	stc
	sbb ax, ax
	retf


	alignb 16
_END_RESIDENT:
; ---------------------------------------------------------------------


_init:
	cmp al, 0x01
	jnz _no_acpi
	cmp cl, 5
	jae short _cpuok
_no_acpi:
	xor ax, ax
	retf
_cpuok:

	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

	xor ax, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

_find_rsdptr:
	mov dx, 0xE000
.loop:
	mov es, dx
	mov si, _RSDPtr
	xor di, di
	mov cx, 4
	rep cmpsw
	jz .found
	inc dx
	jnz .loop
.not_found:
	jmp short _no_acpi

.found:
;	mov es, dx
	mov eax, [es:0x10]
	mov [_rsdt_ptr], eax

_find_facp:
	mov esi, [_rsdt_ptr]
	mov ecx, [fs:esi+4]
	mov edx, 36
	add esi, edx
	sub ecx, edx
	shr ecx, 2
.loop:
	a32 fs lodsd
	cmp dword [fs:eax], 'FACP'
	jz .found
	add esi, byte 4
	loop .loop

	jmp _no_acpi

.found:
	mov [_facp_ptr], eax
	mov ebx, eax
	mov edx, [fs:ebx+48]
	or edx, edx
	jz _no_acpi
	mov [_SMI_CMD], dx
	mov ax, [fs:ebx+52]
	or ax, ax
	jz _no_acpi
	mov [_ACPI_ENABLE], ax
	mov cx, [fs:ebx+64]
	mov [_PM1a_CONTROL], cx
	mov cx, [fs:ebx+68]
	mov [_PM1b_CONTROL], cx

%ifdef DEBUG
	mov edx, ebx
	mov cx, 5
.loop_dump:
	call _dump32
	add edx, 16
	loop .loop_dump
%endif

	mov edx, [fs:ebx+40]
	mov [_dsdt_ptr], edx

_find_s5:
	mov ecx, [fs:edx+4]
	lea esi, [edx+36]
	lea edi, [edx+ecx]
.loop:
	a32 fs lodsb
	cmp al, 0x08
	jnz .no_name
	a32 fs lodsb
	cmp al, '\\'
	jz .skip
	dec esi
.skip:
	a32 fs lodsd
	cmp eax,'_S5_'
	jnz .no_name
	a32 fs lodsb
	cmp al, 0x12
	jz .found

.no_name:
	cmp esi, edi
	jb .loop

	xor ax, ax
	retf

.found:

%ifdef DEBUG
	lea edx, [esi-6]
	call _dump32
%endif

	a32 fs lodsb
	and al, 0xC0
	shr al, 6
	movzx eax, al
	lea esi, [esi+eax+1]

	mov al, [fs:esi]
	cmp al, 0x0A
	jnz .skip1
	inc esi
.skip1:
	a32 fs lodsb
	movzx ax, al
	shl ax, 10
	or ax, 0x2000
	mov [_SLP_TYPa], ax

	mov al, [fs:esi]
	cmp al, 0x0A
	jnz .skip2
	inc esi
.skip2:
	a32 fs lodsb
	movzx ax, al
	shl ax, 10
	or ax, 0x2000
	mov [_SLP_TYPb], ax

	; then enable ACPI
	mov dx, [_SMI_CMD]
	mov al, [_ACPI_ENABLE]
	out dx, al

	les bx, [_osz_systbl]
	mov [es:bx+OSZ_SYSTBL_ACPI], word _acpi_power
	mov [es:bx+OSZ_SYSTBL_ACPI+2], cs

	mov ax, (_END_RESIDENT-_HEAD)/16
	retf


%ifdef DEBUG

_dump32:
	push edi
	push esi
	push edx
	push ecx

	mov edi, edx
	shr edx, 16
	call _disp_hex_16
	mov dx, di
	call _disp_hex_16
	mov esi, edi
	mov cx, 16
.loop:
	push cx
	mov al, ' '
	call _conout
	a32 fs lodsb
	mov dl, al
	call _disp_hex_8
	pop cx
	loop .loop
	mov al, ' '
	call _conout
	mov cx, 16
	mov esi, edi
.loop2:
	a32 fs lodsb
	cmp al, 0x7F
	jae .nochar
	cmp al, 0x20
	jae .char
.nochar:
	mov al, '.'
.char:
	call _conout
	loop .loop2
	mov al, 10
	call _conout

	pop ecx
	pop edx
	pop esi
	pop edi
	ret


_conout:
	mov ah, BIOS_CONOUT
	jmp _call_bios

_puts:
	push si
	mov si, dx
.loop:
	lodsb
	or al,al
	jz short .end
	call _conout
	or si, si
	jnz short .loop
.end:
	pop si
	ret

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
	mov al, dl
	and al, 0x0F
	add al, '0'
	cmp al, 0x3A
	jb .no_0A
	add al, 0x41-0x3A
.no_0A:
	call _conout
	pop cx
	loop .loop
	pop bx
	ret

%endif


_RSDPtr:
	db "RSD PTR "

	alignb 16
_END:
