;;	-*- coding: utf-8 -*-
;;
;;	ACPI Utility for OSZ
;;
;;	Copyright (c) 1998-2016, MEG-OS project
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
[org 0x0100]

start:
	xor bp, bp

	mov ah, OSZ_DOS_GET_ACPI
  xor bx, bx
	call bp
  or bx, bx
  jnz .acpi_found
  mov dx, noacpi_msg
  mov ah, 0x09
  int 0x21
  ret

.acpi_found:
  mov [_acpi_bios  ], bx
  mov [_acpi_bios+2], es

  mov ah, 0x00
  call far [_acpi_bios]
  mov [_n_entries], cx

  xor di, di
.loop:
  mov cx, di
  mov ah, 0x01
  call far [_acpi_bios]

  mov edx, ebx
  call _disp_hex_32
  mov al, ' '
  int 0x29
  mov edx, [es:ebx+4]
  call _disp_hex_32
  mov al, ' '
  int 0x29
  mov cx, 4
.print_loop:
  mov al, [es:ebx]
  int 0x29
  inc ebx
  loop .print_loop
  mov al, 10
  int 0x29

  inc di
  cmp di, [cs:_n_entries]
  jb .loop

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
	int 0x29
	pop cx
	loop .loop
	ret

_disp_hex_32:
  mov cx, 8
.loop:
	push cx
	mov cl, 4
	rol edx, cl
	mov al, dl
	and al, 0x0F
	add al, '0'
	cmp al, 0x3A
	jb .no_0A
	add al, 0x61-0x3A
.no_0A:
	int 0x29
	pop cx
	loop .loop
	ret


  alignb 16

_acpi_bios  dd 0
_n_entries  dw 0

noacpi_msg  db "ERROR: ACPI NOT FOUND.", 13, 10, "$"
