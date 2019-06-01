;;
;;	BIOS for IBM PC Compatible
;;
;;	Copyright (c) MEG-OS project
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



[BITS 16]
[ORG 0]


_aux_out:
    push ds
    push dx
    xor dx, dx
    mov ds, dx
    mov dx, [ds:0x400]
    out dx, al
    pop dx
    pop ds
    ret

_aux_in:
    push ds
    push dx
    xor dx, dx
    mov ds, dx
    mov dx, [ds:0x400]
    in al, dx
    pop dx
    pop ds
    ret

_aux_in_st:
    ret


_INIT:
    cli
    cld
    xor ax, ax
    mov ss, ax
    mov sp, 0x400
    mov ax, cs
    mov ds, ax
    mov es, ax

    mov ax, 0x3F8
    mov [ss:0x400], ax

    mov si, banner
.loop:
    lodsb
    or al, al
    jz .end
    mov dx, 0x3F8
    out dx, al
    jmp .loop
.end:

_forever:
    cli
    hlt
    jmp _forever


banner  db "Hello world", 13, 10, 0

	times 0xFFF0 - ($-$$) db 0

__RESET:
    jmp 0xF000:_INIT

	times 0x10000 - ($-$$) db 0
