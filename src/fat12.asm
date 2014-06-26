;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Z - Minimal FAT12 Driver
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

;;	MAX 4.5KB
%define	_fat12_fat_buffer	(_END)
;;	MAX 8KB
%define	_fat12_dir_buffer	(_END+0x1200)
%define	MAX_BUFFER			0x3200
;;	MAX 256B
%define	_dir_buff			(_END+0x3200)

%define	SIZE_BSS			0x3300

%define	MAX_FILENAME		64


[bits 16]
_HEAD:
	db 0xC3, 0x5A
	dw (_END-_HEAD)/16

_crt:
	mov [_osz_systbl], bx
	mov [_osz_systbl+2], es

%if 0
	mov ah, BIOS_INIT_DISK
	call _call_bios
	or ax, ax
	jnz .install_ok
	xor ax, ax
	retf
.install_ok:
%endif

	xor ax, ax
	mov es, ax
	mov bx, 0x3F*4
	
	mov ax, [es:bx]
	mov cx, [es:bx+2]
	mov [_int3F_old], ax
	mov [_int3F_old+2], cx
	mov [es:bx], word _int3F
	mov [es:bx+2], cs

	call _fat12_init
	
	mov ax, (SIZE_BSS + _END-_HEAD)/16
	retf


_call_bios:
	db 0x9A
_osz_systbl	dd 0
	ret


_int3F:
	cmp ah, OSZ_I3F_IFS
	jz _int3F_ifs
	db 0xEA
_int3F_old	dd 0

_int3F_ifs:
	xchg ax, bx
	xor bh, bh
	add bx, bx
	call [cs:_IFS_func_tbl + bx]
	iret


_fat12_init:
	push cs
	pop ds
	push cs
	pop es

	push si
	push di

	xor ax, ax
	mov di, _fat12_fat_buffer
	mov cx, MAX_BUFFER/2
	rep stosw

	mov ah, BIOS_INIT_DISK
	call _call_bios
	mov dx, ax

	mov si, _bpb_table
	mov di, _current_disk_id
.find_bpb:
	lodsw
	or al, al
	jz .invalid_bpb
	cmp ax, dx
	jz short .found_bpb
	add si, byte 4
	jmp short .find_bpb
.invalid_bpb:
	stosw
	stosw
	stosw
	stosw
	stosw
	mov [_n_root_entries], ax
	jmp _end_init

.found_bpb:
	stosw
	lodsw
	stosw
	mov cl, ah
	xor ah, ah
	xor ch, ch
	add al, al
	inc al
	stosw
	add ax, cx
	stosw
	lodsw
	stosw
	add al, ah
	mov cl, al
	mov ax, 128
	shl ax, cl
	stosw
	mov cl, 4
	shr ax, cl
	stosw

.end_bpb:

	mov si, _fat12_packet
	mov [si+8], word 1
	mov [si+6], cs
	mov [si+4], word _fat12_fat_buffer
	xor ah, ah
	mov al, [_n_sectors_fat]
	mov [si+2], ax
	mov ah, BIOS_READ_DISK
	call _call_bios

	mov ax, [_begin_root_dir]
	mov [si+8], ax
	mov [si+4], word _fat12_dir_buffer
	xor ah, ah
	mov al, [_n_sectors_root_dir]
	mov [si+2], ax
	mov ah, BIOS_READ_DISK
	call _call_bios

	;	scan number of entries of root dir
_scan_root:
	mov cx, 0xE0 ; number of entries of root dir
	xor dx,dx
	mov si, _fat12_dir_buffer
.loop:
	lodsb
	or al,al
	jz .end
	inc dx
	add si, byte 31
	loop .loop
.end:
	mov [_n_root_entries], dx

_end_init:
	pop di
	pop si
	ret


%define	ENUM_SIZE_STACK		16
%define	ENUM_LOCAL_INDEX	-2
%define	ENUM_SAVED_OUTPUT	-4
_fat12_enum_file:
	push ds
	push es
	push bx
	push si
	push di
	push bp
	mov bp, sp
	sub sp, ENUM_SIZE_STACK
	
	push ds
	pop es
	push cs
	pop ds
	
	mov di, dx
	mov bx, ax
	mov si, ax
	mov cl, 5
	shl si, cl
	add si, _fat12_dir_buffer

.loop:
	cmp [_n_root_entries], bx
	jbe short .end_over
	
	mov al, [si]
	or al, al ; end of dir
	jz short .end_over
	cmp al, 0xE5 ; deleted file
	jz short .skip
	cmp al, 0x2E ; special dotted dir (./..)
	jz short .skip
	mov dl, [si+0x0B]
	test dl, 0x08
	jnz short .skip

	mov [bp+ENUM_LOCAL_INDEX], bx

	mov cx, 32
	rep movsb
	add di, byte 32
	sub si, byte 32

	jmp short .conv_filename
.skip:
	inc bx
	add si, byte 0x20
	jmp short .loop

.end:
	mov sp, bp
	pop bp
	pop di
	pop si
	pop bx
	pop es
	pop ds
	ret

	
.end_over:
	xor ax,ax
	jmp short .end


.conv_filename:
	mov [bp+ENUM_SAVED_OUTPUT], di
	cmp bx, byte 1
	jb short .no_lfn
	cmp byte [si-0x20+0x0B], 0x0F
	jnz short .no_lfn

	;;	LFN check sum
	mov cx, 11
	xor dl, dl
	xor bx, bx
.loop_sum:
	ror dl, 1
	add dl, [si+bx]
	inc bx
	loop .loop_sum
	
	xor ax, ax
	xor bx, bx
.lfn_loop:
	inc ax
	sub bx, byte 0x20
	cmp byte [si+bx+0x0B], 0x0F
	jnz short .no_lfn
	cmp byte [si+bx+0x0D], dl
	jnz short .no_lfn
	mov cl, [si+bx]
	cmp al, cl
	jz .lfn_pass
	sub cl, 0x40
	cmp al, cl
	jnz .no_lfn
.lfn_last:
	call _lfn_progress
	mov ax, [bp+ENUM_LOCAL_INDEX]
	inc ax
	jmp short .end
.lfn_pass:
	call _lfn_progress
	jmp short .lfn_loop

.no_lfn:
	mov di, [bp+ENUM_SAVED_OUTPUT]

	;;	 8.3 to ASCIZ
	mov dl, [si+0x0C]
	mov al, ' '
	mov bx, 8
.loop_find_sp_base:
	cmp al, [si+bx-1]
	jnz .end_find_sp_base
	dec bx
	jnz short .loop_find_sp_base
	mov bx, 8

.end_find_sp_base:
	test dl, 0x08
	jnz .base_lower
	mov cx, bx
	rep movsb
	sub si, bx
	sub di, bx
	jmp short .end_base
.base_lower:
	mov cx, bx
.loop_base_lower:
	mov al, [si+bx-1]
	call _to_lower
	mov [es:di+bx-1], al
	dec bx
	jnz .loop_base_lower
	mov bx, cx
.end_base:
	mov al, [si+8]
	cmp al, ' '
	jz .end_ext
	mov byte [es:di+bx], '.'
	inc bx
	mov cx, [si+0x09]

	test dl, 0x08
	jnz .ext_lower
	mov [es:di+bx], al
	inc bx
	cmp cx, 0x2020
	jz short .end_ext
	mov [es:di+bx], cl
	inc bx
	cmp ch, 0x20
	jz short .end_ext
	mov [es:di+bx], ch
	inc bx
	jmp short .end_ext

.ext_lower:
	call _to_lower
	mov [es:di+bx], al
	inc bx
	cmp cx, 0x2020
	jz short .end_ext
	mov al, cl
	call _to_lower
	mov [es:di+bx], al
	inc bx
	mov al, ch
	cmp al, 0x20
	jz .end_ext
	call _to_lower
	mov [es:di+bx], al
	inc bx

.end_ext:
.end_conv:
	xor al, al
	mov [es:di+bx], al

	mov ax, [bp+ENUM_LOCAL_INDEX]
	inc ax
	jmp .end


_lfn_progress:
	push ax
	push dx
	push si

	lea si, [si+bx+1]
	mov cx, 5
.loop1:
	lodsw
	call _lfn_conv_char
	loop .loop1
	add si, byte 3
	mov cx, 6
.loop2:
	lodsw
	call _lfn_conv_char
	loop .loop2
	lodsw
	lodsw
	call _lfn_conv_char
	lodsw
	call _lfn_conv_char
	xor al, al
	mov [es:di], al

	pop si
	pop dx
	pop ax
	ret

_lfn_conv_char:
	cmp ax, byte 0x007F
	ja .no_ascii
	stosb
	ret

.no_ascii:
	mov al, '?'
	stosb
	ret



_fat12_open:

	push ds
	pop es
	mov di, dx

	push cs
	pop ds

	xor ax, ax
.loop_find:
	mov dx,_dir_buff
	call _fat12_enum_file
	mov cx, ax
	jcxz .nofile
	
	mov si, _dir_buff + 64
	xor bx, bx
.loop_cmp:
	mov al, [si+bx]
	call _to_lower
	mov dl, al
	mov al, [es:di+bx]
	call _to_lower
	cmp al, dl
	jnz .not_equal
	or al,al
	jz .found
	inc bx
	jmp short .loop_cmp

.not_equal:
	mov ax, cx
	jmp short .loop_find

.nofile:
	mov ax, 0xFFFF
	ret

.found:
	mov ax, cx
	dec ax
	ret



_fat12_close:
	ret


_fat12_nop:
	xor ax, ax
	ret


_fat12_get_cwd:
	push ds
	pop es
	mov di, si
	push cs
	pop ds
	mov si, _dummy_path
	lodsb
	mov cl, al
	xor ch, ch
	rep movsb
	xor ax, ax
	stosb
	ret


_fat12_read:
	cmp si, [cs:_n_root_entries]
	jb .valid_handle
	mov ax, -1
	ret

.valid_handle:
	push bp
	mov bp, sp
	sub sp, 2

	mov cl, 5
	shl si, cl
	add si, _fat12_dir_buffer
	mov bx, [cs:si + 0x1A]
	mov ax, [cs:si + 0x1C]
	mov di, [cs:si + 0x1E]
	mov [bp-2], ax
	mov cx, [cs:_clst_byte]
	dec cx
	add ax, cx
	adc di, byte 0
	mov cl, 7
	add cl, [cs:_sector_shift]
	add cl, [cs:_cluster_shift]
	shr ax, cl
	neg cl
	add cl, 16
	shl di, cl
	or di, ax
	jz short .end_z
	or bx, bx
	jz short .end_z

	mov si, _fat12_packet
	
	mov ax, dx
	and ax, 0x000F
	mov [cs:si+4], ax
	mov cl, 4
	shr dx, cl
	mov ax, ds
	add ax, dx
	mov [cs:si+6], ax
	
	push cs
	pop ds

	mov ax, bx
.loop:
	mov cl, [_cluster_shift]

	dec ax
	dec ax
	shl ax, cl
	add ax, [_begin_clust_2]
	mov [si+8],ax

	mov ax, 1
	shl ax, cl
	mov [si+2], ax

	mov ah, BIOS_READ_DISK
	call _call_bios

	mov ax, [_clst_para]
	add [si+6], ax

	dec di
	je short .end_s

	mov ax, bx
	add ax, ax
	add bx, ax
	shr bx, 1
	mov ax, [_fat12_fat_buffer + bx]
	jnc .even
	mov cl, 4
	shr ax, cl
.even:
	and ax, 0x0FFF
	mov bx, ax

	jmp short .loop

.end_s:
	mov ax, [bp-2]
	jmp short .end
.end_z:
	xor ax, ax
.end:
	mov sp, bp
	pop bp
	ret


_to_lower:
	cmp al, 'A'
	jb .noa
	cmp al, 'Z'
	ja .noa
	or al, 0x20
.noa:
	ret




	alignb 16

_IFS_func_tbl:
	dw _fat12_get_cwd
	dw _fat12_nop ; set cwd
	dw _fat12_open
	dw _fat12_nop; close
	dw _fat12_read; read
	dw _fat12_nop; write
	dw _fat12_nop; seek
	dw _fat12_enum_file


_fat12_packet:
	db 0x10, 0x00
	dw 0x0001
	dd 0
	dd 0, 0


	; internal BPB
_current_disk_id	dw 0
_n_sectors_fat		db 0
_n_sectors_root_dir	db 0
_begin_root_dir		dw 0
_begin_clust_2		dw 0
_sector_shift		db 0
_cluster_shift		db 0
_clst_byte			dw 0
_clst_para			dw 0
_n_root_entries		dw 0


	; Compressed internal BPB
_bpb_table:
	dw 0x2709 ; 2D CHRN(40,2,9,2) 360KB
	db 2, 7, 2, 1
	dw 0x4F09 ; 2DD CHRN(80,2,9,2) 720KB
	db 3, 7, 2, 1
	dw 0x4F0F ; 2HC CHRN(80,2,15,2) 1200KB
	db 7, 14, 2, 0
	dw 0x4F12 ; 2HD CHRN(80,2,18,2) 1440KB
	db 9, 14, 2, 0
	dw 0x4C08 ; 2HD CHRN(77,2,8,3) 1232KB
	db 2, 6, 3, 0
	db 0

_dummy_path	db 3, "//A"


	alignb 16
_END:
