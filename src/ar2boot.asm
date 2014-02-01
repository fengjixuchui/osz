;;	-*- coding: utf-8 -*-
;;
;;  ar2boot - MEG-OS Arlequin Second boot (subset of multiboot)
;;
;;	Copyright (c) 1998-2013, MEG-OS project
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

; enable NEC98 architecture
%define	CONFIG_ENABLE_NEC98		1
%define	KERNEL_MAGIC_NEC98		0xD9A39821

; (UNIMPLEMENTED)
%define	CONFIG_ENABLE_FMT		0

; enable VESA and set mode
%define	CONFIG_BOOT_VESA_MODE	0
;0x0101
; initialize with palette
%define	CONFIG_SET_PALETTE		1
; enable line 50
%define	CONFIG_CONSOLE_LINE50	0

%define	CONFIG_MINIMAL_MEMORY	1

%define	MAX_MMAP_COUNT	8

%define	MULTIBOOT_HEADER_MAGIC			0x1BADB002
%define	MULTIBOOT_BOOTLOADER_MAGIC		0x2BADB002


[section .text]
[bits 16]
_cs_base:

	;;	init register
	mov bx,cs
	mov ss,bx
	mov esp,_start_archive
	mov ds,bx
	push byte 0
	popf

	;;	check IPL signature
	cmp ax,0x1eaf
	jz .megipl
	mov [MBI_boot_device+3],dl
	jmp _init_PCAT
.megipl:
	mov [MBI_boot_device+3],ch
	mov [save_arch],cl
%if CONFIG_ENABLE_NEC98
	or cl,cl
	jz near _init_PC98
%endif
	dec cl
	jz near _init_PCAT
forever:
	sti
	hlt
	jmp short forever

_panic:
	push cs
	pop ds
	call _puts
	mov si,_lf_msg
	call _puts
	jmp short forever



_PS2_wait_for_out:
	in al,064h
	test al,2
	in al,060h
	jnz _PS2_wait_for_out
	ret




%if CONFIG_ENABLE_NEC98
_init_PC98:

	;;	ES=0
	xor ax,ax
	mov es,ax

	;;  MEM SIZE
	movzx ecx,byte [es:0x0501]
	and ecx,byte 7
	inc ecx
	shl ecx,7
	mov [MBI_mem_lower],ecx
	movzx eax,byte [es:0x0401]
	movzx edx,word [es:0x0594]
	shl eax,7
	shl edx,10
	lea eax,[eax+edx]
	mov [MBI_mem_upper],eax

	;;	SIGNATURE
	shl cx,6
	dec cx
	mov fs,cx
	mov eax,KERNEL_MAGIC_NEC98
	mov [fs:0x000C],eax

	;;  SETUP VIDEO
	mov word [_text_vram],0xA000
	mov al,[es:0x45C]
	test al,0x40
	jz short .no_pegc

	mov ax,0x300C
	mov bx,0x3200
	int 0x18
	mov ax,0x4D00
	mov cx,0x0100
	int 0x18
	mov ah,0x0C
	int 0x18
	mov ah,0x40
	int 0x18

	push word 0xE000
	pop es
	mov ax,0x0001
	mov [es:0x0100],al
	mov [es:0x0102],ax

	mov dword [VMI_linear_vram],0xFFF00000

	jmp short .end_vga
.no_pegc:
.end_vga:
	mov dx,0E120h
	mov ah,16h
	int 18h

	;;	FDC off
	mov al,0x10
	out 0x94,al

	;;  A20 CONTROL
	xor al,al
	out 0F2h,al

%if	CONFIG_SET_PALETTE
	mov si,_vesa_palette
	xor cx,cx
_loop_set_pal_98:
	mov al,cl
	out 0xA8,al
	lodsb
	out 0xAE,al
	lodsb
	out 0xAA,al
	lodsb
	out 0xAC,al
	inc si
	inc cx
	cmp cx,16
	jb _loop_set_pal_98
%endif

	jmp _next
%endif




;;	start depend
_init_PCAT:

	;;	ES=0
	xor ax,ax
	mov es,ax

	;;	init DISK BIOS
	;xor ax,ax
	xor dl,dl
	int 0x13
	xor ax,ax
	mov dl,0x80
	int 0x13

	;;	FDC off
	mov al,0x0C
	mov dx,0x03F0+0x02
	out dx,al
	and byte [es:0x043F],0xFE

	;;	memory size
	movzx eax,word [es:0x413]
	mov [MBI_mem_lower],eax
	mov ah,0x88
	int 0x15
	movzx eax,ax
	mov [MBI_mem_upper],eax

	;;	ES=CS
	push cs
	pop es

	;;	init screen
%if CONFIG_BOOT_VESA_MODE
	mov ax,0x4F01;
	mov cx,CONFIG_BOOT_VESA_MODE
	mov di,VBE_VIDEO_MODE_INFORMATION
	int 0x10
	jc short .VBE_bad
	mov ax,[VMI_mode]
	mov cx,0x0099
	and ax,cx
	cmp ax,cx
	jnz short .VBE_bad
	mov ax,0x4F02
	mov bx,0x8000|CONFIG_BOOT_VESA_MODE
	int 0x10
	jnc short .VBE_good
.VBE_bad:
%endif
	mov ax,0x0003
	int 0x10
%if CONFIG_CONSOLE_LINE50
	mov ax,0x1112
	xor bx,bx
	int 0x10
%endif
%if CONFIG_BOOT_VESA_MODE
	xor eax,eax
	mov [VMI_linear_vram],eax
%endif
.VBE_good:

	;;	parse SMAP
	xor ebx,ebx
	push ebx
	sub esp,20
smap_cont:
	mov eax,0xE820
	mov edx,0x534D4150
	mov edi,esp
	mov ecx,20
	int 0x15
	jc short .end

	;;	fix mem_upper
	mov eax,[edi+4]
	or eax,[edi+12]
	jnz short .no_fix_upper
	mov eax,[edi]
	cmp eax,0x00100000
	jnz short .no_fix_upper
	mov eax,[edi+16]
	cmp eax,0x00000001
	jnz short .no_fix_upper
	mov eax,[edi+8]
	shr eax,10
	mov [MBI_mem_upper],eax
.no_fix_upper:

	mov esi,edi
	mov edi,[MBI_mmap_length]
	add edi,mmap
	mov eax,20
	stosd
	mov ecx,5
	rep movsd

	add eax,4
	add [MBI_mmap_length],eax

	or ebx,ebx
	jz short .end
	mov eax,[esp+20]
	inc eax
	mov [esp+20],eax
	cmp eax,MAX_MMAP_COUNT
	jle smap_cont
.end:
	add esp,24

	;;	Legacy PC A20
%if 0
	call _PS2_wait_for_out
	mov al,0AEh
	out 064h,al
	call _PS2_wait_for_out
	mov al,0D1h
	out 064h,al
	call _PS2_wait_for_out
	mov al,0DFh
	out 060h,al
	call _PS2_wait_for_out
	mov al,0A8h
	out 064h,al
%endif

	;;	OADG A20
	in al,0x92
	or al,0x02
	out 0x92,al

%if	CONFIG_SET_PALETTE
	;;	palette
	mov dx,0x3DA
	in al,dx
	mov al,0x20
	mov dl,0xC0
	out dx,al
	xor al,al
	mov dl,0xC8
	out dx,al
	mov cx,16
	mov si,_vesa_palette
	mov dl,0xC9
_loop_set_pal_at:
	mov al,[si+2]
	shr al,2
	out dx,al
	mov al,[si+1]
	shr al,2
	out dx,al
	mov al,[si]
	shr al,2
	out dx,al
	add si,4
	loop _loop_set_pal_at
%endif




;;	end depend
_next:

	;;  A20 Check
	xor ax,ax
	mov ds,ax
	dec ax
	mov es,ax
	xor si,si
	mov di,16
	mov cx,256
	rep cmpsd
	push cs
	pop ds
	jnz _A20_ok
	mov si,a20_err_msg
	jmp _panic
_A20_ok:


	;;	memory check
	cmp dword [MBI_mem_upper],CONFIG_MINIMAL_MEMORY*0x400
	jae memok
	mov si,nomem_msg
	jmp _panic
memok:


	;;	find multiboot sign
	xor bx,bx
scan_multiboot:
	mov eax,MULTIBOOT_HEADER_MAGIC
	cmp eax,[_start_archive+bx]
	jnz short .no_magic
	add eax,[_start_archive+bx+0x04]
	add eax,[_start_archive+bx+0x08]
	jz short .found
.no_magic:
	add bx,4
	cmp bx,8192
	jle scan_multiboot
	mov si,badmb_msg
	jmp _panic
.found:
	mov [multiboot_header],bx


	;; Enter to protected mode
	mov bp,cs
	movzx ebp,bp
	shl ebp,4
	add [__GDT+2],ebp
	cli
	lgdt [cs:__GDT]
	mov eax,cr0
	or	eax,0x000050021	; AM/WP/NE/PE
	mov cr0,eax
	db 0xEB,0x00

	xor ax,ax
	lea ecx,[ebp+now_in_prot]
	mov dx,0x0008
	push ax
	push ax
	push ax
	push byte 0x10
	push ecx
	iretd




_puts:
	mov al,[cs:save_arch]
%if CONFIG_ENABLE_NEC98
	or al,al
	jz short _puts_PC98
%endif
	dec al
	jz short _puts_PCAT
%if CONFIG_ENABLE_FMT
_puts_FMT:
pfl0:
	lodsb
	or al,al
	jz pfle
	push ds
	push si
	push cs
	pop ds
	mov si,_puts_FMT_param
	mov [si+3],al
	mov bh,002h
	db 09Ah
	dd 0FFFB001Eh
	inc byte [si+1]
	pop si
	pop ds
	jmp pfl0
pfle:
	ret
%endif

_puts_PCAT:
%if CONFIG_BOOT_VESA_MODE
	mov eax,[VMI_linear_vram]
	or eax,eax
	jz short .nocls
	mov ax,0x0003
	int 0x10
	xor eax,eax
	mov [VMI_linear_vram],eax
.nocls
%endif
.loop:
	lodsb
	or al,al
	jz short .le
	cmp al,0x0A
	jz short .lf
	mov ah,0x0E
	mov bx,0x0007
	int 0x10
	jmp short .loop
.lf:
	mov ax,0x0E0D
	int 0x10
	mov ax,0x0E0A
	int 0x10
	jmp short .loop
.le:
	ret

_puts_PC98:
	push es
	push di
	mov es,[cs:_text_vram]
.loop0:
	mov al,[cs:_col]
	mov cl,80
	mul cl
	add al,[cs:_row]
	adc ah,0
	add ax,ax
	mov di,ax
.loop1:
	lodsb
	or al,al
	jz .le
	cmp al,0x0A ; LF
	jz .lf
	stosb
	inc di
	inc byte [cs:_row]
	jmp short .loop1
.lf:
	inc byte [cs:_col]
	xor al,al
	mov [cs:_row],al
	jmp .loop0
.le:
	pop di
	pop es
	ret




align 16
[bits 32]
now_in_prot:
	mov ss,edx
	;mov esp,0x8000
	lea esp,[ebp+_start_archive]
	mov ds,edx
	mov es,edx
	mov fs,eax
	mov gs,eax

	;;	adjust multiboot info
	add [ebp+MBI_cmdline],ebp
	add [ebp+MBI_boot_loader_name],ebp

	mov eax,[ebp+VMI_linear_vram]
	mov ecx,0x0800
	or eax,eax
	jz .no_vmi
	or [ebp+MBI_flags],ecx
	add [ebp+MBI_vbe_mode_info],ebp
.no_vmi:

	mov eax,[ebp+MBI_mmap_length]
	or eax,eax
	jz .no_mmap
	mov ecx,0x0040
	lea edx,[ebp+mmap]
	or [ebp+MBI_flags],ecx
	mov [ebp+MBI_mmap_addr],edx
.no_mmap:




	;;	locate kernel
	mov edx,[ebp+multiboot_header]
	lea edx,[ebp+edx+_start_archive]

	mov esi,[edx+0x0C]
	mov edi,[edx+0x10]
	mov ecx,[edx+0x14]
	sub esi,edi
	sub esi,edx
	neg esi
	sub ecx,edi
	rep movsb
	mov ecx,[edx+0x18]
	sub ecx,[edx+0x14]
	jle .nobss
	xor eax,eax
	rep stosb
.nobss:

	mov ecx,[edx+0x1C]
	lea ebx,[ebp+Multiboot_information]
	mov eax,MULTIBOOT_BOOTLOADER_MAGIC
	call ecx
	hlt
	jmp short $-1




	;;	GDT
alignb 16
__GDT:
	dw (__end_GDT-__GDT-1),__GDT ,0x0000,0x0000 ; 00 NULL
	dw 0xFFFF,0x0000,0x9200,0x00CF	; 08 32bit KERNEL DATA FLAT
	dw 0xFFFF,0x0000,0x9A00,0x00CF	; 10 32bit KERNEL TEXT FLAT
__end_GDT:

%if	CONFIG_SET_PALETTE
_vesa_palette:
	dd 0x000000,0x000099,0x009900,0x009999,0x990000,0x990099,0x999900,0xAAAAAA
	dd 0x555555,0x0000FF,0x00FF00,0x00FFFF,0xFF0000,0xFF00FF,0xFFFF00,0xFFFFFF
%endif

multiboot_header		dd 0

Multiboot_information:
MBI_flags				dd 0x00000207
MBI_mem_lower			dd 0
MBI_mem_upper			dd 0
MBI_boot_device			dd 0x00FFFFFF
MBI_cmdline				dd cmdline
MBI_mods_count			dd 0
MBI_mods_addr			dd 0
MBI_syms				resd 4
MBI_mmap_length			dd 0
MBI_mmap_addr			dd 0
MBI_drives_length		dd 0
MBI_drives_addr			dd 0
MBI_config_table		dd 0
MBI_boot_loader_name	dd boot_loader_name
MBI_apm_table			dd 0
MBI_vbe_control_info	dd 0
MBI_vbe_mode_info		dd VBE_VIDEO_MODE_INFORMATION
MBI_vbe_mode			dw 0
MBI_vbe_interface_seg	dw 0
MBI_vbe_interface_off	dw 0
MBI_vbe_interface_len	dw 0
;	/* 0400 */	uint32_t	apm_table;
;	/* 0800 */	uint32_t	vbe_control_info,vbe_mode_info;

mmap			resb 24*MAX_MMAP_COUNT

VBE_VIDEO_MODE_INFORMATION:
VMI_mode					dw 0x99
							db 0,0
							dw 0,0,0,0
							dd 0
VMI_bytes_par_scanline		dw 640
VMI_width					dw 640
VMI_height					dw 480
							db 0,0,0
VMI_bits_par_pixel			db 8
							db 0
VMI_memory_modes			db 0x04
							db 0,0,0
VMI_red_mask_size			db 0
VMI_red_field_position		db 0
VMI_green_mask_size			db 0
VMI_green_field_position	db 0
VMI_blue_mask_size			db 0
VMI_blue_field_position		db 0
							db 0,0,0
VMI_linear_vram				dd 0

_text_vram		dw 0xB800
_col			db 0
_row			db 0
%if CONFIG_ENABLE_FMT
_puts_FMT_param	db 0,0,1,0
%endif
save_arch		db 0x01


_lf_msg:
	db 10,0

a20_err_msg:
	db "A20 hardware error.",0

nomem_msg:
	db "Not enough extended memory.",0

badmb_msg:
	db "not supported kernel.",0

boot_loader_name:
	db "ar2boot",0

;;	default command line
cmdline		db "/kernel.bin",0

alignb 16
_start_stack:
	times 0x400-((_start_stack-_cs_base)&0x1FF) db 0
_start_archive:
