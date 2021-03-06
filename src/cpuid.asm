;;	-*- coding: utf-8 -*-
;;
;;	Display CPUID information for OSZ/DOS
;;	WTFPL/PUBLIC DOMAIN

[bits 16]
[org 0x0100]

_main:
	xor bp, bp ; OSZ signature

	call _DETECT_CPUID

	mov dx, _prefix_msg
	mov ah, 9
	int 0x21

	mov al, [_cpuid_family_id]
	xor ah, ah
	cmp al, 0
	jz .cpu8086
	dec ax
	jz .cpu186
	dec ax
	jz .cpu286
	dec ax
	jz .cpu286
	dec ax
	jz .cpu386
	dec ax
	jz .cpu486
	call _detect_cpuid_string
	jmp short .end_cpu

.cpu8086:
	mov dx, _8086_msg
	jmp short .end_cpu
.cpu186:
	mov dx, _186_msg
	jmp short .end_cpu
.cpu286:
	mov dx, _286_msg
	jmp short .end_cpu
.cpu386:
	mov dx, _386_msg
	jmp short .end_cpu
.cpu486:
	mov dx, _486_msg
	jmp short .end_cpu
.end_cpu:
	mov ah, 9
	int 0x21

	mov al, [_cpuid_family_id]
	cmp al, 5
	jae short .has_cpuid_feature
	jmp .no_cpuid_feature
.has_cpuid_feature:

	mov dx, _cpuid_feat_msg
	mov ah, 9
	int 0x21

	push ds
	pop es
	mov di, _cpuid_string_buffer

	mov eax, 0x00000001
	xor ecx, ecx
	xor edx, edx
	xor ebx, ebx
	cpuid
	push ecx
	push edx
	push ecx
	call _dump_hex_32
	mov al, ' '
	stosb
	pop edx
	call _dump_hex_32
	mov al, 10
	stosb

	pop edx
	mov si, _cpuid_feat_0d
	call _parse_cpuid_feature

	pop edx
	mov si, _cpuid_feat_0c
	call _parse_cpuid_feature

	mov al, "$"
	stosb

	mov dx, _cpuid_string_buffer
	mov ah, 9
	int 0x21

	mov eax, 0x80000000
	cpuid
	cmp eax, 0x80000001
	jb short .no_cpuid_feature8

	mov dx, _cpuid_feat8_msg
	mov ah, 9
	int 0x21

	mov di, _cpuid_string_buffer

	mov eax, 0x80000001
	xor ecx, ecx
	xor edx, edx
	xor ebx, ebx
	cpuid
	push ecx
	push edx
	push ecx
	call _dump_hex_32
	mov al, ' '
	stosb
	pop edx
	call _dump_hex_32
	mov al, 10
	stosb

	pop edx
	mov si, _cpuid_feat_8d
	call _parse_cpuid_feature

	pop edx
	mov si, _cpuid_feat_8c
	call _parse_cpuid_feature

	mov al, "$"
	stosb

	mov dx, _cpuid_string_buffer
	mov ah, 9
	int 0x21

.no_cpuid_feature8:
.no_cpuid_feature:

	ret



_detect_cpuid_string:
	push es
	push bx
	push si
	push di
	push ds
	pop es

	mov di, _cpuid_string_buffer

	xor eax, eax
	cpuid
	mov [di+0x00], ebx
	mov [di+0x04], edx
	mov [di+0x08], ecx
	add di, byte 12
	mov al, ' '
	stosb

	mov eax, 0x00000001
	cpuid
	mov edx, eax
	call _dump_hex_32

	mov eax, 0x80000000
	cpuid
	cmp eax, 0x80000004
	jb short .no_brand
	mov al, ' '
	stosb

	mov eax, 0x80000002
	cpuid
	mov [di+0x00], eax
	mov [di+0x04], ebx
	mov [di+0x08], ecx
	mov [di+0x0C], edx
	mov eax, 0x80000003
	cpuid
	mov [di+0x10], eax
	mov [di+0x14], ebx
	mov [di+0x18], ecx
	mov [di+0x1C], edx
	mov eax, 0x80000004
	cpuid
	mov [di+0x20], eax
	mov [di+0x24], ebx
	mov [di+0x28], ecx
	mov [di+0x2C], edx
	mov al, "$"
	mov [di+0x30], al

	mov si, di
.loop_remove_sp:
	lodsb
	cmp al, ' '
	jz .loop_remove_sp
.loop_brand:
	stosb
	or al, al
	jz .end_brand
	lodsb
	jmp .loop_brand
.end_brand:

.no_brand:

	mov dx, _cpuid_string_buffer

	pop di
	pop si
	pop bx
	pop es
	ret


_parse_cpuid_feature:
	xor eax, eax
.loop:
	lodsb
	or al, al
	jz short .end
	and al, 0x7F
	bt edx, eax
	lodsb
	jnc short .not_found
	mov cx, ax
	mov al, ' '
	stosb
	rep movsb
	jmp short .loop
.not_found:
	add si, ax
	jmp short .loop

.end:
	ret


	; DETECT CPU
_DETECT_CPUID:
	mov di, _cpuid_family_id
	xor al, al
	mov [di], al

	mov cx, 0x0121
	shl ch, cl
	jz short .end_cpu

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
	mov byte [di], 1
	jz short .end_cpu

	or cx,dx
	push cx
	popf
	pushf
	pop ax
	and ax,dx
	mov byte [di], 2
	jz short .end_cpu

	pushfd
	pop eax
	mov ecx,eax
	xor eax,0x00040000
	push eax
	popfd
	xor eax,ecx
	mov byte [di], 3
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
	mov byte [di], 4
	jz short .env_no_cpuid

	mov byte [di], 5

	mov eax,0x80000000
	cpuid
	cmp eax,0x80000000
	jbe .env_no_cpuid80000000
	mov eax,0x80000001
	cpuid
	bt edx,29
	jnc short .env_no_amd64
	mov byte [di], 6
.env_no_amd64:
.env_no_cpuid80000000:
.env_no_cpuid:

.end_cpu:
	ret


_prefix_msg	db "CPU: $"
_8086_msg	db "8086/8088$"
_186_msg	db "80186$"
_286_msg	db "80286$"
_386_msg	db "80386$"
_486_msg	db "486$"

_cpuid_feat_msg	db 13, 10, "Feature: $"
_cpuid_feat8_msg	db  13, 10, "Extended Feature: $"

_dump_hex_32:
	mov cx, 8
.loop:
	rol edx, 4
	mov bx, dx
	and bx, byte 0x000F
	mov al, [_hex_tbl+bx]
	stosb
	loop .loop
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
	mov bx, dx
	and bx, byte 0x000F
	mov al, [_hex_tbl+bx]
	int 0x29
	pop cx
	loop .loop
	pop bx
	ret


; CPUID FEATURE STRING TABLE
_cpuid_feat_0d: ; 0000_0001_EDX
	db 0x80, 3, "FPU"
	db  1, 3, "VME"
	db  2, 2, "DE"
	db  3, 3, "PSE"
	db  4, 3, "TSC"
	db  5, 3, "MSR"
	db  6, 3, "PAE"
	db  7, 3, "MCE"
	db  8, 3, "CX8"
	db  9, 4, "APIC"
	db 10, 3, "???"
	db 11, 3, "SEP"
	db 12, 4, "MTRR"
	db 13, 3, "PGE"
	db 14, 3, "MCA"
	db 15, 4, "CMOV"
	db 16, 3, "PAT"
	db 17, 6, "PSE-36"
	db 18, 3, "PSN"
	db 19, 5, "CLFSH"
	db 20, 3, "???"
	db 21, 2, "DS"
	db 22, 4, "ACPI"
	db 23, 3, "MMX"
	db 24, 4, "FXSR"
	db 25, 3, "SSE"
	db 26, 4, "SSE2"
	db 27, 2, "SS"
	db 28, 2, "HT"
	db 29, 2, "TM"
	db 30, 4, "IA64"
	db 31, 3, "PBE"
	db 0

_cpuid_feat_0c: ; 0000_0001_ECX
	db 0x80, 4, "SSE3"
	db  1, 9, "PCLMULQDQ"
	db  2, 6, "DTES64"
	db  3, 7, "MONITOR"
	db  4, 6, "DS_CPL"
	db  5, 3, "VMX"
	db  6, 3, "SMX"
	db  7, 3, "EST"
	db  8, 3, "TM2"
	db  9, 5, "SSSE3"
	db 10, 7, "CNXT-ID"
	db 11, 3, "???"
	db 12, 3, "FMA"
	db 13, 4, "CX16"
	db 14, 4, "XTPR"
	db 15, 4, "PDCM"
	db 16, 3, "???"
	db 17, 4, "PCID"
	db 18, 3, "DCA"
	db 19, 5, "SSE41"
	db 20, 5, "SSE42"
	db 21, 6, "X2APIC"
	db 22, 5, "MOVBE"
	db 23, 6, "POPCNT"
	db 24, 12, "TSC-DEADLINE"
	db 25, 3, "AES"
	db 26, 5, "XSAVE"
	db 27, 6, "OSXAVE"
	db 28, 3, "AVX"
	db 29, 4, "F16C"
	db 30, 5, "RDRND"
	db 31, 10, "HYPERVISOR"
	db 0

_cpuid_feat_8d: ; 8000_0001_EDX
	db 11, 7, "SYSCALL"
	db 20, 2, "NX"
	db 22, 4, "MMX+"
	db 25, 19, "Fast-FXSAVE/FXRSTOR"
	db 27, 6, "RDTSCP"
	db 29, 5, "AMD64"
	db 30, 6, "3DNOW+"
	db 31, 5, "3DNOW"
	db 0

_cpuid_feat_8c: ; 8000_0001_ECX
	db 0x80, 9, "LAHF/SAHF"
	db  1, 10, "CMP-Legacy"
	db  2, 3, "SVM"
	db  4, 3, "CR8"
	db 0

	alignb 16
_hex_tbl	db "0123456789abcdef"

_cpuid_family_id	db 0

_cpuid_string_buffer:
	;times 256 db 0
