; Pipo - Splash Sound of Japanese Personal Computer for OSZ
; PUBLIC DOMAIN

%include "osz.inc"

[bits 16]
[org 0x0100]
	mov cx, 2000
	call _beep
	mov cx, 1000

	;call _beep
	;ret
_beep:
	mov ah, BIOS_BEEP
	call bp

	mov ax, 200
	call _wait_tick

	xor cx, cx
	mov ah, BIOS_BEEP
	call bp
	ret


_wait_tick:
	push bx
	push si
	push di
	mov bx, ax

	mov ah, OSZ_DOS_GET_TICK
	call bp
	mov si, ax
	mov di, dx

.loop:
	int 0x28
	mov ah, OSZ_DOS_GET_TICK
	call bp

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

	pop di
	pop si
	pop bx
	ret


