; Chars for OSZ/DOS
; WTFPL/PUBLIC DOMAIN
[bits 16]
[org 0x0100]
	xor bp, bp	; OSZ SIGNATURE

	mov al, ' '
_loop:
	int 0x29
	inc al
	jns _loop
	ret
