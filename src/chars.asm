; Chars for OSZ/DOS
; PUBLIC DOMAIN
[bits 16]
[org 0x0100]
	mov al, ' '
_loop:
	int 0x29
	inc al
	jns _loop
	ret
