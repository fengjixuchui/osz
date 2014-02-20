; Chars for OSZ
%include "osz.inc"
[bits 16]
[org 0x0100]
	mov al, ' '
_loop:
	int 0x29
	inc al
	jns _loop
	ret
