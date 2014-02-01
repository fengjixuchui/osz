; Chars for OSZ
%include "osz.inc"
[bits 16]
[org 0x0100]
	mov dl, ' '
_loop:
	mov cl, OSZ_DOS_CONOUT
	call bp
	inc dx
	cmp dl, 0x7F
	jb _loop
	ret
