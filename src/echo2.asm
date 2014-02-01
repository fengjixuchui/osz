; ECHO for OSZ
%include "osz.inc"
[bits 16]
[org 0x0100]
	mov dx, 0x0081
	mov cl, OSZ_DOS_PUTS
	call bp
	ret
