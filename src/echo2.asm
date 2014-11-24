; ECHO for OSZ
; PUBLIC DOMAIN
%include "osz.inc"
[bits 16]
[org 0x0100]
	xor bp, bp
	mov dx, 0x0081
	mov ah, OSZ_DOS_PUTS
	call bp
	ret
