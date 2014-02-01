; Hello World for OSZ
%include "osz.inc"
[bits 16]
[org 0x0100]
	mov cl, OSZ_DOS_PUTS
	mov dx, hello_msg
	call bp
	ret

hello_msg	db "Hello, world!",0
