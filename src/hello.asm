; Hello World for OSZ/DOS
%include "osz.inc"
[bits 16]
[org 0x0100]
	mov ah, 9
	mov dx, hello_msg
	int 0x21
	ret

hello_msg	db "Hello, world!", 13, 10, "$"
