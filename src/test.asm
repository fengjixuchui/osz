; OSZ TEST APP

[bits 16]
[org 0x0100]

	mov ah, 9
	mov dx, test_msg
	int 0x21
	ret

test_msg	db "It works!", 13, 10, "$"
