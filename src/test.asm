; OSZ TEST APP

[bits 16]
[org 0x0100]

	db 0xC3, 0xE2	; MAGIC WORD

	db 0x10, 0x20	; ldc r0, 0x20
	db 0x11, 0x7F	; ldc r1, 0x7F
	;db 0x15, 0xFF, 0x7F
	db 0x08, 0x08	; for r0, r1
	db 0x0E	; cout
	db 0x09	; end for
