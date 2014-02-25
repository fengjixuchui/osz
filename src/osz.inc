;;	-*- coding: utf-8 -*-
;;
;;	MEG-OS Z - System common definitions
;;
;;	Copyright (c) 1998-2014, MEG-OS project
;;	All rights reserved.
;;	
;;	Redistribution and use in source and binary forms, with or without modification, 
;;	are permitted provided that the following conditions are met:
;;	
;;	* Redistributions of source code must retain the above copyright notice, this
;;	  list of conditions and the following disclaimer.
;;	
;;	* Redistributions in binary form must reproduce the above copyright notice, this
;;	  list of conditions and the following disclaimer in the documentation and/or
;;	  other materials provided with the distribution.
;;	
;;	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;;	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;	ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;


;;	OSZ SYSTEM TABLE
%define	OSZ_SYSTBL_SIZE		0x20
%define	OSZ_SYSTBL_CALLBIOS	0x00 ; INST (JMP FAR)
%define	OSZ_SYSTBL_BIOS		0x01 ; FAR16PTR
%define	OSZ_SYSTBL_CPUID	0x05 ; BYTE
%define	OSZ_SYSTBL_ARCH		0x06 ; BYTE
%define	OSZ_SYSTBL_BOOTDRV	0x07 ; BYTE
%define	OSZ_SYSTBL_MEMSZ	0x08 ; WORD (IN PARA)
%define	OSZ_SYSTBL_LASTMEM	0x0A ; WORD (SEG)
%define	OSZ_SYSTBL_IFS		0x0C ; DWORD
%define	OSZ_SYSTBL_VERSION	0x10 ; WORD
%define	OSZ_SYSTBL_MEMPROT	0x12 ; WORD (IN KB)


;;	BIOS
%define	BIOS_CONST		0
%define	BIOS_CONIN		1
%define	BIOS_CONOUT		2
%define	BIOS_CLS		3
%define	BIOS_POWER		4
%define	BIOS_DISPOSE	5
%define	BIOS_INIT_DISK	6
%define	BIOS_READ_DISK	7
%define	BIOS_WRITE_DISK	8
%define	BIOS_READ_MASS	9
%define	BIOS_WRITE_MASS	10


;;	BDOS
%define	OSZ_DOS_EXIT	0x00
%define	OSZ_DOS_CONIN	0x01
%define	OSZ_DOS_CONOUT	0x02
%define	OSZ_DOS_CONST	0x03
%define	OSZ_DOS_PUTS	0x04
%define	OSZ_DOS_GETS	0x05

%define	OSZ_DOS_SYSINFO	0x08

%define	OSZ_DOS_IFS		0x10
%define	OSZ_DOS_GET_CWD	0x10
%define	OSZ_DOS_SET_CWD	0x11
%define	OSZ_DOS_OPEN	0x12
%define	OSZ_DOS_CLOSE	0x13
%define	OSZ_DOS_READ	0x14
%define	OSZ_DOS_WRITE	0x15
%define	OSZ_DOS_SEEK	0x16
%define	OSZ_DOS_ENUM_FILE	0x17
%define	OSZ_DOS_IOCTL	0x18


;;	TERMIOS
%define	OSZ_TERMIOS_IGNBRK	0x0001
%define	OSZ_TERMIOS_BRKINT	0x0002
%define	OSZ_TERMIOS_ICANON	0x0010
%define	OSZ_TERMIOS_ECHO	0x0020
%define	OSZ_TERMIOS_VMIN	0x0000
%define	OSZ_TERMIOS_VTIME	0x0001


