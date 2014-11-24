;;  -*- coding: utf-8 -*-
;;
;;  MEG OSZ - System common definitions
;;
;;  Copyright (c) 1998-2014, MEG-OS project
;;  All rights reserved.
;;  
;;  Redistribution and use in source and binary forms, with or without modification, 
;;  are permitted provided that the following conditions are met:
;;  
;;  * Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;  
;;  * Redistributions in binary form must reproduce the above copyright notice, this
;;    list of conditions and the following disclaimer in the documentation and/or
;;    other materials provided with the distribution.
;;  
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;;  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;


;;  OSZ SYSTEM TABLE
%define OSZ_SYSTBL_SIZE     0x20
%define OSZ_SYSTBL_CALLBIOS 0x00 ; INST (JMP FAR)
%define OSZ_SYSTBL_BIOS     0x01 ; FAR16PTR
%define OSZ_SYSTBL_CPUID    0x05 ; BYTE
%define OSZ_SYSTBL_ARCH     0x06 ; BYTE
%define OSZ_SYSTBL_BOOTDRV  0x07 ; BYTE
%define OSZ_SYSTBL_VERSION  0x08 ; WORD
%define OSZ_SYSTBL_REVISION 0x0A ; WORD
%define OSZ_SYSTBL_MEMSZ    0x0C ; WORD (IN PARA)
%define OSZ_SYSTBL_MEMPROT  0x0E ; WORD (IN KB)
%define OSZ_SYSTBL_LASTMEM  0x10 ; WORD (SEG)


;;  BIOS
%define BIOS_CONST          0x00
%define BIOS_CONIN          0x01
%define BIOS_CONOUT         0x02
%define BIOS_CLS            0x03
%define BIOS_POWER          0x04
%define BIOS_DISPOSE        0x05
%define BIOS_INIT_DISK      0x06
%define BIOS_READ_DISK      0x07
%define BIOS_WRITE_DISK     0x08
%define BIOS_BEEP           0x09
%define BIOS_GET_TICK       0x0A
%define BIOS_FD_STATUS      0x0B

%define BIOS_READ_MASS      99999
%define BIOS_WRITE_MASS     99999


;;  BDOS
%define OSZ_DOS_EXIT        0x00
%define OSZ_DOS_CONIN       0x01
%define OSZ_DOS_CONOUT      0x02
%define OSZ_DOS_CONST       0x03
%define OSZ_DOS_PUTS        0x04
%define OSZ_DOS_GETS        0x05

%define OSZ_DOS_SYSINFO     0x08
%define OSZ_DOS_BEEP        0x09
%define OSZ_DOS_GET_TICK    0x0A
%define OSZ_DOS_WAIT_TICK   0x0B

%define OSZ_DOS_IFS         0x10
%define OSZ_DOS_GET_CWD     0x10
%define OSZ_DOS_SET_CWD     0x11
%define OSZ_DOS_OPEN        0x12
%define OSZ_DOS_CLOSE       0x13
%define OSZ_DOS_READ        0x14
%define OSZ_DOS_WRITE       0x15
%define OSZ_DOS_SEEK        0x16
%define OSZ_DOS_ENUM_FILE   0x17
%define OSZ_DOS_IOCTL       0x18


;;  IFS
%define OSZ_IFS_GET_CWD     0x00
%define OSZ_IFS_SET_CWD     0x01
%define OSZ_IFS_OPEN        0x02
%define OSZ_IFS_CLOSE       0x03
%define OSZ_IFS_READ        0x04
%define OSZ_IFS_WRITE       0x05
%define OSZ_IFS_SEEK        0x06
%define OSZ_IFS_ENUM_FILE   0x07
%define OSZ_IFS_FAST_FIND   0x08
%define OSZ_IFS_IOCTL       0x09
%define OSZ_IFS_INSTALL     0x0A



;;  INT 3F MULTIPLEX
%define OSZ_I3F_IFS         0x10
%define OSZ_I3F_HANDLE_MAGIC    0x55AA


;;  TERMIOS
%define OSZ_TERMIOS_IGNBRK  0x0001
%define OSZ_TERMIOS_BRKINT  0x0002
%define OSZ_TERMIOS_ICANON  0x0010
%define OSZ_TERMIOS_ECHO    0x0020
%define OSZ_TERMIOS_VMIN    0x0000
%define OSZ_TERMIOS_VTIME   0x0001


