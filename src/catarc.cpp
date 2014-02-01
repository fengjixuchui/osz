/* -*- coding: utf-8 -*-

A File Archiver Tool for Mastering MEG-OS

Copyright (c) 1998-2008, MEG-OS Project
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/
#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
//#include <sys/types.h>
#include <sys/stat.h>
//#include <unistd.h>

#pragma pack(1)


//#define	CONFIG_USE_RJC


#define	pathdat		".arc-data.tmp"
#define	pathtek		".arc-z.tmp"
#define	pathstub	".arc-stub.tmp"

#define	MAX_FILENAME	255

#define	OPT_FILENAME_FLAGS_BASE_UPPER	0x0001
#define	OPT_FILENAME_FLAGS_BASE_LOWER	0x0002
#define	OPT_FILENAME_FLAGS_BASE_CASE	(OPT_FILENAME_FLAGS_BASE_UPPER|OPT_FILENAME_FLAGS_BASE_LOWER)
#define	OPT_FILENAME_FLAGS_EXT_UPPER	0x0010
#define	OPT_FILENAME_FLAGS_EXT_LOWER	0x0020
#define	OPT_FILENAME_FLAGS_EXT_CASE		(OPT_FILENAME_FLAGS_EXT_UPPER|OPT_FILENAME_FLAGS_EXT_LOWER)

#define	MULTIBOOT_MAGIC					0x1BADB002

struct cat_header {
	char		signature[4];
	uint32_t	files,base,size_total,size;
	uint8_t		padding_shift;
	uint8_t		RESERVED[11];
};

struct file_stack_long {
	char		filename[MAX_FILENAME+1];
	uint32_t	ctime,mtime,offset,size;
};

struct file_stack_short {
	char		filename[16];
	uint32_t	ctime,mtime,offset,size;
};

struct FAT_DIRENT {
	uint8_t		filename[11];
	uint8_t		attr[2];
	uint8_t		ctime_tenth;
	uint32_t	ctime;
	uint16_t	adate;
	uint16_t	index_hi;
	uint32_t	mtime;
	uint16_t	index;
	uint32_t	size;
};

struct FAT_LFNENT {
	uint8_t		order;
	uint8_t		name1[10];
	uint8_t		attr;
	uint8_t		type;
	uint8_t		checksum;
	uint8_t		name2[12];
	uint16_t	MBZ;
	uint8_t		name3[4];
};

struct FAT_BPB {
	uint8_t		jumps[3];
	char		oem_name[8];
	uint16_t	bytes_per_sector;
	uint8_t		sectors_per_cluster;
	uint16_t	reserved_sectors;
	uint8_t		n_fats;
	uint16_t	root_entries;
	uint16_t	total_sectors;
	uint8_t		media_descriptor;
	uint16_t	sectors_per_fat;
	uint16_t	sectors_per_track;
	uint16_t	heads;
	uint32_t	hidden_sectors;
	uint32_t	total_sectors32;
	uint8_t		drive_number, UNKNOWN, boot_signature;
	uint32_t	serial_number;
	char		volume_lavel[11];
	char		fs_signature[8];
	uint8_t		RESERVED[2];
	char		SF16_signature[4];
	uint16_t	SF16_max_clusters;
	uint8_t		SF16_misc[2];
	uint32_t	SF16_max_sectors;
	uint8_t		SF16_RESERVED[4];
	uint8_t		codes[430];
	uint8_t		signature[2];
};

#define	MAX_PADDING			12
#define	SIZE_BUFFER			0x1000
static char buff[SIZE_BUFFER];

static char head_signature[]={ '\x7F','A','R','C' };
static char udi_signature[]={ '\x7F','U','D','I' };

const char* invalid_sfn_list[]={
	"CON    ",
	"AUX    ",
	"PRN    ",
	"NUL    ",
	"CLOCK$ ",
	"LPT0   ","LPT1   ","LPT2   ","LPT3   ","LPT4   ","LPT5   ","LPT6   ","LPT7   ","LPT8   ","LPT9   ",
	"COM0   ","COM1   ","COM2   ","COM3   ","COM4   ","COM5   ","COM6   ","COM7   ","COM8   ","COM9   ",
	NULL
};

int LFN_validate_filename_char(char c){
	switch(c){
	case 0x20:
	case 0x21:
	case 0x23 ... 0x29:
	case 0x2B ... 0x2E:
	case '0' ... '9':
	case 0x3B:
	case 0x3D:
	case 'A' ... 'Z':
	case 0x5B:
	case 0x5D ... 0x5F:
	case 'a' ... 'z':
	case 0x7B:
	case 0x7D:
	case 0x7E:
		return c;
	default:
		return '\0';
	}
}

int FAT_validate_filename_char(char c){
	switch(c){
	case 0x21:
	case 0x23 ... 0x29:
	case 0x2D:
	case '0' ... '9':
	case 'A' ... 'Z':
	case 0x5E:
	case 0x5F:
	case 0x7B:
	case 0x7D:
	case 0x7E:
		return c;
	case 'a' ... 'z':
		return c-0x20;
	default:
		return '_';
	}
}

void parse_filename(char* p,uint8_t* q,uint8_t NT_reserved_flag){
	int e=8;
	if(q[e]==' '){	// no ext
		for(;e&&q[e-1]==' ';e--){}
		if(NT_reserved_flag & 0x08){
			for(int i=0;i<e;i++){
				p[i]=tolower(q[i]);
			}
		}else{
			for(int i=0;i<e;i++){
				p[i]=(q[i]);
			}
		}
		p[e]=0;
	}else{	//	has extension
		for(;e&&q[e-1]==' ';e--){}
		int i=0;
		if(NT_reserved_flag & 0x08){
			for(;i<e;i++){
				p[i]=tolower(q[i]);
			}
		}else{
			for(;i<e;i++){
				p[i]=(q[i]);
			}
		}
		p[i]='.',i++;
		if(NT_reserved_flag & 0x10){
			for(int n=0;n<3;n++,i++){
				p[i]=tolower(q[8+n]);
				if(p[i]==' ') break;
			}
		}else{
			for(int n=0;n<3;n++,i++){
				p[i]=(q[8+n]);
				if(p[i]==' ') break;
			}
		}
		p[i]=0;
	}
}

int write_zero(FILE* file,size_t size){
	if(!size) return 0;
	memset(buff,0,SIZE_BUFFER);
	if(size<SIZE_BUFFER){
		fwrite(buff,1,size,file);
	}else{
		for(int i=0;i<size/SIZE_BUFFER;i++){
			fwrite(buff,1,SIZE_BUFFER,file);
		}
		fwrite(buff,1,size%SIZE_BUFFER,file);
	}
	return 0;
}

int if_collide_shortfilename(file_stack_long* indexes,FAT_DIRENT* dir,int files,FAT_DIRENT* file){
	char buff83[16];
	parse_filename(buff83,file->filename,0);
	for(int i=0;i<files;i++){
		if(!strcasecmp(indexes[i].filename,buff83)){
			return 1;
		}
	}
	for(int i=0;i<files;i++){
		if(!memcmp(dir[i].filename,file->filename,11)){
			return 1;
		}
	}
	return 0;
}

void write_fat_entry(uint8_t* _fat,int fattype,int offset,int value){
	switch(fattype){
	case 12:
		{
			int offset_fat=offset*3/2;
			if(offset&1){
				_fat[offset_fat]=((value&0xF)<<4)|(_fat[offset_fat]&0x0F);
				_fat[offset_fat+1]=value>>4;
			}else{
				_fat[offset_fat]=value;
				_fat[offset_fat+1]=((value>>8)&0x0F)|(_fat[offset_fat+1]&0xF0);
			}
			break;
		}
	case 16:
		{
			uint16_t* fat=(uint16_t*)_fat;
			fat[offset]=value;
			break;
		}
	case 32:
		{
			uint32_t* fat=(uint32_t*)_fat;
			fat[offset]=value&0x0FFFFFFF;
			break;
		}
	}
}

int compute_bits_length(uint64_t data){
	int bits=0;
	for(;data;data>>=1){
		bits++;
	}
	return bits;
}

#ifdef CONFIG_USE_RJC
int rjc(int size, uint8_t* p0, int mode)
/* mode:	0:decode, 1:encode */
{
	uint8_t *p = p0, *p1 = p0 + size, *pp = p0 - 4;
	int ofs0 = 0, ofs = 0, ofs1 = size;
	int i, j, k, m = 0;
	while (p < p1) {
		if (0xe8 <= *p && *p <= 0xe9 && &p[4] < p1) {  /* e8 (call32), e9 (jmp32) */
	r32:
			p++;
			if (p - pp < 4)
				continue;
			i = p[0] | p[1] << 8 | p[2] << 16 | p[3] << 24;
			k = i;
			j = (p - p0) + ofs + 4; /* 相対アドレス基点 */
			pp = p;
			if (i == 0 || i == 0x80808080)
				i ^= 0x80808080;
			if (mode) { /* encode */
				if (ofs0 - j <= i && i < ofs1 - j)
					i += j;
				else if (ofs1 - j <= i && i < ofs1)
					i -= ofs1 - ofs0;
			} else { /* decode */
				if (ofs0 <= i && i < ofs1)
					i -= j;
				else if (ofs0 - j <= i && i < ofs0)
					i += ofs1 - ofs0;
			}
			if (i == 0 || i == 0x80808080)
				i ^= 0x80808080;
			if (i != k) {
				p[0] =  i		& 0xff;
				p[1] = (i >>  8) & 0xff;
				p[2] = (i >> 16) & 0xff;
				p[3] = (i >> 24) & 0xff;
				p += 4;
				m++;
			}
			continue;
		}
		p++;
		if (p[-1] == 0x0f && &p[4] < p1 && (p[0] & 0xf0) == 0x80) /* 0f 8x (jcc32) */
			goto r32;
	}
	return m;
}
#endif

long get_file_size(FILE* file){
	long temp=ftell(file);
	fseek(file,0,SEEK_END);
	long result=ftell(file);
	fseek(file,temp,SEEK_SET);
	return result;
}

uint32_t time_to_dos_file_time(time_t* t){
	tm* tm=localtime(t);
	return 
		(tm->tm_sec/2)+
		((tm->tm_min)<<5)+
		((tm->tm_hour)<<11)+
		((tm->tm_mday)<<16)+
		((tm->tm_mon+1)<<21)+
		((tm->tm_year-80)<<25);
}




int main(int argc, char* argv[]) {
	file_stack_long fs;
	cat_header ch;

	FILE* finfile;
	int read_size=0;
	const char *arcfile=NULL,*opt_bs=NULL,*opt_stub=NULL,*arcfile_z=NULL;
	const char *bim2bin="bim2bin";
	int begin_arcs=0;
	int opt_rsize=0;
	int padding_size=0,padding_mask=0;
	FAT_BPB bpb;
	int fattype,sf16=0,max_clusters,max_sectors;
	bool opt_auto_attributes=false;
	bool opt_timestamp=true;
	bool opt_lfn=true;
	bool opt_force_tilde=false;
	bool opt_ntreserved=true;
	bool opt_force_sfn=false;
	bool opt_force_lfn=false;
	bool opt_touch=false;
	bool opt_touch_all=false;
	bool opt_udi=false;
	int opt_tek=0,opt_stk=0;
#ifdef CONFIG_USE_RJC
	int opt_rjc=0;
#endif

	size_t size_stub=0;
	uint8_t* stub=NULL;

	time_t time_local;
	time(&time_local);

	for(int i=1;i<argc;i++){
		if(argv[i][0]=='-'){
			switch(argv[i][1]){
				case '-':{
					const char* argv_opt_long=argv[i]+2;
					if(!strcmp(argv_opt_long,"rsize")){ // Record size
						opt_rsize=atoi(argv[i+1]);
						i++;
						break;
					}
					if(!strcmp(argv_opt_long,"stub")){ // specify stub file
						opt_stub=argv[i+1];
						i++;
						break;
					}
					if(!strcmp(argv_opt_long,"bs")){ // specify boot sector file
						opt_bs=argv[i+1];
						i++;
						break;
					}
					if(!strcmp(argv_opt_long,"bim2bin")){ // specify bim2bin
						bim2bin=argv[i+1];
						i++;
						break;
					}
					if(!strcmp(argv_opt_long,"tek1")){
						opt_tek=1;
						opt_stk=0;
						break;
					}
					if(!strcmp(argv_opt_long,"tek5")){
						opt_tek=5;
						opt_stk=0;
						break;
					}
					if(!strcmp(argv_opt_long,"stk1")){
						opt_tek=1;
						opt_stk=1;
						break;
					}
#ifdef CONFIG_USE_RJC
					if(!strcmp(argv_opt_long,"rjc")){
						opt_rjc=1;
						break;
					}
#endif
					if(!strcmp(argv_opt_long,"force-sfn")){
						opt_force_sfn=true;
						break;
					}
					if(!strcmp(argv_opt_long,"force-lfn")){
						opt_force_lfn=true;
						break;
					}
					if(!strcmp(argv_opt_long,"no-lfn")){
						opt_lfn=false;
						break;
					}
					if(!strcmp(argv_opt_long,"force-tilde")){
						opt_force_tilde=true;
						break;
					}
					if(!strcmp(argv_opt_long,"strip-tilde")){
						opt_force_tilde=false;
						break;
					}
					if(!strcmp(argv_opt_long,"ntreserved")){
						opt_ntreserved=true;
						break;
					}
					if(!strcmp(argv_opt_long,"no-ntreserved")){
						opt_ntreserved=false;
						break;
					}
					if(!strcmp(argv_opt_long,"auto-attributes")){
						opt_auto_attributes=true;
						break;
					}
					if(!strcmp(argv_opt_long,"no-auto-attributes")){
						opt_auto_attributes=false;
						break;
					}
					if(!strcmp(argv_opt_long,"touch-all")){
						opt_touch_all=true;
						opt_touch=true;
						break;
					}
					if(!strcmp(argv_opt_long,"timestamp")){
						opt_timestamp=true;
						break;
					}
					if(!strcmp(argv_opt_long,"no-timestamp")){
						opt_timestamp=false;
						break;
					}
					if(!strcmp(argv_opt_long,"universal-driver")){
						opt_udi=true;
						break;
					}
				}
				default:
					printf("Unknown option: %s",argv[i]);
					return -1;
			}
		}else{
			if(arcfile){
				begin_arcs=i;
				break;
			}else{
				arcfile=argv[i];
			}
		}
	}
	if(opt_udi){
		opt_bs=NULL;
		opt_rsize=12;
		if(!opt_tek){
			opt_tek=5;
			opt_stk=0;
		}
	}
	if(opt_force_sfn){
		opt_force_lfn=false;
		opt_lfn=false;
		opt_force_tilde=false;
		opt_ntreserved=false;
	}else if(opt_force_lfn){
		opt_lfn=true;
		opt_ntreserved=false;
	}
	if(!arcfile||!begin_arcs){
		printf(
			"Archive files for mastering\n"
			"usage: %s [OPTIONS...] ARCHIVE FILES...\n"
		,argv[0]);
		return 1;
	}

	if(opt_bs){
		FILE* fipl=fopen(opt_bs,"rb");
		if(!fipl){
			fprintf(stderr,"Error opening [%s]\n",opt_bs);
			return -1;
		}
		fread(&bpb,1,sizeof(bpb),fipl);
		fclose(fipl);

		if(bpb.signature[0]!=0x55 || bpb.signature[1]!=0xAA){
			fprintf(stderr,"Error unknown BPB signature (%02x %02x)\n",bpb.signature[0],bpb.signature[1]);
			return -1;
		}

		opt_rsize=compute_bits_length(bpb.bytes_per_sector*bpb.sectors_per_cluster)-1;

		//	SF16
		if(bpb.SF16_signature[0]==0x53 && bpb.SF16_signature[1]==0x46
		&& bpb.SF16_signature[2]==0x31 && bpb.SF16_signature[3]==0x36){
			sf16=1;
			fattype=16;
			max_sectors=bpb.SF16_max_sectors;
			max_clusters=(max_sectors-0x280)/bpb.sectors_per_cluster;
		}else{
			//	Legacy FAT
			if(bpb.total_sectors==0 && bpb.total_sectors32>0xFFFF){
				max_sectors	= bpb.total_sectors32;
			}else{
				max_sectors	= bpb.total_sectors;
			}
			max_clusters = (max_sectors-(bpb.reserved_sectors+(bpb.n_fats*bpb.sectors_per_fat)+(bpb.root_entries*sizeof(FAT_DIRENT)/bpb.bytes_per_sector)))/bpb.sectors_per_cluster;
			if(max_clusters < 4085){
				fattype=12;
			}else if(max_clusters < 65525){
				fattype=16;
			}else{
				fattype=32;
			}
		}

		char fattypestring[16];
		sprintf(fattypestring,"FAT%d   ",fattype);
		if(memcmp(bpb.fs_signature,fattypestring,8)){
			fprintf(stderr,"Error imcompatible file system [%.8s]:[%.8s]\n",
				bpb.fs_signature,fattypestring);
			return -1;
		}
		if(fattype>16){
			fprintf(stderr,"Sorry, FAT32 is not supported.\n");
			return -1;
		}

		printf("Boot sector for FAT%d [%.8s], %d bytes/sector, %d bytes/cluster, %d sectors, %d clusters.\n",
			fattype,bpb.oem_name,bpb.bytes_per_sector,1<<opt_rsize,max_sectors,max_clusters);

	}
	if(opt_rsize>MAX_PADDING) opt_rsize=MAX_PADDING;
	padding_size = 1<<opt_rsize;
	padding_mask = padding_size-1;
	if(opt_stub){
		FILE* fstub=fopen(opt_stub,"rb");
		if(!fstub){
			fprintf(stderr,"Error opening [%s]\n",opt_stub);
			return -1;
		}
		size_stub=get_file_size(fstub);
		stub=(uint8_t*)malloc(size_stub);
		fread(stub,1,size_stub,fstub);
		fclose(fstub);
	}

	if(opt_tek){
		arcfile_z=arcfile;
		arcfile=pathtek;
	}

	FAT_DIRENT* dir = (FAT_DIRENT*)malloc(sizeof(FAT_DIRENT)*argc);
	memset(dir,0,sizeof(FAT_DIRENT)*argc);
	file_stack_long* indexes = (file_stack_long*)malloc(sizeof(file_stack_long)*argc);
	memset(indexes,0,sizeof(file_stack_long)*argc);

	FILE* fdat=fopen(pathdat,"wb+");
	memset(&ch,0,sizeof(ch));
	for(int file_index=begin_arcs;file_index<argc;file_index++){
		memset(&fs,0,sizeof(fs));

		char* argv_copy=(char*)malloc(1+strlen(argv[file_index]));
		strcpy(argv_copy,argv[file_index]);
		const char* infile=argv_copy;

		// Convert '\' to '/' in file path
		if(strchr(argv_copy,'\\')){
			for(int i=0;argv_copy[i];i++){
				if(argv_copy[i]=='\\')
					argv_copy[i]='/';
			}
		}

		fs.offset=get_file_size(fdat);

		{
			char* p;

			if(p=strchr(argv_copy,'=')){ // RENAME
				infile=p+1;
				int filename_len=p-argv_copy;
				if(filename_len>MAX_FILENAME) filename_len=MAX_FILENAME;
				strncpy(fs.filename,argv_copy,filename_len);
				printf(" + %s (%s)\n",fs.filename,infile);
			}else if(p=strrchr(argv_copy,'/')){ // PATHed FILE
				strncpy(fs.filename,p+1,MAX_FILENAME);
				printf(" + %s (%s)\n",fs.filename,argv_copy);
			}else{ // NO PATH FILE
				strncpy(fs.filename,argv_copy,MAX_FILENAME);
				printf(" + %s\n",fs.filename);
			}

			for(int i=0;fs.filename[i];i++){
				if(!LFN_validate_filename_char(fs.filename[i])){
					fprintf(stderr,"Invalid filename: [%s]\n",fs.filename);
					return -1;
				}
			}

			//	long file name の衝突
			if(!opt_udi){
				for(int i=0;i<ch.files;i++){
					char buff83[16];
					parse_filename(buff83,dir[i].filename,0);
					if(!strcasecmp(buff83,fs.filename) || !strcasecmp(indexes[i].filename,fs.filename)){
						fprintf(stderr,"File name collided: [%s]\n",fs.filename);
						return -1;
					}
				}
			}

			if(opt_bs){

				bool force_tilde=false;

				// 8.3 short file name の生成
				memset(dir[ch.files].filename,' ',11);
				int basename_len;
				if(fs.filename[0]=='.'){
					force_tilde=opt_force_tilde;
					p = strrchr(fs.filename+1,'.');
				}else{
					p = strrchr(fs.filename,'.');
				}
				if(p){
					basename_len = p-fs.filename;
				}else{
					basename_len = strlen(fs.filename);
				}
				if(basename_len>8){
					force_tilde=opt_force_tilde;
					basename_len=8;
				}
				for(int i=0;i<basename_len;i++){
					dir[ch.files].filename[i]=FAT_validate_filename_char(fs.filename[i]);
				}
				if(p){
					int ext_len=strlen(p+1);
					if(ext_len>3){
						force_tilde=true;
						ext_len=3;
					}
					for(int i=0;i<ext_len;i++){
						dir[ch.files].filename[i+8]=FAT_validate_filename_char(p[i+1]);
					}
				}
				//	MSDOS予約名の衝突
				for(int i=0;invalid_sfn_list[i];i++){
					if(!memcmp(dir[ch.files].filename,invalid_sfn_list[i],7)){
						fprintf(stderr,"Invalid filename in DOS: [%s]\n",fs.filename);
						return -1;
					}
				}

				if(opt_auto_attributes){
					//	HIDDEN
					if(fs.filename[0]==('.')){
						dir[ch.files].attr[0]|=0x02;
					}
					//	SYSTEM
					if(!memcmp(dir[ch.files].filename+8,"SYS",3)
						||!strcasecmp(fs.filename,"command.com")
						||!strcasecmp(fs.filename,"autoexec.bat")
					){
						dir[ch.files].attr[0]|=0x04;
					}
				}

				//	short file name の衝突回避
				if(opt_force_tilde){
					char buff83[16];
					parse_filename(buff83,dir[ch.files].filename,0);
					if(strcasecmp(buff83,fs.filename)){
						force_tilde=true;
					}
				}
				if(opt_force_sfn){
					if(if_collide_shortfilename(indexes,dir,ch.files,&dir[ch.files])){
						fprintf(stderr,"Shoft file name collided: [%s]\n",fs.filename);
						return -1;
					}
				}else{
					int tilde_pos=basename_len;
					bool hash_not_used=true;
					uint16_t hash=0;
					for(int i=0;fs.filename[i];i++){
						hash=(hash<<15)+(hash>>1)+fs.filename[i];
					}
					if(tilde_pos>6) tilde_pos=6;
					if(force_tilde){
						dir[ch.files].filename[tilde_pos]='~';
						dir[ch.files].filename[tilde_pos+1]='1';
					}
					//	チルダと数字による衝突回避
					for(int i=force_tilde?2:1;if_collide_shortfilename(indexes,dir,ch.files,&dir[ch.files]);i++){
						if(hash_not_used && i>4){ // Windows 2000 はハッシュを埋め込む
							char buff[16];
							sprintf(buff,"%04X",hash);
							memcpy(dir[ch.files].filename+((tilde_pos>2)?2:1),buff,4);
							hash_not_used=false;
							i=1;
							tilde_pos=(tilde_pos>2)?6:5;
						}
						char buff[16];
						int tilde_len=sprintf(buff,"~%d",i);
						if(tilde_pos+tilde_len>8) tilde_pos = 8-tilde_len;
						memcpy(dir[ch.files].filename+tilde_pos,buff,tilde_len);
					}
				}
			}
		}
		finfile=fopen(infile,"rb");
		if(!finfile){
			fprintf(stderr,"Error opening [%s]\n",infile);
			return -1;
		}
		if(opt_timestamp){
			if(opt_touch){
				fs.mtime=time_to_dos_file_time(&time_local);
			}else{
				struct stat fst;
				stat(infile,&fst);
				fs.mtime=time_to_dos_file_time(&fst.st_mtime);
			}
			dir[ch.files].mtime=fs.mtime;
			dir[ch.files].ctime=fs.ctime=time_to_dos_file_time(&time_local);
		}
		dir[ch.files].size=fs.size=get_file_size(finfile);
		if(fs.size){
			dir[ch.files].index=2+fs.offset/padding_size;
		}
		indexes[ch.files] = fs;
		while(read_size=fread(buff,1,SIZE_BUFFER,finfile)){
			fwrite(buff,1,read_size,fdat);
		}
		fclose(finfile);
		if(opt_rsize){
			int n_padding = padding_size - (fs.size & padding_mask);
			if(n_padding>0 && n_padding!=padding_size){
				write_zero(fdat,n_padding);
			}
		}
		free(argv_copy);
		ch.files++;
	}

	FILE* fimg=fopen(arcfile,"wb");
	//	BOOT_SECTOR & FAT & DIR or CAT FILE HEADER
	if(opt_bs){
		fwrite(&bpb,1,sizeof(bpb),fimg);

		int n_padding=bpb.bytes_per_sector*bpb.reserved_sectors-sizeof(bpb);
		if(n_padding){
			write_zero(fimg,n_padding);
		}

		uint8_t* fat=(uint8_t*)malloc(bpb.sectors_per_fat*bpb.bytes_per_sector);
		memset(fat,0,bpb.sectors_per_fat*bpb.bytes_per_sector);
		write_fat_entry(fat,fattype,0,0xFFFFFF00+bpb.media_descriptor);
		write_fat_entry(fat,fattype,1,0xFFFFFFFF);
		if(sf16){
			for(int i=max_clusters;i<0x10000;i++){
				write_fat_entry(fat,fattype,i+2,0xFFFFFFF7);
			}
		}
		for(int i=0;i<ch.files;i++){
			if(dir[i].size){
				int index=dir[i].index;
				int size=(padding_size+dir[i].size-1)/padding_size-1;
				for(int j=0;j<size;j++){
					write_fat_entry(fat,fattype,index+j,index+j+1);
				}
				write_fat_entry(fat,fattype,index+size,0xFFFFFFFF);
			}
		}
		for(int i=0;i<bpb.n_fats;i++){
			fwrite(fat,1,bpb.sectors_per_fat*bpb.bytes_per_sector,fimg);
		}

		int root_dir_entries=0;
		for(int i=0;i<ch.files;i++){

			//	LFNは必要か？
			bool require_lfn=true;
			char buff83[16];
			parse_filename(buff83,dir[i].filename,0);
			if(!opt_force_lfn && !strcasecmp(buff83,indexes[i].filename)){
				char* filename=indexes[i].filename;
				char* p=strchr(filename,'.');
				int basename_len=p?p-filename:strlen(filename);
				int ext_len=p?strlen(p+1):0;
				if(basename_len<=8 && ext_len<=3){
					int flags=0;
					for(int j=0;j<basename_len;j++){
						char c=filename[j];
						switch(c){
							case 'A'...'Z':
								flags|=OPT_FILENAME_FLAGS_BASE_UPPER;
								break;
							case 'a'...'z':
								flags|=OPT_FILENAME_FLAGS_BASE_LOWER;
								break;
						}
					}
					for(int j=0;j<ext_len;j++){
						char c=p[1+j];
						switch(c){
							case 'A'...'Z':
								flags|=OPT_FILENAME_FLAGS_EXT_UPPER;
								break;
							case 'a'...'z':
								flags|=OPT_FILENAME_FLAGS_EXT_LOWER;
								break;
						}
					}
					if(((flags&OPT_FILENAME_FLAGS_BASE_CASE)!=OPT_FILENAME_FLAGS_BASE_CASE)
						&& ((flags&OPT_FILENAME_FLAGS_EXT_CASE)!=OPT_FILENAME_FLAGS_EXT_CASE)
						&& (opt_ntreserved || !(flags&(OPT_FILENAME_FLAGS_EXT_LOWER|OPT_FILENAME_FLAGS_BASE_LOWER)) )
					){
						if(flags&OPT_FILENAME_FLAGS_BASE_LOWER){
							dir[i].attr[1]|=0x08;
						}
						if(flags&OPT_FILENAME_FLAGS_EXT_LOWER){
							dir[i].attr[1]|=0x10;
						}
						require_lfn=false;
					}
				}
			}

			if(opt_force_lfn || (opt_lfn && require_lfn)){
				FAT_LFNENT fatlfn;
				memset(&fatlfn,0,sizeof(fatlfn));
				fatlfn.attr = 0x0F;
				for(int j=0;j<11;j++){
					fatlfn.checksum=(fatlfn.checksum<<7)+(fatlfn.checksum>>1)+dir[i].filename[j];
				}
				int lfn_len = strlen(indexes[i].filename);
				int lfn_ents = (lfn_len+12)/13;
				for(int j=lfn_ents,j0=0;j>0;j0++,j--){
					uint16_t ucs_part[13];
					memset(ucs_part,0xFF,26);
					fatlfn.order = (j0)?j:0x40+j;
					for(int k=0;k<13;k++){
						ucs_part[k] = indexes[i].filename[j*13-13+k];
						if(ucs_part[k]==0) break;
					}
					memcpy(&fatlfn.name1,ucs_part,10);
					memcpy(&fatlfn.name2,ucs_part+5,12);
					memcpy(&fatlfn.name3,ucs_part+11,4);
					fwrite(&fatlfn,1,sizeof(fatlfn),fimg);
					root_dir_entries++;
				}
			}
			fwrite(dir+i,1,sizeof(FAT_DIRENT),fimg);
			root_dir_entries++;
		}
		write_zero(fimg,sizeof(FAT_DIRENT)*(bpb.root_entries-root_dir_entries));
	}else{
		memcpy(ch.signature,head_signature,sizeof(head_signature));
		ch.size=get_file_size(fdat);
		ch.base=(sizeof(cat_header)+(ch.files+1)*sizeof(file_stack_short)+padding_mask)&~padding_mask;
		ch.size_total=ch.base+ch.size;
		ch.padding_shift=opt_rsize;
		fwrite(&ch,1,sizeof(ch),fimg);
		file_stack_short fss;
		memset(&fss,0,sizeof(fss));
		for(int i=0;i<ch.files;i++){
			memcpy(&fss.filename,&indexes[i].filename,15);
			fss.filename[15]=0;
			memcpy(&fss.mtime,&indexes[i].mtime,12);
			fwrite(&fss,1,sizeof(file_stack_short),fimg);
		}
		write_zero(fimg,ch.base-get_file_size(fimg));
	}
	fseek(fdat,0,SEEK_SET);
	while(read_size=fread(buff,1,SIZE_BUFFER,fdat)){
		fwrite(buff,1,read_size,fimg);
	}
	if(opt_rsize){
		int n_padding = padding_size - (get_file_size(fimg) & padding_mask);
		if(n_padding>0 && n_padding!=padding_size){
			write_zero(fdat,n_padding);
		}
	}
	if(opt_bs){
		int r=max_sectors-(get_file_size(fimg)/bpb.bytes_per_sector);
		for(int i=0;i<r;i++){
			write_zero(fimg,bpb.bytes_per_sector);
		}
	}

#ifdef CONFIG_USE_RJC
	if(opt_rjc){
		size_t size_rjc_buff=GetFileSize(himg,NULL);
		uint8_t* rjc_buff=(uint8_t*)(size_rjc_buff);
		SetFilePointer(himg,0,NULL,0);
		ReadFile(himg,rjc_buff,size_rjc_buff,&p,NULL);
		rjc(size_rjc_buff, rjc_buff, 1);
		SetFilePointer(himg,0,NULL,0);
		WriteFile(himg,rjc_buff,size_rjc_buff,&p,NULL);
		delete[] rjc_buff;
	}
#endif

	fclose(fimg);
	fclose(fdat);
	remove(pathdat);

	if(opt_tek){
		char buff[256];
		sprintf(buff,
			"%s -osacmp -tek%d %s in:%s out:%s"
			,bim2bin
			,opt_tek
			,(opt_stk)?"BS:0":""
			,pathtek
			,arcfile_z
			);
		int r=system(buff);
		remove(pathtek);
		if(r){
			fprintf(stderr,"exec error: %s\n",buff);
			return r;
		}
		arcfile=arcfile_z;
		arcfile_z=0;
	}

	if(opt_stub){
		rename(arcfile,pathstub);
		finfile=fopen(pathstub,"rb");
		fimg=fopen(arcfile,"wb");

		// Multiboot ヘッダーの修正
		{
			uint32_t* p=(uint32_t*)stub;
			uint32_t* q=NULL;
			for(int i=0;i<size_stub/4;i++,p++){
				if(p[0]==MULTIBOOT_MAGIC && p[2]==-(p[1]+MULTIBOOT_MAGIC)){
					q=p;
				}
			}
			if(q){
				printf("stub: Multiboot signature found: at %08x\n",(intptr_t)q-(intptr_t)stub);
				q[5]=q[6]=q[4]+size_stub+get_file_size(finfile);
			}
		}
		fwrite(stub,1,size_stub,fimg);

		while(read_size=fread(buff,1,SIZE_BUFFER,finfile)){
			fwrite(buff,1,read_size,fimg);
		}
		fclose(finfile);
		fclose(fimg);
		remove(pathstub);
	}

	return 0;
}

