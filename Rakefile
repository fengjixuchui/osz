#! /usr/bin/rake
# -*- coding: utf-8 -*-
#
# Rakefile for MEG-OS Zero
# Copyright(C)2014 MEG-OS project, ALL RIGHTS RESERVED.
#
require 'rake/clean'
require 'rake/packagetask'
#require 'yaml'

PATH_OUTPUT     = "bin/"
PATH_SRC        = "src/"
PATH_MISC       = "misc/"

PATH_BOOT_FLP	= "#{PATH_OUTPUT}floppy.img"
PATH_CATARC     = "#{PATH_OUTPUT}catarc"

APP_EXT			= ".com"

PATH_FDBOOT_IPL = "#{PATH_OUTPUT}fdboot.bin"
PATH_OSZ2BOOT_BIN = "#{PATH_OUTPUT}osz2boot.bin"
PATH_OSBIOS_BIN = "#{PATH_OUTPUT}oszbio.bin"
PATH_OSBIO2_BIN = "#{PATH_OUTPUT}oszn98.bin"
PATH_OSSHELL_BIN = "#{PATH_OUTPUT}oszshell.bin"
PATH_OSIFS_BIN  = "#{PATH_OUTPUT}oszifs.bin"
PATH_FAT12_BIN	= "#{PATH_OUTPUT}fat12.bin"
PATH_OS_SYS 	= "#{PATH_OUTPUT}osz.sys"


CC              = "clang -Os"
AS              = "yasm -s"
AFLAGS32        = "-f elf32"
AFLAGS64        = "-f elf64"
CATARC          = PATH_CATARC




SRCS = FileList["**/*.c"] << FileList["**/*.cpp"] << FileList["**/*.asm"]
OBJS = SRCS.ext('o')

CLEAN.include(OBJS)
CLOBBER.include(FileList["bin/**/*"])


directory PATH_OUTPUT

TARGET  = [ PATH_BOOT_FLP ]
SUB_TASKS = [ :tools ]

SUB_TASKS.each do |sub_task|
  task sub_task => [sub_task.to_s + ":build"]
end

desc "Defaults"
task :default => [PATH_OUTPUT, SUB_TASKS, TARGET].flatten do |t|
  puts "#### BUILD DONE ####"
end

desc "Run with qemu"
task :run => :default do
  sh "qemu-system-x86_64 -k ja -m 256 -boot a -fda #{ PATH_BOOT_FLP } -localtime -M pc"
end


desc "Run with Cable3"
task :cable3 => :default do
  begin
    sh "stty cbreak raw -echo min 0"
    sh "./cable3/8086tiny ./cable3/bios #{ PATH_BOOT_FLP }"
  ensure
    sh "stty cooked echo"
  end
end


desc "Run with 8086run"
task :run86 => :default do
  sh "./8086run/8086run #{ PATH_BOOT_FLP }"
end



####
# tools
namespace :tools do

  targets = [ PATH_CATARC ]

  task :build => [targets].flatten do
  end

  file "#{PATH_CATARC}" => "#{PATH_SRC}catarc.cpp" do |t|
    sh "#{ CC } -o #{t.name} #{t.prerequisites.join(' ')}"
  end

end

####
# OSZ
namespace :osz do

  APPS = [ "hello", "chars", "echo2", "cpuid" ].collect do |t|
	bin = "#{PATH_OUTPUT}#{t}#{ APP_EXT }"
    src = PATH_SRC + File.basename(bin, APP_EXT) + ".asm"
    file bin => src do |t|
      sh "#{ AS } -f bin -o #{t.name} #{t.prerequisites.join(' ')}"
    end
    bin
  end
  
  EXTRAS = [
    "#{PATH_SRC}hello.asm",
	FileList["extras/*"]
  ].flatten

  [PATH_FDBOOT_IPL, PATH_OSZ2BOOT_BIN, PATH_OSBIOS_BIN, PATH_OSBIO2_BIN, PATH_OSSHELL_BIN, PATH_FAT12_BIN].each do |bin|
    src = PATH_SRC + File.basename(bin, ".bin") + ".asm"
    file bin => src do |t|
      sh "#{ AS } -f bin -o #{t.name} #{t.prerequisites.join(' ')}"
    end
  end

  PATH_TFDISK_SRC = "#{ PATH_SRC }tfdisk/tfdisk.asm"
  PATH_TFDISK_BIN = "#{ PATH_OUTPUT }tfdisk#{ APP_EXT }"
  APPS << PATH_TFDISK_BIN
  
  PATH_TFMBR_BIN = "#{ PATH_OUTPUT }tfmbr.ipl"
  PATH_EXIPL_BIN = "#{ PATH_OUTPUT }exboot.ipl"
  PATH_IPL16_BIN = "#{ PATH_OUTPUT }hdbt16.ipl"
  PATH_IPL32_BIN = "#{ PATH_OUTPUT }hdbt32.ipl"

  [PATH_TFMBR_BIN, PATH_EXIPL_BIN, PATH_IPL16_BIN, PATH_IPL32_BIN].each do | bin|
    src = PATH_SRC + "tfdisk/" + File.basename(bin, ".ipl") + ".asm"
    file bin => src do |t|
      sh "#{ AS } -f bin -o #{t.name} #{t.prerequisites.join(' ')}"
    end
  end

  file PATH_TFDISK_BIN => [PATH_TFDISK_SRC, PATH_TFMBR_BIN, PATH_EXIPL_BIN, PATH_IPL16_BIN, PATH_IPL32_BIN] do |t|
    sh ["#{ AS } -i #{ PATH_SRC} -f bin -o #{t.name}",
      "-DPATH_MBR=\\\"#{File.expand_path(PATH_TFMBR_BIN)}\\\"",
      "-DPATH_EXIPL=\\\"#{File.expand_path(PATH_EXIPL_BIN)}\\\"",
      "-DPATH_IPL16=\\\"#{File.expand_path(PATH_IPL16_BIN)}\\\"",
      "-DPATH_IPL32=\\\"#{File.expand_path(PATH_IPL32_BIN)}\\\"",
      PATH_TFDISK_SRC].join(' ')
  end


  file PATH_OS_SYS => [PATH_OSZ2BOOT_BIN, PATH_OSBIOS_BIN, PATH_OSBIO2_BIN, PATH_FAT12_BIN, PATH_OSSHELL_BIN] do |t|
    sh "cat #{t.prerequisites.join(' ')} > #{ t.name }"
  end

  ROOT_FILES = [PATH_OS_SYS, APPS, EXTRAS].flatten
  file PATH_BOOT_FLP => [ PATH_FDBOOT_IPL, ROOT_FILES].flatten do |t|
    sh "#{ CATARC } --bs #{PATH_FDBOOT_IPL} #{ t.name } '#{ ROOT_FILES.join("' '") }'"
  end

end

