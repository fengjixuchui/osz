#! /usr/bin/rake
# -*- coding: utf-8 -*-
#
# Rakefile for MEG-OS Z
# Copyright(C)2014 MEG-OS project, ALL RIGHTS RESERVED.
#
require 'rake/clean'
require 'rake/packagetask'
#require 'yaml'

CC		= "clang -Os"
AS		= "yasm -s"
AFLAGS	= "-f bin"

PATH_OUTPUT     = "bin/"
PATH_SRC        = "src/"
PATH_MISC       = "misc/"

PATH_BOOT_FLP	= "#{PATH_OUTPUT}floppy.img"
PATH_CATARC     = "#{PATH_OUTPUT}catarc"

APP_EXT			= ".com"

PATH_FDBOOT_IPL = "#{PATH_OUTPUT}fdboot.bin"
PATH_OS_SYS 	= "#{PATH_OUTPUT}osz.sys"


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

	# normal apps
  APPS = %w[ hello chars echo2 cpuid pipo test ].collect do |t|
	bin = "#{ PATH_OUTPUT }#{ t }#{ APP_EXT }"
    src = "#{ PATH_SRC }#{ t }.asm"
    file bin => src do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
    end
    bin
  end

	# extras
  EXTRAS = [
    "#{PATH_SRC}hello.asm",
	FileList["extras/*"]
  ].flatten

	# tfdisk
  PATH_TFDISK_SRC = "#{ PATH_SRC }tfdisk/tfdisk.asm"
  PATH_TFDISK_BIN = "#{ PATH_OUTPUT }tfdisk#{ APP_EXT }"
  APPS << PATH_TFDISK_BIN
  
  PATH_TFMBR_BIN = "#{ PATH_OUTPUT }tfmbr.bin"
  PATH_EXIPL_BIN = "#{ PATH_OUTPUT }exboot.bin"
  PATH_IPL16_BIN = "#{ PATH_OUTPUT }hdbt16.bin"
  PATH_IPL32_BIN = "#{ PATH_OUTPUT }hdbt32.bin"

  [PATH_TFMBR_BIN, PATH_EXIPL_BIN, PATH_IPL16_BIN, PATH_IPL32_BIN].each do | bin|
    src = PATH_SRC + "tfdisk/" + File.basename(bin, ".bin") + ".asm"
    file bin => src do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
    end
  end

  file PATH_TFDISK_BIN => [PATH_TFDISK_SRC, PATH_TFMBR_BIN, PATH_EXIPL_BIN, PATH_IPL16_BIN, PATH_IPL32_BIN] do |t|
    sh ["#{ AS } #{ AFLAGS } -i #{ PATH_SRC} -o #{t.name}",
      "-DPATH_MBR=\\\"#{File.expand_path(PATH_TFMBR_BIN)}\\\"",
      "-DPATH_EXIPL=\\\"#{File.expand_path(PATH_EXIPL_BIN)}\\\"",
      "-DPATH_IPL16=\\\"#{File.expand_path(PATH_IPL16_BIN)}\\\"",
      "-DPATH_IPL32=\\\"#{File.expand_path(PATH_IPL32_BIN)}\\\"",
      PATH_TFDISK_SRC].join(' ')
  end

	# kernel
  OSZ_MODS = %w[ osz2boot oszbio oszn98 fat12 oszshell ].collect do |t|
	bin = "#{ PATH_OUTPUT }mod_#{ t }.bin"
    src = "#{ PATH_SRC }#{ t }.asm"
    file bin => src do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
    end
    bin
  end

  file PATH_OS_SYS => OSZ_MODS do |t|
    sh "cat #{ t.prerequisites.join(' ') } > #{ t.name }"
  end

	# misc
  [ PATH_FDBOOT_IPL ].each do |bin|
    src = PATH_SRC + File.basename(bin, ".bin") + ".asm"
    file bin => src do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
    end
  end

	# fd image
  ROOT_FILES = [PATH_OS_SYS, APPS, EXTRAS].flatten.sort {|a, b| File.basename(a).upcase <=> File.basename(b).upcase }
  file PATH_BOOT_FLP => [ PATH_FDBOOT_IPL, ROOT_FILES].flatten do |t|
    sh "#{ PATH_CATARC } --bs #{ PATH_FDBOOT_IPL } #{ t.name } '#{ ROOT_FILES.join("' '") }'"
  end

end

