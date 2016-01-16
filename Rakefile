# -*- coding: utf-8 -*-
#
# Rakefile for OSZ
# Copyright(C)2014,2015 MEG-OS project, ALL RIGHTS RESERVED.
#
require 'rake/clean'
require 'rake/packagetask'

CC		= "clang -Os"
AS		= "yasm -s"
AFLAGS	= "-f bin"

PATH_OUTPUT     = "bin/"
PATH_SRC        = "src/"
PATH_MISC       = "misc/"

PATH_FULL_IMG	= "#{PATH_OUTPUT}full.img"
PATH_CATARC     = "#{PATH_OUTPUT}catarc"

APP_EXT			= ".com"

PATH_OS_SYS 	= "#{PATH_OUTPUT}kernel.sys"


SRCS = FileList["**/*.c"] << FileList["**/*.cpp"] << FileList["**/*.asm"]
OBJS = SRCS.ext('o')

CLEAN.include(FileList["#{PATH_OUTPUT}/**/*"])

directory PATH_OUTPUT

TASKS = [ :tools, :osz ]

TASKS.each do |t|
  task t => [t.to_s + ":build"]
end

desc "Defaults"
task :default => [PATH_OUTPUT, TASKS].flatten


desc "Run with qemu"
task :runqemu => :default do
  sh "qemu-system-x86_64 -k ja -m 256 -boot a -fda #{ PATH_FULL_IMG } -localtime -M pc"
end

desc "Run with 8086run"
task :run => :default do
  sh "./8086run/8086run -strict #{ PATH_FULL_IMG }"
end



####
# tools
namespace :tools do

  targets = [ PATH_CATARC ]

  task :build => [targets].flatten

  file "#{PATH_CATARC}" => "#{PATH_SRC}catarc.cpp" do |t|
    sh "#{ CC } -o #{t.name} #{t.prerequisites.join(' ')}"
  end

end

def make_disk(output, ipl, files)
  #puts "MAKEDISK #{ output } <= #{ ipl } #{ files.join (' ') }"
  file output => [ 'Rakefile', PATH_CATARC, ipl, files].flatten do |t|
    sh "#{ PATH_CATARC } --bs #{ ipl } #{ t.name } '#{ files.join("' '") }'"
  end
end


####
# OSZ
namespace :osz do

  PATH_OSZ_INC = "#{ PATH_SRC}osz.inc"

  ALL_OBJS = []

  # make kernel mods
  def make_mod(name)
    t = name
    bin = "#{ PATH_OUTPUT }#{ t }.bin"
    src = "#{ PATH_SRC }#{ t }.asm"
    file bin => [src, PATH_OSZ_INC] do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{ src }"
    end
    ALL_OBJS.push bin
    bin
  end

  # make normal apps
  def make_app(name)
    t = name
    bin = "#{ PATH_OUTPUT }#{ t }#{ APP_EXT }"
    src = "#{ PATH_SRC }#{ t }.asm"
    file bin => [src, PATH_OSZ_INC] do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{ src }"
    end
    ALL_OBJS.push bin
    bin
  end

  def make_kernel(output, locore, files)
    file output => [ 'Rakefile', locore, files].flatten do |t|

      size_dirent = 8

      bin_locore = IO.binread(locore).unpack('C*')
      puts "LOCORE: #{ locore } (#{ bin_locore.length })"

      dir = []

      blob = []
      files.each do |file|
        dirent = Array.new(size_dirent, 0)
        name = File.basename(file, '.*').upcase
        bin = IO.binread(file).unpack('C*')
        dirent[0] = bin.length & 0xFF
        dirent[1] = (bin.length>>8) & 0xFF
        name.unpack('C*').each_with_index do |c, i|
          dirent[2+i] = c
        end
        puts " + #{ name } <= #{ file } (#{ bin.length })"
        dir += dirent.slice(0, size_dirent)
        blob += bin
      end
      dir += Array.new(size_dirent, 0)

      obj = bin_locore + dir + blob
      size = obj.length
      raise "#{ output }: Out of Segment!" if size >= 0x10000
      obj[5] = files.length
      obj[6] = size & 0xFF
      obj[7] = (size >> 8) & 0xFF
      offset = bin_locore.length + dir.length
      obj[8] = offset & 0xFF
      obj[9] = (offset >> 8) &0xFF

      puts "OUTPUT: #{ obj.length }"
      IO.write(t.name, obj.pack('C*'))
    end
  end


  # normal apps
  APP_DEFAULTS = %w(hello chars echo2 bf pipo ).collect {|t| make_app(t) }
  APP_NO_DEFAULTS = %w(cpuid).collect {|t| make_app(t) }

  # extras
  EXTRAS = [
    FileList["extras/*"]
    ].flatten

  # tfdisk
  PATH_TFDISK_SRC = "#{ PATH_SRC }tfdisk/tfdisk.asm"
  PATH_TFDISK_BIN = "#{ PATH_OUTPUT }tfdisk#{ APP_EXT }"

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

  file PATH_TFDISK_BIN => [PATH_TFDISK_SRC, PATH_TFMBR_BIN, PATH_EXIPL_BIN, PATH_IPL16_BIN, PATH_IPL32_BIN, PATH_OSZ_INC] do |t|
    sh ["#{ AS } #{ AFLAGS } -i #{ PATH_SRC} -o #{t.name}",
      "-DPATH_MBR=\\\"#{File.expand_path(PATH_TFMBR_BIN)}\\\"",
      "-DPATH_EXIPL=\\\"#{File.expand_path(PATH_EXIPL_BIN)}\\\"",
      "-DPATH_IPL16=\\\"#{File.expand_path(PATH_IPL16_BIN)}\\\"",
      "-DPATH_IPL32=\\\"#{File.expand_path(PATH_IPL32_BIN)}\\\"",
      PATH_TFDISK_SRC].join(' ')
  end
  ALL_OBJS << PATH_TFDISK_BIN
  APP_NO_DEFAULTS << PATH_TFDISK_BIN


  # kernel
  OSZ_LOCORE = make_mod('osz2boot')
  OSZ_MODS = %w(oszbio oszn98 fat12 oszdos).collect {|t| make_mod(t) }
  make_kernel PATH_OS_SYS, OSZ_LOCORE, OSZ_MODS


  # ipls
  IPLS = {}
  %w(F0_1440 F9_720 FE_1232).each do |ipl|
    srcipl = "#{ PATH_SRC }fdipl.asm"

    bpbsrc = "#{ PATH_SRC }bpb_#{ ipl }.asm"
    bpbbin = "#{ PATH_OUTPUT }bpb_#{ ipl }.bin"
    file bpbbin => bpbsrc do |t|
      sh "#{ AS } #{ AFLAGS } -o #{ t.name } #{ t.prerequisites.join(' ') }"
    end

    output = "#{ PATH_OUTPUT }ipl_#{ ipl }.bin"
    file output => [srcipl, bpbbin] do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{ srcipl } -DPATH_BPB=\\\"#{ File.expand_path(bpbbin) }\\\""
    end

    IPLS[ipl.to_sym] = output
  end


  # images
  APP_FULL = [APP_DEFAULTS, APP_NO_DEFAULTS]
  IMAGES = []
  [
    { name: :mini, ipl: :F0_1440, files: [APP_DEFAULTS, EXTRAS] },
    { name: :full98, ipl: :FE_1232, files: [APP_FULL, EXTRAS] },
    { name: :full, ipl: :F0_1440, files: [APP_FULL, EXTRAS] }
  ].each do |imgdef|
    output = "#{ PATH_OUTPUT }#{ imgdef[:name] }.img"
    files = [imgdef[:files]].flatten.sort {|a, b| File.basename(a).upcase <=> File.basename(b).upcase }
    files.unshift PATH_OS_SYS
    make_disk output, IPLS[imgdef[:ipl]], files
    IMAGES << output
  end


  desc "Build OSZ"
  task :build => [ALL_OBJS, IMAGES].flatten

end
