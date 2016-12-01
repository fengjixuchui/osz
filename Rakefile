# -*- coding: utf-8 -*-
#
# Rakefile for OSZ
# Copyright(C)2014,2015,2016 MEG-OS project, ALL RIGHTS RESERVED.
#
require 'rake/clean'
require 'rake/packagetask'
require 'msgpack'

PATH_OUTPUT     = "bin/"
PATH_SRC        = "src/"
PATH_TOOLS      = "tools/"
PATH_MISC       = "misc/"
PATH_TEMP       = "temp/"

CC		  = ENV['CC'] || ENV['CXX'] || "clang"
CFLAGS  = "-Os"
AS		  = ENV['AS'] || "nasm"
AFLAGS	= "-s -I #{ PATH_SRC } -f bin"

PATH_FULL_IMG   = "#{PATH_OUTPUT}full.vfd"
PATH_CATARC     = "#{PATH_TOOLS}catarc"
PATH_BIM2BIN    = "#{PATH_TOOLS}bim2bin"

APP_EXT			= ".com"

PATH_OS_SYS 	= "#{PATH_OUTPUT}kernel.sys"


SRCS = FileList["**/*.c"] << FileList["**/*.cpp"] << FileList["**/*.asm"]
OBJS = SRCS.ext('o')

CLEAN.include(FileList["#{PATH_OUTPUT}/**/*"])

directory PATH_OUTPUT
directory PATH_TEMP

TASKS = [ :tools, :osz ]

TASKS.each do |t|
  task t => [t.to_s + ":build"]
end

desc "Defaults"
task :default => [PATH_OUTPUT, PATH_TEMP, TASKS].flatten


desc "Run with 8086run"
task :run => :default do
  sh "./8086run/8086run -strict #{ PATH_FULL_IMG }"
end


desc "Run with qemu"
task :runqemu => :default do
  sh "qemu-system-x86_64 -k ja -m 256 -boot a -fda #{ PATH_FULL_IMG } -localtime -M pc"
end


####
# tools
namespace :tools do

  targets = [ PATH_CATARC, PATH_BIM2BIN ]

  desc "Build Tools"
  task :build => [targets].flatten

  file "#{PATH_CATARC}" => "#{PATH_TOOLS}catarc.cpp" do |t|
    sh "#{ CC } #{ CFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
  end

  file "#{PATH_BIM2BIN}" => "#{PATH_TOOLS}bim2bin.c" do |t|
    sh "#{ CC } #{ CFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
  end

end

def make_disk(output, ipl, files)
  # TODO: replace by fully ruby implementation
  file output => [ 'Rakefile', PATH_CATARC, ipl, files].flatten do |t|
    #puts "MAKEDISK #{ output } <= #{ ipl } #{ files }"
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

  def make_kernel(output, locore, mods, init_files)
    file output => [ 'Rakefile', locore, mods, init_files].flatten do |t|

      bin_locore = IO.binread(locore).unpack('C*')
      puts "LOCORE: #{ locore } $#{ bin_locore.length.to_s(16) } (#{ bin_locore.length })"

      # mods (bios, bdos)
      tbl = []
      blob = []
      mods.each do |file|
        bin = IO.binread(file).unpack('C*')
        puts " MOD #{ file } ($#{ bin.length.to_s(16) } #{ bin.length })"
        tbl += [ bin.length & 0xFF, (bin.length>>8) & 0xFF ]
        blob += bin
      end
      tbl += [0, 0]
      offset1 = bin_locore.length + tbl.length
      offset2 = offset1 + blob.length

      # PADDING
      if ((offset2%16) > 0) then
        blob += Array.new(16-(offset2%16), 0)
        offset2 = offset1 + blob.length
      end

      # initrd
      rdblob = []
      dir = []
      msgpack = {}
      init_files.each_with_index do |file, index|
        name = File.basename(file)
        orgsize = File.size(file)
        temp = "#{ PATH_TEMP }#{ name }.tek"
        sh "#{ PATH_BIM2BIN } -osacmp -tek1 BS:0 in:#{ file } out:#{ temp }"
        bin = File.binread(temp).unpack('C*')
        bin.shift(16)
        rate = 100.0 * bin.length / orgsize rescue 0
        puts " INITRD #{ file } ($#{ bin.length.to_s(16) } #{ bin.length } <= #{ orgsize } #{ '%0.2f' % rate }%)"
        rdblob += bin
        msgpack[name]= bin.pack('C*')
      end
      IO.write("#{PATH_TEMP}msgpack.bin", msgpack.to_msgpack)
      initrd = dir + rdblob
      size_initrd = initrd.length

      obj = bin_locore + tbl + blob + initrd
      size = obj.length
      raise "#{ output }: Out of Segment!" if size >= 0x10000
      obj[4] = size & 0xFF
      obj[5] = (size >> 8) & 0xFF
      obj[6] = offset1 & 0xFF
      obj[7] = (offset1 >> 8) & 0xFF
      obj[8] = offset2 & 0xFF
      obj[9] = (offset2 >> 8) & 0xFF
      obj[10] = size_initrd & 0xFF
      obj[11] = (size_initrd >> 8) & 0xFF

      puts "OUTPUT: $#{ obj.length.to_s(16) } (#{ obj.length })"
      IO.write(t.name, obj.pack('C*'))
    end
  end


  # normal apps
  APP_DEFAULTS = %w(hello chars echo2 bf pipo ).collect {|t| make_app(t) }
  APP_NO_DEFAULTS = %w(acpi cpuid).collect {|t| make_app(t) }

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
  locore = make_mod('osz2boot')
  mods = %w(oszbio oszn98 oszacpi fat12 oszdos).collect {|t| make_mod(t) }
  init_files = %w().collect {|t| make_app(t) }
  make_kernel PATH_OS_SYS, locore, mods, init_files


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
    output = "#{ PATH_OUTPUT }#{ imgdef[:name] }.vfd"
    files = [imgdef[:files]].flatten.sort {|a, b| File.basename(a).upcase <=> File.basename(b).upcase }
    files.unshift PATH_OS_SYS
    make_disk output, IPLS[imgdef[:ipl]], files
    IMAGES << output
  end


  desc "Build OSZ"
  task :build => [ALL_OBJS, IMAGES].flatten

end
