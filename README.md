MEG-OS Z
===

* MEG-OS Z (OSZ) は Retro UI アプリケーション をサポートする小さな 16bit OS です。
* 主要な機能は 8086/8088 を搭載したシステムでも動作することを目標にしています。
* 現在のバージョンでおそらく必要な最小メモリは 128KB で、推奨する容量はもっと多くなります。


# APPS

## hello

* Hello world のサンプルです。実用性はありません。

## chars

* 文字表示のお題サンプルアプリです。実用性はありません。

## echo2

* 引数表示のお題サンプルアプリです。実用性はありません。

## cpuid

* 動作中の CPU の情報を表示します。
* cpuid 命令に対応している最近の CPU の場合は表示される情報が増えます。

## tfdisk

* HDD などのパーティションに MEG-OS 用 IPL をインストールして起動できるようにするためのツール (TinyFDISK) の移植版です。
* 386 以上の CPU と LBA に対応した BIOS が必要です。
* fdisk という名前ですがパーティションの作成・削除をする機能はありません。付ける予定もありません。
* アルファ版で HDD にアクセスする危険なコマンドなので自己責任でお願いします。
