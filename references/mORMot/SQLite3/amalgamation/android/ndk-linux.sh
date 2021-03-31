#!/bin/sh

ndk=/home/superdad/Public/android-ndk-r21d

$ndk/ndk-build clean
$ndk/ndk-build all

bindir=./obj/local

targetdir=./../../../static/i386-android
cp $bindir/x86/libsqlite3.a $targetdir/libsqlite3.a
cp $ndk/toolchains/x86-4.9/prebuilt/linux-x86_64/lib/gcc/i686-linux-android/4.9.x/libgcc.a $targetdir/libgcc.a

targetdir=./../../../static/x86_64-android
cp $bindir/x86_64/libsqlite3.a $targetdir/libsqlite3.a
cp $ndk/toolchains/x86_64-4.9/prebuilt/linux-x86_64/lib/gcc/x86_64-linux-android/4.9.x/libgcc.a $targetdir/libgcc.a

targetdir=./../../../static/arm-android
cp $bindir/armeabi-v7a/libsqlite3.a $targetdir/libsqlite3.a
cp $ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/lib/gcc/arm-linux-androideabi/4.9.x/libgcc.a $targetdir/libgcc.a
# cp $ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/lib/gcc/arm-linux-androideabi/4.9.x/armv7-a/libgcc.a $targetdir/libgcc.a

targetdir=./../../../static/aarch64-android
cp $bindir/arm64-v8a/libsqlite3.a $targetdir/libsqlite3.a
cp $ndk/toolchains/aarch64-linux-android-4.9/prebuilt/linux-x86_64/lib/gcc/aarch64-linux-android/4.9.x/libgcc.a $targetdir/libgcc.a
