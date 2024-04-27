#!/usr/bin/env bash

set -e

#---------------------------------------------------------------------------------
# Basics (targets)
#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

BASEDIR=$PWD         # the base dir for buildings
SRCDIR=$BASEDIR/libusb                # the source code dir
prefix=$BASEDIR/libusbbin             # installation directory

# export CFLAGS="-O2 -m32"
# export CFLAGS="-O2 -m32 -Wl,--add-stdcall-alias,--kill-at"
# export LDFLAGS="-m32 -Wl,--add-stdcall-alias -Wl,'--input-def ${SRCDIR}/libusb/libusb-1.0.def'"
# export LDFLAGS="-m32"
# export RCFLAGS="--target=pe-i386"
# export DLLTOOLFLAGS="-m i386 -f --32"

cflags="-O2 -Wl,--add-stdcall-alias,--kill-at"

# enable extra warnings
cflags+=" -Winline"
cflags+=" -Wmissing-include-dirs"
cflags+=" -Wnested-externs"
cflags+=" -Wpointer-arith"
cflags+=" -Wredundant-decls"
cflags+=" -Wswitch-enum"

# Vanilla bootstrap
# $SRCDIR/bootstrap.sh

#---------------------------------------------------------------------------------
# Build and install libusb
#---------------------------------------------------------------------------------

# cd $SRCDIR
# make clean 2>&1

mkdir -p $BASEDIR/libusbbuild
cd $BASEDIR/libusbbuild

CFLAGS="${cflags}" CXXFLAGS="${cflags}" $SRCDIR/configure \
    --prefix=$prefix \
    --disable-examples-build --disable-tests-build \
    2>&1 | tee libusb_configure.log

#     --disable-shared \


make    all 2>&1 | tee libusb_make.log
make    install 2>&1 | tee libusb_install.log

# dlltool -k -d $SRCDIR/libusb/libusb-1.0.def -l $prefix/lib/libusb-1.0.a

mkdir -p $prefix/buildscript
cp $BASEDIR/$0 $prefix/buildscript/
