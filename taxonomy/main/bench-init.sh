#!/bin/bash

export CC="gcc -save-temps"

bench=$1

error_check () {
    if [ $1 -ne 0 ] 
    then
        echo "problem in "$2" phase; operator needed"
        popd
        exit 1
    fi
}

checkout () {
    case "$bench" in 
        fbc) svn co https://fbc.svn.sourceforge.net/svnroot/fbc/trunk/FreeBASIC fbc
            ;;
        gmp) hg clone http://gmplib.org:8000/gmp gmp
            ;;
        gzip) git clone http://git.savannah.gnu.org/cgit/gzip.git gzip
            ;;
        libtiff) git clone git://git.ghostscript.com/thirdparty/libtiff.git libtiff
            ;;
        lighttpd) svn co svn://svn.lighttpd.net/lighttpd/trunk lighttpd
            ;;
        php) svn co http://svn.php.net/repository/php/php-src/trunk php
            ;;
        python) hg clone http://hg.python.org/cpython python
            ;;
        wireshark) svn co http://anonsvn.wireshark.org/wireshark/trunk wireshark
            ;;
        *) 
            ;;
    esac
}

update () {
    case "$bench" in 
        fbc|lighttpd|php|wireshark) svn revert -R * 
            svn up --force
            ;;
        gmp|python) hg up -C
            ;;
        gzip) git checkout -f  ab9eb0da6e773
            ;;
        libtiff) git checkout -f 201ca19d0b6e
            ;;
        *) 
            ;;
    esac
}

autogen () {
    case "$bench" in 
        gmp) sh .bootstrap 
            ;;
        gzip) ./bootstrap
            ;;
        libtiff|lighttpd|wireshark) sh autogen.sh
            ;;
        php) ./buildconf
            ;;
        *) # python doesn't do this step; fbc is separate
            ;;
    esac        
}

configure () {
    case "$bench" in 
        gmp|libtiff|php|python|gzip|wireshark) ./configure --prefix=/home/claire/local-root
            ;;
        lighttpd) ./configure --prefix=/home/claire/local-root --without-pcre
            ;;
        *) # fbc is separate
            ;;
    esac        
}

if [ ! -d $bench ] 
then
    checkout $bench
    if [ ! -d $bench ] 
    then
        error_check 1 "checkout"
    fi
fi
pushd $bench
error_check $? "pushd" 
update
error_check $? "update"
autogen
error_check $? "autogen"
configure
error_check $? "configure"

case "$bench" in
    fbc) pushd src/compiler/obj/linux
        ../../configure --prefix=/home/claire/local-root
        make; make install
        popd
        
        pushd src/rtlib/obj/linux
        ../../configure --prefix=/home/claire/local-root CFLAGS=-O2
        export MULTITHREADED=
        make
        make MULTITHREADED=1
        make install
        popd
        
        pushd src/gfxlib2/obj/linux
        ../../configure --prefix=/home/claire/local-root CFLAGS=-O2
        make
        make install
        popd
        ;;
    *) make -k
        ;;
esac
error_check $? "make"

popd
