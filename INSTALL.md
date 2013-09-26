
teeterl uses Apache Portable Runtime (apr.apache.org) as a portability layer.
teeterl also depends on the old version of Erlang/OTP (R13B04).

## Build steps

1. Download Erlang/OTP R13B04 and build it from sources. Other versions of
Erlang/OTP WILL NOT WORK.

	wget http://www.erlang.org/download/otp_src_R13B04.tar.gz
	tar xzf otp_src_R13B04.tar.gz
	cd otp_src_R13B04
	./configure
	make
	make install

1. Download apr-1.4.8.tar.gz and apr-util-1.5.2.tar.gz from
[here](http://apr.apache.org/download.cgi).

1. Build apr library:

	tar xzf apr-1.4.8.tar.gz
	cd apr-1.4.8
	./configure
	make

1. Build apr-util library:

	tar xzf apr-util-1.5.2.tar.gz
	cd apr-util-1.5.2
	./configure --with-apr=../apr-1.4.8
	make

1. Clone teeterl repository:

	git clone http://github.com/maximk/teeterl
	cd teeterl

1. Create symbolic links to include directories:

	cd include
	ln -s ../../apr-1.4.8/include apr
	ln -s ../../apr-util-1.5.2/include apr-util

1. Copy apr and apr-util static libraries to the teeterl tree.

	mkdir lib
	cp ../apr-1.4.8/.libs/libapr-1.a lib
	cp ../apr-util-1.5.2/.libs/libaprutil-1.a lib

1. Update erl and erlc paths in the Makefile to point to the R13B04 version of
Erlang/OTP.

1. Run make.

	make

After the clean build run teeterl:

	./teeterl

