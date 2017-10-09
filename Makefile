clib: lib/cbits.c
	gcc -fPIC -shared lib/cbits.c -o lib/cbits.so
install-clib: lib/cbits.so
	sudo mv lib/cbits.so /usr/lib
	sudo ln -sf /usr/lib/cbits.so /usr/lib/libcbits.so
