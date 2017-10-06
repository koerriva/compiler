clib: lib/cbits.c
	gcc -fPIC -shared lib/cbits.c -o lib/cbits.so
	sudo cp lib/cbits.so /usr/lib
