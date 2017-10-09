clib: lib/cbits.c
	gcc -fPIC -shared lib/cbits.c -o lib/cbits.so
install-clib: lib/cbits.so
	sudo cp lib/cbits.so /usr/lib
	sudo ln -sf /usr/lib/cbits.so /usr/lib/libcbits.so
test: lang/c.clj lib/cbits.so
	stack clean
	stack build
	stack exec compiler lang/c.clj
	gcc main.o lib/cbits.so -o main.out
clean: 
	rm *.o *.out lib/*.so
