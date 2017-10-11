clib: lib/rt.c
	gcc -fPIC -shared lib/rt.c -o lib/rt.so
install-clib: lib/cbits.so
	sudo cp lib/rt.so /usr/lib
	sudo ln -sf /usr/lib/rt.so /usr/lib/librt.so
compile: lang/c.clj lib/rt.so
	stack clean
	stack build
	stack exec compiler lang/c.clj
	gcc main.o -lrt -o main.out
test: lang/c.clj
	stack exec compiler lang/c.clj
clean: 
	rm *.o *.out lib/*.so
