clib: lib/cbits.c
	gcc -fPIC -shared lib/cbits.c -o lib/cbits.so
	sudo cp lib/cbits.so /usr/lib
test: lang/c.clj
	gcc -fPIC -shared lib/cbits.c -o lib/cbits.so
	stack clean
	stack build
	stack exec compiler lang/c.clj
	gcc libx.o lib/cbits.so -o x.out
	./x.out
clean: *.o *.out lib/*.so
	rm *.o *.out lib/*.so
