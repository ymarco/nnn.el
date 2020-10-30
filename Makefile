CC=gcc
CFLAGS=-fPIC

.PHONY: clean

fired-module.so: fired-module.o
	$(CC) -shared -o $@ $<
fired-module.o: fired-module.c fired-module.h

clean:
	rm *.o *.so
