CC = gcc
AR = ar

all: libfoo.so libbar.so libdym1.a libdym2.a a.out

libfoo.so: foo.c libdym1.a
	$(CC) -o libfoo.so -shared -fPIC -fvisibility=hidden foo.c -L. -ldym1

libbar.so: bar.c libdym2.a
	$(CC) -o libbar.so -shared -fPIC -fvisibility=hidden bar.c -L. -ldym2

libdym1.a : dym1.c
	$(CC) -o dym1.o -fPIC -shared -fvisibility=hidden -c dym1.c
	$(AR) -rf libdym1.a dym1.o

libdym2.a : dym2.c
	$(CC) -o dym2.o -fPIC -shared -fvisibility=hidden -c dym2.c
	$(AR) -rf libdym2.a dym2.o

a.out: test.c libfoo.so libbar.so
	$(CC) -g -o a.out test.c -L. -lfoo -lbar

clean:
	rm -f *.o *.a *.so *.out
