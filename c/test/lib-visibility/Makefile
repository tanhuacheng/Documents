CC = gcc

all: libfoo.so libbar.so a.out

libfoo.so: foo.c
	$(CC) -fPIC -fvisibility=hidden -shared -o libfoo.so foo.c

libbar.so: bar.c
	$(CC) -fPIC -fvisibility=hidden -shared -o libbar.so bar.c

a.out: test.c libfoo.so libbar.so
	$(CC) -g test.c -o a.out -L. -lfoo -lbar

clean:
	rm -f libfoo.so libbar.so a.out
