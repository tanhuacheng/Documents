CC = gcc

a.out: nbd.c zero-cross.c
	$(CC) -Wall -g --std gnu11 -o a.out nbd.c zero-cross.c -lm

clean:
	rm -f a.out
