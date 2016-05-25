VPATH = ./src : ./include

tlist : tlist.o ds_list.o
	cc -o tlist tlist.o ds_list.o

tlist.o : tlist.c ds_list.h
	cc -c -I ./include tlist.c
ds_list.o : ds_list.c ds_list.h types.h
	cc -c -I ./include ./src/ds_list.c

.PHONY : clean
clean:
	-rm ds_list *.o

