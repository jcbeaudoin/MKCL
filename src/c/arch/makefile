
all: Call.s Return.s

Call.s: Call.c
	gcc -S Call.c

Return.s: Return.c
	gcc -S Return.c


clean:
	rm -f Call.s Return.s

distclean: clean


realclean: distclean
	$(RM) *~


