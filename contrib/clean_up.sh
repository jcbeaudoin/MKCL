#!/bin/bash
find . -name "*.o" -print -exec rm {} \;
find . -name "*.so" -print -exec rm {} \;
find . -name "*.fas" -print -exec rm {} \;
find . -name "*.fasb" -print -exec rm {} \;
find . \( -name "ASDF-TMP-*.[ch]" -or -name "ASDF-TMP-*.data" \) -print -exec rm {} \;
find . \( -name "ASDF__TMP__*.[ch]" -or -name "ASDF__TMP__*.data" \) -print -exec rm {} \;
find . -name "*.a" -print -exec rm {} \;
find . -name "MKCLINIT*" -print -exec rm {} \;
find . -name "MKC*.c" -print -exec rm {} \;
find . -name "MKC*.tmp" -print -exec rm {} \;
(cd asdf; make clean)
