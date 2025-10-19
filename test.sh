gcc -c -g -O0  mythread.c
#cobc -d -g -O0 -x repro.cob mythread.o --save-temps -Q -flto=auto -Q -ffat-lto-objects
/Users/calvarez/dev/projects/others/gnucobol-code/cobc/cobc -g -O0 -x repro.cob mythread.o --save-temps -fno-recursive-check
./repro $*
