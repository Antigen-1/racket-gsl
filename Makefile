.PHONY: all, clean

all: main
	$(MAKE) -C gsl-fork -f build.mk
	raco dist racket-gsl main

main_rkt_merged.zo: main.rkt library.rkt namespace.rkt error.rkt block.rkt
	raco demod main.rkt

main: main_rkt_merged.zo
	raco exe -o main main.rkt

clean:
	$(MAKE) -C gsl-fork -f build.mk clean
	-rm -rf racket-gsl main main_rkt_merged.zo
