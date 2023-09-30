.PHONY: all, clean

all: main
	$(MAKE) -C gsl -f build.mk
	raco dist racket-gsl main

main_rkt_merged.zo: main.rkt
	raco demod main.rkt

main: main_rkt_merged.zo
	raco exe -o main main.rkt

clean:
	$(MAKE) -C gsl -f build.mk clean
	-rm -rf racket-gsl main main_rkt_merged.zo
