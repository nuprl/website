all: seminars.html

seminars.html: seminars.rkt
	chmod +w $@
	racket -t $< > $@
	chmod -w $@
