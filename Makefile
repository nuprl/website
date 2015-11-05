all: contact.html index.html people.html publications.html seminars.html software.html teaching.html

%.html: %.rkt
	chmod +w $@
	racket -t $< > $@
	chmod -w $@
