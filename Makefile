TARGETS=contact.html index.html new-members.html people.html seminars.html software.html teaching.html publications.html

all: $(TARGETS)

%.html: %.rkt templates/*.html
	if [ -f $@ ]; then chmod +w $@; fi
	racket -t $< > $@
	chmod -w $@

clean:
	rm -f $(TARGETS)
