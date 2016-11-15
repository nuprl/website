TARGETS=contact.html index.html new-members.html people.html seminars.html software.html teaching.html publications.html
.PHONY: blog

all: setup blog $(TARGETS)

preview: all
	raco frog -p

setup:
	raco pkg install --auto --skip-installed gregor

publication.html: publication.rkt publication-data.rkt templates.rkt
	if [ -f $@ ]; then chmod +w $@; fi
	racket -t $< > $@
	chmod -w $@

%.html: %.rkt templates.rkt
	if [ -f $@ ]; then chmod +w $@; fi
	racket -t $< > $@
	chmod -w $@

blog:
	cd blog && make

clean:
	rm -f $(TARGETS)
