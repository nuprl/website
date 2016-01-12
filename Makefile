TARGETS=contact.html index.html new-members.html people.html seminars.html software.html teaching.html publications.html

all: $(TARGETS)

%.html: %.rkt templates/footer.html templates/header.html templates/nav.html templates/subpage-title.html
	if [ -f $@ ]; then chmod +w $@; fi
	racket -t $< > $@
	chmod -w $@

clean:
	rm -f $(TARGETS)
