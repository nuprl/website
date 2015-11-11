all: contact.html index.html people.html seminars.html software.html teaching.html

%.html: %.rkt templates/footer.html templates/header.html templates/nav.html templates/subpage-title.html
	chmod +w $@
	racket -t $< > $@
	chmod -w $@
