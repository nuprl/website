all: setup blog

setup:
	raco pkg install --auto --skip-installed frog honu

blog:
	raco frog -b
	cp author_redirects/* tags/

clean:
	raco frog -c
	rm index.html sitemap.txt
