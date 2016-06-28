all: setup blog

setup:
	raco pkg install --auto --skip-installed frog honu

blog:
	raco frog -b

clean:
	raco frog -c
	rm index.html sitemap.txt
