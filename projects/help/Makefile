sources = $(wildcard *.md)

pages = $(sources:.md=.html)

all: $(pages)

%.html: %.md LINKS HEADER
	replace -r < $*.md \
	| markdown \
	| sed -e '1 s/<p/INCLUDE(HEADER)<TITLE/' -e '1 s/p>/TITLE>/' \
	| replace -r | replace -r \
	> $*.html

clean:
	rm $(pages)

