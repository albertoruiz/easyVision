sources = $(wildcard *.md)

pages = $(sources:.md=.html)

all: $(pages)

%.html: %.md LINKS HEADER
	cat $*.md | replace LINKS "`cat LINKS`" | markdown | sed -e '1 s/<p>//' -e '1 s/<\/p>//' > $*aux.html
	./putheader $*aux.html > $*.html
	rm $*aux.html

clean:
	rm $(pages)

