sources = $(wildcard *.md)

pages = $(sources:.md=.html)

all: $(pages)

replace: replace.hs
	ghc -O replace.hs
	rm *.o *.hi

%.html: %.md LINKS HEADER replace
	./replace -r < $*.md \
	| markdown \
	| sed -e '1 s/<p/INCLUDE(HEADER)<TITLE/' -e '1 s/p>/TITLE>/' \
	| ./replace -r \
	> $*.html

clean:
	rm $(pages) replace

