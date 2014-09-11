sources = $(wildcard *.md)

pages = $(sources:.md=.html)

all: $(pages)

%.html: %.md LINKS HEADER
	hreplace < $*.md \
	| markdown \
	| hreplace --rules='HSFILE name = !HSCOLOUR>{-- name\n\n#!INCLUDE name};<p>!TITLE X = #!REPLACE>{#!INCLUDE(HEADER)!WITH[title]=X}<p>; --!SPACE../ = --!SPACE' \
	> $*.html

clean:
	rm $(pages)

ifneq ($(wildcard Makefile-dev),)
include Makefile-dev
endif

