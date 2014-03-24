TOMLDOCDIR=toml.docdir

update: $(TOMLDOCDIR)
	rm -f *.html html.stamp
	cp $</* .
