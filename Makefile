
.PHONY: all clean distclean test doc

all: doc

R6RS_DEFINITIONS=find|filter|remove|unicode-.*|char->utf8-list|cset->utf8-pattern

irregex-r6rs.scm: irregex.scm
	perl -e 'BEGIN{$$/=""}' -ane "s/@ submatch/submatch/g; s/^(\\(define \\*allow-utf8-mode\\?\\* +#).(.*)/\\1f\\2/sm; s/^(\\(define \([-\\w]*utf8[-\\w]* ([-\\w]+) [^)]*\)).*/\\1 \\2)\n\n/sm; print unless /^\(define +\(?($(R6RS_DEFINITIONS))\\s/" < $< > $@

irregex-stalin.scm: irregex.scm regex-dna.scm read-string.scm error.scm
	cat $^ | grep -v '^(use ' > $@

#	              -e '(mistie-load "footnote.mistie")' \

irregex-base.html: irregex.doc irregex.mistie irregex.css
	csi -R mistie -e '(mistie-load "plain.mistie")' \
	              -e '(mistie-load "scmhilit.mistie")' \
	              -e '(define h-page-count 0)' \
	              -e '(mistie-load "xref.mistie")' \
	              -e '(mistie-load "irregex.mistie")' \
	              -e '(mistie-main "$<")' > $@

# you didn't see this
irregex.html: irregex-base.html
	grep '^<a name="SECTION_' $< |\
	  perl -ape 's{<a name="(SECTION_(\d+)(\.\d+)?)"><h[12]>(?:[.\d]+)(?:\s|&nbsp;)*([^<>]*).*}{($$3?($$3==.1?($$sub=1,"<ol>\n"):""):(($$x=$$sub,$$sub=0,$$x>0)?"</ol>\n":""))."<li><a href=\"#$$1\">$$4</a>"}ge;' > irregex-toc.html
	perl -ape 's{^<!--\s*TOC\s*-->}{"<ol>\n".`cat irregex-toc.html`."</ol>"}e' $< > $@
	rm -f $< irregex-toc.html

doc: irregex.html

test:
	csi -script test-irregex.scm
	csi -script test-irregex-scsch.scm

clean:
	rm -f *~ */*~ *.so

distclean: clean
	rm -f *.html

dist: doc
	rm -f irregex-`cat VERSION`.tgz
	mkdir irregex-`cat VERSION`
	for f in `hg manifest`; do mkdir -p irregex-`cat VERSION`/`dirname $$f`; ln -s `pwd`/$$f irregex-`cat VERSION`/$$f; done
	cd irregex-`cat VERSION`; for f in `echo ../*.html`; do ln -s $$f; done; cd ..
	tar cphzvf irregex-`cat VERSION`.tgz irregex-`cat VERSION`
	rm -rf irregex-`cat VERSION`
