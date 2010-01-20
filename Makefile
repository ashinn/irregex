
.PHONY: all clean distclean test doc

all: doc

# these crazy make hacks will go away with the 0.8 refactoring

R6RS_DEFINITIONS=find|filter|remove|unicode-.*|char->utf8-list|cset->utf8-pattern

irregex-r6rs.scm: irregex.scm Makefile
	perl -e 'BEGIN{$$/=""}' -ane "s/^(\\(define \\*allow-utf8-mode\\?\\* +#).(.*)/\\1f\\2/sm; s/^(\\(define \([-\\w]*utf8[-\\w]* ([-\\w]+) [^)]*\)).*/\\1 \\2)\n\n/sm; print unless /^\(define +\(?($(R6RS_DEFINITIONS))\\s/" < $< > $@

irregex-stalin.scm: irregex.scm regex-dna.scm read-string.scm error.scm
	cat $^ | grep -v '^(use ' > $@

irregex-jazz.scm: irregex.scm Makefile
	perl -e 'BEGIN{$$/=""}' -e 'END{print "\n)\n"}' -ape "s/^(\\(define \\*allow-utf8-mode\\?\\* +#).(.*)/\\1f\\2/sm; s/^(\\(define irregex-tag .*)/(unit irregex.implementation.irregex\n\n(declare (proper-tail-calls) (block) (fixnum) (inline) (inlining-limit 700) (standard-bindings) (extended-bindings))\n\n\\1/sm; s/^\\(define \\(?integer-log .*/(define (integer-log n) (if (zero? n) n (- (integer-length n) 1)))\n\n/sm; s/^\\(define \\(bit-.*//sm; s/\\bbit-(not|ior|and)\\b/bitwise-\\1/gsm; s/\\bbit-shl\\b/fxarithmetic-shift-left/gsm; s/\\bbit-shr\\b/fxarithmetic-shift-right/gsm; if (/^\\(define .*multi-state/) {s/24/16/gsm; s/\\b(vector)\\b/u16\\1/gsm; s/\\(u16vector-ref mst 0\\)/(equal?-hash mst)/gsm; }" < $< > $@

irregex-base.html: irregex.doc irregex.mistie irregex.css
	csi -R mistie -e '(mistie-load "plain.mistie")' \
	              -e '(mistie-load "scmhilit.mistie")' \
	              -e '(define h-page-count 0)' \
	              -e '(mistie-load "xref.mistie")' \
	              -e '(mistie-load "timestamp.mistie")' \
	              -e '(mistie-load "irregex.mistie")' \
	              -e '(mistie-main "$<")' > $@

irregex.html: irregex-base.html
	grep '^<a name="SECTION_' $< |\
	  perl -ape 's{<a name="(SECTION_(\d+)(\.\d+)?)"><h[12]>(?:[.\d]+)(?:\s|&nbsp;)*([^<>]*).*}{($$3?($$3==.1?($$sub=1,"<ol>\n"):""):(($$x=$$sub,$$sub=0,$$x>0)?"</ol>\n":""))."<li><a href=\"#$$1\">$$4</a>"}ge;' > irregex-toc.html
	perl -ape 's{^<!--\s*TOC\s*-->}{"<ol>\n".`cat irregex-toc.html`."</ol>"}e' $< > $@
	rm -f $< irregex-toc.html

doc: irregex.html

test:
	csi -script test-all.scm

clean:
	rm -f *~ */*~ *.so

distclean: clean
	rm -f *.html

dist: doc
	rm -f irregex-`cat VERSION`.tar.gz
	mkdir irregex-`cat VERSION`
	for f in `hg manifest`; do mkdir -p irregex-`cat VERSION`/`dirname $$f`; ln -s `pwd`/$$f irregex-`cat VERSION`/$$f; done
	cd irregex-`cat VERSION`; for f in `echo ../*.html`; do ln -s $$f; done; cd ..
	tar cphzvf irregex-`cat VERSION`.tar.gz irregex-`cat VERSION`
	rm -rf irregex-`cat VERSION`
