A fully portable and efficient R[456](456.md)RS implementation
of regular expressions, supporting both POSIX syntax with various (irregular) PCRE extensions, as well as SCSH's SRE syntax.  DFA matching is used when possible, otherwise a closure-compiled NFA approach is used.

See the manual at http://synthcode.com/scheme/irregex/