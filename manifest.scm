(use-modules (gnu packages chicken)
             (gnu packages guile)
             (gnu packages scheme)
             (guix build-system chicken)
             (guix download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix profiles)            ; For packages->manifest in the REPL
             )

(define chicken-matchable
  (package
   (name "chicken-matchable")
   (version "1.1")
   (source
    (origin
     (method url-fetch)
     (uri (egg-uri "matchable" version))
     (sha256
      (base32
       "0bizkac4a926lbk0v2m05ysq359mzhfsqh973m72jc4gcj4azr5p"))))
   (build-system chicken-build-system)
   (arguments `(#:egg-name "matchable"))
   (native-inputs `(("chicken-test" ,chicken-test)))
   (home-page "https://wiki.call-cc.org/egg/matchable")
   (synopsis "Hygienic MATCH replacement")
   (description "Pattern matching allows complicated control decisions based on
data structure to be expressed in a concise manner.  Pattern matching is found
in several modern languages, notably Standard ML, Haskell and Miranda.")
   (license license:public-domain)))

(packages->manifest (list coreutils

                          chibi-scheme

                          chicken
                          chicken-matchable
                          chicken-srfi-1
                          chicken-test

                          gauche

                          guile-3.0))
