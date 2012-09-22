;;; draw-graph
;;
;; This is a quick and dirty program which can generate Graphviz DOT
;; instructions for drawing a graph of the NFA or DFA corresponding to
;; the SRE given on standard input.
;;
;; The SRE is (read) in and passed as (irregex <input>), so it may be
;; a string containing a POSIX regular expression.
;;
;; This is mostly useful for debugging or understanding irregex's code.
;;
;; Example:
;; echo '(seq "x" (or "y" "z") "abc")' | csi -s draw-graph.scm nfa | dot -Tpng > nfa.png
;;
;; You'll need a recent enough version of Graphviz, which understands HTML tags
;;

;(use utf8)                              ; needed?

(include "irregex.scm")

;; Ensure nonprintable characters are not emitted directly.  Chicken doesn't
;; handle UTF-8 100% correctly, it seems, and Graphviz doesn't at handle it all.
(define (char->string c)
  (if (or (char-numeric? c) (char-alphabetic? c))
      (string c)
      (sprintf "[~A]" (char->integer c))))

(define (cset->label cset)
  (if (char? cset)
      (char->string cset)
      (string-intersperse (map (lambda (cs)
                                 (if (eqv? (car cs) (cdr cs))
                                     (char->string (car cs))
                                     (sprintf "~A..~A"
                                              (char->string (car cs)) (char->string (cdr cs)))))
                               (vector->list cset))
                          " | ")))

(define (draw-nfa sre)
  (let ((nfa (sre->nfa sre 0)))
    (unless nfa
      (print "Could not convert to NFA (backtracking only?)")
      (exit 1))
    (printf "digraph {\n")
    (printf "\tnode [shape=circle]\n\n")
    ;; Invisible start state -> nfa-start-state looks like arrow from outside
    (printf "\tstart -> ~A\n" (nfa-start-state nfa))
    (printf "\tstart [style=invis]\n")
    (let lp ((states (list (nfa-start-state nfa)))
             (seen '()))
      (unless (null? states)
        (if (member (car states) seen)
            (lp (cdr states) seen)
            (let* ((st (car states))
                   (trans (nfa-get-state-trans nfa st))
                   (eps (nfa-get-epsilons nfa st))
                   (todo (append (map car eps) (cdr states))))
              (newline)
              (for-each (lambda (ns)
                          ;; For some obscure reason, label=<~A> doesn't work,
                          ;; so let's just hope HTML entities keep working inside
                          ;; normally-quoted strings...
                          (printf "\t~A -> ~A [label=\"~A\"]\n"
                                  st (car ns) (if (cdr ns)
                                                  (sprintf "&epsilon;/t~A" (cdr ns))
                                                  "&epsilon;")))
                        eps)
              (if (null? trans)
                  (lp todo (cons st seen))
                  (let ((ns (cdr trans))
                        (cs (car trans)))
                    (printf "\t~A -> ~A [label=~S]\n" st ns (cset->label cs))
                    (lp (cons ns todo) (cons st seen))))))))
    (printf "\t0 [shape=doublecircle]\n") ; Final state
    (printf "}\n")))

(define (finalizer->label fin)
  (string-intersperse (map (lambda (tag&slot)
                             (let ((tag (car tag&slot))
                                   (slot (cdr tag&slot)))
                               (sprintf "t~A &larr; m~A(~A)" tag slot tag)))
                           (sort fin (lambda (a b) (< (car a) (car b)))))
                      "<br/>"))

(define (commands->label copy&set-commands)
  (sprintf "<br/>~A~A"
           (string-intersperse
            (map (lambda (c)
                   (let ((tag (vector-ref c 0))
                         (ss (vector-ref c 1))
                         (ds (vector-ref c 2)))
                     (sprintf "m~A(~A) &larr; m~A(~A)" ds tag ss tag)))
                 (or (car copy&set-commands) '())) "<br/>")
           (string-intersperse
            (map (lambda (s)
                   (let ((slot (cdr s))
                         (tag (car s)))
                     (sprintf "m~A(~A) &larr; p" slot tag)))
                 (or (cdr copy&set-commands) '())) "<br/>")))

(define (draw-dfa sre)
  (let* ((nfa (sre->nfa sre 0))
         (dfa (and nfa (nfa->dfa nfa))))
    (unless dfa
      (print "Could not convert to DFA (backtracking only?)")
      (exit 1))
    (let ((first-state (cadr (dfa-init-state dfa))))
      (printf "digraph {\n")
      (printf "\tnode [shape=circle]\n\n")
      (printf "\tstart [style=invis]\n")
      (printf "\tstart -> ~A [label=<~A>]\n"
              (cadr first-state)
              (commands->label (dfa-cell-commands dfa first-state)))
      (let lp ((states (list (dfa-next-state dfa first-state)))
               (state-numbers (list (cadr first-state)))
               (seen '()))
        (unless (null? states)
          (if (memv (car state-numbers) seen)
              (lp (cdr states) (cdr state-numbers) seen)
              (let* ((st (car states))
                     (sn (car state-numbers))
                     (trans (cdr st))
                     (todo-states (append (map (lambda (s)
                                                 (dfa-next-state dfa s))
                                               trans)
                                          (cdr states)))
                     (todo-numbers (append (map cadr trans)
                                           (cdr state-numbers))))
                (newline)
                (when (car st)
                  (printf "\t~A -> end_~A[label=<~A>]\n"
                          sn sn (finalizer->label (car st)))
                  (printf "\t~A [shape=doublecircle]\n" sn)
                  (printf "\tend_~A [style=invis]\n" sn))
                (for-each (lambda (tr)
                            (printf "\t~A -> ~A [label=<~A ~A>]\n"
                                    sn (cadr tr)
                                    (cset->label (car tr))
                                    (commands->label (dfa-cell-commands dfa tr))))
                          trans)
                (lp todo-states todo-numbers (cons sn seen)))))))
    (printf "}\n")))

(define (show-usage)
  (printf "Usage: ~A [nfa | dfa]\n" (program-name))
  (exit 1))

(cond ((not (= (length (command-line-arguments)) 1))
       (show-usage))
      ((string=? "nfa" (car (command-line-arguments)))
       (draw-nfa (maybe-string->sre (read))))
      ((string=? "dfa" (car (command-line-arguments)))
       (draw-dfa (maybe-string->sre (read))))
      (else (show-usage)))