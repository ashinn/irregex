These are the currently planned changes for IrRegex:

  * Refactor and port to Chibi module system using include files (allows for easy conditional includes)
  * Write a translator from chibi module system to other module systems that's more robust than the current Makefile hacks I'm using
  * Make chunked strings an optional loaded module
  * Rewrite DFA match extraction to avoid closures - should be insignificantly slower but allow for pre-compiled regexps
  * When DFA isn't possible, rewrite backtracking algorithm to use non-backtracking NFA (long-term goal)
  * Additional utilities and optimizations