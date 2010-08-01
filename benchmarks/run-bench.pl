#! /usr/bin/env perl

use strict;

sub bench ($$$) {
  my ($name, $sub, $n) = @_;
  my $start = times;
  for (my $i=0; $i<$n; $i++) { $sub->(); }
  print "$name: ".((times-$start)*1000)."\n";
}

open(IN, "< re-benchmarks.txt");
while (<IN>) {
  next if /^\s*(?:#.*)?$/;
  my ($name, $pat, $str, $prefix, $compn, $execn) = split(/\t/);
  bench("$name: compile-time", sub {eval "/$pat/"}, $compn);
  my ($rx, $rxm, $str2);
  eval "\$rx = qr/$pat/";
  eval "\$rxm = qr/^$pat\$/";
  bench("$name: match-time", sub {$str =~ $rxm}, $execn);
  for (my $mult=1; $execn>=10; $mult*=10, $execn/=10) {
    $str2 = (($prefix x $mult).$str);
    bench("$name: search prefix x $mult", sub {$str2 =~ $rx}, $execn);
  }
}
close(IN);


