#!/usr/bin/perl
for (@ARGV) {
  open(FILE,$_) || do {warn "Cannot open $_"; next;};
  while(<FILE>) {print;}
  close FILE ;
}
