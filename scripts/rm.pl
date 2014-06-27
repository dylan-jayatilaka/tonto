#!/usr/bin/perl
use ExtUtils::Command;
for (@ARGV) {
  -f && do { rm_f; }
}
exit 0;
