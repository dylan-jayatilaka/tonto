#!/usr/bin/perl
#*******************************************************************************
# This script compares two Tonto output files.  It exits with 0 exit status if
# they are equal.
# It ignores predefined differences, such as date/time, version number, and
# platform identifier.
#
# (c) Daniel Grimwood, The University of Western Australia, 2004.
# (c) Dylan Jayatilaka, The University of Western Australia, 2004.
#
# $Id: compare_output.pl 3618 2011-12-20 05:36:02Z dylan_ $
# 
#*******************************************************************************
use strict;

my $filenamea = shift;
my $filenameb = shift;

my @skip; # array of regular expressions to match lines to ignore.

# default skip regexp
push @skip, '^\s*$'; # blank lines.
push @skip, '^\s*Timer started';
push @skip, '^\s*Version:';
push @skip, '^\s*Platform:';
push @skip, '^\s*Build-date:';
push @skip, '^\s*Wall-clock time taken';
push @skip, '^\s*CPU time taken';
push @skip, '^\s*time taken';
push @skip, '^\s*Warning from';
push @skip, '^\s*Routine call stack:';
push @skip, '^\s*Call \s*Routine name \s*Memory Used';
push @skip, '^\s*\d+\.\s*[A-Z.]+:\w+\s+\d+\s*$';
push @skip, 'Memory usage report';
push @skip, '^\s*Memory currently used      =';
push @skip, '^\s*Maximum memory used        =';
push @skip, '^\s*Memory blocks used         =';
push @skip, '^\s*Maximum memory blocks used =';
push @skip, '^\s*Memory limit               =';
push @skip, '^\s*Call stack level           =';
push @skip, '^\s*Maximum call stack depth   =';
push @skip, '^\s*Warning from MOLECULE.MAIN:run ... memory leak';
push @skip, '^\s*SYSTEM: Memory usage report:';

open(FILE1,"<",$filenamea) || die "\nError opening $filenamea:\n$!";
open(FILE2,"<",$filenameb) || die "\nError opening $filenameb:\n$!";

my $equal = &compare; # do the comparison
# print "equal = $equal\n";

close(FILE2);
close(FILE1);

exit (! $equal);

#*******************************************************************************

sub compare {
# do the comparison, by looping through both files at the same time.  Not
# necessarily at the same rate.  Returns 1 if they are the same.
   PAIR_LINE_LOOP: while (1) {             # Loop over paurs of lines ...

      my $line1 = <FILE1>;
      my $line2 = <FILE2>;

      OK_LINE1: while (1) {
         last OK_LINE1 if (!$line1);
         chomp($line1); 
         foreach my $skip (@skip) {
            next if $line1 !~ /${skip}/;   # Skip explicitly ignored -skip lines
            $line1 = <FILE1>;
            next OK_LINE1;
         }
         last OK_LINE1;
      }

      OK_LINE2: while (1) {
         last OK_LINE2 if (!$line2);
         chomp($line2);
         foreach my $skip (@skip) {
            next if $line2 !~ /${skip}/;   # Skip explicitly ignored -skip lines
            $line2 = <FILE2>;
            next OK_LINE2;
         }
         last OK_LINE2;
      }

      return (0) if ( $line1 and !$line2); # EOF on FILE2 but not FILE1
      return (0) if (!$line1 and  $line2); # EOF on FILE1 but not FILE2
      return (1) if (!$line1 and !$line2); # EOF on both files

      $line1 =~ s/-(0.0*)( |$)/$1$2/g; # Replace -0.0 with 0.0
      $line2 =~ s/-(0.0*)( |$)/$1$2/g; # Replace -0.0 with 0.0
      $line1 =~ s/\s+/ /g;             # Condense all whitespace
      $line2 =~ s/\s+/ /g;             # Condense all whitespace
      $line1 =~ s/\W+//g;              # Remove non-word characters
      $line2 =~ s/\W+//g;              # Remove non-word characters

    # if ($line1 ne $line2) {
    # print "line1:$line1\n";
    # print "line2:$line2\n";
    # }

      return (0) if ($line1 ne $line2); # Lines differ, not same

   }

}
