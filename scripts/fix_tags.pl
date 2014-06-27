#!/usr/bin/perl
#-------------------------------------------------------------------------------
#
# fix_tags.pl
#
# This script is used to fix the tags file produced from Exuberant ctags, 
# so that it can cope with files with curly brackets in it. It simply places
# quotes around the file names.
#
# Usage : ./fix_tags.pl tags
#                
# Where : "tags" is the tags file.
#
# (c) Dylan Jayatilaka, University of Western Australia, 2004.
#
# $Id: fix_tags.pl 2504 2004-04-01 06:35:31Z dylan $
#-------------------------------------------------------------------------------

use strict;             # Make sure the scope of all variables is declared.

if (@ARGV==0) { die "no tags file specified, stopped" } ;
if (@ARGV>1)  { die "too many arguments specified" } ;

my $tags = shift(@ARGV);
my $fix  = "$tags.fix";
   
open(TAGS,$tags)  || die "tags file $tags does not exist, stopped" ;
open(FIX, "> $fix");

my ($old,$new);

while ($old = <TAGS>) { 
   $new = $old;
   $new =~ s/(^\w+\t)([\w.{}]+)/$1\"$2\"/;
   if ($2 =~ /{/) { print FIX $new; }
   else           { print FIX $old; }
}

close FIX;
close TAGS;

rename($fix,$tags);
