#!/usr/bin/perl
# ==================================================================
#
# This perl script adds dependencies for Fortran 95 files whose 
# .foo files have get_from(...) directives. It is specific to the
# foo langauge and Tonto.
#
# Usage:
#
# perl -w make_foo_dependencies.pl -o outfile -src foofile
#
# where:
#
# -o   depfile : places foo-dependency info in file "depfile"
# -src foofile : causes processing source .foo file "foofile"
#
# (c) Copyright 2003-- Daniel Grimwood, Dylan Jayatilaka
# (c) Copyright 2012-- Dylan Jayatilaka
#
# $Id: make_foo_dependencies.pl 4396 2014-05-31 10:07:16Z dylan_ $
# ==================================================================


use File::Basename;

# The name of the current module
my $module_name; 

# Where the get_from's are read from ...
my $foofile;

# Where the dependencies are deposited ...
my $depfile;

# ============================
# Parse command-line arguments
# ============================

# Argument error?
my $argerr = 0;

# Must be at least one argument
$argerr = 1 if ($#ARGV+1==0);

# Extract command-line arguments
while (@ARGV) {

    $arg = shift;

    SWITCH: for ($arg) {
        /^-src$/       && do { $foofile = shift; last; };
        /^-o$/         && do { $depfile = shift; last; };
        warn "Error : unexpected argument $arg\n";
        $argerr = 1;
    }

}

# Unprotect curly braces
$foofile =~ s/\\{/{/g;
$depfile =~ s/\\{/{/g;

# Any errors?
$foofile || do {$argerr = 1; warn "Error : -src flag not supplied\n"};
$depfile || do {$argerr = 1; warn "Error : -o flag not supplied\n"};

# ===================================================
# Print out standard help message if there's an error
# ===================================================

if ($argerr==1) {

    die(
        "\nUsage:\n",
        "\t perl -w make_dependencies.perl -o outfile -src filename\n\n",
        "where :\n",
        "\t\"-o depfile\"\t\t: Output dependency information to file\n",
        "\t\t\t\t  \"depfile\"\n",
        "\t\"-src foofile\"\t\t: Process foo source file \"foofile\"\n");
}

# ===========================================
# Get and store all the get_from dependencies
# ===========================================

# Open the file
open(FILE, $foofile) or die "Cannot find file $_[0]\n";

# The (non-unique) list of get_froms
my    @gets;
undef @gets;

# Loop over file lines ...
while (<FILE>) {

  # Extract (lower case) module name
  if (/^module\s+([A-Z][\w{,}.]*[\w}])/) {
     $module_name = lc($1);
  }

  # Extract get_from ...
  if (/get_from/) {

    if (/(?:.+):::(?:.+)get_from\(\s*([A-Z][A-Z0-9_{,}.]*[A-Z0-9_}])/) {

       # get_from(MODULE:xxxx)
       $x = $1; 
       push(@gets, lc($x));
       # print "x = $x\n";

    } elsif (/(?:.+):::(?:.+)get_from\(/) { 

       # get_from(xxxx)
       push(@gets, $module_name);
       # print "m = $module_name\n";

    }

  }

}

close(FILE);

# =============================
# Make the unique get_from list
# =============================

my    @getlist;
undef @getlist;

if (@gets) {

   foreach $get (&unique(@gets)) {
      ($name = $get) =~ s/$/.foo/;
      push (@getlist,'$(foodir)/' . $name);
   }

}

# ==============================
# Print out the foo dependencies
# ==============================

open(OUTFILE,">" . $depfile) or die "Cannot open $depfile for writing.\n";

print OUTFILE "# get_from() dependencies for $foofile:\n";

if (@getlist) {

   # The base part of the foofile (minus directory).
   my $base;
   $base = basename($foofile);
 # $base =~ s/\.[^\.]+$//;

   $word = '$(f95dir)/' . "$base" . '.$(FSUFFIX) :';
   &PrintWords($word, @getlist);

   print OUTFILE "\n";
   $word = '$(htmldir)/' . "$base" . '.html :';
   &PrintWords($word, @getlist);
}

print OUTFILE "\n\n";             

close(OUTFILE);


# ==================================================================
# &PrintWords(@list);
# Print the @list to span multiple lines if necessary.
# Based on the one from makemake.perl.
# ==================================================================

sub PrintWords {

    my ($columns, $wordlength);

    $columns = 79;

    foreach $word (@_) {

        $wordlength = length($word);

        if ($columns == 79) {
            print OUTFILE "$word";
            $columns -= $wordlength + 1;
        } elsif ($wordlength + 1 < $columns) {
            print OUTFILE " $word";
            $columns -= $wordlength + 1;
        } else {
           print OUTFILE " \\\n\t$word";
           $columns = 71 - $wordlength;
        }

    }

}

# ==================================================================
# &unique(@list)
# Return the @list in original order minus duplicates.
# ==================================================================

sub unique {

    my %words;

    foreach $x (@_) {
      $words{$x} = 1;
    # $_ = join(" ",@words);
    # if (! /\b\Q$x\E\b/) { push(@words,$x); }
    }

    return (keys %words);

}

