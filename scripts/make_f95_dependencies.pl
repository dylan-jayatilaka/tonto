#!/usr/bin/perl
# ============================================================================
#
# This perl script makes dependency files from Fortran 95 source code.
#
# Usage :
#
#  perl -w make_dependencies.perl 
#     [-uc] 
#     [-skip_use mod_name] ... [-skip_use mod_name] 
#     [-prog_ext name] 
#     [-mod_ext name] 
#     [-obj_ext name] 
#     [-I include_dir] 
#     -o outfile 
#     -src infile
#
#  Where :
#
#     -uc                  : means translate module names to uppercase.
#                            (This is for Cray Unicos and SGI).
#
#     -skip_use mod_name   : means USE statements which reference
#                            module "mod_name" should be ignored.  No rules
#                            are produced for these.  To ignore multiple
#                            modules, include multiple "skip_use" arguments
#
#     -prog_ext name       : specify extension "name" for programs.
#                            Default is exe. If not defined, assume we
#                            are producing and object file dependency.
#
#     -mod_ext name        : specify extension "name" for modules.
#                            Default is mod.
#
#     -obj_ext name        : specify extension "name" for objects.
#                            Default is o.
#
#     -I include_dir       : Use "include_dir" to search for included files
#
#     -o outfile"          : Output dependency information to file "outfile"
#
#     -src infile"         : Process source file "infile"
#
# Notes:
#
# . The script looks for "program", "module", "use", "include" and
#   "#include" keywords.
#
# . The object file that is made from the source file has the same name as the
#   source file but with ".o" extension.  Object files are prefixed
#   with the $(objdir) directory. 
#
# . If the source file is for a program, then the extension is ".exe"
#   or what is overriden with the -prog_ext argument. Program files
#   are prefixed with the $(bindir) directory.  
#
# . The default module extension is ".mod", but this can be overridden
#   with the -mod_ext command line argument. Module files are prefixed
#   with the $(moddir) directory.
#
# . By default, module names from "use" statements are output in
#   lowercase, but this can be overridden using -uc for those
#   compilers that do uppercase instead.
#
# . The script can handle multiple modules in the source file, in which case they
#   are all targets in the single "make" dependency rule.  The list of
#   these modules is saved as a variable, called $(x.module_list),
#   where x is the directory-and-suffix-stripped part of the source file.
#   The beauty of this is that in GNU make, $($(@F).module_list) gives
#   you all the targets, and $^ gives you all the prerequisites!
#
# . The script also has the ability to *not* output for certain
#   modules appearing in "use" statements. By default, only "use"
#   statements which are in lower case are included in the dependency
#   list. This is a good idea if you use modules supplied by your
#   compiler vendor and you don't want to tell "make" to go searching
#   system directories for them.  By default, "service_routines" is
#   not output in dependency lists (this comes with Lahey's LF95). 
#
# Copyright 2002 Daniel Grimwood, <reaper@theochem.uwa.edu.au>
# Copyright 2012 Dylan Jayatilaka
#
# Thanks to :
#   Ted Stern <stern@cray.com> for comments and a script to hack.
#   Michael Wester <wester@math.unm.edu> for ideas from makemake.perl, via Ted.
#
# $Id: make_f95_dependencies.pl 4396 2014-05-31 10:07:16Z dylan_ $ #
# ============================================================================


use File::Basename;

# ================
# Default settings
# ================

$ModCase=LCase;
# Default to lowercase module file basename.

$prog_ext="exe";
# Default program extension.

$mod_ext="mod";
# Default module extension.

# default object extension.
$obj_ext="o";

# define modules that are totally ignored.
push @skip_USE, service_routines;
push @skip_USE, SERVICE_ROUTINES;

# ================
# Argument parsing
# ================

$argerr=0;
$argerr=1 if ($#ARGV+1==0);

while (@ARGV) {
    $arg=shift;
    SWITCH: for ($arg) {
        /^-uc$/        && do { $ModCase=UCase; last; };
        /^-src$/       && do { $filename=shift; last; };
        /^-prog_ext$/  && do { $prog_ext=shift; last; };
        /^-mod_ext$/   && do { $mod_ext=shift; last; };
        /^-obj_ext$/   && do { $obj_ext=shift; last; };
        /^-o$/         && do { $depsfile=shift; last; };
        /^-skip_use$/  && do { push @skip_USE, shift; last; };
        /^-I$/         && do { push @include_dirs, shift; last; };
        warn "Error : unexpected argument $arg\n";
        $argerr=1;
    }
}

# ============
# Error checks
# ============

$prog_ext ne '' || do {$argerr=1; warn "Error : -prog_ext flag is null string\n"};
$obj_ext  ne '' || do {$argerr=1; warn "Error : -obj_ext flag is null string\n"};
$mod_ext  ne '' || do {$argerr=1; warn "Error : -mod_ext flag is null string\n"};

$filename || do {$argerr=1; warn "Error : -src flag not supplied\n"};
$depsfile || do {$argerr=1; warn "Error : -o flag not supplied\n"};

# Print out standard help message if there's an error.
if ($argerr==1) {
    die(
        "\nUsage:\n",
        "\t perl -w make_dependencies.perl [-uc] [-skip_use mod_name] \\\n",
        "\t\t... [-skip_use mod_name] [-prog_ext name] \\\n",
        "\t\t[-obj_ext name] [-I include_dir] -o outfile -src infile\n\n",
        "Where :\n",
        "\t\"-uc\"\t\t\t: means translate module names to uppercase.\n",
        "\t\t\t\t  (This is for Cray Unicos and SGI).\n",
        "\t\"-skip_use mod_name\"\t: means USE statements which reference\n",
        "\t\t\t\t  module \"mod_name\" should be ignored.  No\n",
        "\t\t\t\t  rules are produced for these.  To ignore\n",
        "\t\t\t\t  multiple modules, include multiple\n",
        "\t\t\t\t  \"skip_use\" arguments\n",
        "\t\"-prog_ext name\"\t: specify extension \"name\" for programs.\n",
        "\t\t\t\t  Default is exe\n",
        "\t\"-mod_ext name\"\t\t: specify extension \"name\" for modules.\n",
        "\t\t\t\t  Default is mod\n",
        "\t\"-obj_ext name\"\t\t: specify extension \"name\" for objects.\n",
        "\t\t\t\t  Default is o\n",
        "\t\"-I include_dir\"\t: Look for included files in \"include_dir\"\n",
        "\t\"-o outfile\"\t\t: Output dependency information to file\n",
        "\t\t\t\t  \"outfile\"\n",
        "\t\"-src infile\"\t\t: Process source file \"infile\"\n");
}

# ============================================
# Now to construct all the variables and lists
# ============================================

undef @incs;
undef @modules;
undef @dependencies;
undef @moddefs;
undef $prog;
undef @modlist;

# Get the base part of the file. 
# (minus extension and directory).
$base = basename($filename);
$base =~ s/\.[^\.]+$//;

# Process the $filename for used modules, module definitions, 
# and program statements
&process_fortran_file($filename);

# Recursively search include files.
if (@incs) {
  $n=0;
  while ($n < scalar(@incs)) {
    &process_fortran_file($incs[$n]);
  } continue { $n++; }
}

# If not a "program" statement, then 
# assume we are producing an object file.
if (! $prog) {
  $prog_ext = $obj_ext;
}

# If modules are defined, create macros 
# for modules and a combined object+module
# target rule:
if (@moddefs) {

    # @modlist stores module names with .$mod_ext on the end
    foreach $module (&uniq(@moddefs)) {
        ($name = $module) =~ s/$/.$mod_ext/;
        push(@modlist, $name);
    }

    # Redefine @moddefs to the new @modlist
    @moddefs = @modlist;

    # Redfine @modlist with each element of @moddefs surrounded by
    # MAKE parens:
    undef @modlist;
    foreach $module (@moddefs) {
      ($name = $module) =~ s/.*/\$\(moddir\)\/$&/;
      push(@modlist, $name);
    }
}

# Define the main target file to be compiled.
($fullfile = $base) =~ s/$/.$prog_ext/;

if ($prog) {
  
  # Output the program with directory $(bindir).
  $fullfile =~ s/^/\$(bindir)\//;

} else {

  # Output the object with directory $(objdir).
  $fullfile =~ s/^/\$(objdir)\//;

}

# ===========================================
# Open the dependency output file for writing
# ===========================================

open(OUTFILE,">" . $depsfile) or die "Cannot open $depsfile for writing.\n";

if (@modlist) {

    # This defines the macro $(base.prog_ext.module_list) as the list of modules
    # produced.  Also, macros like $(modname.$mod_ext.module_list) are also
    # produced which contain the same information.
    print OUTFILE "#Variables containing complete list of generated modules:\n";
    $word = "$base.$prog_ext.module_list :=";

    &PrintWords($word, @modlist);
    print OUTFILE "\n";

    foreach $module (@moddefs) {
      $name = $module . ".module_list";
      $word = "$name :=";
      &PrintWords($word, @modlist);
      print OUTFILE "\n";
    }
    print OUTFILE "\n";

    # This is done in case there is no simple mapping between module filename
    # and object filename.
    print OUTFILE "#Associate an object file with the generated modules:\n";
    foreach $module (@moddefs) {
        $name = $module . ".associated_object";
        print OUTFILE "$name := \$(objdir)\/$base.$prog_ext\n";
    }
    print OUTFILE "\n";
}

# Print out extra dependencies for the 
# object file beyond the default:

if (@incs || @modules) {

    $skipUSE = &$ModCase( join(" ",@skip_USE) );

    foreach $module (&uniq(@modules)) {
        $a = $skipUSE;
        next if ($a =~ /$module/ );
        ($name = $module) =~ s/.*/\$\(moddir)\/$&.$mod_ext/;
        push(@dependencies, $name);
    }

}
 
print OUTFILE "#Dependencies of all files produced:\n";

if (@modlist) {
  $word = "$fullfile \$($base.$prog_ext.module_list) :";
  &PrintWords($word, $filename, @dependencies, &uniq(@incs));
} else {
  $word = "$fullfile :";
  &PrintWords($word, $filename, @dependencies, &uniq(@incs));
}

print OUTFILE "\n\n";             # Add newline to .dep file
close(OUTFILE);


# Graph dependency file                  

open(OUTFILE,">" . $depsfile . ".gv" ) or die "Cannot open $depsfile.gv for writing.\n";

print OUTFILE "#Graph of usage for $base:\n";

if (@modlist) {
  &PrintGraph(@dependencies, &uniq(@incs));
}
print OUTFILE "\n\n";             # Add newline to .gv file
close(OUTFILE);



# ===================================================
# &PrintGraph(list);
# ===================================================

sub PrintGraph {
    foreach $word (@_) {
       $word = basename($word);
       $word =~ s/_module\.mod//;
       print OUTFILE "$word -> $base\n";
    }
}


# ===================================================
# &PrintWords(list);
# print the list to span multiple lines if necessary.
# Based on the one from makemake.perl.
# ===================================================

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

# ===================================================
# &uniq(list)
# return the list in original order minus duplicates.
# ===================================================

sub uniq {
    my @words;
    foreach $x (@_) {
      $_ = join(" ",@words);
      if (! /\b$x\b/) { push(@words,$x); }
    }
    return @words;
}

# ============================
# &UCase(string)
# convert string to upper case
# ============================

sub UCase {
    return uc($_[0]);
}

# ============================
# &LCase(string)
# convert string to lower case
# ============================

sub LCase {
    return lc($_[0]);
}

# ============================================================================
# &process_fortran_filr(filename)
# Search $filename for used modules, program statemenets, and included
# files. Store these in arrays.
# ============================================================================

sub process_fortran_file {

  my ($dir,$filehead,$inc,$inchead,$use,$usehead);

  $dir = "";

  # Find which directory the file is located
  $filehead = "";
  if (! open(FILE, $_[0])) {             # include file doesnt exist
    foreach $dir (@include_dirs) {       # look in the include paths
       if (!open(FILE, "$dir/$_[0]")) { close(FILE); next }
       else                           { $filehead = "$dir/"; last }
    }
    if ($filehead eq "") { die "Cannot find file $_[0]\n"; }
  }

  my $line;

  # Find which directory the include-files are located.
  # Place them in @incs
  while ($line=<FILE>) {

    if ($line =~ m'include'io &&
        $line =~ /^\s*(\#|\?\?)*\s*include\s+[\"\']([^\"\']+)[\"\']/io) {

       $inc = $2;

       $inchead="";
       if (! open(INC, $inc)) {          # include file doesnt exist
          foreach $dir (@include_dirs) { # look in the include paths
             if (!open(INC, "$dir/$inc")) { close(INC); next }
             else                         { $inchead = "$dir/"; last }
          }
          if ($inchead eq "") { die "Cannot find include file $inc\n"; }
       }
       close(INC);
       push(@incs, $inchead . $inc);

    }

    # Store only lower case "used" module names in @modules
    $line =~ /^\s*use\s+(\w+)/o &&       
            do {push(@modules, &$ModCase("$1"))};

    # Store module names in @moddefs
    $line =~ /^\s*module\s+(\w+)/io && 
            do {push(@moddefs, &$ModCase("$1")) if not (lc($1) eq "procedure")};

    # Store program title in $prog
    $line =~ /^\s*program\s+(\w+)/io && 
            do {$prog = &$ModCase($1)};
  }

  close(FILE);

  # Remove redundant entries.
  @incs = &uniq(@incs);
  @modules = &uniq(@modules);

}

