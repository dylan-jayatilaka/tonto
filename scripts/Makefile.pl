#!/usr/bin/perl

#  Run this script to configure the Makefile for your system.
#  It runs some tests such as determining what your compiler is.

# This module is added manually, since it is not in the perl distribution.
push @INC,'./scripts';
require File::Which;
import File::Which 'which';           # An implementation of the Unix "which" command.

use POSIX 'uname';                    # Need this for uname.
use Getopt::Long;                     # Get the command line options.
use File::Spec 'splitpath';           # To get the path to files.

my $OS = '';                          # Name of the operating system.
my $MAKE = '';
my $SRCDIR = '.';                     # Directory for the source files
my $INSTALLDIR = '/usr/local/bin';    # Directory to install executables.
my $PERL = '';                        # Location of the perl executable.
my $FC = '';                          # The fortran compiler command.
my $FOPTNS_TYPE = '';                 # FOPTNS setting: whether fast, debug or custom
my $COMPILER_VENDOR = '';             # Name of the compiler vendor.
my $COMPILER_ID_ = '';                # Compiler ID
my $havelibs = 0;                     # Whether have determined the libraries to use.
my $FSUFFIX = 'f95';                  # Fortran files have this suffix.
my $PLATFORM_ID = '';                 # e.g. LAHEY-lf95-on-WINDOWS
my $PLATFORM_ID_ = '';                # e.g. LAHEY_lf95_on_WINDOWS
my $PLATFORM_INFO_FILE = '';          # This is the compiler vendor and operating system
my $GIT_VERSION = `git rev-parse --short HEAD`; # The git version
my $INT_KIND = 4;                     # The default integer kind
my $BIN_KIND = 4;                     # The default logical kind
my $REAL_KIND = 8;                    # The default real kind
my $CPX_KIND = 8;                     # The default complex kind
my $BRACES_EXPANDING = '';            # The default for braces expanding

my $show_help = 0;
my $show_defaults = 0;
my ($tmp1,$tmp2);

my %vendors = (                                # This contains a list of string
'PATHSCALE'  => 'PathScale',                   # identifiers for each vendor.
'IBM'        => 'IBM',
'IBM'        => 'XL Fortran',
'NAG'        => 'NAG',
'WORKSHOP'   => 'WorkShop',
'SUN'        => 'Sun',
'PGI'        => 'Portland',
'INTEL'      => 'Intel',
'GNU'        => '(g95|gfortran)',
);

################################################################################
# Process the command line arguments, if any.

GetOptions(  'help' => \$show_help,
             'show_defaults' => \$show_defaults,
             'installdir=s' => \$INSTALLDIR,
             'fc=s' => \$FC,
             'srcdir=s' => \$SRCDIR,
) || do { &show_options; exit 1; };

if ($show_help) {
  print STDOUT "\n";
  print STDOUT "This script sets up the Makefile for Tonto to compile on your system.\n";
  print STDOUT "You probably do not need to set any options.\n";
  &show_options;
}

if ($show_defaults) {
  print STDOUT "\n";
  print STDOUT "Default directory for source files is \"$SRCDIR\"\n";
  print STDOUT "Default directory to install programs to is \"$INSTALLDIR\"\n";
  exit 0;
}

if ($show_help || $show_defaults) { exit 0; }

-d "$SRCDIR" || do { print STDERR "Error : Source directory does not exist\n";
                     exit 0;
                   };

-f "$SRCDIR/scripts/Makefile.in" || do { print STDERR "Error : Makefile.in does not exist in scripts/\n";
                                          exit 0;
                                        };

################################################################################

print STDERR "Getting operating system name ...";
$OS = &get_OS;
&print_result($OS);

print STDERR "Checking for perl ...";
$PERL = &check_for_program('perl');
&print_result($PERL);

print STDERR "Checking for make ...";
($tmp,$MAKE) = &check_for_programs('gmake','make');
&print_result($MAKE);

if ($MAKE ne '') {
  print STDERR 'Checking whether GNU make ...';
  $have_gnu_make = &check_is_GNU_make;
  &print_boolean($have_gnu_make);
}

# print STDERR "Checking for brace expansion ...";
# $shell = $ENV{'SHELL'};
# if ($shell =~ /bash/i) { # This if bash is set in Makefile.in
#   $BRACES_EXPANDING = '+B';
#   &print_result($BRACES_EXPANDING);
# } else {
#   &print_result('None');
# }


print STDERR "Checking for Fortran compiler ...";
if (defined $FC && $FC ne '') {
  if (-f $FC) {
    $FULLFC = $FC;
    ($tmp1,$tmp2,$FC) = File::Spec->splitpath($FULLFC);
  } else {
    ($FC,$FULLFC) = &check_for_programs($FC);
  }
} else {
  ($FC,$FULLFC) =
  &check_for_programs('pathf95','mpif95','lf95','ifort','ifc','efc',
             'frt','xlf','pgf95','f95','f90','g95','gfortran','nagfor');
}
&print_result($FULLFC);

# print STDERR "\n";  # Just a gap to separate checks from Makefile stuff.

if ($FC ne '') {

  print STDERR "Determining Fortran compiler vendor ...";
  $COMPILER_VENDOR = &get_vendor($FC);
  if ($COMPILER_VENDOR ne '') {&print_result($COMPILER_VENDOR)}
  else {&print_result('cannot determine');}

  $PLATFORM_ID = "${COMPILER_VENDOR}-${FC}-on-${OS}";
  $PLATFORM_ID_ = "${COMPILER_VENDOR}_${FC}_on_${OS}";
  $COMPILER_ID_ = "${COMPILER_VENDOR}_${FC}";
  $PLATFORM_INFO_FILE = "${SRCDIR}/platforms/${PLATFORM_ID}";

  print STDERR "Determining Fortran options used ...";
  $FOPTNS_TYPE = &get_foptns_type;
  &print_result($FOPTNS_TYPE);

  print STDERR "Determining Fortran default integer kind ...";
  &get_default_integer_kind;
  &print_result($INT_KIND);

  print STDERR "Determining Fortran default logical kind ...";
  &get_default_logical_kind;
  &print_result($BIN_KIND);

  print STDERR "Determining Fortran default double precision kind ...";
  &get_default_double_precision_kind;
  &print_result($REAL_KIND);

  print STDERR "Determining Fortran default double complex kind ...";
  &get_default_double_complex_kind;
  &print_result($CPX_KIND);

  print STDERR "Compiler options taken from $PLATFORM_INFO_FILE\n";


  print STDERR "Makefile located at $PLATFORM_ID/$FOPTNS_TYPE/Makefile\n";
  &check_siteconfig;

} else {

  die "Cannot find a Fortran compiler on your system.";

}

# Escape blank spaces in file names
$INSTALLDIR =~ s/ /\\ /g;
$SRCDIR =~ s/ /\\ /g;
$PERL =~ s/ /\\ /g;
$MAKE =~ s/ /\\ /g;
$FULLFC =~ s/ /\\ /g;

&do_substitutions_into_Makefile;

################################################################################
sub print_result {
  if (defined $_[0] && $_[0] ne '') {
    print STDERR " $_[0]\n";
  } else {
    print STDERR " none\n";
  }
}

################################################################################
sub print_boolean {
  if ($_[0]) {
    print STDERR " yes\n";
  } else {
    print STDERR " no\n";
  }
}

################################################################################
sub get_OS {
  my($tmp);
  $tmp = (POSIX::uname())[0];
  if (! defined $tmp || $tmp eq '') {$tmp = 'unknown'}
  $tmp =~ s/cygwin(.*)/windows/i;
  $tmp =~ s/(?<=windows)(.*)//i;
  return uc($tmp);
}

################################################################################
sub check_for_programs {
  my($tmp,$word);
  for $word (@_) {
    $tmp = &check_for_program($word);
    if (defined $tmp && $tmp ne '') {return ($word,$tmp);}
  }
  return ('','');
}

################################################################################
sub check_for_program {
  return which("$_[0]");
}

################################################################################
sub get_default_integer_kind {
  my($INTTEST);
  unlink("inttest.map","inttest","inttest.f90","inttest.out","inttest.exe","inttest.o","inttest.obj");
  open(INTTEST,">","inttest.f90");
  print INTTEST "program main\n";
  print INTTEST "  integer :: i\n";
  print INTTEST "  print *,kind(i)\n";
  print INTTEST "end program\n";
  close(INTTEST);
  system("${FC} -o ./inttest ./inttest.f90 > /dev/null 2>&1");
  if ( -x 'inttest.exe')  { system("./inttest.exe > inttest.out"); }
  if ( -x 'inttest')      { system("./inttest     > inttest.out"); }
  if (open(INTTEST,"<","inttest.out")) {
     $INT_KIND = <INTTEST>;
     chomp($INT_KIND);
     $INT_KIND =~s/\s+//g;
     $INT_KIND =~s/\W+//g;
  }
  unlink("inttest.f90");
  unlink("inttest.map","inttest","inttest.out","inttest.exe","inttest.o","inttest.obj");
}

################################################################################
sub get_default_double_precision_kind {
  my($REALTEST);
  unlink("realtest.map","realtest","realtest.f90","realtest.out","realtest.exe","realtest.o","realtest.obj");
  open(REALTEST,">","realtest.f90");
  print REALTEST "program main\n";
  print REALTEST "  print *,kind(1.0d0)\n";
  print REALTEST "end program\n";
  close(REALTEST);
  system("${FC} -o realtest.exe realtest.f90 > /dev/null 2>&1");
  if ( -x 'realtest.exe') { system("./realtest.exe > realtest.out"); }
  if ( -x 'realtest')     { system("./realtest     > realtest.out"); }
  if (open(REALTEST,"<","realtest.out")) {
     $REAL_KIND = <REALTEST>;
     chomp($REAL_KIND);
     $REAL_KIND =~s/\s+//g;
     $REAL_KIND =~s/\W+//g;
  }
  unlink("realtest.map","realtest","realtest.f90","realtest.out","realtest.exe","realtest.o","realtest.obj");
}

################################################################################
sub get_default_double_complex_kind {
  my($COMPLEXTEST);
  unlink("complextest.map","complextest","complextest.f90","complextest.out","complextest.exe","complextest.o","complextest.obj");
  open(COMPLEXTEST,">","complextest.f90");
  print COMPLEXTEST "program main\n";
  print COMPLEXTEST "  print *,kind((1.0d0,1.0d0))\n";
  print COMPLEXTEST "end program\n";
  close(COMPLEXTEST);
  system("${FC} -o complextest.exe complextest.f90 > /dev/null 2>&1");
  if ( -x 'complextest.exe') { system("./complextest.exe > complextest.out"); }
  if ( -x 'complextest')     { system("./complextest     > complextest.out"); }
  if (open(COMPLEXTEST,"<","complextest.out")) {
     $CPX_KIND = <COMPLEXTEST>;
     chomp($CPX_KIND);
     $CPX_KIND =~s/\s+//g;
     $CPX_KIND =~s/\W+//g;
  }
  unlink("complextest.map","complextest","complextest.f90","complextest.out","complextest.exe","complextest.o","complextest.obj");
}

################################################################################
sub get_default_logical_kind {
  my($BINTEST);
  unlink("bintest.map","bintest","bintest.f90","bintest.out","bintest.exe","bintest.o","bintest.obj");
  open(BINTEST,">","bintest.f90");
  print BINTEST "program main\n";
  print BINTEST "  logical :: l\n";
  print BINTEST "  print *,kind(l)\n";
  print BINTEST "end program\n";
  close(BINTEST);
  system("${FC} -o bintest.exe bintest.f90 > /dev/null 2>&1");
  if ( -x 'bintest.exe') { system("./bintest.exe > bintest.out"); }
  if ( -x 'bintest')     { system("./bintest     > bintest.out"); }
  if (open(BINTEST,"<","bintest.out")) {
     $BIN_KIND = <BINTEST>;
     chomp($BIN_KIND);
     $BIN_KIND =~s/\s+//g;
     $BIN_KIND =~s/\W+//g;
  }
  unlink("bintest.map","bintest","bintest.f90","bintest.out","bintest.exe","bintest.o","bintest.obj");
}

################################################################################
sub check_is_GNU_make {
  my($is_gnu,$MAKE_OUT);
  $is_gnu = 0;
  defined($MAKE) or die "no MAKE variable defined";
  system("${MAKE} -v > conf.test 2>&1");
  if (! -f "conf.test") {return(0)};
  open(MAKE_OUT,"conf.test");
  while (<MAKE_OUT>) {
    if (m/\bGNU\b/o) {$is_gnu = 1}
  }
  close(MAKE_OUT);
  unlink("conf.test");
  return($is_gnu);
}

################################################################################
sub get_vendor {
  my(@trystring,$vec,$vendor,$search);
  open(CONFTEST,"> conftest.${FSUFFIX}");
  print CONFTEST "program main\n";
  print CONFTEST "  write(*,*) 1\n";
  print CONFTEST "end program\n";
  close(CONFTEST);

  unlink("conf.test");
  $vendor = 'UNKNOWN';
  @trystr = (
    "$FC -V >> conf.test 2>&1",
    "$FC -v >> conf.test 2>&1",
    "$FC -version >> conf.test 2>&1",
    "$FC --version >> conf.test 2>&1",
    "$FC -V conftest.${FSUFFIX} >> conf.test 2>&1",
    "$FC -v conftest.${FSUFFIX} >> conf.test 2>&1",
    "$FC -version conftest.${FSUFFIX} >> conf.test 2>&1",
    "$FC --version conftest.${FSUFFIX} >> conf.test 2>&1",
  );
  THIS : foreach $str (@trystr) {
    system($str);
    foreach $ven (keys(%vendors)) {
      open(CONFOUT,"< conf.test") || next;
      $search = $vendors{$ven};
      $tmp = $search;
      while (<CONFOUT>) {
        if (m/$search/) {
          $vendor = $ven;
          close(CONFOUT);
          last THIS;
        }
      }
      close(CONFOUT);
    }
    if ($vendor ne 'UNKNOWN') {last}
  }
  unlink("conftest.${FSUFFIX}");
  unlink("conf.test","a.out","conf.exe");
  return $vendor;
}

################################################################################
sub get_foptns_type {

  open(INFILE,"< platforms/$PLATFORM_ID");

  my $loc = " ";

  while(<INFILE>) {

    if    (/^FOPTNS\s*=\s*\$\(FFAST\)/)  { $loc = "fast"; }
    elsif (/^FOPTNS\s*=\s*\$\(FDEBUG\)/) { $loc = "debug"; }
    elsif (/^FOPTNS\s*=\s*\$\(FPROF\)/)  { $loc = "custom"; }

    next if ($loc eq " ");

    last;

  }

  close(INFILE);

  return $loc;

}

################################################################################
sub check_siteconfig {
  if (! -f $PLATFORM_INFO_FILE) {
    print STDERR "\n";
    print STDERR "Note: File ${PLATFORM_INFO_FILE} created from template.\n";
    print STDERR "Please edit ${PLATFORM_INFO_FILE} and reuse this script.\n";
    open(SCt,"${SRCDIR}/platforms/template");
    open(SC,"> $PLATFORM_INFO_FILE");
    while(<SCt>) {
      s/^(FC.*?=)(.*)/$1 $FC/;
      print SC;
    }
    close(SC);
    close(SCt);
  }
}

################################################################################
sub show_options {
  print STDOUT "\n";
  print STDOUT "Valid options include :\n";
  print STDOUT "  --help                 Show this message and then exit.\n";
  print STDOUT "  --srcdir=DIR           The source files are in DIR.\n";
  print STDOUT "  --installdir=DIR       Compiled programs go into DIR.\n";
  print STDOUT "  --fc=COMPILER          Set the fortran compiler command to COMPILER.\n";
  print STDOUT "  --show_defaults        Show default settings.\n";
}

################################################################################
sub do_substitutions_into_Makefile {

  -f "$SRCDIR/scripts/Makefile.in" || do {print STDERR "Makefile.in not found in scripts/"; exit 1};

  # Make the Makefile directory location
  -d "$PLATFORM_ID"      || do { system("mkdir $PLATFORM_ID") };
  -d "$PLATFORM_ID/$FOPTNS_TYPE" || do { system("mkdir $PLATFORM_ID/$FOPTNS_TYPE") };

  open(OUTFILE,"> $PLATFORM_ID/$FOPTNS_TYPE/Makefile");

  open(INFILE,"< $SRCDIR/scripts/Makefile.in");

  while(<INFILE>) {
    s/\@INSTALLDIR\@/$INSTALLDIR/g;
    s/\@SRCDIR\@/$SRCDIR/g;
    s/\@PERL\@/$PERL/g;
    s/\@MAKE\@/$MAKE/g;
    s/\@BRACESEXPANDING\@/$BRACES_EXPANDING/g;
    s/\@OS\@/$OS/g;
    s/\@FC\@/$FULLFC/g;
    s/\@FOPTNS_TYPE\@/$FOPTNS_TYPE/g;
    s/\@COMPILER_ID_\@/$COMPILER_ID_/g;
    s/\@PLATFORM_INFO_FILE\@/$PLATFORM_INFO_FILE/g;
    s/\@PLATFORM_ID\@/$PLATFORM_ID/g;
    s/\@PLATFORM_ID_\@/$PLATFORM_ID_/g;
    s/\@GIT_VERSION\@/$GIT_VERSION/g;
    s/\@INT_KIND\@/$INT_KIND/g;
    s/\@BIN_KIND\@/$BIN_KIND/g;
    s/\@REAL_KIND\@/$REAL_KIND/g;
    s/\@CPX_KIND\@/$CPX_KIND/g;
    print OUTFILE;
  }

  close(OUTFILE);
  close(INFILE);

  # Link the Makefile
  system("rm -f Makefile");
  system("ln -s $PLATFORM_ID/$FOPTNS_TYPE/Makefile Makefile");
  system("rm -f $PLATFORM_ID.$FOPTNS_TYPE.make");
  system("ln -s $PLATFORM_ID/$FOPTNS_TYPE/Makefile $PLATFORM_ID.$FOPTNS_TYPE.make");

}
