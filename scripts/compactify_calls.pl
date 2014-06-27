#!/usr/bin/perl
#-------------------------------------------------------------------------------
#
# compactify_calls.pl
#
# Synopsis:
#
# This script is used to eliminate redundant routines when compiling an
# executable in the Tonto system. 
#
# Given that the .rcf routine call files exist for all the modules in Tonto,
# this script works through the calls made by a particular caller module and
# makes a .usd file for every called module. The .usd file contains a list of
# routines actually called by the caller module. Later, when compiling the
# .foo code into Fortran for a particular called module, the .usd list for that
# module is read and used to eliminate those routines which do not appear in the
# list (i.e. the redundant routines). This may help to eliminate unneccesary
# compilation resources, by reducing the outputted Fortran code.
#
# Usage : ./compactify_calls.pl -caller module -called module1 module2 ...
#                
# Where : 
#   
#    -caller module.rcf       means that "module.rcf" is the name of the routine
#                             call file (.rcf file) whose corresponding module \
#                             contains the calling routines or program.
#
#    -called module1.rcf ...  means the list of modules which are called by the
#                             caller module
#
#    All modules "module.rcf", "module1.rcf", ... must end in the .rcf file 
#    extension. Such .rcf files are made by the foo.pl script.
#
# (c) Dylan Jayatilaka, University of Western Australia, 2004.
#
# $Id: compactify_calls.pl 2839 2006-05-22 03:30:34Z dylan_ $
#-------------------------------------------------------------------------------

# Make sure the scope of all variables is declared.

use strict;             

#  Analyse the command line arguments

if (@ARGV==0) { die "No arguments specified. Stopped" } ;

# Extract the command line arguments

my $caller_file;
my @called_files;

while (@ARGV) {
    my $arg = shift;
    $_ = $arg;
    if (/^-/) {
        /^-caller\b/ && do { $caller_file = shift;      next; };
        /^-called\b/ && do { push(@called_files,shift); next; };
    } else {
        # no option? must be a -called
        push(@called_files,$arg); next;
    }
}

# Check if both options -caller and -called were used.

my $caller_module;

if (! defined $caller_file) { 
    die "No -caller module specified. Stopped";
}

$caller_file   =~ /^([\w{,}]+)(\.[\w]+)*?\.rcf/;
$caller_module = '' ;
if (defined $1) { $caller_module = $1 } 
if (defined $2) { $caller_module .= $2 } 
$caller_module = uc($caller_module);

 # if (! defined @calledfiles) { 
 #     die "No -called modules specified. Stopped"
 # }

# Make the list of all foo files. We'll read them next.

my @all_files; # All the foo files
my %module;    # Same as @allfiles, without .rcf ending

push(@all_files,$caller_file,@called_files);
 print "@all_files\n";

# Check all end in ".rcf" and all exist; read them in.

my $foofile;
my $head_name;
my $tail_name;
my $routine;
my $module;
my $called_routine;
my $called_module;
my $indent = '';

foreach $foofile (@all_files) {

   # get the head and tail of $foofile

   $foofile   =~ /([\w{,}]+)(\.\w+)*?(\.rcf)?$/;
   $head_name = '' ;
   if (defined $1) { $head_name = $1 } 
   if (defined $2) { $head_name .= $2 } 
   $tail_name = '';
   if (defined $3) { $tail_name = $3; }

   # does $foofile have a head?

   if ($head_name eq "") {
        die "No head part for .rcf file \"$foofile\". Stopped";
   }

   # does $foofile end in .rcf ?

   if ($tail_name eq "") {
        die "Tail of .rcf file \"$foofile\" does not end in .rcf. Stopped";
   }

   # does .rcf exist? Then open it.

   if (! open(RCFFILE, "$head_name.rcf")) {
        die ".rcf file \"$head_name.rcf\" does not exist";
   }

   # Append to the list of module names.

   $module = uc($head_name);
   next if (defined $module{$module});
   print "Module $module:";

   # Read in all the routines and their calls.

   my $line;
   my $explicit_call;
   my @item;

  print "\n";

   while ($line = <RCFFILE>) {

       # Remove calls: or interfaces: part, and space
       chomp($line);
       next if $line =~ /^ *$/;
       $line =~ s/\w+:$//; 
       $line =~ s/ *//g;

       # Split the line into items
       if ($line =~ /::/) { @item = split(/::/,$line); $explicit_call = 1; }
       else               { @item = split(/:/,$line);  $explicit_call = 0; }

       # One item: a routine definition or interfce
       if (@item==1) {
          $routine = $item[0];
          print "   Found routine \"$routine\":\n";
          if (! defined $module{$module}{$routine}) {
             %{$module{$module}{$routine}} = ();
          }
       }
       # Two items: routines called by a routine
       else {
          if (! defined $module{$module}{$routine}) {
             die "Routine \"$routine\" in module \"$module\" was not found.  Stopped";
          }
          $called_module  = $item[0];
          $called_routine = $item[1];
          # For explicit calls put an extra colon in front
          if ($explicit_call==1) { $called_routine = ":" . $called_routine; }
          print "      Found call to \"$called_module:$called_routine\"\n";
          push(@{$module{$module}{$routine}{$called_module}},$called_routine);
       }
   }

   close RCFFILE;

}

# Loop over the caller routines, and get called routines

print "\n";
print "Caller file   is \"$caller_file\"\n";
print "Caller module is \"$caller_module\"\n";
print "\n";

my @called_routine;
my %call_list = ();

foreach $routine (keys %{$module{$caller_module}}) {
   print "$caller_module:$routine\n";
   foreach $called_module (keys %{$module{$caller_module}{$routine}}) {
      @called_routine = @{$module{$caller_module}{$routine}{$called_module}};
 # print "called_module = $called_module\n";
 # print "called_routine = @called_routine\n";
      &add_calls_from($called_module,@called_routine);
   }
}

# Print out the .usd files for all the called routines

my $usdfile;

print "\n";
print "The list of used routines:\n";
print "\n";

foreach $module (keys %call_list) {
   print "module = $module\n";
   $usdfile = lc($module) . ".usd";
   open(USDFILE, ">$usdfile");
   foreach $routine (keys %{$call_list{$module}}) {
      $routine =~ s/^://;
      print "   $routine\n";
      print USDFILE "$routine\n";
   }
   close USDFILE;
}

###################################################################
sub add_calls_from {

   my $called_module = shift;
   my @called_routine = @_;

   my $indent_keep = $indent;
   $indent = $indent.'   ';
 
   my $routine;
   my $rout;
   my $explicit_call;
   my $module_call;
 
   foreach $routine (@called_routine) {

      if ($routine =~ s/^://) { $explicit_call = 1; }
      else                    { $explicit_call = 0; }

      print "$indent$called_module:$routine\n";

      if (! defined $call_list{$called_module}{$routine}) {

         if (! defined $module{$called_module}) {
            die "The .rcf file for module \"$called_module\" was not scanned.  Stopped";
         }

         if (! defined $module{$called_module}{$routine}) {
            if ($explicit_call == 1) {
               print "$indent\"$routine\" in \"$called_module\" appears to be a module variable\n";
               next;
            } else {
               die "Routine \"$routine\" in \"$called_module\" was not found in the rcf file.  Stopped";
            }
         }

         $call_list{$called_module}{$routine} = 1;

         foreach $module_call (keys %{$module{$called_module}{$routine}}) {
            &add_calls_from($module_call,@{$module{$called_module}{$routine}{$module_call}});
         }

      }

   }

   $indent = $indent_keep;
}
