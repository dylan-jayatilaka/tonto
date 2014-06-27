########################################################################
#
# literal is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################
#
#  Project      :  File Preprocessor - literal module
#  Filename     :  $RCSfile$
#  Author       :  $Author: dylan $
#  Maintainer   :  Darren Miller: darren@cabaret.demon.co.uk
#  File version :  $Revision: 2353 $
#  Last changed :  $Date: 2003-11-21 17:25:02 +0800 (Fri, 21 Nov 2003) $
#  Description  :  This module allows literal strings ("string") to
#                  pass through filepp WITHOUT any macros in the string
#                  being replaced
#  Licence      :  GNU copyleft
#
########################################################################
# THIS IS A FILEPP MODULE, YOU NEED FILEPP TO USE IT!!!
# usage: filepp -m literal.pm <files>
########################################################################

package Literal;

use strict;

# version number of module
my $VERSION = '1.1.0';

# flag to say if in string or not
my $in_string1 = 0;

# if LITERAL_REVERSE is defined, only replace macros that are in strings.
if(Filepp::Ifdef("LITERAL_REVERSE")) { $in_string1 = 1; }

# char to start and end literal strings with
#my $literal = '"';
my $literal = '["\']';

my @Strings;
my @Literal;
my @Macros;

# tokens used for tempoarily replacing literals
my @Tokens = ('0','1','2','3','4','5','6','7','8','9',
	      'a','b','c','d','e','f','g','h','i','j',
	      'k','l','m','n','o','p','q','r','s','t',
	      'u','v','w','x','y','z','A','B','C','D',
	      'E','F','G','H','I','J','K','L','M','N',
	      'O','P','Q','R','S','T','U','V','W','X',
	      'Y','Z','_');

# get next token - converts integer into token, error if int out of range
sub GetToken
{
    my $i = shift;
    my $tokens = $#Tokens+1;
    my $count = $tokens*$tokens;
    if($i >= ($count*$tokens)) {
	Filepp::Error("literal: internal error, cannot find unused token");
    }
    my $token = $Tokens[int($i/$count)];
    if($i >= $count) { $i -= $count*(int($i/$count)); }

    $count /= $tokens;
    $token = $token.$Tokens[int($i/$count)];
    if($i >= $count) { $i -= $count*(int($i/$count)); }

    $token = $token.$Tokens[$i];
    return $token;
}

# replace all macros that do NOT appear in literal strings
sub ReplaceDefines
{
    my $in_string = $in_string1;
    my ($i,$char,$strchr);

    my $string = shift;
    if($string =~ /$literal/) {
	# split up line into bits in quotes and bits not in quotes
	@Strings = @Literal = @Macros = ();

        $i = 0;
        $strchr = '';
        $in_string = 0;
        push @Strings, '';
        push @Literal, 0;
        while ($i<length $string) {
          $char = substr $string,$i,1;
          if ($char =~ m/$literal/) {
#          if ($char =~ m/['"]/) {
            if ($in_string && $char eq $strchr) {
              # End a string.
              $in_string = 0;
              $Strings[$#Strings] .= $char;
              push @Strings, '';
              push @Literal, 0;
            } elsif ($in_string) {
              # Just another character in the string.
              $Strings[$#Strings] .= $char;
            } else {
              # Start a string.
              $in_string = 1;
              $strchr = $char;
              push @Strings, $char;
              push @Literal, 1;
            }
          } else {
            # Just another character outside a string.
            $Strings[$#Strings] .= $char;
          }
          $i++;
        }
	$string = "";

 	my $i;
	my $j = 0;
	for($i=0; $i<=$#Strings; $i++) {
	    # replace all literals with a token
	    if($Literal[$i]) {
		my $token = GetToken($j++);
		my $macro = "\e".$token."\e";
		# check token not already in use
 		while($macro ne Filepp::ReplaceDefines($macro)) {
 		    Filepp::Debug("literal: warning token ".$token.
 				  " cannot be used");
		    $token = GetToken($j++);
		    $macro = "\e".$token."\e";
 		}
		Filepp::Debug("literal: using token ".$token." for ".
			      $Strings[$i]);
		$string = $string.$macro;
		push(@Macros, $macro);
	    }
	    else {
		$string = $string.$Strings[$i];
	    }
	}
	# replace macros
	@Macros = reverse(@Macros);
	$string = Filepp::ReplaceDefines($string);
	# replace tokens with literals
	for($i=0; $i<=$#Strings; $i++) {
	    if($Literal[$i]) {		
		my $macro = pop(@Macros);
		my $replace = $Strings[$i];
		$string =~ s/$macro/$replace/g;
	    }
	}
    }
    else {
	# no literals in string
	$string = Filepp::ReplaceDefines($string);
    }
    return $string;    
}

sub outside_of_string {
# Return whether we are outside of a quoted string.
  my(@tmp,$i,$y,$in_single,$in_double);
  $i = 0;
  $in_single = 0;
  $in_double = 0;
  while ($i < length $_[0]) {
    $y = substr $_[0],$i,1;
    if ($y eq '\'' && ! $in_double) { $in_single = !  $in_single;}
    if ($y eq '"' && ! $in_single) { $in_double = ! $in_double;}
    $i++;
  }
  return (! ($in_single || $in_double));
}

# remove filepp's ReplaceDefines routine and replace it with Literals.
Filepp::RemoveProcessor("ReplaceDefines");
Filepp::AddProcessor("Literal::ReplaceDefines");

return 1;

########################################################################
# End of file
########################################################################
