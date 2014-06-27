package Tonto;

use strict;

# This sets the continuation line length for macros
my $linewidth = 125;
my $last_was_blank = 0;
my $include_level = 0;

sub split_by_comment {
# split the line into it's non-comment and comment parts, if applicable.
  my ($x,$y,$i,$left,$right);
  $x = $_[0];
  chomp($x);
  if ($x !~ '!') { # no comment
    return ($x,'');
  } else {
    $i = 0;
    while ($i < length $x) {
      $y = substr $x,$i,1;
      if ($y eq '!') {
        $left = substr $x,0,$i;
        $right = substr $x,$i;
        if (&outside_of_string($left)) {
          return ($left,$right);
        }
      }
      $i++;
    }
    return ($x,'');
  }
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

#sub RemoveComments {
## Remove the ! and everything after it.  This only works for one-line strings.
#  my ($prev,$this,$i,$x,@split_input);
#
#  # A quick optimisation.
#  if ($_[0] =~ m/[!]/o) {
#
#    # Remove the newline.
#    @split_input = split(/$/, $_[0]);
#
#    foreach $x (@split_input) {
#      chomp $x;
#      $prev = '';
#      $i = 0;
#      while ($i<length $x) {
#        $this = substr $x,$i,1;
#        if ($this eq '!' && &outside_of_string(substr $x,0,$i)) {
#          $x = substr $x,0,$i;
#          last;
#        }
#        $prev = $this;
#        $i++;
#      }
#
#      # Remove spaces from ends of lines.
#      $x =~ s/\s+$//o;
#    }
#
#    return join "\n", @split_input;
#  }
#
#  # No comments.
#  return $_[0];
#}

sub StartLevel {
   $include_level = $include_level + 1;
}

sub FinishLevel {
   $include_level = $include_level - 1;
}

sub RemoveIncludedComments {
# Remove the ! and everything after it.  This only works for one-line strings.
  my ($prev,$this,$i,$x,@split_input);

  if ($include_level<=1) { return $_[0] }

  # A quick optimisation.
  if ($_[0] =~ m/[!]/o) {

    # Remove the newline.
    @split_input = split(/$/, $_[0]);

    foreach $x (@split_input) {
      chomp $x;
      $prev = '';
      $i = 0;
      while ($i<length $x) {
        $this = substr $x,$i,1;
        if ($this eq '!' && &outside_of_string(substr $x,0,$i)) {
          $x = substr $x,0,$i;
          last;
        }
        $prev = $this;
        $i++;
      }

      # Remove spaces from ends of lines.
      $x =~ s/\s+$//o;
    }

    return join "\n", @split_input;
  }

  # No comments.
  return $_[0];
}

sub LineWrap {
# Split a line so that it is no longer than 130 characters.  Uses the &
# continuation character.
  my ($i,$str,$left,$right,$out,$comment,$already_has_continue,$cont);
  my $inp = $_[0];

  $inp =~ s/\s+(?=\n)$//; # remove spaces from EOL.
  $out = '';

  # Take out the comment if present, put it back at the end.
  ($inp,$comment) = &split_by_comment($inp);

  # Do we already have a continue statement?  We want to keep it.
  $already_has_continue = ($inp =~ s/(&\s*)$//s);
  $cont = '';
  if (defined $1) { $cont = $1; }

  # Insert continuation characters.  Doesn't matter if we split in the middle of
  # a word or string.
  while (length($inp) > 0) {
    if (length($inp)>$linewidth+1) {
      $out .= substr($inp,0,$linewidth);
      $inp = substr($inp,$linewidth);
      $out .= "&\n&";
    } else {
      $out .= $inp;
      $inp = '';
    }
  }

  # We might have added an extra continuation.  Remove it.
  if ($out =~ s/&\n&\s*$//s) { print STDERR "x\nx\nx\n"; }

  # Add original continuation character if we had one.
  if ($already_has_continue) { $out .= $cont . $comment . "\n"; }
  else                       { $out .= $comment . "\n"; }

  return $out;
}

sub Remove_Multiple_Blank_Lines {
  my $inp = $_[0];
  if ($inp =~ m/^$/s) {
    if ($last_was_blank) {$inp = '';}
    $last_was_blank = 1;
  } else {
    $last_was_blank = 0;
  }
  return $inp;
}

Filepp::AddOpenInputFunc("Tonto::StartLevel");
Filepp::AddCloseInputFunc("Tonto::FinishLevel");

Filepp::AddProcessor("Tonto::RemoveIncludedComments",1);
Filepp::AddProcessor("Tonto::LineWrap",0);
Filepp::AddProcessor("Tonto::Remove_Multiple_Blank_Lines",0);

return 1;
