#!/usr/bin/perl
# This script makes the TONTO-nav-bar.html from the .html files.
#
# ! $Id: make_navbar.pl 3967 2013-05-16 06:29:00Z dylan_ $
#
################################################################################

use File::Spec('abs2rel','canonpath');

$htmldir = $ARGV[0];
$f95docdir = $ARGV[1];
$docdir = $ARGV[2];

opendir DIR,$htmldir;
@dirlist = grep(/_short\.html$/,readdir(DIR));
closedir DIR;

$rel_path = File::Spec->abs2rel($htmldir,$docdir);
$htmldir = File::Spec->canonpath($rel_path);
$rel_path = File::Spec->abs2rel($f95docdir,$docdir);
$f95docdir = File::Spec->canonpath($rel_path);

print "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Frameset//EN\">";
print "<HTML>\n";
print "<HEAD>\n";
print "  <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=iso-8859-1\">\n";
print "  <META name=\"robots\" content=\"noindex, nofollow\">\n";
print "  <TITLE>Tonto Navigation</TITLE>\n";
print "  <LINK REL=\"stylesheet\" HREF=\"tonto.css\" TYPE=\"text/css\">\n";
print "  <BASE TARGET=\"main\">\n";
print "</HEAD>\n";
print "<BODY>\n";

print "<DIV CLASS=\"TITLE\">\n";
print "<BR><A CLASS=\"LOGO\">Tonto</A><BR><BR>\n";
print "<IMG SRC=\"hr.png\" HEIGHT=10 WIDTH=100%>\n";
print "</DIV>\n";
print "<H2 CLASS=\"TITLE\"><A HREF=\"htmlmanual/index.html\">Main Manual</A></H2>\n";
print "<IMG SRC=\"hr.png\" HEIGHT=10 WIDTH=100%>\n";

print "<H2 CLASS=\"TITLE\">Modules</H3>\n";

foreach $i (@dirlist) {
  $_=$i;
  s/_short\.html$//go;
  push @filelist,$_;
}
@modlist = grep(!/^run_/o,@filelist);
@proglist = grep(/^run_/o,@filelist);

foreach $i (@modlist) {
   print "<BR><A HREF=\"$htmldir/$i\_short.html\">$i</A> <A HREF=\"$htmldir/$i.html\">.foo</A>  <A HREF=\"$f95docdir/$i.F95\">.F95</A>\n";
}

print "<BR><BR>\n";
print "<H2 CLASS=\"TITLE\">Programs</H3>\n";

foreach $i (@proglist) {
   print "<BR><A HREF=\"$htmldir/$i\_short.html\">$i</A> <A HREF=\"foofiles/$i.foo\">.foo</A>  <A HREF=\"$f95docdir/$i.F95\">.F95</A>\n";
}

print "<BR><BR>\n";
print "<IMG SRC=\"hr.png\" HEIGHT=10 WIDTH=100%>\n";
print "</DIV>\n";
print "</BODY>\n";
print "</HTML>\n";
