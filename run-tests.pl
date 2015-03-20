#!/usr/bin/env perl

use Getopt::Long;
use FindBin;
use File::Find;

our $FORM = "(xmls::test)";
our $SEPARATOR="";
our $usage = <<'USAGE';
usage: run-tests.sh [options] [tests]
options: 
    --sbcl run tests with sbcl (default)
    --cmucl run tests with cmucl
    --abcl run tests with abcl
    --ccl run tests with clozure common lisp
    --allegro run tests with Allegro Common Lisp, ANSI mode
    --allegromodern run tests with Allegro Common Lisp, modern case-sensitive mode
    --all run all tests in tests directory
    --verbose output parsed xml
USAGE

our $command = $ENV{SBCL} || "sbcl";
our $CMDLINE="${command} --no-userinit --eval \'(require :asdf)\' --eval \'(progn (load \"xmls.asd\") (asdf:load-system \"xmls\"))\' --eval";

my $help = 0;
my $verbose = 0;
GetOptions ( "abcl" => \&lisp_handler,
             "ccl" => \&lisp_handler,
             "cmucl" => \&lisp_handler,
             "allegro" => \&lisp_handler,
             "allegromodern" => \&lisp_handler,
             "sbcl" => \&lisp_handler,
             "clisp" => \&lisp_handler,
             "help" => \$help,
             "usage" => \$help,
             "verbose" => \&set_verbose,
             "all" => \&set_all_tests,
             );

unless ( $TESTS ) {
    set_all_tests();
}

{ my $command =  "$CMDLINE \"$FORM\" $SEPARATOR $TESTS";
  print "$command\n" if $verbose;
  system $command;
}


# subroutines from here on down...

sub set_verbose {
    $FORM="(progn (setf xmls::*test-verbose* t)(xmls::test))";
    $verbose = 1;
}

our @all_tests;
sub set_all_tests {
    File::Find::find({wanted => \&wanted}, $FindBin::RealBin);
    if ($verbose) {
      print STDERR "Test list is:\n";
      foreach my $test (@all_tests) {
        print STDERR "\t$test\n";
      }
    }
    $TESTS = join(" ", @all_tests);
}

sub wanted {
    /^.*\.xml\z/s
    && push @all_tests, $File::Find::name;
}

sub usage {
    print $usage;
}

sub lisp_handler {
    my $lisp = shift;
    if ( $lisp eq "abcl" ) {
           $command=$ENV{ABCL} || "abcl";
           $CMDLINE="${command} --noinit --noinform --eval \'(require :asdf)\' --load xmls.asd --eval \'(asdf:load-system :xmls)\' --eval";
       } elsif ( $lisp eq "ccl" ) {
           $command=$ENV{CCL} || "ccl";
           $CMDLINE="${command} --no-init --quiet --eval \'(require :asdf)\' --load xmls.asd --eval '(asdf:load-system :xmls)' --eval";
           $SEPARATOR="--";
       } elsif ( $lisp eq "cmucl" ) {
           $command=$ENV{CMUCL} || "lisp";
           $CMDLINE="${command} -eval \'(require :asdf)\' -load xmls.asd -eval \'(asdf:load-system :xmls)\' -eval";
       } elsif ($lisp eq "allegro") {
           $command=$ENV{ALLEGRO} || "alisp";
           $CMDLINE="${command} -q -e \'(require :asdf)\' -L xmls.asd -e \'(asdf:load-system :xmls)\' -e";
           $SEPARATOR="--";
       } elsif ($lisp eq "allegromodern") {
           $command=$ENV{ALLEGROMODERN} || "mlisp";
           $CMDLINE="${command} -q -e \'(require :asdf)\' -L xmls.asd -e \'(asdf:load-system :xmls)\' -e";
           $SEPARATOR="--";
       } elsif ($lisp eq "sbcl") {
           # the default...
       } elsif ($lisp eq "clisp") {
           $command=$ENV{CLISP} || "clisp";
           $CMDLINE="${command} -norc -ansi -x \'(require :asdf)\' -i xmls.asd -x \'(asdf:load-system :xmls)\' -x";
           $SEPARATOR="--";
       }
}


