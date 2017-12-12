#!/usr/bin/env perl

use Getopt::Long;
use FindBin;
use File::Find;

our $FORM = "(xmls::test)";
our $EVAL = "--eval";
our $SEPARATOR="";
our $usage = <<'USAGE';
usage: run-tests.sh [options] [tests]
options: 
    --abcl run tests with abcl
    --allegro run tests with Allegro Common Lisp, ANSI mode
    --allegromodern run tests with Allegro Common Lisp, modern case-sensitive mode
    --ccl run tests with clozure common lisp
    --cmucl run tests with cmucl
    --sbcl run tests with sbcl (default)
    --all run all tests in tests directory (default)
    --verbose output parsed xml
USAGE

our $command = $ENV{SBCL} || "sbcl";
our $CMDLINE="${command} --no-userinit ";
our $SEPARATOR="--";
our $LOAD="--load";
my $help = 0;
my $verbose = 0;
$ENV{"CL_SOURCE_REGISTRY"}=$FindBin::RealBin . ":" unless $ENV{"CL_SOURCE_REGISTRY"};
GetOptions ( "abcl" => \&lisp_handler,
             "ccl" => \&lisp_handler,
             "cmucl" => \&lisp_handler,
             "allegro" => \&lisp_handler,
             "allegromodern" => \&lisp_handler,
             "sbcl" => \&lisp_handler,
             "clisp" => \&lisp_handler,
             "help" => \$help,
             "usage" => \$help,
             "verbose" => \&set_verbose
             );

if ($help) {
  print $usage;
  exit 0;
}

# unless ( $TESTS ) {
#     set_all_tests();
# }

# {
#   my $command =  "$CMDLINE $EVAL \"(require :asdf)\" $EVAL \"(asdf:load-system :xmls)\" $EVAL \"$FORM\" $SEPARATOR $TESTS";
#   print "$command\n" if $verbose;
#   my $code = system $command;
#   if ($code != 0) {
#     print "XMLS parsing tests failed.\n";
#     exit $code
#   } else {
#     if ($verbose) {
#     }
#   }
# }

{
print STDERR "Running ASDF tests.\n";
my $cmd = "$CMDLINE $LOAD $FindBin::RealBin/run-tests.lisp";
print STDERR "Command for 5AM tests is:\n\t$cmd\n";
my $code = system $cmd;
print STDERR "ASDF test output code is: $code\n";
if ($code) {
  $code = $code >> 8;
  print STDERR "Exiting script with code $code\n";
  # this is going wrong...
  exit $code;
}
print STDERR "Done running ASDF tests.\n";
exit 0;
}

# subroutines from here on down...

sub set_verbose {
    $FORM="(progn (setf xmls::*test-verbose* t)(xmls::test))";
    $verbose = 1;
}

# our @all_tests;
# sub set_all_tests {
#     File::Find::find({wanted => \&wanted}, "$FindBin::RealBin/tests/");
#     if ($verbose) {
#       print STDERR "Test list is:\n";
#       foreach my $test (@all_tests) {
#         print STDERR "\t$test\n";
#       }
#     }
#     $TESTS = join(" ", @all_tests);
# }

# sub wanted {
#     /^.*\.xml\z/s
#     && push @all_tests, $File::Find::name;
# }

sub usage {
    print $usage;
}

sub lisp_handler {
    my $lisp = shift;
    if ( $lisp eq "abcl" ) {
           $command=$ENV{ABCL} || "abcl";
           $CMDLINE="${command} --noinit --noinform"; # --eval \'(require :asdf)\' --load xmls.asd --eval \'(asdf:load-system :xmls)\' ";
       } elsif ( $lisp eq "ccl" ) {
           $command=$ENV{CCL} || "ccl";
           $CMDLINE="${command} --no-init --quiet"; # --eval \'(require :asdf)\' --load xmls.asd --eval '(asdf:load-system :xmls)' ";
           $SEPARATOR="--";
       } elsif ( $lisp eq "cmucl" ) {
           $command=$ENV{CMUCL} || "lisp";
           $EVAL="-eval"; $LOAD="-load";
           $CMDLINE="${command} -noinit " #-eval \'(require :asdf)\' -load xmls.asd -eval \'(asdf:load-system :xmls)\' ";
       } elsif ($lisp eq "allegro") {
           $command=$ENV{ALLEGRO} || "alisp";
           $EVAL = "-e"; $LOAD="-L";
           $CMDLINE="${command} -q"; # -e \'(require :asdf)\' -L xmls.asd -e \'(asdf:load-system :xmls)\' ";
           $SEPARATOR="--";
       } elsif ($lisp eq "allegromodern") {
           $command=$ENV{ALLEGROMODERN} || "mlisp";
           $EVAL = "-e";
           $LOAD = "-L";
           $CMDLINE="${command} -q "; #-e \'(require :asdf)\' -L xmls.asd -e \'(asdf:load-system :xmls)\' ";
           $SEPARATOR="--";
       } elsif ($lisp eq "sbcl") {
           # the default...
       } elsif ($lisp eq "clisp") {
           $command=$ENV{CLISP} || "clisp";
           $EVAL = "-x";
           $LOAD = "-i";
           $CMDLINE="${command} -norc -ansi"; # -x \'(require :asdf)\' -i xmls.asd -x \'(asdf:load-system :xmls)\' ";
           $SEPARATOR="--";
       }
}


