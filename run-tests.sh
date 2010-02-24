#!/bin/sh
# $Id$

FORM="(xmls::test)"
SEPARATOR=""

usage () {
    cat <<USAGE
usage: run-tests.sh [options] [tests]
options: 
    --sbcl run tests with sbcl (default)
    --cmucl run tests with cmucl
    --all run all tests in tests directory
    --verbose output parsed xml
    --allegro run tests with Allegro Common Lisp, ANSI mode
    --allegromodern run tests with Allegro Common Lisp, modern case-sensitive mode
USAGE
    exit 1
}

CMDLINE="sbcl --noinform --load xmls --load xmlrep-helpers --eval"
while [ $# -gt 0 ]; do 
    case $1 in
        --cmucl)
            CMDLINE="lisp -load xmls -load xmlrep-helpers -eval"
            shift
            ;;
        --allegro)
            CMDLINE="alisp -q -L xmls -L xmlrep-helpers -e"
            SEPARATOR="--"
            shift
            ;;
        --allegromodern)
            CMDLINE="mlisp -q -L xmls -L xmlrep-helpers -e"
            SEPARATOR="--"
            shift
            ;;
        --all)
            TESTS="tests/*/*"
            shift
            ;;
        --verbose)
            FORM="(progn (setf xmls::*test-verbose* t)(xmls::test))"
            shift
            ;;
        *)
            TESTS="$*"
            break
            ;;
        esac
done



if test -z "$TESTS"; then
    usage
fi

$CMDLINE "$FORM" $SEPARATOR $TESTS
