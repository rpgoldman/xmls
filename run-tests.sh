#!/bin/sh

CMDLINE="sbcl --noinform --load xmls --eval"
FORM="(xmls::test)"
ALLEGROCMD="/usr/local/acl/acl62/lisp"
SEPARATOR=""

usage () {
    cat <<USAGE
usage: run-tests.sh [options] [tests]
options: 
    --sbcl run tests with sbcl (default)
    --cmucl run tests with cmucl
    --all run all tests in tests directory
    --verbose output parsed xml
    --acl62 run tests with Allegro Common Lisp, v6.2
USAGE
    exit 1
}

while [ $# -gt 0 ]; do 
    case $1 in
        --cmucl)
            CMDLINE="lisp -load xmls -eval"
            shift
            ;;
        --acl62)
            CMDLINE="${ALLEGROCMD} -L xmls -e"
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
