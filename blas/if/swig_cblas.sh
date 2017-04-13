#!/bin/bash

usage_exit() {
    echo "Usage: $0 [-i interface]" 1>&2
    exit 1
}

if [ ${#} = 0 ]
then
    usage_exit
fi

while getopts i:h OPT
do
    case ${OPT}
    in
        i)  swig_interface=${OPTARG}
            ;;
        h)  usage_exit
            ;;
        \?) usage_exit
            ;;
    esac
done

shift $((OPTIND - 1))

output=${swig_interface}.lisp

swig -cffi -generate-typedef -noswig-lisp ${swig_interface}.i

sed -i '.ORG' \
    -e $'1s/^/;;; blas\/if\/cblas.lisp\\\n\\\n(cl:in-package "EIGEN")\\\n\\\n/' ${output}
