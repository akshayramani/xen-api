#!/bin/sh

# This script is called from the 'api' build with two arguments:
#
# uninstall.sh <v6.hg checkout> <xen-api.hg checkout>
#
# and it removes the proprietary code from the xen-api.hg checkout
# and unpatches the OMakefile

set -e

V6=$1
API=$2

if [ ! -d "$V6" ]; then
  echo "$V6 is not a directory"
  exit 1
fi

if [ ! -d "$API" ]; then
  echo "$API is not a directory"
  exit 1
fi

for i in lpe.ml lpe_stubs.c realv6.ml realv6.mli v6d.ml
do
  rm -f $API/ocaml/license/$i
done
cd $API
echo Done
