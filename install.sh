#!/bin/sh

# This script is called from the 'api' build with two arguments:
#
# install.sh <v6.hg checkout> <xen-api.hg checkout>
#
# and it copies the proprietary code into the xen-api.hg checkout
# and prepares it for building by patching the OMakefile

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

for i in lpe.ml lpe.mli lpe_stubs.c realv6.ml realv6.mli v6d.ml v6d.mli v6globs.ml grace_retry.ml grace_retry.mli v6alert.ml v6alert.mli
do
  cp $V6/$i $API/ocaml/license
done
cd $API
echo Done
