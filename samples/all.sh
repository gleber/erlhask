#!/bin/bash

# set -e
# set -x

includes=$(find -type d | xargs -n 1 echo -I)
# echo $includes

for i in `find -iname "*.erl" | grep -v _SUITE | grep /src/`; do
    dir=$(dirname $i)
    b=$(basename $i)
    ebin=${dir/src/ebin}
    out="$ebin/${b%.erl}.core"
    if test -f $out; then
        # echo "$i skipped"
        continue
    fi
    echo $i
    mkdir -p $ebin
    # erlc $includes -o $ebin +to_core $i
done
