#!/bin/bash

hprotoc="$(which hprotoc)"

if [[ -z "$hprotoc" ]]; then
    echo "error: can't continue without hprotoc" 1>&2
    echo "to fix:" 1>&2
    echo 1>&2
    echo "cabal install hprotoc" 1>&2
    exit 1
fi

sed -e 's/Rpb//g' \
    -e 's/Req\>/Request/g' \
    -e 's/Resp\>/Response/g' \
    -e 's/MapRedR/MapReduceR/g' \
    -e 's/DelR/DeleteR/' \
    -e 's/ClientId/ClientID/' \
    -e 's/[dD]oc/Document/g' \
    -e 's/import "riak.proto";//g' \
    -e 's/option java.*$//g' \
    -e 's/_\([a-z]\)/\u\1/g' \
    riak_pb/src/riak.proto \
    riak_pb/src/riak_kv.proto \
    riak_pb/src/riak_search.proto > ./Protocol.proto

hprotoc -p Database.Riak ./Protocol.proto
# rm Protocol.proto