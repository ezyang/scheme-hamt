#!/bin/bash
if [ -z "$1" ]
then
    echo "Usage: $0 OUT.csv"
    exit 1
fi
mit-scheme < compile.scm
(mit-scheme --stack 1000 --heap 1000 --load hamt prb-tree perf-test | tee "$1")</dev/null
