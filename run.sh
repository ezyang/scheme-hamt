#!/bin/bash
mit-scheme < compile.scm
(mit-scheme --load hamt prb-tree perf-test | tee "$1")</dev/null
