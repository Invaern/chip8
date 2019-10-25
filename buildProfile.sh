#!/usr/bin/env sh
stack build --profile --work-dir .stack-work-profiling --ghc-options "-fprof-auto -fprof-cafs -O2"