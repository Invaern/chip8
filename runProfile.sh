#!/usr/bin/env sh
stack --profile --work-dir .stack-work-profiling exec -- chip8-exe +RTS -P -l -L50 -hc -sstderr