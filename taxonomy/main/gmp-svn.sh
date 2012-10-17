#!/bin/bash

revnum=$1

cd gmp
hg revert --all
hg up -C -r $revnum
cd ..