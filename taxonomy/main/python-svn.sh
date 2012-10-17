#!/bin/bash

revnum=$1

cd python
hg revert --all
hg up -C -r $revnum
cd ..