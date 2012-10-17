#!/bin/bash

revnum=$1

cd gzip
git checkout -f $revnum
cd ..