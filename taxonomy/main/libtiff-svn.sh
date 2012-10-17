#!/bin/bash

revnum=$1

cd libtiff
git checkout -f $revnum
cd ..