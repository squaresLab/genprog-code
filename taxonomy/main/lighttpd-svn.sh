#!/bin/bash

revnum=$1

cd lighttpd
svn revert -R *
svn up --force -r $revnum
cd ..