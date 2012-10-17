#!/bin/bash

revnum=$1

cd wireshark
svn revert -R *
svn up --force -r $revnum
cd ..