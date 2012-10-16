#!/bin/bash

revnum=$1

cd php
svn revert -R *
svn up --force -r $revnum
cd ..