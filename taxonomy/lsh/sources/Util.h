/*
 * Copyright (c) 2004-2005 Massachusetts Institute of Technology.
 * All Rights Reserved.
 *
 * MIT grants permission to use, copy, modify, and distribute this software and
 * its documentation for NON-COMMERCIAL purposes and without fee, provided that
 * this copyright notice appears in all copies.
 *
 * MIT provides this software "as is," without representations or warranties of
 * any kind, either expressed or implied, including but not limited to the
 * implied warranties of merchantability, fitness for a particular purpose, and
 * noninfringement.  MIT shall not be liable for any damages arising from any
 * use of this software.
 *
 * Author: Alexandr Andoni (andoni@mit.edu), Piotr Indyk (indyk@mit.edu)
 */
/* file reading, comparisons, and other basic utils */

#ifndef UTIL_INCLUDED
#define UTIL_INCLUDED


#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <unistd.h>
#include <sys/times.h>
#include <sys/types.h>
#include <ctype.h>
#include <regex.h>
#include <string.h>
#include "BasicDefinitions.h"

using namespace std;
/* input file processing */
void usage (int code, char * programName);
char * str2CharStar(string line); 
PPointT readPoint(char * line, char * comment); 
PPointT * readDataSetFromFile(char * filename, char * vec_files, bool sampleData); 
bool readParamsFile(char * paramsFile, PPointT * dataSetPoints);

/* comparison and checks */
int compareInt32T(const void *a, const void *b); 


#define CHECK_INT(v) { \
  if (v <= 0) { \
    fprintf(stderr, "Incorrect integer value for variable %s\n", #v); \
    usage(1, argv[0]); \
  }}
#define CHECK_FLOAT(v) { \
  if (v < 1e-3) { \
    fprintf(stderr, "Incorrect float value for variable %s\n", #v); \
    usage(1, argv[0]); \
  }}

#endif
