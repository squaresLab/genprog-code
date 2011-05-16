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
 * Modified by: Stephane Glondu (stephane.glondu@dptinfo.ens-cachan.fr)
 */

/*
  The main entry file containing the main() function. The main()
  function parses the command line parameters and depending on them
  calls the corresponding functions.
 */

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <utility>
#include <unistd.h>
#include <time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <ctype.h>
#include <regex.h>
#include "headers.h"

using namespace std;

#define ENUM_BUCKETS

PointT ** generateSampleQueries(PointT ** dataSetPoints, char * queryFname) {
    PointT ** sampleQueries;
    printf("nSampleQueries: %d\n", nSampleQueries);
    FAILIF(NULL == (sampleQueries = (PointT **)MALLOC(nSampleQueries * sizeof(PointT *))));

    if (queryFname == NULL){
        // Choose several data set points for the sample query points.
        for(IntT i = 0; i < nSampleQueries; i++){
            sampleQueries[i] = dataSetPoints[genRandomInt(0, nPoints - 1)];
        }
    } else {
        FILE *queryFile = fopen(queryFname, "rt");
        sampleQueries = readDataSetFromFile(queryFname,NULL,0,true);
    }
    return sampleQueries;

}

/*
  The main entry to LSH package. Depending on the command line
  parameters, the function computes the R-NN data structure optimal
  parameters and/or construct the R-NN data structure and runs the
  queries on the data structure.
 */
int main(int argc, char *argv[]){

  FAILIF(0 != regcomp(&preg[ENUM_CPROP_FILE], "FILE:([^,]+)", REG_EXTENDED));
  FAILIF(0 != regcomp(&preg[ENUM_CPROP_MSG], "MSG:(\\{[^}]+\\})", REG_EXTENDED));
  FAILIF(0 != regcomp(&preg[ENUM_CPROP_BENCH], "BENCH:([^,]+)", REG_EXTENDED));
  FAILIF(0 != regcomp(&preg[ENUM_IPROP_TID], "TEMPLATEID:([^,]+)", REG_EXTENDED));
  FAILIF(0 != regcomp(&preg[ENUM_IPROP_REVNUM], "REVNUM:([^,]+)", REG_EXTENDED));
  FAILIF(0 != regcomp(&preg[ENUM_IPROP_LINESTART], "LINESTART:([^,]+)", REG_EXTENDED));
  FAILIF(0 != regcomp(&preg[ENUM_IPROP_LINEEND], "LINEEND:([^,]+)", REG_EXTENDED));
  FAILIF(0 != regcomp(&preg[ENUM_CPROP_TYPE], "TYPE:([^,]+)", REG_EXTENDED));

  //initializeLSHGlobal();
  availableTotalMemory = (unsigned int)8e8;  // 800MB by default

  // Parse the command-line parameters.
  bool computeParameters = false, group = false, do_time_exp = false, simple=true;
  char *paramsFile=NULL, *dataFile= NULL, *queryFile = NULL, *vec_files = NULL;
  int reduce = 0; 

  srand(time(NULL));
  printf("DiffBuckets run with args: ");
  for(int i = 0; i < argc; i++) {
      printf("%s ", argv[i]);
  }
  printf("\n"); fflush(stdout);
  for (int opt; (opt = getopt(argc, argv, "x:tr:al:gs:q:p:P:R:cf:")) != -1; ) {
    // Needed: -p -f -R
    switch (opt) {
      case 'x':
        filtering = true;
        filterType = optarg;
        break;
      case 't': 
        do_time_exp = true;
        break;
      case 'r': // reduce the data set size to something reasonable.  This is a percentage out of 100
        reduce = atoi(optarg);
        break;
      case 'a': aggressive_filter = true; 
        break;
      case 'l': vec_files = optarg;
        break;
      case 's': nSampleQueries = atoi(optarg); 
        break;
      case 'q': queryFile = optarg; break;
      case 'p': paramsFile = optarg; break;
      case 'P': successProbability = atof(optarg); break;
      case 'c':
        fprintf(stderr, "Warning: will compute parameters\n");
        computeParameters = true;
        break;
      case 'R':
        nRadii = 1;
        FAILIF(NULL == (listOfRadii = (RealT*)MALLOC(nRadii * sizeof(RealT))));
        FAILIF(NULL == (memRatiosForNNStructs = (RealT*)MALLOC(nRadii * sizeof(RealT))));
        listOfRadii[0] = strtod(optarg, NULL);
        memRatiosForNNStructs[0] = 1;
        break;
      case 'f':
        printf("reading from file: %s\n", optarg);
        dataFile = optarg;
        break;
      case 'g': // group output by template
        group = true;
        break;
      default:
        fprintf(stderr, "Unknown option: -%c\n", opt);
        usage(1, argv[0]);
    }
  }

  if (optind < argc) {
    fprintf(stderr, "There are unprocessed parameters left\n");
    usage(1, argv[0]);
  }

      PointT ** dataSet = readDataSetFromFile(dataFile,vec_files,reduce,true);
      printf("number of points: %d\n", nPoints);
      
      DPRINTF("Allocated memory (after reading data set): %d\n", totalAllocatedMemory);
      CHECK_INT(availableTotalMemory);
      CHECK_INT(nPoints);
      CHECK_INT(pointsDimension);
      CHECK_INT(nRadii);
      
      if (nPoints > MAX_N_POINTS) {
          printf("Error: the structure supports at most %d points (%d were specified).\n", MAX_N_POINTS, nPoints);
          fprintf(ERROR_OUTPUT, "Error: the structure supports at most %d points (%d were specified).\n", MAX_N_POINTS, nPoints);
          return 1;
      }
      if(simple) {
          PointT ** sampleQueries = generateSampleQueries(dataSet, queryFile); 
          simpleBuckets(computeParameters,group,do_time_exp,paramsFile,dataSet,sampleQueries);
      }
      else {  
/*          complexBuckets(dataSet);
          pair<PointMap,PointMap> maps = makeMapsFromDataSet(dataSet);
          PointMap context_map = maps.first;
          PointMap change_map = maps.second;*/

      }
  return 0;
}
