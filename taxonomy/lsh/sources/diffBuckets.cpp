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

ListPair * generateSampleQueries(dataT * data, char * queryFname) { 
    ListPair * retval = (ListPair *) MALLOC(data->nTypes * sizeof(ListPair)); 

    if (queryFname == NULL){
        // Choose several data set points for the sample query points.
        for(IntT type_index = 0; type_index < data->nTypes; type_index++) {
            PointT ** sampleQueries = (PointT **)MALLOC(data->nSampleQueries * sizeof(PointT *));
            for(IntT query = 0; query < data->nSampleQueries; query++) {
                sampleQueries[query] = data->dataSetPoints[type_index][genRandomInt(0, data->nPoints[type_index] - 1)];
            }
            retval[type_index] = make_pair(sampleQueries, data->nSampleQueries);
        }
    } else {
        FILE *queryFile = fopen(queryFname, "rt");
        retval[0] = readDataSetFromFile(queryFname,NULL,0);
        fclose(queryFile);
    }
    return retval;
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
  configT * params = new configT();

  bool simple=true;
  char *paramsFile=NULL, *dataFile= NULL, *queryFile = NULL, *vec_files = NULL;
  int nSampleQueries = 100;

  set<double> radii;

  srand(time(NULL));
  printf("DiffBuckets run with args: ");
  for(int i = 0; i < argc; i++) {
      printf("%s ", argv[i]);
  }
  printf("\n"); fflush(stdout);
  for (int opt; (opt = getopt(argc, argv, "bx:tr:al:gs:q:p:P:R:cf:")) != -1; ) {
    // Needed: -p -f -R
    switch (opt) {
      case 'b':
        simple = false;
        break;
      case 'x':
        params->filtering = true;
        params->filterType = optarg;
        break;
      case 't': 
        params->do_time_exp = true;
        break;
      case 'r': // reduce the data set size to something reasonable.  This is a percentage out of 100
        params->reduce = atoi(optarg);
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
        params->computeParameters = true;
        break;
      case 'R':
        radii.insert(strtod(optarg, NULL));
        break;
      case 'f':
        printf("reading from file: %s\n", optarg);
        dataFile = optarg;
        break;
      case 'g': // group output by template
        params->group = true;
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

  ListPair retpair = readDataSetFromFile(dataFile,vec_files,params->reduce);
// sanity checks

  printf("number of points: %d\n", retpair.second);
  DPRINTF("Allocated memory (after reading data set): %d\n", totalAllocatedMemory);
  CHECK_INT(availableTotalMemory);
  CHECK_INT(retpair.second);
  CHECK_INT(radii.size());
  if (retpair.second > MAX_N_POINTS) {
      printf("Error: the structure supports at most %d points (%d were specified).\n", MAX_N_POINTS, retpair.second);
      fprintf(ERROR_OUTPUT, "Error: the structure supports at most %d points (%d were specified).\n", MAX_N_POINTS, retpair.second);
      return 1;
  }
  fflush(stdout);
// initialize dataT and related structures

  dataT * data = new dataT(simple ? 1 : 2,radii.size(), retpair.second,  nSampleQueries, retpair.first);
  if(!simple) data->initComplex();

  data->setQueries(generateSampleQueries(data,queryFile));

  memRatiosForNNStructs = (RealT **) MALLOC(data->nTypes * sizeof(RealT*));
  for(IntT i = 0; i < data->nTypes; i ++) {
      memRatiosForNNStructs[i] = (RealT*) MALLOC(radii.size() * sizeof(RealT));
  }

  printf("Computing parameters\n"); fflush(stdout);
  computeParameters(params,data,radii,paramsFile);
  printf("Done computing parameters\n"); fflush(stdout);
  
  if(simple)
    simpleBuckets(params,data);
  else
    complexBuckets(data);
  return 0;
}
