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
#include <set>
#include "headers.h"

using namespace std;

// Tranforming <memRatiosForNNStructs> from
// <memRatiosForNNStructs[i]=ratio of mem/total mem> to
// <memRatiosForNNStructs[i]=ratio of mem/mem left for structs i,i+1,...>.
void transformMemRatios(){
  RealT sum = 0;
  for(IntT i = nRadii - 1; i >= 0; i--){
    sum += memRatiosForNNStructs[i];
    memRatiosForNNStructs[i] = memRatiosForNNStructs[i] / sum;
    //DPRINTF("%0.6lf\n", memRatiosForNNStructs[i]);
  }
  ASSERT(sum <= 1.000001);
}

#define ENUM_BUCKETS

class PairComp {
public:
    bool operator () (pair<char *, int> lhs, pair<char *, int> rhs) const {
        int cmp = strcmp(lhs.first, rhs.first);
        if(cmp != 0) { return cmp < 0; }
        else { return lhs.second < rhs.second; }
    }
};

bool pointIsNotFiltered(PointT * bucketEle,PointT * queryPoint,set<pair<char*,int>,PairComp > templates,set<pair<char*,int>,PairComp > revs) {
    bool different_template = 
      (strcmp(bucketEle->cprop[ENUM_CPROP_BENCH],
              queryPoint->cprop[ENUM_CPROP_BENCH]) == 0) ||
      bucketEle->iprop[ENUM_PPROP_TID] != queryPoint->iprop[ENUM_PPROP_TID];
    pair<char*,int> tempPair = make_pair(bucketEle->cprop[ENUM_CPROP_BENCH],bucketEle->iprop[ENUM_PPROP_TID]);
    pair<char*,int> revPair = make_pair(bucketEle->cprop[ENUM_CPROP_BENCH],bucketEle->iprop[ENUM_PPROP_REVNUM]);
    bool not_seen_template = templates.count(tempPair) == 0;
      
    bool different_file = 
      (strcmp(bucketEle->cprop[ENUM_CPROP_BENCH],
              queryPoint->cprop[ENUM_CPROP_BENCH]) == 0) ||
      strcmp(bucketEle->cprop[ENUM_CPROP_FILE], queryPoint->cprop[ENUM_CPROP_FILE]) != 0;

    bool not_seen_rev = revs.count(revPair) == 0;

    if(aggressive_filter) {
        bool retval = different_template &&  not_seen_template && different_file && not_seen_rev;
/*        if(!retval) {
        printf("template1: %d, template2: %d, bench1: %s, bench2: %s, rev1: %d, rev2: %d\n",
               bucketEle->iprop[ENUM_PPROP_TID], queryPoint->iprop[ENUM_PPROP_TID],
               bucketEle->cprop[ENUM_CPROP_BENCH], queryPoint->cprop[ENUM_CPROP_BENCH],
               bucketEle->iprop[ENUM_PPROP_REVNUM], queryPoint->iprop[ENUM_PPROP_REVNUM]);
        if(!different_template) { printf("same template\n"); }
        if(!not_seen_template) { printf("seen template\n"); }
        if(!different_template) { printf("same benchmark\n"); }
        if(!not_seen_rev) { printf("seen rev\n"); }
        }*/
        return retval;
    } else {
      return different_template && not_seen_template;
    }
}

void computeParametersAndPrepare(bool computeParameters, char* paramsFile, PointT ** dataSetPoints, PointT ** sampleQueries) {
  
    if(!computeParameters) {
        computeParameters = readParamsFile(paramsFile,dataSetPoints);
    } 
    if (computeParameters) {
    Int32T sampleQBoundaryIndeces[nSampleQueries];
    // Compute the array sampleQBoundaryIndeces that specifies how to
    // segregate the sample query points according to their distance
    // to NN.
    sortQueryPointsByRadii(pointsDimension,
                           nSampleQueries,
                           sampleQueries,
                           nPoints,
                           dataSetPoints,
                           nRadii,
                           listOfRadii,
                           sampleQBoundaryIndeces);

    // Compute the R-NN DS parameters
    // if a parameter file is given, output them to that file, and continue
    // otherwise, output them to stdout, and exit

    FILE *fd;
    if (paramsFile == NULL) {
        fd = stdout;
    } else {
        fd = fopen(paramsFile, "wt");
        if (fd == NULL) {
            fprintf(stderr, "Unable to write to parameter file %s\n", paramsFile);
            exit(1);
        }
    }

    fprintf(fd, "%d\n", nRadii);
    transformMemRatios();
    
    for(IntT i = 0; i < nRadii; i++) {
      // which sample queries to use
        Int32T segregatedQStart = (i == 0) ? 0 : sampleQBoundaryIndeces[i - 1];
        Int32T segregatedQNumber = nSampleQueries - segregatedQStart;
        if (segregatedQNumber == 0) {
            // XXX: not the right answer
            segregatedQNumber = nSampleQueries;
            segregatedQStart = 0;
        }
        ASSERT(segregatedQStart < nSampleQueries);
        ASSERT(segregatedQStart >= 0);
        ASSERT(segregatedQStart + segregatedQNumber <= nSampleQueries);
        ASSERT(segregatedQNumber >= 0);
        RNNParametersT optParameters = computeOptimalParameters(listOfRadii[i],
                                                                successProbability,
                                                                nPoints,
                                                                pointsDimension,
                                                                dataSetPoints,
                                                                segregatedQNumber,
                                                                sampleQueries + segregatedQStart,
                                                                (Uns32T)((availableTotalMemory - totalAllocatedMemory) * memRatiosForNNStructs[i]));
        printRNNParameters(fd, optParameters);
    }
    if (fd == stdout) {
      exit(0);
    } else {
      fclose(fd);
      ASSERT(!readParamsFile(paramsFile,dataSetPoints));
    }

  }
  printf("========================= Structure built =========================\n");
  printf("nPoints = %d, Dimension = %d\n", nPoints, pointsDimension);
  printf("lowerBound = %d, upperBound = %d\n", lowerBound, upperBound);

}


class PointComp {
public:
    bool operator () (PointT lhs, PointT rhs) 
    { return lhs.index < rhs.index; }
};

class TResultEle {
public:
    int templateID;
    set<PointT,PointComp> * queryPoints;
    set<PointT,PointComp> * neighbors;
    TResultEle * next;
    TResultEle * prev;

    TResultEle(int tid) : templateID(tid), next(NULL), prev(NULL) {
        queryPoints = new set<PointT,PointComp>();
        neighbors = new set<PointT, PointComp>();
    }
};

int compare_for_template(const void *p1, const void *p2) {
  PointT * a = (PointT *)p1;
  PointT * b = (PointT *)p2;
  int c = a->iprop[ENUM_PPROP_REVNUM] - b->iprop[ENUM_PPROP_REVNUM]; 
  if (c) {
    return c;
  } else {
      return a->iprop[ENUM_PPROP_TID] - b->iprop[ENUM_PPROP_TID];
  }

}

pair<TResultEle *, TResultEle*> insertQueryBucket(PointT * queryPoint, TResultEle * buckets) {
    TResultEle * currentResult = NULL, *walker = buckets;

    while(walker != NULL && walker->templateID < queryPoint->iprop[ENUM_PPROP_TID]) {
        walker = walker->next;
    } 
    if(walker == NULL || walker->templateID != queryPoint->iprop[ENUM_PPROP_TID]) {
        currentResult = new TResultEle(queryPoint->iprop[ENUM_PPROP_TID]); 
        if(buckets == NULL) {
            buckets = currentResult;
            currentResult->next = NULL;
        } else {
            if(walker == NULL) { // insert at the end.  Annoying.
                walker=buckets;
                while(walker->next != NULL) walker=walker->next;
                walker->next = currentResult;
                currentResult->prev = walker;
                currentResult->next = NULL;
            } else {
                // walker points to the thing we should insert before
                ASSERT(walker != NULL);
                if(walker->prev == NULL) {
                    buckets = currentResult;
                }
                currentResult->next = walker;
                currentResult->prev = walker->prev;
                walker->prev = currentResult;
            }
        }
    } else {
        ASSERT(walker->templateID == queryPoint->iprop[ENUM_PPROP_TID]);
        currentResult = walker;
    }
    currentResult->queryPoints->insert(*queryPoint);
    return make_pair(buckets,currentResult);
}

int print_bucket(PointT * begin, PointT * cur, PointT * queryPoint, int nBucketedPoints) {
    set<pair<char*,int >, PairComp > templatesSeen;
    set<pair<char*, int>, PairComp > revsSeen;
    templatesSeen.insert(make_pair(begin->cprop[ENUM_CPROP_BENCH],begin->iprop[ENUM_PPROP_TID]));
    revsSeen.insert(make_pair(begin->cprop[ENUM_CPROP_BENCH],begin->iprop[ENUM_PPROP_REVNUM]));

    for (PointT * p = begin; p < cur; p++)  {
        ASSERT(p != NULL);
        nBucketedPoints++;
        if(pointIsNotFiltered(p,queryPoint,templatesSeen,revsSeen)) {
            templatesSeen.insert(make_pair(p->cprop[ENUM_CPROP_BENCH],p->iprop[ENUM_PPROP_TID]));
            revsSeen.insert(make_pair(p->cprop[ENUM_CPROP_BENCH],p->iprop[ENUM_PPROP_REVNUM]));
            printf("%05d\tdist:%0.1lf \t BENCH: %s \tTID:%d\tFILE %s\tREVNUM: %d\tMSG:%s\n", 
                   p->index, sqrt(p->distance),
                   p->cprop[ENUM_CPROP_BENCH],
                   p->iprop[ENUM_PPROP_TID],
                   p->cprop[ENUM_CPROP_FILE],
                   p->iprop[ENUM_PPROP_REVNUM],
                   p->cprop[ENUM_CPROP_MSG]);
        }
    }
    return nBucketedPoints;
}

void print_groups(TResultEle * buckets) {
  TResultEle * walker = buckets;
  while(walker != NULL) {
      printf("\nTemplate %d: ", walker->templateID);
      printf("Indicative Query Point: ");
      PointT indicativePoint = *(walker->queryPoints->begin());
      printPoint(&indicativePoint);
      printf("%d Neighbors:\n", walker->neighbors->size());
      set<PointT, PointComp>::iterator it = walker->neighbors->begin();
      for(; it != walker->neighbors->end(); it++) {
          PointT blah = *it; // C++ is the dumbest thing ever. 
          printf("TID:%d\tdist:%0.1lf \t BENCH: %s \t FILE %s\tREVNUM: %d\tMSG:%s\n", 
                 blah.iprop[ENUM_PPROP_TID],
                 sqrt(blah.distance),
                 blah.cprop[ENUM_CPROP_BENCH],
                 blah.cprop[ENUM_CPROP_FILE],
                 blah.iprop[ENUM_PPROP_REVNUM],
                 blah.cprop[ENUM_CPROP_MSG]);
      }
      walker = walker->next;
  }
}

void computeVectorClusters(PointT ** dataSetPoints, bool group) {

  // output vector clusters according to the filtering parameters.
    TimeVarT meanQueryTime = 0;
    int nBuckets = 0, nBucketedPoints = 0, nQueries = 0;
    TResultEle * buckets = NULL, *currentResult = NULL;
    PointT *result = (PointT *)MALLOC(nPoints * sizeof(PointT));
    bool seen[nPoints];

    memset(seen, 0, nPoints * sizeof(bool));

    for(IntT i = 0; i < nPoints; nQueries++, i++) {
        // find the next unseen point
        while (i < nPoints && seen[i]) i++;
        if (i >= nPoints) break;
        PointT * queryPoint = dataSetPoints[i];
        if(group) {
            pair<TResultEle *, TResultEle *> retval = insertQueryBucket(queryPoint, buckets);
            buckets = retval.first;
            currentResult = retval.second;
        }
        // get the near neighbors.
        IntT nNNs = getRNearNeighbors(nnStructs[0], queryPoint, result, nPoints);
        meanQueryTime += timeRNNQuery;

        qsort(result, nNNs, sizeof(*result), comparePoints);
        PointT * cur = result+ 1, *end = result + nNNs, *begin = result;
        set<pair<char*,int >, PairComp > templatesSeen;
        set<pair<char*, int>, PairComp > revsSeen;
        bool printed = false;
        templatesSeen.insert(make_pair(begin->cprop[ENUM_CPROP_BENCH],begin->iprop[ENUM_PPROP_TID]));
        revsSeen.insert(make_pair(begin->cprop[ENUM_CPROP_BENCH],begin->iprop[ENUM_PPROP_REVNUM]));

        int sizeBucket = 0; // the first un-filtered point, which excludes the query point

        seen[begin->index] = true;
        while (cur < end) {
            ASSERT(cur != NULL);
            if ( pointIsNotFiltered(cur,queryPoint,templatesSeen,revsSeen) ) {
                templatesSeen.insert(make_pair(cur->cprop[ENUM_CPROP_BENCH],cur->iprop[ENUM_PPROP_TID]));
                revsSeen.insert(make_pair(cur->cprop[ENUM_CPROP_BENCH],cur->iprop[ENUM_PPROP_REVNUM]));
                if(group) currentResult->neighbors->insert(*cur);
                sizeBucket++;
            }
            seen[cur->index] = true;
            cur++;
        }
        if(!group && sizeBucket >= lowerBound && (upperBound < lowerBound || sizeBucket <= upperBound)) {
            nBuckets++;
            printf("\nQuery point %d: ", i);
            printPoint(queryPoint);
            printf("Bucket size %d, found %d NNs at distance %0.6lf (radius no. %d). NNs are:\n",
                   sizeBucket, nNNs, (double)(listOfRadii[0]), 0);
            nBucketedPoints = print_bucket(begin,end,queryPoint,nBucketedPoints);
        }
    }
    if(group) print_groups(buckets);

    // Simple statistics and finish
    if (nQueries > 0) {
        meanQueryTime = meanQueryTime / nQueries;
        printf("\n%d queries, Mean query time: %0.6lf\n", nQueries, (double)meanQueryTime);
        printf("%d buckets, %d points (out of %d, %.2f %%) in them\n",
               nBuckets, nBucketedPoints, nPoints, 100*(float)nBucketedPoints/(float)nPoints);
    } 
}


void clusterOverTime(PointT ** dataSetPoints) {

    PointT * result = (PointT *)MALLOC(nPoints * sizeof(PointT));

    IntT i = 0;
    while(i < nPoints) {
        int currRevision = dataSetPoints[i]->iprop[ENUM_PPROP_REVNUM]; 
        printf("Revision: %d\n", currRevision);
        while(i < nPoints && dataSetPoints[i]->iprop[ENUM_PPROP_REVNUM] == currRevision) {
            int currTemplate = dataSetPoints[i]->iprop[ENUM_PPROP_TID];

            TResultEle * currentResult = new TResultEle(currTemplate);
            
            set<pair<char*,int >, PairComp > templatesSeen;
            set<pair<char*, int>, PairComp > revsSeen;
            IntT j = i;
            for(j = i; j < nPoints && dataSetPoints[j]->iprop[ENUM_PPROP_TID] == currTemplate; j++) {
                PointT * queryPoint = dataSetPoints[j];
                currentResult->queryPoints->insert(*queryPoint);
                IntT nNNs = getRNearNeighbors(nnStructs[0], queryPoint, result, nPoints);
                qsort(result, nNNs, sizeof(*result), comparePoints);
                PointT * cur = result, *end = result + nNNs;
                while(cur < end && cur->iprop[ENUM_PPROP_REVNUM] < currRevision) {
                    ASSERT(cur != NULL);
                    if ( pointIsNotFiltered(cur,queryPoint,templatesSeen,revsSeen) ) {
                        templatesSeen.insert(make_pair(cur->cprop[ENUM_CPROP_BENCH],cur->iprop[ENUM_PPROP_TID]));
                        revsSeen.insert(make_pair(cur->cprop[ENUM_CPROP_BENCH],cur->iprop[ENUM_PPROP_REVNUM]));
                        currentResult->neighbors->insert(*cur);
                    }
                    cur++;
                }
            }
            printf("\tTemplate: %d, indicative query point: ", currTemplate);
            PointT indicativePoint = *(currentResult->queryPoints->begin());
            printPoint(&indicativePoint);
            printf("\t\t%d Neighbors:\n", currentResult->neighbors->size());
            set<PointT, PointComp>::iterator it = currentResult->neighbors->begin();
            for(; it != currentResult->neighbors->end(); it++) {
                PointT blah = *it; // C++ is the dumbest thing ever. 
                printf("\t\t\tTID:%d\tdist:%0.1lf \t BENCH: %s \t FILE %s\tREVNUM: %d\tMSG:%s\n", 
                       blah.iprop[ENUM_PPROP_TID],
                       sqrt(blah.distance),
                       blah.cprop[ENUM_CPROP_BENCH],
                       blah.cprop[ENUM_CPROP_FILE],
                       blah.iprop[ENUM_PPROP_REVNUM],
                       blah.cprop[ENUM_CPROP_MSG]);
            }
            i = j;
        }
    }
}


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
        sampleQueries = readDataSetFromFile(queryFname,NULL,false,0);
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

  //initializeLSHGlobal();
  availableTotalMemory = (unsigned int)8e8;  // 800MB by default

  // Parse the command-line parameters.
  bool computeParameters = false, group = false, do_time_exp = false, double_format = false;
  char *paramsFile=NULL, *dataFile= NULL, *queryFile = NULL, *vec_files = NULL;
  int reduce = 0; 

  srand(time(NULL));
  printf("DiffBuckets run with args: ");
  for(int i = 0; i < argc; i++) {
      printf("%s ", argv[i]);
  }
  printf("\n"); fflush(stdout);
  for (int opt; (opt = getopt(argc, argv, "tr:al:gs:q:p:P:R:cf:")) != -1; ) {
    // Needed: -p -f -R
    switch (opt) {
      case 'd':
        double_format = true;
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
  PointT ** sampleQueries = generateSampleQueries(dataSet, queryFile); 

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

  computeParametersAndPrepare(computeParameters,paramsFile,dataSet,sampleQueries);
  printf("after compute parameters and prepare\n");
  if(do_time_exp) {
      clusterOverTime(dataSet);
  } else 
    computeVectorClusters(dataSet, group);
  return 0;
}
