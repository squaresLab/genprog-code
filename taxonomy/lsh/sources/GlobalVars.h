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

#ifndef GLOBALVARS_INCLUDED
#define GLOBALVARS_INCLUDED

#ifndef GLOBALVARS_CPP
#define DECLARE_EXTERN extern
#define EXTERN_INIT(x)
#else
#define DECLARE_EXTERN
#define EXTERN_INIT(x) x
#endif

DECLARE_EXTERN Uns32T availableTotalMemory EXTERN_INIT(= DEFAULT_MEMORY_MAX_AVAILABLE);

DECLARE_EXTERN TimeVarT timeComputeULSH;
DECLARE_EXTERN TimeVarT timeGetBucket;
DECLARE_EXTERN TimeVarT timeCycleBucket;
DECLARE_EXTERN TimeVarT timeDistanceComputation;
DECLARE_EXTERN TimeVarT timeResultStoring;
DECLARE_EXTERN TimeVarT timePrecomputeHash;
DECLARE_EXTERN TimeVarT timeGBHash;
DECLARE_EXTERN TimeVarT timeChainTraversal;
DECLARE_EXTERN TimeVarT timeBucketCreation;
DECLARE_EXTERN TimeVarT timeBucketIntoUH;
DECLARE_EXTERN TimeVarT timeCycleProc;
DECLARE_EXTERN TimeVarT timeRNNQuery;
DECLARE_EXTERN TimeVarT timeCopyingULSHs;
DECLARE_EXTERN TimeVarT timeTotalBuckets;
DECLARE_EXTERN TimeVarT timeUnmarking;

DECLARE_EXTERN BooleanT timingOn EXTERN_INIT(= TRUE);
DECLARE_EXTERN TimeVarT currentTime EXTERN_INIT(= 0);
DECLARE_EXTERN TimeVarT timevSpeed EXTERN_INIT(= 0);

DECLARE_EXTERN IntT nOfDistComps EXTERN_INIT(= 0);
DECLARE_EXTERN IntT totalAllocatedMemory EXTERN_INIT(= 0);
DECLARE_EXTERN IntT nGBuckets EXTERN_INIT(= 0);
DECLARE_EXTERN IntT nBucketsInChains EXTERN_INIT(= 0);
//DECLARE_EXTERN IntT nPointsInBuckets EXTERN_INIT(= 0); // total # of points found in collinding buckets (including repetitions)
DECLARE_EXTERN IntT nAllocatedGBuckets EXTERN_INIT(= 0);
DECLARE_EXTERN IntT nAllocatedBEntries EXTERN_INIT(= 0);

DECLARE_EXTERN BooleanT noExpensiveTiming  EXTERN_INIT(= FALSE);

#define N_SAMPLE_QUERY_POINTS 100


// GLOBAL VARIABLES 

DECLARE_EXTERN bool filtering EXTERN_INIT( = false);
DECLARE_EXTERN char * filterType EXTERN_INIT ( = ""); 
DECLARE_EXTERN bool aggressive_filter EXTERN_INIT( = false);
// Number of points in the data set.

DECLARE_EXTERN IntT nPoints EXTERN_INIT( = 0);
DECLARE_EXTERN Int32T nSampleQueries EXTERN_INIT(= N_SAMPLE_QUERY_POINTS);
DECLARE_EXTERN IntT pointsDimension EXTERN_INIT(= 0);
DECLARE_EXTERN int upperBound EXTERN_INIT(= 0);
DECLARE_EXTERN int lowerBound EXTERN_INIT(= 1);

// The success probability of each point (each near neighbor is
// reported by the algorithm with probability <successProbability>).
DECLARE_EXTERN RealT successProbability EXTERN_INIT(= 0.95);

// Same as <thresholdR>, only an array of R's (for the case when
// multiple R's are specified).
DECLARE_EXTERN RealT * listOfRadii EXTERN_INIT(= NULL);
DECLARE_EXTERN IntT nRadii EXTERN_INIT(= 0);

DECLARE_EXTERN RealT *memRatiosForNNStructs EXTERN_INIT (= NULL);

DECLARE_EXTERN regex_t preg[ENUM_IPROP_LAST_NOT_USED];

// Linked list structure for PointT
typedef struct TPointTList_s {
  PointT * hd;
  TPointTList_s *tl;
} TPointTList;

DECLARE_EXTERN RNNParametersT *algParameters EXTERN_INIT (= NULL);
DECLARE_EXTERN PRNearNeighborStructT *nnStructs EXTERN_INIT(= NULL);

#endif
