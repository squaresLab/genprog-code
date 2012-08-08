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
#ifndef GEOMETRY_INCLUDED
#define GEOMETRY_INCLUDED

// CLG: somehow this header turned into a set of type definitions.  
#include<map>
#include<set>
using namespace std;


/* properties of a point */
typedef enum {
  ENUM_CPROP_FILE, 
  ENUM_CPROP_MSG,
  ENUM_CPROP_BENCH,
  ENUM_CPROP_LAST_NOT_USED,
  ENUM_IPROP_TID,
  ENUM_IPROP_REVNUM,
  ENUM_IPROP_LINESTART,
  ENUM_IPROP_LINEEND,
  ENUM_IPROP_TYPE,
  ENUM_IPROP_LAST_NOT_USED
} reg_prop_t;

typedef enum {
  ENUM_PPROP_TID,
  ENUM_PPROP_REVNUM,
  ENUM_PPROP_LINESTART,
  ENUM_PPROP_LINEEND,
  ENUM_PPROP_TYPE,
  ENUM_PPROP_LAST_NOT_USED
} pprop_t;

typedef enum {
    ENUM_CONTEXT,
    ENUM_CHANGE,
    ENUM_UNUSED
} type_t;

// A simple point in d-dimensional space. A point is defined by a
// vector of coordinates. 

class PointT {

public:
    PointT(int dimension) : dimension(dimension) {
        coordinates = (RealT*) malloc(dimension * sizeof(RealT));
    }
    IntT index; // the index of this point in the dataset list of points
    IntT dimension;
    RealT sqrLength;
    RealT *coordinates;
    char * cprop[ENUM_CPROP_LAST_NOT_USED];
    RealT distance;
    int iprop[ENUM_IPROP_LAST_NOT_USED - ENUM_CPROP_LAST_NOT_USED];
};


class PointComp {
public:
    bool operator () (PointT * lhs, PointT * rhs) 
    { return lhs->index < rhs->index; }
};

typedef set<PointT *, PointComp> PointSet;
typedef map<int, PointSet> PointMap;

class TResultEle {
public:
    int templateID;
    PointSet * queryPoints;
    PointSet * neighbors;
    TResultEle * next;
    TResultEle * prev;

    TResultEle(int tid) : templateID(tid), next(NULL), prev(NULL) {
        queryPoints = new PointSet();
        neighbors = new PointSet();
    }
};

typedef pair<char *, int> SimplePair;
typedef pair<PointT **, int> ListPair;

class PairComp {
public:
    bool operator () (SimplePair lhs, SimplePair rhs) const {
        int cmp = strcmp(lhs.first, rhs.first);
        if(cmp != 0) { return cmp < 0; }
        else { return lhs.second < rhs.second; }
    }
};

typedef set<SimplePair,PairComp> PairSet;

// feels like this doesn't belong here, but I can't figure out where else to put it.
bool pointIsNotFiltered(PointT * bucketEle,PointT * queryPoint,PairSet templates,PairSet revs);


RealT distance(IntT dimension, PointT * p1, PointT * p2);
int comparePoints(const void *p1, const void *p2);
int compareForTemplate(const void *p1, const void *p2);
void printPoint(PointT * point);
int printBucket(PointT * begin, PointT * cur, PointT * queryPoint, int nBucketedPoints);
void printGroup(TResultEle * walker);
void printGroups(TResultEle * buckets);

class configT {
public:
    bool computeParameters, group, do_time_exp, filtering, simple;
    type_t filterType;
    int reduce;

    configT() 
        : computeParameters(false), group(false), do_time_exp(false), 
          filtering(false), filterType(ENUM_UNUSED), reduce(0), simple(true)
    { } 
};

class dataT {
private: 

    void makeMapsFromDataSet();
    ListPair * separatePoints(PointMap mymap);

public:
    PointT **dataSetPoints[ENUM_UNUSED], **sampleQueries[ENUM_UNUSED];
    int nPoints[ENUM_UNUSED], nSampleQueries, nRadii, nTypes, pointsDimension[ENUM_UNUSED];
    RealT ** listOfRadii;
    PointMap maps[ENUM_UNUSED];

    void initComplex();
    void setQueries(ListPair * sqInfo);
    dataT(int nt, int nr, int np, int sq, PointT ** initialData);

};

#endif
