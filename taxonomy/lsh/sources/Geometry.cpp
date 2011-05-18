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

#include "headers.h"


bool pointIsNotFiltered(PointT * bucketEle,PointT * queryPoint,PairSet templates,PairSet revs) {
    bool different_template = 
      (strcmp(bucketEle->cprop[ENUM_CPROP_BENCH],
              queryPoint->cprop[ENUM_CPROP_BENCH]) == 0) ||
      bucketEle->iprop[ENUM_PPROP_TID] != queryPoint->iprop[ENUM_PPROP_TID];
    SimplePair tempPair = make_pair(bucketEle->cprop[ENUM_CPROP_BENCH],bucketEle->iprop[ENUM_PPROP_TID]);
    SimplePair revPair = make_pair(bucketEle->cprop[ENUM_CPROP_BENCH],bucketEle->iprop[ENUM_PPROP_REVNUM]);
    bool not_seen_template = templates.count(tempPair) == 0;
      
    bool different_file = 
      (strcmp(bucketEle->cprop[ENUM_CPROP_BENCH],
              queryPoint->cprop[ENUM_CPROP_BENCH]) == 0) ||
      strcmp(bucketEle->cprop[ENUM_CPROP_FILE], queryPoint->cprop[ENUM_CPROP_FILE]) != 0;

    bool not_seen_rev = revs.count(revPair) == 0;

    if(aggressive_filter) {
        return different_template && not_seen_template && different_file && not_seen_rev;
    } else {
      return different_template && not_seen_template;
    }
}

// Returns the Euclidean distance from point <p1> to <p2>.
RealT distance(IntT dimension, PointT * p1, PointT * p2){
  RealT result = 0;

  for (IntT i = 0; i < dimension; i++){
    result += SQR(p1->coordinates[i] - p2->coordinates[i]);
  }

  return SQRT(result);
}


int comparePoints(const void *p1, const void *p2)
{
    PointT * a = (PointT *) p1;
    PointT * b = (PointT *)p2;
    int c = strcmp(a->cprop[ENUM_CPROP_FILE], b->cprop[ENUM_CPROP_FILE]);
    if (c) return c;
    else {
        c =  a->iprop[ENUM_PPROP_TID] - b->iprop[ENUM_PPROP_TID];
        if(c) return c;
        else return (int) (a->distance - b->distance);
    }
}

int compareForTemplate(const void *p1, const void *p2) {
  PointT * a = (PointT *)p1;
  PointT * b = (PointT *)p2;
  int c = a->iprop[ENUM_PPROP_REVNUM] - b->iprop[ENUM_PPROP_REVNUM]; 
  if (c) {
    return c;
  } else {
      return a->iprop[ENUM_PPROP_TID] - b->iprop[ENUM_PPROP_TID];
  }

}


void printPoint(PointT * point) {
    printf("Point index: %05d\t TID:%d\tBENCH: %s \tFILE %s\tREVNUM: %d\tMSG:%s\n",
           point->index, 
           point->iprop[ENUM_PPROP_TID],
           point->cprop[ENUM_CPROP_BENCH],
           point->cprop[ENUM_CPROP_FILE],
           point->iprop[ENUM_PPROP_REVNUM],
           point->cprop[ENUM_CPROP_MSG]);
}

int printBucket(PointT * begin, PointT * cur, PointT * queryPoint, int nBucketedPoints) {
    PairSet templatesSeen, revsSeen;
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

void printGroup(TResultEle * walker) {
      printf("\nTemplate %d: ", walker->templateID);
      printf("Indicative Query Point: ");
      PointT * indicativePoint = *(walker->queryPoints->begin());
      printPoint(indicativePoint);
      printf("%d Neighbors:\n", walker->neighbors->size());
      PointSet::iterator it = walker->neighbors->begin();
      for(; it != walker->neighbors->end(); it++) {
          PointT * blah = *it; // C++ is the dumbest thing ever. 
          printf("TID:%d\tdist:%0.1lf \t BENCH: %s \t FILE %s\tREVNUM: %d\tMSG:%s\n", 
                 blah->iprop[ENUM_PPROP_TID],
                 sqrt(blah->distance),
                 blah->cprop[ENUM_CPROP_BENCH],
                 blah->cprop[ENUM_CPROP_FILE],
                 blah->iprop[ENUM_PPROP_REVNUM],
                 blah->cprop[ENUM_CPROP_MSG]);
      }
}

void printGroups(TResultEle * buckets) {
    for(TResultEle * walker = buckets; walker != NULL; walker = walker->next) {
      printGroup(walker);
  }
}

void dataT::insertIntoMap(PointMap * mymap, PointT * ele) {
    int tid = ele->iprop[ENUM_PPROP_TID];
    if(mymap->count(tid) > 0) {
        PointSet * oldval = (*mymap)[tid]; 
        oldval->insert(ele);
        mymap->erase(tid);
        mymap->insert(pair<int, PointSet*>(tid,oldval));
    } else {
        PointSet * set = new PointSet();
        set->insert(ele);
        mymap->insert(pair<int, PointSet*>(tid,set));
    }
}
    
pair<PointMap * ,PointMap *> dataT::makeMapsFromDataSet() {
    PointMap *context_map, *change_map;
    context_map = new map<int, PointSet*>();
    change_map = new map<int, PointSet*>();
    for(Int32T i = 0; i < nPoints[0]; i ++) {
        PointT * point = dataSetPoints[0][i];
        if(point->iprop[ENUM_PPROP_TYPE] == ENUM_CHANGE) {
          insertIntoMap(change_map, point);
        } else {
          insertIntoMap(context_map, point);
        }
    }
    return make_pair(context_map,change_map);
}

ListPair * dataT::separatePoints(PointMap * mymap) {
    int nPoints = 0;
    for(PointMap::const_iterator it = mymap->begin(); it != mymap->end(); it++) {
        PointSet * blah = it->second;
        nPoints += blah->size();
    }    
    PointT ** dataPoints = (PointT **) MALLOC(nPoints * sizeof(PointT * ));
    nPoints = 0;
    for(PointMap::const_iterator it = mymap->begin(); it != mymap->end(); it++) {
        PointSet * blah = it->second;
        for(PointSet::const_iterator ele = blah->begin(); ele != blah->end(); ele++, nPoints++) {
            PointT * p = (*ele);
            dataPoints[nPoints] = p;
        }
    }
    return new ListPair(dataPoints, nPoints);
}

void dataT::initComplex() {
    pair<PointMap*,PointMap*> maps = makeMapsFromDataSet();
    context_map = maps.first;
    changes_map = maps.second;
    ListPair * context_points = separatePoints(maps.first), * change_points = separatePoints(maps.second);
    free(dataSetPoints[0]);
    free(dataSetPoints);
    dataSetPoints = (PointT***) MALLOC(nTypes * sizeof(PointT**));

    dataSetPoints[0] = context_points->first;
    dataSetPoints[1] = change_points->first;
    free(nPoints);

    nPoints = (int *) MALLOC(nTypes * sizeof(int));
    nPoints[0] = context_points->second;
    nPoints[1] = change_points->second;

    free(pointsDimension);
    pointsDimension = (int *) MALLOC(nTypes * sizeof(int));
    pointsDimension[0] = dataSetPoints[0][0]->dimension;
    pointsDimension[1] = dataSetPoints[1][0]->dimension;

}
 
void dataT::setQueries(ListPair * sqInfo) {
    sampleQueries = (PointT ***) malloc(nTypes * sizeof(PointT**));
    for(int i = 0; i < nTypes; i++) {
        sampleQueries[i] = sqInfo[i].first;
        nSampleQueries = sqInfo[i].second;
    }
}

dataT::dataT(int nt, int nr, int np, int sq, PointT ** initialData) 
    : sampleQueries(NULL), nSampleQueries(sq), nRadii(nr), nTypes(nt),
      context_map(NULL), changes_map(NULL)
{ 
    dataSetPoints = (PointT***) MALLOC(nt * sizeof(PointT**));
    dataSetPoints[0] = initialData;
    nPoints = (int *) MALLOC(nt * sizeof(int));
    nPoints[0] = np;
    pointsDimension = (int * ) MALLOC(nt * sizeof(int));
    pointsDimension[0] = initialData[0]->dimension;
    listOfRadii = (RealT **) MALLOC(nt * sizeof(RealT *));
    for(int i = 0; i < nt; i++) {
        listOfRadii[i] = (RealT *) MALLOC(nr * sizeof(RealT));
    }
}
