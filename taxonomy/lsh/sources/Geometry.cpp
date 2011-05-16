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

void printGroup(TResultEle * walker) {
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
}

void printGroups(TResultEle * buckets) {
    for(TResultEle * walker = buckets; walker != NULL; walker = walker->next) {
      printGroup(walker);
  }
}
