#include <stdio.h>
#include <stdlib.h>
#include <map>
#include <set>
#include "headers.h"

void complexBuckets(PointT ** dataSet, PointT ** sampleQueries) {

}


void insert_into_map(PointMap mymap, PointT * ele) {
    PointSet * set = NULL;
    int tid = ele->iprop[ENUM_PPROP_TID];
    if(mymap.count(tid) > 0) {
        mymap[tid]->insert(*ele);
    } else {
        set = new PointSet();
        set->insert(*ele);
        mymap[tid] = set;
    }
}

pair<PointMap,PointMap> makeMapsFromDataSet(PointT ** dataSetPoints) {
    PointMap context_map, change_map;
    for(Int32T i = 0; i < nPoints; i ++) {
        PointT * point = dataSetPoints[i];
        insert_into_map(context_map, point);
        insert_into_map(change_map, point);
    }
    return make_pair(context_map,change_map);
}

