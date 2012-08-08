#include "headers.h"

pair<TResultEle *, TResultEle*> insertQueryBucket(int template_id, TResultEle * buckets) {
    TResultEle * currentResult = NULL, *walker = buckets;

    while(walker != NULL && walker->templateID < template_id) {
        walker = walker->next;
    } 
    if(walker == NULL || walker->templateID != template_id) {
        currentResult = new TResultEle(template_id);
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
        ASSERT(walker->templateID == template_id);
        currentResult = walker;
    }
    return make_pair(buckets,currentResult);
}

bool Buckets::wrong_type(PointT * point,configT * config) {
    if(config->filtering) {
        return point->iprop[ENUM_PPROP_TYPE] != config->filterType;
    } 
    else false;
}




PointMap * change_map = NULL;
PointSet * change_vecs = NULL;

RealT smallest_distance(int dim, PointSet comparison_vecs) {
    RealT smallest=-1.0;
    PointSet::iterator it1 = change_vecs->begin();
    PointSet::iterator it2 = comparison_vecs.begin();
    for(; it1 != change_vecs->end(); it1++) {
        PointT * vec1 = *(it1);
        for(; it2 != comparison_vecs.end(); it2++) {
            PointT * vec2 = *(it2);
            RealT dist = distance(dim, vec2,vec2);
            if(smallest < 0 || dist < smallest) {
                smallest = dist;
            }
        }
    }
    return smallest;
}

int buckComparePoints(const void * p1, const void * p2) {
    PointT * a = (PointT *) p1; 
    PointT * b = (PointT *) p2; 
    PointSet a_change_vecs = change_map->find(a->iprop[ENUM_PPROP_TID])->second;
    PointSet b_change_vecs = change_map->find(b->iprop[ENUM_PPROP_TID])->second; 
    RealT smallest_a=smallest_distance(a->dimension, a_change_vecs);
    RealT smallest_b=smallest_distance(b->dimension, b_change_vecs);
    return smallest_a - smallest_b;
}

void Buckets::complexClusters() {
    printf("complexClusters\n"); fflush(stdout);
    for(IntT radius = 0; radius < data->nRadii; radius++) {
        TResultEle * buckets = NULL, *currentResult = NULL;;

        PointMap context_map = data->maps[ENUM_CONTEXT];
        PointMap change_map = data->maps[ENUM_CHANGE];
        PointMap::iterator map_iter = context_map.begin();
        for(; map_iter != context_map.end(); map_iter++) {
            IntT template_id = map_iter->first;
            printf("template_id: %d\n", template_id); fflush(stdout);
            PointSet context_vecs = map_iter->second;
            PointSet change_vecs = change_map.find(template_id)->second;

            PointSet::iterator set_iter = context_vecs.begin();
            pair<TResultEle *, TResultEle *> retval = insertQueryBucket(template_id, buckets);
            buckets = retval.first;
            currentResult = retval.second;
            currentResult->queryPoints = &(context_vecs);
            PointSet * neighbors = new PointSet();
            PointT * queryPoint = NULL;
            for(; set_iter != context_vecs.end(); set_iter++) {
                queryPoint = *(set_iter);
                PointT *result = (PointT *)MALLOC(data->nPoints[ENUM_CONTEXT] * sizeof(PointT));
                IntT nNNs = getRNearNeighbors(nnStructs[ENUM_CONTEXT][radius], queryPoint, result, data->nPoints[ENUM_CONTEXT]);
                for(IntT i = 0; i < nNNs; i++) {
                    neighbors->insert(&result[i]);
                }
            }
            PointT * result = (PointT *) MALLOC(neighbors->size() * sizeof(PointT));
            PointSet::iterator neigh_iter = neighbors->begin();
            for(IntT i = 0; i < neighbors->size(), neigh_iter != neighbors->end(); i++, neigh_iter++) {
                PointT * neighbor = *(neigh_iter);
                result[i] = *neighbor;
            }
//            change_vecs = data->maps[ENUM_CHANGE]->find(queryPoint->iprop[ENUM_PPROP_TID])->second;
            change_map = data->maps[ENUM_CHANGE];
            qsort(result, neighbors->size(), sizeof(*result), buckComparePoints);
//            change_vecs = NULL; // ghetto hack is ghetto
//            change_map = NULL;

            printf("\nQuery template %d: ", template_id);
            printPoint(queryPoint);
            for(IntT i = 0; i < neighbors->size(); i++) {
                printf("Neighbors.size(): %d, at distance %0.6lf (radius no. %d). NNs are:\n",
                       neighbors->size(), (double)(data->listOfRadii[ENUM_CONTEXT][radius]), 0);
                PointT * p = &result[i];
                printf("%05d\tdist:%0.1lf \t BENCH: %s \tTID:%d\tFILE %s\tREVNUM: %d\tMSG:%s\n", 
                       p->index, sqrt(p->distance),
                       p->cprop[ENUM_CPROP_BENCH],
                       p->iprop[ENUM_PPROP_TID],
                       p->cprop[ENUM_CPROP_FILE],
                       p->iprop[ENUM_PPROP_REVNUM],
                       p->cprop[ENUM_CPROP_MSG]);
            }
        }
    }
}

void Buckets::computeVectorClusters() {

  // output vector clusters according to the filtering parameters.
    for(IntT typei = 0; typei < data->nTypes; typei++) {
        printf("Computing vector clusters for points of type %d\n", typei);
        IntT nPoints = data->nPoints[typei];
        bool group = config->group;
        TimeVarT meanQueryTime = 0;
        int nBuckets = 0, nBucketedPoints = 0, nQueries = 0;
        TResultEle * buckets = NULL, *currentResult = NULL;
        PointT *result = (PointT *)MALLOC(nPoints * sizeof(PointT));
        bool seen[nPoints];

        for(IntT radius = 0; radius < data->nRadii; radius++) {
            memset(seen, 0, nPoints * sizeof(bool));
            for(IntT i = 0; i < nPoints; nQueries++, i++) {
                // find the next unseen point
                while (i < nPoints && (seen[i] || wrong_type(data->dataSetPoints[typei][i],config) )) i++;
                if (i >= nPoints) break;
                PointT * queryPoint = data->dataSetPoints[typei][i];
                if(group) {
                    pair<TResultEle *, TResultEle *> retval = insertQueryBucket(queryPoint->iprop[ENUM_PPROP_TID], buckets);
                    buckets = retval.first;
                    currentResult = retval.second;
                    currentResult->queryPoints->insert(queryPoint);
                } 
                
                // get the near neighbors.
                IntT nNNs = getRNearNeighbors(nnStructs[typei][radius], queryPoint, result, nPoints);
                meanQueryTime += timeRNNQuery;
                
                qsort(result, nNNs, sizeof(*result), comparePoints);
                PointT * cur = result+ 1, *end = result + nNNs, *begin = result;
                PairSet templatesSeen, revsSeen;
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
                        if(group) currentResult->neighbors->insert(cur);
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
                           sizeBucket, nNNs, (double)(data->listOfRadii[typei][radius]), 0);
                    nBucketedPoints = printBucket(begin,end,queryPoint,nBucketedPoints);
                }
            }
            if(group) printGroups(buckets);
        }
        // Simple statistics and finish
        if (nQueries > 0) {
            meanQueryTime = meanQueryTime / nQueries;
            printf("\n%d queries, Mean query time: %0.6lf\n", nQueries, (double)meanQueryTime);
            printf("%d buckets, %d points (out of %d, %.2f %%) in them\n",
                   nBuckets, nBucketedPoints, nPoints, 100*(float)nBucketedPoints/(float)nPoints);
        } 
    }
}

void Buckets::clusterOverTime() {
    for(int typei = 0; typei < data->nTypes; typei++ ) {
        printf("Clustering points of type %d over time.\n", typei);
        PointT ** dataSetPoints = data->dataSetPoints[typei];
        int nPoints = data->nPoints[typei];
        qsort(dataSetPoints, nPoints, sizeof(*dataSetPoints), compareForTemplate); 
        PointT * result = (PointT *)MALLOC(nPoints * sizeof(PointT));

        for(int radius = 0; radius < data->nRadii; radius++) {
            printf("Radius: %d\n", data->listOfRadii[typei][radius]);
            IntT i = 0;
            while(i < nPoints) {
                int currRevision = dataSetPoints[i]->iprop[ENUM_PPROP_REVNUM]; 
                printf("Revision: %d\n", currRevision);
                while(i < nPoints && dataSetPoints[i]->iprop[ENUM_PPROP_REVNUM] == currRevision) {
                    int currTemplate = dataSetPoints[i]->iprop[ENUM_PPROP_TID];
                    
                    TResultEle * currentResult = new TResultEle(currTemplate);
                    
                    PairSet templatesSeen, revsSeen;
                    IntT j = i;
                    for(; j < nPoints && dataSetPoints[j]->iprop[ENUM_PPROP_TID] == currTemplate; j++) {
                        PointT * queryPoint = dataSetPoints[j];
                        currentResult->queryPoints->insert(queryPoint);
                        IntT nNNs = getRNearNeighbors(nnStructs[0][0], queryPoint, result, nPoints);
                        qsort(result, nNNs, sizeof(*result), comparePoints);
                        PointT * cur = result, *end = result + nNNs;
                        while(cur < end && cur->iprop[ENUM_PPROP_REVNUM] < currRevision) {
                            ASSERT(cur != NULL);
                            if ( pointIsNotFiltered(cur,queryPoint,templatesSeen,revsSeen) ) {
                                templatesSeen.insert(make_pair(cur->cprop[ENUM_CPROP_BENCH],cur->iprop[ENUM_PPROP_TID]));
                                revsSeen.insert(make_pair(cur->cprop[ENUM_CPROP_BENCH],cur->iprop[ENUM_PPROP_REVNUM]));
                                currentResult->neighbors->insert(cur);
                            }
                            cur++;
                        }
                    }
                    printGroup(currentResult);
                    i = j;
                }
            }
        }
    }
}
