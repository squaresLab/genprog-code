#include "headers.h"

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

bool wrong_type(PointT * point,configT * config) {
    if(config->filtering) {
        int cmp = strcmp(point->cprop[ENUM_CPROP_TYPE], config->filterType);
        return cmp != 0;
    } 
    else false;
}

void computeVectorClusters(dataT * data, configT * config) {

  // output vector clusters according to the filtering parameters.
    IntT nPoints = data->nPoints[0];
    bool group = config->group;
    TimeVarT meanQueryTime = 0;
    int nBuckets = 0, nBucketedPoints = 0, nQueries = 0;
    TResultEle * buckets = NULL, *currentResult = NULL;
    PointT *result = (PointT *)MALLOC(nPoints * sizeof(PointT));
    bool seen[nPoints];

    // FIXME: this all assumes 1 radius
    memset(seen, 0, nPoints * sizeof(bool));

    for(IntT i = 0; i < nPoints; nQueries++, i++) {
        // find the next unseen point
        while (i < nPoints && (seen[i] || wrong_type(data->dataSetPoints[0][i],config) )) i++;
        if (i >= nPoints) break;
        PointT * queryPoint = data->dataSetPoints[0][i];
        if(group) {
            pair<TResultEle *, TResultEle *> retval = insertQueryBucket(queryPoint, buckets);
            buckets = retval.first;
            currentResult = retval.second;
        } 

        // get the near neighbors.
        IntT nNNs = getRNearNeighbors(nnStructs[0][0], queryPoint, result, nPoints);
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
                   sizeBucket, nNNs, (double)(data->listOfRadii[0][0]), 0);
            nBucketedPoints = printBucket(begin,end,queryPoint,nBucketedPoints);
        }
    }
    if(group) printGroups(buckets);

    // Simple statistics and finish
    if (nQueries > 0) {
        meanQueryTime = meanQueryTime / nQueries;
        printf("\n%d queries, Mean query time: %0.6lf\n", nQueries, (double)meanQueryTime);
        printf("%d buckets, %d points (out of %d, %.2f %%) in them\n",
               nBuckets, nBucketedPoints, nPoints, 100*(float)nBucketedPoints/(float)nPoints);
    } 
}

void clusterOverTime(PointT ** dataSetPoints, int nPoints) {
    qsort(dataSetPoints, nPoints, sizeof(*dataSetPoints), compareForTemplate); 
    PointT * result = (PointT *)MALLOC(nPoints * sizeof(PointT));

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
                currentResult->queryPoints->insert(*queryPoint);
                IntT nNNs = getRNearNeighbors(nnStructs[0][0], queryPoint, result, nPoints);
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
            printGroup(currentResult);
            i = j;
        }
    }
}

void simpleBuckets(configT * config, dataT * data) {
    if(config->do_time_exp) clusterOverTime(data->dataSetPoints[0], data->nPoints[0]);
    else computeVectorClusters(data, config);
}
