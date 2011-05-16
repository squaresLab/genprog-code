#include "headers.h"

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

bool wrong_type(PointT * point) {
    if(filtering) {
        int cmp = strcmp(point->cprop[ENUM_CPROP_TYPE], filterType);
        return cmp != 0;
    } 
    else false;
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
        while (i < nPoints && (seen[i] || wrong_type(dataSetPoints[i])) ) i++;
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
                   sizeBucket, nNNs, (double)(listOfRadii[0]), 0);
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

void clusterOverTime(PointT ** dataSetPoints) {
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
            printGroup(currentResult);
            i = j;
        }
    }
}

void simpleBuckets(bool computeParameters, bool group, bool do_time_exp, char * paramsFile, PointT ** dataSet, PointT ** sampleQueries) {
    computeParametersAndPrepare(computeParameters,paramsFile,dataSet,sampleQueries);
    if(do_time_exp) {
        clusterOverTime(dataSet);
    } else 
      computeVectorClusters(dataSet, group);

}
