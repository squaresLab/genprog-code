#ifndef SIMPLE_BUCKETS
#define SIMPLE_BUCKETS

void computeParametersAndPrepare(bool computeParameters, char* paramsFile, PointT ** dataSetPoints, PointT ** sampleQueries);
bool wrong_type(PointT * point);
void computeVectorClusters(PointT ** dataSetPoints, bool group);
void clusterOverTime(PointT ** dataSetPoints);
void simpleBuckets(bool computeParameters, bool group, bool do_time_exp, char * paramsFile, PointT ** dataSet, PointT ** sampleQueries);

#endif
