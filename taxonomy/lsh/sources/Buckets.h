#ifndef SIMPLE_BUCKETS
#define SIMPLE_BUCKETS

bool wrong_type(PointT * point, configT * config);
void computeVectorClusters(dataT * data, configT * config);
void clusterOverTime(dataT * data); 
void simpleBuckets(configT * config_params, dataT * data);

#endif
