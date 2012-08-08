#ifndef SIMPLE_BUCKETS
#define SIMPLE_BUCKETS

class Buckets {
private:
    bool wrong_type(PointT * point, configT * config);

    dataT * data;
    configT * config;
public:
    Buckets(dataT * data, configT * config) :
        data(data),config(config) { }
    void computeVectorClusters();
    void clusterOverTime();
    void complexClusters();
};

#endif
