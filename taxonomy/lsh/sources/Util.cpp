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

/*
  Prints the usage of diffBuckets.
 */
void usage(int code, char *programName) {
  printf("Usage: %s [options: see source code] data_set_file [params_file]\n", programName);
  exit(code);
}


char * str2CharStar(string line) {
    char * line_star = new char[line.size() + 1];
    copy(line.begin(), line.end(), line_star);
    line_star[line.size()] = '\0';
    return line_star;
}


PointT * readPoint(char * line, char * comment, int dimension) {
    PointT * p = new PointT(dimension);
    RealT sqrLength = 0;
    IntT d;
    char *t;

    if (comment != NULL) {
        int a, b;
        regmatch_t pmatch[2];
        // Find/copy string properties from comment
        for (int i = ENUM_CPROP_FILE; i < ENUM_CPROP_LAST_NOT_USED; i++) {
            if (regexec(&preg[i], comment, 2, pmatch, 0) == 0 &&
                (a = pmatch[1].rm_so) != -1) {
                b = pmatch[1].rm_eo;
                FAILIF(NULL == (p->cprop[i] = (char*)MALLOC(b-a+1)));
                memmove(p->cprop[i], comment + a, b-a);
                p->cprop[i][b-a] = '\0';
            } else {
                p->cprop[i] = NULL;
            }
        }
        
// find/copy integer properties from comment
        for (int i = ENUM_CPROP_LAST_NOT_USED + 1; i < ENUM_IPROP_LAST_NOT_USED; i++) {
            if (regexec(&preg[i], comment, 2, pmatch, 0) == 0 &&
                (a = pmatch[1].rm_so) != -1) {
                b = pmatch[1].rm_eo;
                char t = comment[b];
                comment[b] = '\0';
                p->iprop[i-ENUM_CPROP_LAST_NOT_USED-1] = atoi(comment + a);
                comment[b] = t;
            } else {
                p->iprop[i-ENUM_CPROP_LAST_NOT_USED-1] = 0;
            }
        }
    }
    
    for (d = 0, t = line; *t != '\0' && d < dimension; d++) {
        while ( !isdigit(*t) && *t != '\0' && *t != '.') t++;
        p->coordinates[d] = strtof(t, &t);
        sqrLength += SQR(p->coordinates[d]);
    }
    
    p->index = -1;
    p->sqrLength = sqrLength;
    return p;
}


// Reads in the data set points from <filename> in the array
// <dataSetPoints>. Each point get a unique number in the field
// <index> to be easily identifiable.
PointT ** walkerToList(int nPoints, TPointTList * pointsStart) {
  PointT ** dataSetPoints = (PointT**)MALLOC(nPoints * sizeof(PointT*));

  for(int i = 0; i < nPoints; i++) {
      ASSERT(pointsStart != NULL);
      dataSetPoints[i] = pointsStart->hd;
      dataSetPoints[i]->index = i;
      TPointTList *cur = pointsStart->tl;
      free(pointsStart);
      pointsStart = cur;
  }
  return dataSetPoints;

}

pair<PointT **,int> readDataSetFromFile(char *filename, char * vec_files, int reduce)
{
    PointT ** dataSetPoints = NULL;
    TPointTList *pointsStart, *pointsWalker;
    string line, comment;
    char ** files = (char **) MALLOC(sizeof(char *));;
    int num_files = 0;
    FAILIF(NULL == (pointsWalker = (TPointTList*)MALLOC(sizeof(TPointTList))));
    int nPoints = 0;

    pointsWalker->tl = NULL;
    pointsStart=pointsWalker;

    if (vec_files == NULL) {
        files[0] = filename;
        num_files ++;
    } else {
        ifstream inFile(vec_files,ios::in);
        int space = 1;
        while(getline(inFile,line)) {
            if(num_files >= space) {
                char ** temp = (char **) MALLOC((num_files + 2) * sizeof(char *)); 
                  memcpy(temp,files,num_files * sizeof(char *));
                  free(files);
                  files = temp;
                space += 2;
            }
            if (line[line.length()-1] == '\n') line[line.length()-1] = '\0';
            char * linec = str2CharStar(line);
            files[num_files] = linec;
            num_files++;
        }
    }

    for(int i = 0; i < num_files; i++) {
        filename = files[i];
        printf("file name: %s\n", filename);
        fflush(stdout);
        ifstream inFile(filename,ios::in);
        while(getline(inFile,line)) 
        {
            if (line[0] == '#') {
                // the line is a comment
                comment = line;
                if (comment[line.length()-1] == '\n') comment[line.length()-1] = '\0';
                line = ""; 
            } else {
                // the line is a point
                    // compute the dimension
            int pointsDimension = 0;

                    int p = 0;
                    while (line[p] == ' ' || line[p] == '\n' || line[p] == '\r' || line[p] == '\t') p++;
                    while (line[p] != '\0') {
                        while (line[p] != ' ' && line[p]!='\t' && line[p]!='\r' && line[p]!='\n' && line[p] != '\0') p++;
                        pointsDimension++;
                        while (line[p] == ' ' || line[p] == '\n' || line[p] == '\r' || line[p] == '\t') p++;
                }
                
                // add the new point to the queue
                int random = genRandomInt(0,100);
                if(random <= (100 - reduce)) {
                    pointsWalker->hd = readPoint(str2CharStar(line), str2CharStar(comment), pointsDimension);
                    FAILIF(NULL == (pointsWalker->tl = (TPointTList*)MALLOC(sizeof(TPointTList))));
                    pointsWalker = pointsWalker->tl;
                    nPoints++;
                }
            } // end of new point handling
        } // end of file
    }
    printf("total number of points: %d\n", nPoints);
  // put the points in the array and free the point list
    return make_pair(walkerToList(nPoints, pointsStart), nPoints);
}

bool readParamsFile(dataT * data, char * paramsFile)
{
    FILE *pFile = fopen(paramsFile, "rt");
    if (pFile == NULL) {
        fprintf(stderr,
                "Warning: could not open %s, will try to compute parameters "
                "and write them to that file\n", paramsFile);
        return true;
    } else {
        fscanf(pFile, "%d\n", &(data->nRadii));
        fprintf(stderr, "Using the following R-NN DS parameters (from %s):\n", paramsFile);
        FAILIF(NULL == (nnStructs = (PRNearNeighborStructT**)MALLOC(data->nTypes * sizeof(PRNearNeighborStructT*))));
        FAILIF(NULL == (algParameters = (RNNParametersT**)MALLOC(data->nTypes * sizeof(RNNParametersT*))));

        for(IntT typei = 0; typei < data->nTypes; typei++) {
            fprintf(stderr, "N radii = %d, nPoints = %d\n", data->nRadii, data->nPoints[typei]);
            nnStructs[typei] = (PRNearNeighborStructT*) MALLOC(data->nRadii * sizeof(PRNearNeighborStructT));
            algParameters[typei] = (RNNParametersT*)MALLOC(data->nRadii * sizeof(RNNParametersT));
            for(IntT radi = 0; radi < data->nRadii; radi++) {
                algParameters[typei][radi] = readRNNParameters(pFile);
                printRNNParameters(stderr, algParameters[typei][radi]);
                nnStructs[typei][radi] = initLSH_WithDataSet(algParameters[typei][radi], data->nPoints[typei], data->dataSetPoints[typei]);
            }
        }
        fclose(pFile);
        return false;
    }
}

void computeParameters(configT * config, dataT * data, set<double> radii, char* paramsFile) {
    if(!config->computeParameters) {
        printf("reading params file\n"); fflush(stdout);
        config->computeParameters = readParamsFile(data,paramsFile);
        printf("done reading params file\n"); fflush(stdout);
    }
    if (config->computeParameters) {
        FILE *fd;

        if (paramsFile == NULL)
            fd = stdout;
        else {
            fd = fopen(paramsFile, "wt");
            if (fd == NULL) {
                fprintf(stderr, "Unable to write to parameter file %s\n", paramsFile);
                exit(1);
            }
        }

        fprintf(fd, "%d\n", data->nRadii);

        for(IntT type_index = 0; type_index < data->nTypes; type_index++) {
            set<double>::iterator it = radii.begin();
            for(IntT r = 0; it != radii.end(); it++, r++) {
                RealT blah = *it;
                data->listOfRadii[type_index][r] = blah;
            }
            
            Int32T sampleQBoundaryIndeces[data->nSampleQueries];
            // Compute the array sampleQBoundaryIndeces that specifies how to
            // segregate the sample query points according to their distance
            // to NN.

            sortQueryPointsByRadii(data->pointsDimension[type_index],
                                   data->nSampleQueries,
                                   data->sampleQueries[type_index],
                                   data->nPoints[type_index],
                                   data->dataSetPoints[type_index],
                                   data->nRadii,
                                   data->listOfRadii[type_index],
                                   sampleQBoundaryIndeces);
    
            // Compute the R-NN DS parameters
            // if a parameter file is given, output them to that file, and continue
            // otherwise, output them to stdout, and exit
        
            transformMemRatios(type_index, data->nRadii);
        
            for(IntT i = 0; i < data->nRadii; i++) {
                // which sample queries to use
                Int32T segregatedQStart = (i == 0) ? 0 : sampleQBoundaryIndeces[i - 1];
                Int32T segregatedQNumber = data->nSampleQueries - segregatedQStart;
                if (segregatedQNumber == 0) {
                    // XXX: not the right answer
                    segregatedQNumber = data->nSampleQueries;
                    segregatedQStart = 0;
                }
                ASSERT(segregatedQStart < data->nSampleQueries);
                ASSERT(segregatedQStart >= 0);
                ASSERT(segregatedQStart + segregatedQNumber <= data->nSampleQueries);
                ASSERT(segregatedQNumber >= 0);
                RNNParametersT optParameters = computeOptimalParameters(data->listOfRadii[type_index][i],
                                                                        successProbability,
                                                                        data->nPoints[type_index],
                                                                        data->pointsDimension[type_index],
                                                                        data->dataSetPoints[type_index],
                                                                        segregatedQNumber,
                                                                        data->sampleQueries[type_index] + segregatedQStart,
                                                                        (Uns32T)((availableTotalMemory - totalAllocatedMemory) * memRatiosForNNStructs[type_index][i]));
                printRNNParameters(fd, optParameters);
            }
        }
        
        if (fd == stdout) exit(0);
        else {
            fclose(fd);
            ASSERT(!readParamsFile(data,paramsFile));
        }
        
    }
    for(int type_index = 0; type_index < data->nTypes; type_index++) {
        free(data->listOfRadii[type_index]);
    }
    free(data->listOfRadii);

    data->listOfRadii = (RealT **) MALLOC(sizeof(RealT));

    for(int type_index = 0; type_index < data->nTypes; type_index++) {
        data->listOfRadii[type_index] = (RealT *) MALLOC(data->nRadii * sizeof(RealT *));
        for(IntT j = 0; j < data->nRadii; j++) {
            data->listOfRadii[type_index][j] = algParameters[type_index][j].parameterR;
        }
    }
}

int compareInt32T(const void *a, const void *b){
  Int32T *x = (Int32T*)a;
  Int32T *y = (Int32T*)b;
  return (*x > *y) - (*x < *y);
}
