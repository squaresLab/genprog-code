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


PPointT readPoint(char * line, char * comment){
  
  PPointT p;
  RealT sqrLength = 0;
  FAILIF(NULL == (p = (PPointT)MALLOC(sizeof(PointT))));
  FAILIF(NULL == (p->coordinates = (RealT*)MALLOC(pointsDimension * sizeof(RealT))));
  IntT d;
  char *t;

  if (comment != NULL) {
    int a, b;
    regmatch_t pmatch[2];
    // Find/copy string properties from comment
    for (int i = ENUM_CPROP_FILE; i < ENUM_CPROP_LAST_NOT_USED; i++) {
        if (regexec(&preg[i], comment, 2, pmatch, 0) ==   0 &&
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

  for (d = 0, t = line; *t != '\0' && d < pointsDimension; d++) {
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
PPointT * readDataSetFromFile(char *filename, bool sampleData)
{
    PPointT * dataSetPoints = NULL;
    TPPointTList *pointsStart, *pointsWalker;
    string line, comment;

    FAILIF(NULL == (pointsWalker = (TPPointTList*)MALLOC(sizeof(TPPointTList))));

    if(sampleData) nPoints = 0; 
    else nSampleQueries = 0;

    pointsWalker->tl = NULL;
    pointsStart=pointsWalker;
  
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
            if (pointsDimension == 0) {
                // compute the dimension
                int p = 0;
                while (line[p] == ' ' || line[p] == '\n' || line[p] == '\r' || line[p] == '\t') p++;
                while (line[p] != '\0') {
                    while (line[p] != ' ' && line[p]!='\t' && line[p]!='\r' && line[p]!='\n' && line[p] != '\0') p++;
                    pointsDimension++;
                    while (line[p] == ' ' || line[p] == '\n' || line[p] == '\r' || line[p] == '\t') p++;
                }
            }
            // add the new point to the queue
            pointsWalker->hd = readPoint(str2CharStar(line), str2CharStar(comment));
            FAILIF(NULL == (pointsWalker->tl = (TPPointTList*)MALLOC(sizeof(TPPointTList))));
          pointsWalker = pointsWalker->tl;
          if(sampleData) nPoints++;
          else nSampleQueries++;
      } // end of new point handling
  } // end of file

  fflush(stdout);
  // put the points in the array and free the point list
  dataSetPoints = (PPointT*)MALLOC(nPoints * sizeof(PPointT));
  for(IntT i = 0; i < nPoints; i++) {
      ASSERT(pointsStart != NULL);
      dataSetPoints[i] = pointsStart->hd;
      dataSetPoints[i]->index = i;
      TPPointTList *cur = pointsStart->tl;
      free(pointsStart);
      pointsStart = cur;
  }
  return dataSetPoints;
}


bool readParamsFile(char *paramsFile, PPointT * dataSetPoints)
{
    FILE *pFile = fopen(paramsFile, "rt");
    if (pFile == NULL) {
        fprintf(stderr,
                "Warning: could not open %s, will try to compute parameters "
                "and write them to that file\n", paramsFile);
        return true;
    } else {
        fscanf(pFile, "%d\n", &nRadii);
        fprintf(stderr, "Using the following R-NN DS parameters (from %s):\n", paramsFile);
        fprintf(stderr, "N radii = %d, nPoints = %d\n", nRadii, nPoints);
        FAILIF(NULL == (nnStructs = (PRNearNeighborStructT*)MALLOC(nRadii * sizeof(PRNearNeighborStructT))));
        FAILIF(NULL == (algParameters = (RNNParametersT*)MALLOC(nRadii * sizeof(RNNParametersT))));
        for(IntT i = 0; i < nRadii; i++){
            algParameters[i] = readRNNParameters(pFile);
            printRNNParameters(stderr, algParameters[i]);
            nnStructs[i] = initLSH_WithDataSet(algParameters[i], nPoints, dataSetPoints);
        }
        
        pointsDimension = algParameters[0].dimension;
        if (listOfRadii != NULL) FREE(listOfRadii);
        FAILIF(NULL == (listOfRadii = (RealT*)MALLOC(nRadii * sizeof(RealT))));
        for(IntT i = 0; i < nRadii; i++){
            listOfRadii[i] = algParameters[i].parameterR;
        }
        fclose(pFile);
        return false;
    }
}

int compareInt32T(const void *a, const void *b){
  Int32T *x = (Int32T*)a;
  Int32T *y = (Int32T*)b;
  return (*x > *y) - (*x < *y);
}
