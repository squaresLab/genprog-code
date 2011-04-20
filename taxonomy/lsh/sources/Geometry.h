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

#ifndef GEOMETRY_INCLUDED
#define GEOMETRY_INCLUDED

/* A set of properties of a point */
typedef enum {
  ENUM_PPROP_FILE, 
  ENUM_PPROP_LAST_NOT_USED
} pprop_t;

// A simple point in d-dimensional space. A point is defined by a
// vector of coordinates. 
typedef struct _PointT {
  //IntT dimension;
  IntT index; // the index of this point in the dataset list of points
  RealT *coordinates;
  RealT sqrLength; // the square of the length of the vector
  char *filename;
  int prop[ENUM_PPROP_LAST_NOT_USED-1]; // doesn't contain ENUM_PPROP_FILE.
  char *oids;
} PointT, *PPointT;

RealT distance(IntT dimension, PPointT p1, PPointT p2);

#endif
