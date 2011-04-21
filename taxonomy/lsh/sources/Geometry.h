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

/* properties of a point */
typedef enum {
  ENUM_CPROP_FILE, 
  ENUM_CPROP_MSG,
  ENUM_CPROP_BENCH,
  ENUM_CPROP_LAST_NOT_USED,
  ENUM_IPROP_TID,
  ENUM_IPROP_REVNUM,
  ENUM_IPROP_LINESTART,
  ENUM_IPROP_LINEEND,
  ENUM_IPROP_LAST_NOT_USED
} reg_prop_t;

typedef enum {
  ENUM_PPROP_TID,
  ENUM_PPROP_REVNUM,
  ENUM_PPROP_LINESTART,
  ENUM_PPROP_LINEEND,
  ENUM_PPROP_LAST_NOT_USED
} pprop_t;

// A simple point in d-dimensional space. A point is defined by a
// vector of coordinates. 
typedef struct _PointT {
    //IntT dimension;
    IntT index; // the index of this point in the dataset list of points
    RealT *coordinates;
    RealT sqrLength; // the square of the length of the vector
    char * cprop[ENUM_CPROP_LAST_NOT_USED];
    int iprop[ENUM_IPROP_LAST_NOT_USED - ENUM_CPROP_LAST_NOT_USED];
} PointT, *PPointT;

RealT distance(IntT dimension, PPointT p1, PPointT p2);
int comparePoints(const void *p1, const void *p2);

#endif
