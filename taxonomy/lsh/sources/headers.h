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

#ifndef HEADER_H
#define HEADER_H
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>

#include "BasicDefinitions.h"
#include "Random.h"
#include "Geometry.h"
#include "Util.h"
#include "VectorUtil.h"
#include "BucketHashing.h"
#include "LocalitySensitiveHashing.h"
#include "SelfTuning.h"
#include "NearNeighbors.h"
#include "GlobalVars.h"
#include "Buckets.h"
#ifdef DEBUG_MEM
//#include <malloc.h>
#endif

#ifdef DEBUG_TIMINGS
#include <sys/time.h>
#endif

#endif
