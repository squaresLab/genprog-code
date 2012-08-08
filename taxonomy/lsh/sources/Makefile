# DEFINE_FLOAT should be set by a configure script (using testFloat.cpp)
# if this doesn't work, try REAL_DOUBLE instead
DEFINE_FLOAT	= REAL_FLOAT

CPP_OPTS	= -g -O3 -D$(DEFINE_FLOAT) # -Wunused-variable
LINK_OPTS	= -O3 -lm
OUT_DIR		= ../bin

LSH_SRC		= BucketHashing.cpp \
		  Geometry.cpp \
		  LocalitySensitiveHashing.cpp \
		  Random.cpp \
		  Util.cpp \
		  VectorUtil.cpp \
		  GlobalVars.cpp \
		  SelfTuning.cpp \
		  NearNeighbors.cpp \
		  Buckets.cpp

TEST_SRC	=  diffBuckets.cpp

LSH_OBJS	= $(LSH_SRC:.cpp=.o)
TEST_BUILDS	= $(addprefix $(OUT_DIR)/,$(TEST_SRC:.cpp=))

.PHONY: all clean clean_all

all: $(TEST_BUILDS)

$(OUT_DIR)/%: %.o $(LSH_OBJS)
	g++ -o $@ $(LINK_OPTS) $^

clean:  # remove intermediate files
	-rm -f *~ *.o .depend

clean_all: clean  # also remove exe files
	-rm -f $(TEST_BUILDS)

%.o: %.cpp
	g++ -c $(CPP_OPTS) $<

.depend: $(LSH_SRC) $(TEST_SRC)
	g++ -MM $^ > $@

include .depend

