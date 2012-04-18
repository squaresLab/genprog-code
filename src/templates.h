typedef struct _hole {
    void * var;
} hole;

hole ___placeholder___;
#define USE(hole) hole.var = ___placeholder___.var
