typedef struct _hole {
    void * holevar;
} hole;


typedef struct _position {
    void * posvar;
} position;

hole ___placeholder_hole___;
position ___placeholder_position___;

#define USE(hole) hole.holevar = ___placeholder_hole___.holevar
#define ATOMAT(position) position.posvar = ___placeholder_position___.posvar
