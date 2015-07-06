#include "templates.h"

void insert() {
    position instantiation_position __attribute__((type ("stmt")));
    hole __hole1__ __attribute__((type ("stmt")));

    { __blockattribute__(__instantiation_position__)
          ATOMAT(instantiation_position);
        USE(__hole1__);
    }
}

void * returnTemplate () {
    position instantiation_position __attribute__((type("stmt")));
    hole __hole2__ __attribute__((type ("lval")));

    { __blockattribute__(__instantiation_position__)
          ATOMAT(instantiation_position);
        return __hole2__.holevar;
    }

}

void gtZero() {
    position instantiation_position __attribute__((type("stmt")));

    hole __hole2__ __attribute__((type ("lval"))) __attribute__((reference ("instantiation_position")));

    { __blockattribute__(__instantiation_position__)
          if (__hole2__.holevar > 0) {
              ATOMAT(instantiation_position);
          }
    }
}

void delete() {
    position instantiation_position __attribute__((type("stmt")));

    { __blockattribute__(__instantiation_position__)
    }

}
