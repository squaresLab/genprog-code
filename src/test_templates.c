#include "templates.h"

void insert() {
    hole __hole1__ __attribute__((holetype ("stmt"))) __attribute__((constraint ("fault_path")));
    hole __hole2__ __attribute__((holetype ("stmt"))) __attribute__((constraint ("fix_path")));

    { __blockattribute__(__hole1__)
          USE(__hole1__);
        USE(__hole2__);
    }
}

void * returnTemplate () {
    hole __hole1__ __attribute__ ((holetype ("stmt"))) __attribute__((constraint ("fault_path")));
    hole __hole2__ __attribute__((holetype ("lval"))) __attribute__((constraint ("fault_path"))) __attribute__((inscope ("__hole1__")));

    { __blockattribute__(__hole1__)
          __hole1__.var;
          return __hole2__.var;
    }

}

void swap() {
    hole __hole1__ __attribute__((holetype ("stmt"))) __attribute__((constraint ("fault_path")));
    hole __hole2__ __attribute__((holetype ("stmt"))) __attribute__((constraint ("fault_path")));
    { __blockattribute__(__hole1__)
          USE(__hole2__);
    }
    { __blockattribute__(__hole2__)
          USE(__hole1__);
    }
}

void gtZero() {
    hole __hole1__ __attribute__((holetype ("stmt"))) __attribute__((constraint ("fault_path")));
    hole __hole2__ __attribute__((holetype ("lval"))) __attribute__((constraint ("fault_path"))) __attribute__((reference ("__hole1__")));

    { __blockattribute__(__hole1__)
          if (__hole2__.var > 0) {
              USE(__hole1__);
          }
    }
}

void delete() {
    hole __hole1__ __attribute__((holetype ("stmt"))) __attribute__((constraint ("fault_path")));

    { __blockattribute__(__hole1__)
    }

}
