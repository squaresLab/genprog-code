#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <ecl/ecl.h>
#include <string.h>
#include <stdio.h>

extern int main_dll_ELF(int argc, char **argv);

CAMLprim value start_elf(value unit){
  int argc = 1;
  char* argv[1];
  argv[0] = "FAKE!";
  main_dll_ELF(argc, argv);
  return Val_unit;
}

CAMLprim value stop_elf(value unit){
  si_exit(0);
  return Val_unit;
}

CAMLprim value read_elf(value path){
  char *name = String_val(path);
  char buff[255];
  sprintf(buff, "\"%s\"", name);
  return (value)cl_eval(cl_list(2,
                                c_string_to_object("read-elf"),
                                c_string_to_object(buff)));
}

CAMLprim value write_elf(value elf, value path){
  CAMLparam2(elf, path);
  char *name = String_val(path);
  char buff[255];
  sprintf(buff, "\"%s\"", name);
  cl_eval(cl_list(3, c_string_to_object("write-elf"),
                  elf, c_string_to_object(buff)));
  return Val_unit;
}

CAMLprim value show_memory_layout(value elf){
  cl_eval(cl_list(2, c_string_to_object("show-memory-layout"), elf));
  return Val_unit;
}

CAMLprim value get_text(value elf){
  // create an empty OCaml list
  CAMLparam1(elf);
  CAMLlocal1(ml_bytes);

  // get the data and its length
  cl_object text = cl_funcall(3, c_string_to_object("named-section"),
                              elf, c_string_to_object("\".text\""));
  cl_object data = cl_eval(cl_list(2, c_string_to_object("data"), text));
  int length = fix(cl_length(data));
  int i;

  // assign the lisp list element into the OCaml list
  ml_bytes = caml_alloc(length, 0);
  for(i=0; i<(length-1); i++){
    Store_field(ml_bytes, i, Val_int(fix(cl_aref(2, data, MAKE_FIXNUM(i)))));
  }

  CAMLreturn( ml_bytes );
}

CAMLprim value set_text(value elf, value ml_bytes){
  CAMLparam2(elf, ml_bytes);
  int i, length;

  // ocaml array into lisp array
  length = Wosize_val(ml_bytes);
  cl_object bytes = cl_make_array(1, MAKE_FIXNUM(length));
  for (i=0; i<length; i++) {
    si_aset(3, bytes, MAKE_FIXNUM(i), MAKE_FIXNUM(Int_val(Field(ml_bytes, i))));
  }

  // // update the .text section
  // cl_object text = cl_funcall(3, c_string_to_object("named-section"),
  //                             elf, c_string_to_object("\".text\""));
  // cl_object data = cl_funcall(2, c_string_to_object("data"), text);
  // 
  // printf("about to call setf\n");
  // cl_funcall(3, c_string_to_object("setf"), data, bytes);

  cl_funcall(3, c_string_to_object("update-text"), elf, bytes);

  CAMLreturn( Val_unit );
}

CAMLprim value get_text_offset(value elf){
  CAMLparam1(elf);
  // (address (sh (named-section elf ".text")))
  cl_object text = cl_funcall(3, c_string_to_object("named-section"),
                              elf, c_string_to_object("\".text\""));
  cl_object head = cl_funcall(2, c_string_to_object("sh"), text);
  CAMLreturn(Val_int(fix(cl_funcall(2, c_string_to_object("address"), head))));
}

CAMLprim value write_w_text(value elf, value path, value ml_bytes){
  // process parameters
  CAMLparam3(elf, path, ml_bytes);
  int i, length;
  char *name = String_val(path);
  char buff[255];
  sprintf(buff, "\"%s\"", name);

  // ocaml array into lisp array
  length = Wosize_val(ml_bytes);
  cl_object bytes = cl_make_array(1, MAKE_FIXNUM(length));
  for (i=0; i<length; i++) {
    si_aset(3, bytes, MAKE_FIXNUM(i), MAKE_FIXNUM(Int_val(Field(ml_bytes, i))));
  }

  cl_object new_elf = cl_funcall(2, c_string_to_object("copy-elf"), elf);
  cl_object new_text = cl_funcall(3, c_string_to_object("named-section"),
                                  new_elf, c_string_to_object("\".text\""));
  cl_funcall(3, c_string_to_object("set-data"), bytes, new_text);
  cl_eval(cl_list(3, c_string_to_object("write-elf"), new_elf, c_string_to_object(buff)));

  CAMLreturn( Val_unit );
}
