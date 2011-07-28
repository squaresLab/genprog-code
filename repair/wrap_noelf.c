#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <string.h>
#include <stdio.h>

extern int main_dll_ELF(int argc, char **argv);

CAMLprim value start_elf(value unit){
  caml_failwith("start_elf: libelf.so support not compiled in"); 
  return Val_unit;
}

CAMLprim value stop_elf(value unit){
  caml_failwith("stop_elf: libelf.so support not compiled in"); 
  return Val_unit;
}

CAMLprim value read_elf(value path){
  caml_failwith("read_elf: libelf.so support not compiled in"); 
  return Val_unit;
}

CAMLprim value write_elf(value elf, value path){
  caml_failwith("write_elf: libelf.so support not compiled in"); 
  return Val_unit;
}

CAMLprim value show_memory_layout(value elf){
  caml_failwith("show_memory_layout: libelf.so support not compiled in"); 
  return Val_unit;
}

CAMLprim value get_text(value elf){
  caml_failwith("get_text: libelf.so support not compiled in"); 
  return Val_unit;
}

CAMLprim value get_text_offset(value elf){
  caml_failwith("get_text_offset: libelf.so support not compiled in"); 
  return Val_unit;
}

CAMLprim value write_w_text(value elf, value path, value ml_bytes){
  caml_failwith("write_w_text: libelf.so support not compiled in"); 
  return Val_unit;
}
