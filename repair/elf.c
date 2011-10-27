#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <stdio.h>
#include <stdlib.h>

#define ELF32 1
#define ELF64 2
#define READ(buf,x,y) int_from_bytes(buf,(16 + (class * x)),(class * y))
#define CREAD(buf,x,y) int_from_bytes(buf,(16 + (class * x)),y)

/* Elf Header */
#define CLASS(buf)   buf[4]
#define TYPE(buf)    READ(buf, 0,  1)
#define MACHINE(buf) READ(buf, 1,  1)
#define VERSION(buf) READ(buf, 2,  1)
#define ENTRY(buf)   READ(buf, 4,  2)
#define PHOFF(buf)   READ(buf, 8,  2)
#define SHOFF(buf)   READ(buf, 12, 2)
#define FLAGS(buf)   CREAD(buf, 16, 2)
#define EH_SZ(buf)   CREAD(buf, 18, 1)
#define PH_E_SZ(buf) CREAD(buf, 19, 1)
#define PH_NUM(buf)  CREAD(buf, 20, 1)
#define SH_E_SZ(buf) CREAD(buf, 21, 1)
#define SH_NUM(buf)  CREAD(buf, 22, 1)
#define SH_SRND(buf) CREAD(buf, 23, 1)

/* Section Header */
#define SH_NAME(buf,shoff,shesz,ind) int_from_bytes(buf,(shoff + (shesz * ind)), (class * 2))
#define SH_ADDR(buf,shoff,shesz,ind) int_from_bytes(buf,(shoff + (class * 8) + (shesz * ind)), (class * 4))
#define SH_OFF(buf,shoff,shesz,ind) int_from_bytes(buf,(shoff + (class * 12) + (shesz * ind)), (class * 4))
#define SH_SIZE(buf,shoff,shesz,ind) int_from_bytes(buf,(shoff + (class * 16) + (shesz * ind)), (class * 4))

/* Forward declaration of C functions */
int class;
char *read_raw(char *path);
int file_size(char *path);
void write_raw(char *path, char *buf, int size);
void check_magic(char *buf);
int elf_class(char *buf);
unsigned int int_from_bytes(char *buf, int pos, int num);
void print_header_info(char *buf);
int text_section_header(char *buf);
char *section_data(char *buf, int id);
int get_text_address(char *path);
int get_text_offset(char *path);
int get_text_data_size(char *path);
char *get_text_data(char *path);

/*********************************************************************
 *
 *                           OCaml functions
 *
 *********************************************************************/
CAMLprim value text_data(value path){
  CAMLparam1(path);
  CAMLlocal1(ml_bytes);
  char *name = String_val(path);

  int i;
  int size=get_text_data_size(name);
  unsigned char *text=get_text_data(name);
  ml_bytes = caml_alloc(size, 0);

  for(i=0; i<size; i++) Store_field(ml_bytes, i, Val_int(text[i]));

  CAMLreturn( ml_bytes );
}

CAMLprim value text_address(value path){
  CAMLparam1(path);
  char *name = String_val(path);
  CAMLreturn(Val_int(get_text_address(name)));
}

CAMLprim value update_text(value path, value offset, value ml_bytes){
  // process parameters
  CAMLparam3(path, offset, ml_bytes);
  int off = Int_val(offset);
  char *name = String_val(path);

  // raw array with new text
  char *buf = read_raw(name);
  long int size = file_size(name);

  int length = Wosize_val(ml_bytes);
  int i;
  for (i=0; i<length; i++)
    buf[offset+i] = Int_val(Field(ml_bytes, i));

  // write out the resulting byte array
  write_raw(name, buf, size);

  CAMLreturn( Val_unit );
}

/*********************************************************************
 *
 *                        ELF helper functions
 *
 *********************************************************************/

int get_text_address(char *path){
  char *buf = read_raw(path);
  int text_shd;

  /* sanity check */
  check_magic(buf);
  class = CLASS(buf);
  if(!((class == 1) || (class == 2)))
    printf("invalid elf class\n");

  /* find the text section */
  if((text_shd = text_section_header(buf)) < 0)
    printf("text sectin not found\n");

  /* get the size of the text section */
  int shoff = SHOFF(buf);
  int shesz = SH_E_SZ(buf);
  int addr = SH_ADDR(buf,shoff,shesz,text_shd);

  free(buf);
  return addr;
}

int get_text_offset(char *path){
  char *buf = read_raw(path);
  int text_shd;

  /* sanity check */
  check_magic(buf);
  class = CLASS(buf);
  if(!((class == 1) || (class == 2)))
    printf("invalid elf class\n");

  /* find the text section */
  if((text_shd = text_section_header(buf)) < 0)
    printf("text sectin not found\n");

  /* get the size of the text section */
  int shoff = SHOFF(buf);
  int shesz = SH_E_SZ(buf);
  int off = SH_OFF(buf,shoff,shesz,text_shd);

  free(buf);
  return off;
}

int get_text_data_size(char *path){
  char *buf = read_raw(path);
  int text_shd;

  /* sanity check */
  check_magic(buf);
  class = CLASS(buf);
  if(!((class == 1) || (class == 2)))
    printf("invalid elf class\n");

  /* find the text section */
  if((text_shd = text_section_header(buf)) < 0)
    printf("text sectin not found\n");

  /* get the size of the text section */
  int shoff = SHOFF(buf);
  int shesz = SH_E_SZ(buf);
  int size = SH_SIZE(buf,shoff,shesz,text_shd);

  free(buf);
  return size;
}

char *get_text_data(char *path){
  char *buf = read_raw(path);
  int text_shd;

  /* sanity check */
  check_magic(buf);
  class = CLASS(buf);
  if(!((class == 1) || (class == 2)))
    printf("invalid elf class\n");

  /* find the text section */
  if((text_shd = text_section_header(buf)) < 0)
    printf("text sectin not found\n");

  /* get the .text data */
  unsigned char *text = section_data(buf, text_shd);

  free(buf);
  return text;
}

char *section_data(char *buf, int id){
  int shoff = SHOFF(buf);
  int shesz = SH_E_SZ(buf);
  int off = SH_OFF(buf,shoff,shesz,id);
  int size = SH_SIZE(buf,shoff,shesz,id);
  int i, tmp;
  char *data;

  data = (unsigned char *) malloc(size+1);

  for(i=0;i<size;i++)
    data[i] = buf[off+i];
  return data;
}

int text_section_header(char *buf){
  int shoff = SHOFF(buf);
  int shesz = SH_E_SZ(buf);
  int shnum = SH_NUM(buf);
  int shsrnd = SH_SRND(buf);

  int sh_names[shnum];
  int sh_offsets[shnum];

  /* print the names of the section headers */
  int i;
  for(i=0;i<shnum;i++){
    sh_names[i] = SH_NAME(buf,shoff,shesz,i);
    sh_offsets[i] = SH_OFF(buf,shoff,shesz,i);
  }

  /* find the .text section */
  int str_off = sh_offsets[shsrnd];
  char name[256];
  int j;
  for(i=0;i<shnum;i++){
    j=-1;
    do {
      j++;
      name[j] = buf[str_off + sh_names[i] + j];
    } while (name[j] != 0);

    /* show the section names */
    /* printf("name[%d] %s\n", i, name); */

    if ((name[0] == '.') &&
        (name[1] == 't') &&
        (name[2] == 'e') &&
        (name[3] == 'x') &&
        (name[4] == 't') &&
        (name[5] == 0))
      return i;
  }

  return -1;
}

void print_header_info(char *buf){
  switch (class){
  case ELF32: printf("class\t32-bit\n"); break;
  case ELF64: printf("class\t64-bit\n"); break;
  }

  /* print out some information about the elf file */
  printf("type\t%d\n", TYPE(buf));
  printf("machine\t%d\n", MACHINE(buf));
  printf("version\t%d\n", VERSION(buf));
  printf("entry\t%d\n", ENTRY(buf));
  printf("phoff\t%d\n", PHOFF(buf));
  printf("shoff\t%d\n", SHOFF(buf));
  printf("flags\t%d\n", FLAGS(buf));
  printf("eh_sz\t%d\n", EH_SZ(buf));
  printf("ph_e_sz\t%d\n", PH_E_SZ(buf));
  printf("ph_num\t%d\n", PH_NUM(buf));
  printf("sh_e_sz\t%d\n", SH_E_SZ(buf));
  printf("sh_num\t%d\n", SH_NUM(buf));
  printf("sh_srnd\t%d\n", SH_SRND(buf));
}

unsigned int int_from_bytes(char *buf, int pos, int num){
  unsigned char tmp[num];
  int i, t, acc = 0;
  for(i=0;i<num;i++) tmp[i] = buf[(pos + i)];
  for(i=0;i<num;i++) acc += tmp[i] << (8 * i);
  return acc;
}

void check_magic(char *buf){
  const char *magic = "ELF";
  int i;
  for(i=0; i<4; i++)
    if(!(buf[i] == magic[i]))
      printf("ELF Magic number test failed.");
}

char *read_raw(char *path){
  FILE *file;
  char *buf;
  unsigned long length;

  /* open file */
  file = fopen(path, "rb");

  /* find file length */
  fseek(file, 0, SEEK_END);
  length=ftell(file);
  fseek(file, 0, SEEK_SET);

  /* allocate memory */
  buf= (unsigned char *) malloc(length+1);

  /* read file */
  fread(buf, length, 1, file);
  fclose(file);

  return buf;
}

int file_size(char *path){
  FILE *file;
  unsigned long length;

  file = fopen(path, "rb");
  fseek(file, 0, SEEK_END);
  length = ftell(file);
  fclose(file);

  return length;
}

void write_raw(char *path, char *buf, int size){
  FILE *file;
  file = fopen(path, "w");
  int i;
  for(i=0;i<size;i++)
    fputc(buf[i], file);
  fclose(file);
}
