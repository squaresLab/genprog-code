#line 451 "jsonlint.c"
void __globinit_jsonlint_comb(void) ;
#line 918 "json.c"
void *_coverage_fout ;
#line 214 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stddef.h"
typedef unsigned long size_t;
#line 144 "/usr/include/bits/types.h"
typedef long __off_t;
#line 145 "/usr/include/bits/types.h"
typedef long __off64_t;
#line 197 "/usr/include/sys/types.h"
typedef int int32_t;
#line 46 "/usr/include/stdio.h"
struct _IO_FILE;
#line 46
struct _IO_FILE;
#line 46 "/usr/include/stdio.h"
typedef struct _IO_FILE FILE;
#line 177 "/usr/include/libio.h"
typedef void _IO_lock_t;
#line 183 "/usr/include/libio.h"
struct _IO_marker {
   struct _IO_marker *_next ;
   struct _IO_FILE *_sbuf ;
   int _pos ;
};
#line 268 "/usr/include/libio.h"
struct _IO_FILE {
   int _flags ;
   char *_IO_read_ptr ;
   char *_IO_read_end ;
   char *_IO_read_base ;
   char *_IO_write_base ;
   char *_IO_write_ptr ;
   char *_IO_write_end ;
   char *_IO_buf_base ;
   char *_IO_buf_end ;
   char *_IO_save_base ;
   char *_IO_backup_base ;
   char *_IO_save_end ;
   struct _IO_marker *_markers ;
   struct _IO_FILE *_chain ;
   int _fileno ;
   int _flags2 ;
   __off_t _old_offset ;
   unsigned short _cur_column ;
   signed char _vtable_offset ;
   char _shortbuf[1] ;
   _IO_lock_t *_lock ;
   __off64_t _offset ;
   void *__pad1 ;
   void *__pad2 ;
   void *__pad3 ;
   void *__pad4 ;
   size_t __pad5 ;
   int _mode ;
   char _unused2[(15UL * sizeof(int ) - 4UL * sizeof(void *)) - sizeof(size_t )] ;
};
#line 106 "/usr/include/getopt.h"
struct option {
   char const   *name ;
   int has_arg ;
   int *flag ;
   int val ;
};
#line 49 "/usr/include/stdint.h"
typedef unsigned char uint8_t;
#line 50 "/usr/include/stdint.h"
typedef unsigned short uint16_t;
#line 52 "/usr/include/stdint.h"
typedef unsigned int uint32_t;
#line 24 "json.h"
enum __anonenum_json_type_26 {
    JSON_NONE = 0,
    JSON_ARRAY_BEGIN = 1,
    JSON_OBJECT_BEGIN = 2,
    JSON_ARRAY_END = 3,
    JSON_OBJECT_END = 4,
    JSON_INT = 5,
    JSON_FLOAT = 6,
    JSON_STRING = 7,
    JSON_KEY = 8,
    JSON_TRUE = 9,
    JSON_FALSE = 10,
    JSON_NULL = 11
} ;
#line 24 "json.h"
typedef enum __anonenum_json_type_26 json_type;
#line 75 "json.h"
struct __anonstruct_json_config_28 {
   uint32_t buffer_initial_size ;
   uint32_t max_nesting ;
   uint32_t max_data ;
   int allow_c_comments ;
   int allow_yaml_comments ;
   void *(*user_calloc)(size_t nmemb , size_t size ) ;
   void *(*user_realloc)(void *ptr , size_t size ) ;
};
#line 75 "json.h"
typedef struct __anonstruct_json_config_28 json_config;
#line 85 "json.h"
struct json_parser {
   json_config config ;
   int (*callback)(void *userdata , int type , char const   *data ,
                   uint32_t length ) ;
   void *userdata ;
   uint8_t state ;
   uint8_t save_state ;
   uint8_t expecting_key ;
   uint16_t unicode_multi ;
   json_type type ;
   uint8_t *stack ;
   uint32_t stack_offset ;
   uint32_t stack_size ;
   char *buffer ;
   uint32_t buffer_size ;
   uint32_t buffer_offset ;
};
#line 85 "json.h"
typedef struct json_parser json_parser;
#line 110 "json.h"
struct json_printer {
   int (*callback)(void *userdata , char const   *s , uint32_t length ) ;
   void *userdata ;
   char *indentstr ;
   int indentlevel ;
   int afterkey ;
   int enter_object ;
   int first ;
};
#line 110 "json.h"
typedef struct json_printer json_printer;
#line 176 "json.h"
struct stack_elem {
   void *val ;
   char *key ;
   uint32_t key_length ;
};
#line 176 "json.h"
struct json_parser_dom {
   struct stack_elem *stack ;
   uint32_t stack_size ;
   uint32_t stack_offset ;
   void *(*user_calloc)(size_t nmemb , size_t size ) ;
   void *(*user_realloc)(void *ptr , size_t size ) ;
   void *root_structure ;
   void *(*create_structure)(int  , int  ) ;
   void *(*create_data)(int  , char const   * , uint32_t  ) ;
   int (*append)(void * , char * , uint32_t  , void * ) ;
};
#line 176 "json.h"
typedef struct json_parser_dom json_parser_dom;
#line 222 "jsonlint.c"
struct json_val;
#line 222
struct json_val;
#line 222 "jsonlint.c"
struct json_val_elem {
   char *key ;
   uint32_t key_length ;
   struct json_val *val ;
};
#line 228 "jsonlint.c"
union __anonunion_u_29 {
   char *data ;
   struct json_val **array ;
   struct json_val_elem **object ;
};
#line 228 "jsonlint.c"
struct json_val {
   int type ;
   int length ;
   union __anonunion_u_29 u ;
};
#line 228 "jsonlint.c"
typedef struct json_val json_val_t;
#line 43 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stdarg.h"
typedef __builtin_va_list __gnuc_va_list;
#line 105 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stdarg.h"
typedef __gnuc_va_list va_list;
#line 544 "json.c"
struct action_descr {
   int (*call)(json_parser *parser ) ;
   uint8_t type ;
   uint8_t state ;
   uint8_t dobuffer ;
};
/* compiler builtin: 
   void __builtin_varargs_start(__builtin_va_list  ) ;  */
/* compiler builtin: 
   int __builtin_strcmp(char const   * , char const   * ) ;  */
/* compiler builtin: 
   void *__builtin___memmove_chk(void * , void const   * , unsigned long  ,
                                 unsigned long  ) ;  */
/* compiler builtin: 
   char *__builtin_strpbrk(char const   * , char const   * ) ;  */
/* compiler builtin: 
   void *__builtin_memcpy(void * , void const   * , unsigned long  ) ;  */
/* compiler builtin: 
   double __builtin_exp(double  ) ;  */
/* compiler builtin: 
   long double __builtin_nanl(char const   * ) ;  */
/* compiler builtin: 
   double __builtin_cos(double  ) ;  */
/* compiler builtin: 
   char *__builtin_strchr(char * , int  ) ;  */
/* compiler builtin: 
   float __builtin_atan2f(float  , float  ) ;  */
/* compiler builtin: 
   void *__builtin___memcpy_chk(void * , void const   * , unsigned long  ,
                                unsigned long  ) ;  */
/* compiler builtin: 
   double __builtin_asin(double  ) ;  */
/* compiler builtin: 
   int __builtin_ctz(unsigned int  ) ;  */
/* compiler builtin: 
   char *__builtin_stpcpy(char * , char const   * ) ;  */
/* compiler builtin: 
   double __builtin_nans(char const   * ) ;  */
/* compiler builtin: 
   long double __builtin_atan2l(long double  , long double  ) ;  */
/* compiler builtin: 
   float __builtin_logf(float  ) ;  */
/* compiler builtin: 
   int __builtin___fprintf_chk(void * , int  , char const   *  , ...) ;  */
/* compiler builtin: 
   int __builtin___vsprintf_chk(char * , int  , unsigned long  ,
                                char const   * , __builtin_va_list  ) ;  */
/* compiler builtin: 
   char *__builtin___strncpy_chk(char * , char const   * , unsigned long  ,
                                 unsigned long  ) ;  */
/* compiler builtin: 
   float __builtin_log10f(float  ) ;  */
/* compiler builtin: 
   double __builtin_atan(double  ) ;  */
/* compiler builtin: 
   void *__builtin_alloca(unsigned long  ) ;  */
/* compiler builtin: 
   void __builtin_va_end(__builtin_va_list  ) ;  */
/* compiler builtin: 
   int __builtin_strncmp(char const   * , char const   * , unsigned long  ) ;  */
/* compiler builtin: 
   double __builtin_sin(double  ) ;  */
/* compiler builtin: 
   long double __builtin_logl(long double  ) ;  */
/* compiler builtin: 
   float __builtin_coshf(float  ) ;  */
/* compiler builtin: 
   void *__builtin___mempcpy_chk(void * , void const   * , unsigned long  ,
                                 unsigned long  ) ;  */
/* compiler builtin: 
   char *__builtin___strcat_chk(char * , char const   * , unsigned long  ) ;  */
/* compiler builtin: 
   float __builtin_nansf(char const   * ) ;  */
/* compiler builtin: 
   void *__builtin_memset(void * , int  , int  ) ;  */
/* compiler builtin: 
   void __builtin_va_copy(__builtin_va_list  , __builtin_va_list  ) ;  */
/* compiler builtin: 
   float __builtin_sinhf(float  ) ;  */
/* compiler builtin: 
   long double __builtin_log10l(long double  ) ;  */
/* compiler builtin: 
   long double __builtin_coshl(long double  ) ;  */
/* compiler builtin: 
   int __builtin_ffs(unsigned int  ) ;  */
/* compiler builtin: 
   float __builtin_asinf(float  ) ;  */
/* compiler builtin: 
   long double __builtin_nansl(char const   * ) ;  */
/* compiler builtin: 
   double __builtin_frexp(double  , int * ) ;  */
/* compiler builtin: 
   double __builtin_tan(double  ) ;  */
/* compiler builtin: 
   long double __builtin_sinhl(long double  ) ;  */
/* compiler builtin: 
   float __builtin_frexpf(float  , int * ) ;  */
/* compiler builtin: 
   long double __builtin_asinl(long double  ) ;  */
/* compiler builtin: 
   void *__builtin_frame_address(unsigned int  ) ;  */
/* compiler builtin: 
   double __builtin_floor(double  ) ;  */
/* compiler builtin: 
   float __builtin_tanhf(float  ) ;  */
/* compiler builtin: 
   int __builtin_parityl(unsigned long  ) ;  */
/* compiler builtin: 
   int __builtin_clzl(unsigned long  ) ;  */
/* compiler builtin: 
   double __builtin_powi(double  , int  ) ;  */
/* compiler builtin: 
   long double __builtin_frexpl(long double  , int * ) ;  */
/* compiler builtin: 
   float __builtin_atanf(float  ) ;  */
/* compiler builtin: 
   float __builtin_huge_valf(void) ;  */
/* compiler builtin: 
   float __builtin_sqrtf(float  ) ;  */
/* compiler builtin: 
   float __builtin_fmodf(float  ) ;  */
/* compiler builtin: 
   unsigned long __builtin_object_size(void * , int  ) ;  */
/* compiler builtin: 
   void __builtin_va_arg(__builtin_va_list  , unsigned long  , void * ) ;  */
/* compiler builtin: 
   void __builtin_stdarg_start(__builtin_va_list  ) ;  */
/* compiler builtin: 
   long double __builtin_tanhl(long double  ) ;  */
/* compiler builtin: 
   double __builtin_nan(char const   * ) ;  */
/* compiler builtin: 
   void __builtin_return(void const   * ) ;  */
/* compiler builtin: 
   long double __builtin_atanl(long double  ) ;  */
/* compiler builtin: 
   long double __builtin_huge_vall(void) ;  */
/* compiler builtin: 
   float __builtin_inff(void) ;  */
/* compiler builtin: 
   long double __builtin_sqrtl(long double  ) ;  */
/* compiler builtin: 
   long double __builtin_fmodl(long double  ) ;  */
/* compiler builtin: 
   int __builtin___printf_chk(int  , char const   *  , ...) ;  */
/* compiler builtin: 
   float __builtin_floorf(float  ) ;  */
/* compiler builtin: 
   float __builtin_fabsf(float  ) ;  */
/* compiler builtin: 
   int __builtin_popcountll(unsigned long long  ) ;  */
/* compiler builtin: 
   int __builtin___sprintf_chk(char * , int  , unsigned long  , char const   * 
                               , ...) ;  */
/* compiler builtin: 
   int __builtin___vprintf_chk(int  , char const   * , __builtin_va_list  ) ;  */
/* compiler builtin: 
   int __builtin___snprintf_chk(char * , unsigned long  , int  ,
                                unsigned long  , char const   *  , ...) ;  */
/* compiler builtin: 
   long double __builtin_infl(void) ;  */
/* compiler builtin: 
   void *__builtin_mempcpy(void * , void const   * , unsigned long  ) ;  */
/* compiler builtin: 
   long double __builtin_floorl(long double  ) ;  */
/* compiler builtin: 
   int __builtin_ctzl(unsigned long  ) ;  */
/* compiler builtin: 
   long double __builtin_fabsl(long double  ) ;  */
/* compiler builtin: 
   int __builtin_clz(unsigned int  ) ;  */
/* compiler builtin: 
   double __builtin_fabs(double  ) ;  */
/* compiler builtin: 
   int __builtin_popcount(unsigned int  ) ;  */
/* compiler builtin: 
   double __builtin_ceil(double  ) ;  */
/* compiler builtin: 
   double __builtin_ldexp(double  , int  ) ;  */
/* compiler builtin: 
   float __builtin_sinf(float  ) ;  */
/* compiler builtin: 
   float __builtin_acosf(float  ) ;  */
/* compiler builtin: 
   int __builtin___vsnprintf_chk(char * , unsigned long  , int  ,
                                 unsigned long  , char const   * ,
                                 __builtin_va_list  ) ;  */
/* compiler builtin: 
   double __builtin_sinh(double  ) ;  */
/* compiler builtin: 
   int __builtin_ffsll(unsigned long long  ) ;  */
/* compiler builtin: 
   char *__builtin___strcpy_chk(char * , char const   * , unsigned long  ) ;  */
/* compiler builtin: 
   double __builtin_inf(void) ;  */
/* compiler builtin: 
   void __builtin_prefetch(void const   *  , ...) ;  */
/* compiler builtin: 
   long double __builtin_sinl(long double  ) ;  */
/* compiler builtin: 
   long double __builtin_acosl(long double  ) ;  */
/* compiler builtin: 
   double __builtin_sqrt(double  ) ;  */
/* compiler builtin: 
   double __builtin_fmod(double  ) ;  */
/* compiler builtin: 
   char *__builtin_strcpy(char * , char const   * ) ;  */
/* compiler builtin: 
   float __builtin_ceilf(float  ) ;  */
/* compiler builtin: 
   void *__builtin_return_address(unsigned int  ) ;  */
/* compiler builtin: 
   char *__builtin___stpcpy_chk(char * , char const   * , unsigned long  ) ;  */
/* compiler builtin: 
   float __builtin_tanf(float  ) ;  */
/* compiler builtin: 
   int __builtin_parityll(unsigned long long  ) ;  */
/* compiler builtin: 
   float __builtin_ldexpf(float  , int  ) ;  */
/* compiler builtin: 
   int __builtin_types_compatible_p(unsigned long  , unsigned long  ) ;  */
/* compiler builtin: 
   double __builtin_log10(double  ) ;  */
/* compiler builtin: 
   float __builtin_expf(float  ) ;  */
/* compiler builtin: 
   int __builtin_clzll(unsigned long long  ) ;  */
/* compiler builtin: 
   double __builtin_tanh(double  ) ;  */
/* compiler builtin: 
   int __builtin_constant_p(int  ) ;  */
/* compiler builtin: 
   long double __builtin_ceill(long double  ) ;  */
/* compiler builtin: 
   long double __builtin_tanl(long double  ) ;  */
/* compiler builtin: 
   double __builtin_log(double  ) ;  */
/* compiler builtin: 
   long double __builtin_ldexpl(long double  , int  ) ;  */
/* compiler builtin: 
   long double __builtin_expl(long double  ) ;  */
/* compiler builtin: 
   int __builtin_popcountl(unsigned long  ) ;  */
/* compiler builtin: 
   void *__builtin___memset_chk(void * , int  , unsigned long  , unsigned long  ) ;  */
/* compiler builtin: 
   char *__builtin___strncat_chk(char * , char const   * , unsigned long  ,
                                 unsigned long  ) ;  */
/* compiler builtin: 
   double __builtin_huge_val(void) ;  */
/* compiler builtin: 
   __builtin_va_list __builtin_next_arg(void) ;  */
/* compiler builtin: 
   float __builtin_powif(float  , int  ) ;  */
/* compiler builtin: 
   int __builtin___vfprintf_chk(void * , int  , char const   * ,
                                __builtin_va_list  ) ;  */
/* compiler builtin: 
   float __builtin_modff(float  , float * ) ;  */
/* compiler builtin: 
   double __builtin_atan2(double  , double  ) ;  */
/* compiler builtin: 
   char *__builtin_strncpy(char * , char const   * , unsigned long  ) ;  */
/* compiler builtin: 
   long double __builtin_powil(long double  , int  ) ;  */
/* compiler builtin: 
   float __builtin_cosf(float  ) ;  */
/* compiler builtin: 
   unsigned long __builtin_strspn(char const   * , char const   * ) ;  */
/* compiler builtin: 
   long double __builtin_modfl(long double  , long double * ) ;  */
/* compiler builtin: 
   int __builtin_parity(unsigned int  ) ;  */
/* compiler builtin: 
   double __builtin_cosh(double  ) ;  */
/* compiler builtin: 
   char *__builtin_strncat(char * , char const   * , unsigned long  ) ;  */
/* compiler builtin: 
   long __builtin_expect(long  , long  ) ;  */
/* compiler builtin: 
   double __builtin_acos(double  ) ;  */
/* compiler builtin: 
   long double __builtin_cosl(long double  ) ;  */
/* compiler builtin: 
   void __builtin_va_start(__builtin_va_list  ) ;  */
/* compiler builtin: 
   int __builtin_ctzll(unsigned long long  ) ;  */
/* compiler builtin: 
   unsigned long __builtin_strcspn(char const   * , char const   * ) ;  */
/* compiler builtin: 
   int __builtin_ffsl(unsigned long  ) ;  */
/* compiler builtin: 
   float __builtin_nanf(char const   * ) ;  */
#line 1 "jsonlint.o"
/* #pragma merger(0,"/tmp/cil-7zlYBOB2.i","-Wall,-Os,-fPIC") */
#line 148 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) int atoi(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 589
extern  __attribute__((__nothrow__)) void *malloc(size_t __size )  __attribute__((__malloc__)) ;
#line 591
extern  __attribute__((__nothrow__)) void *calloc(size_t __nmemb ,
                                                  size_t __size )  __attribute__((__malloc__)) ;
#line 600
extern  __attribute__((__nothrow__)) void *realloc(void *__ptr , size_t __size )  __attribute__((__warn_unused_result__,
__malloc__)) ;
#line 603
extern  __attribute__((__nothrow__)) void free(void *__ptr ) ;
#line 646
extern  __attribute__((__nothrow__, __noreturn__)) void exit(int __status ) ;
#line 142 "/usr/include/stdio.h"
extern struct _IO_FILE *stdin ;
#line 143
extern struct _IO_FILE *stdout ;
#line 144
extern struct _IO_FILE *stderr ;
#line 213
extern int fclose(FILE *__stream ) ;
#line 248
extern FILE *fopen(char const   * __restrict  __filename ,
                   char const   * __restrict  __modes ) ;
#line 327
extern int fprintf(FILE * __restrict  __stream ,
                   char const   * __restrict  __format  , ...) ;
#line 333
extern int printf(char const   * __restrict  __format  , ...) ;
#line 610
extern size_t fread(void * __restrict  __ptr , size_t __size , size_t __n ,
                    FILE * __restrict  __stream ) ;
#line 616
extern size_t fwrite(void const   * __restrict  __ptr , size_t __size ,
                     size_t __n , FILE * __restrict  __s ) ;
#line 38 "/usr/include/string.h"
extern  __attribute__((__nothrow__)) void *memcpy(void * __restrict  __dest ,
                                                  void const   * __restrict  __src ,
                                                  size_t __n )  __attribute__((__nonnull__(1,2))) ;
#line 59
extern  __attribute__((__nothrow__)) void *memset(void *__s , int __c ,
                                                  size_t __n )  __attribute__((__nonnull__(1))) ;
#line 99
extern  __attribute__((__nothrow__)) int strcmp(char const   *__s1 ,
                                                char const   *__s2 )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 130
extern  __attribute__((__nothrow__)) char *strdup(char const   *__s )  __attribute__((__nonnull__(1),
__malloc__)) ;
#line 256
extern  __attribute__((__nothrow__)) char *strerror(int __errnum ) ;
#line 59 "/usr/include/getopt.h"
extern char *optarg ;
#line 73
extern int optind ;
#line 159
extern  __attribute__((__nothrow__)) int getopt_long(int ___argc ,
                                                     char * const  *___argv ,
                                                     char const   *__shortopts ,
                                                     struct option  const  *__longopts ,
                                                     int *__longind ) ;
#line 43 "/usr/include/bits/errno.h"
extern  __attribute__((__nothrow__)) int *__errno_location(void)  __attribute__((__const__)) ;
#line 124 "json.h"
int json_parser_init(json_parser *parser , json_config *config ,
                     int (*callback)(void *userdata , int type ,
                                     char const   *data , uint32_t length ) ,
                     void *userdata ) ;
#line 128
int json_parser_free(json_parser *parser ) ;
#line 134
int json_parser_string(json_parser *parser , char const   *s , uint32_t length ,
                       uint32_t *processed ) ;
#line 142
int json_parser_is_done(json_parser *parser ) ;
#line 145
int json_print_init(json_printer *printer ,
                    int (*callback)(void *userdata , char const   *s ,
                                    uint32_t length ) , void *userdata ) ;
#line 149
int json_print_free(json_printer *printer ) ;
#line 152
int json_print_pretty(json_printer *printer , int type , char const   *data ,
                      uint32_t length ) ;
#line 197
int json_parser_dom_init(json_parser_dom *dom ,
                         void *(*create_structure)(int  , int  ) ,
                         void *(*create_data)(int  , char const   * , uint32_t  ) ,
                         int (*append)(void * , char * , uint32_t  , void * ) ) ;
#line 205
int json_parser_dom_callback(void *userdata , int type , char const   *data ,
                             uint32_t length ) ;
#line 24 "jsonlint.c"
char *indent_string  =    (char *)((void *)0);
#line 26 "jsonlint.c"
char *string_of_errors[13]  = 
#line 26
  {      (char *)0,      (char *)"out of memory",      (char *)"bad character",      (char *)"stack empty", 
        (char *)"pop unexpected mode",      (char *)"nesting limit",      (char *)"data limit",      (char *)"comment not allowed by config", 
        (char *)"unexpected char",      (char *)"missing unicode low surrogate",      (char *)"unexpected unicode low surrogate",      (char *)"error comma out of structure", 
        (char *)"error in a callback"};
#line 42 "jsonlint.c"
static int printchannel(void *userdata , char const   *data , uint32_t length ) 
{ FILE *channel ;
  int ret ;
  size_t tmp ;

  {
#line 42
  fprintf(_coverage_fout, "1\n");
#line 42
  fflush(_coverage_fout);
#line 44
  channel = (FILE *)userdata;
#line 42
  fprintf(_coverage_fout, "2\n");
#line 42
  fflush(_coverage_fout);
#line 47
  tmp = fwrite((void const   */* __restrict  */)data, (unsigned long )length,
               1UL, (FILE */* __restrict  */)channel);
#line 42
  fprintf(_coverage_fout, "3\n");
#line 42
  fflush(_coverage_fout);
#line 47
  ret = (int )tmp;
#line 42
  fprintf(_coverage_fout, "4\n");
#line 42
  fflush(_coverage_fout);
#line 48
  return (0);
}
}
#line 51 "jsonlint.c"
static int prettyprint(void *userdata , int type , char const   *data ,
                       uint32_t length ) 
{ json_printer *printer ;
  int tmp ;

  {
#line 51
  fprintf(_coverage_fout, "5\n");
#line 51
  fflush(_coverage_fout);
#line 53
  printer = (json_printer *)userdata;
#line 51
  fprintf(_coverage_fout, "6\n");
#line 51
  fflush(_coverage_fout);
#line 55
  tmp = json_print_pretty(printer, type, data, length);
#line 51
  fprintf(_coverage_fout, "7\n");
#line 51
  fflush(_coverage_fout);
#line 55
  return (tmp);
}
}
#line 58 "jsonlint.c"
FILE *open_filename(char const   *filename , char const   *opt , int is_input ) 
{ FILE *input ;
  int *tmp ;
  char *tmp___0 ;
  int tmp___1 ;

  {
#line 58
  fprintf(_coverage_fout, "18\n");
#line 58
  fflush(_coverage_fout);
#line 61
  tmp___1 = strcmp(filename, "-");
#line 58
  fprintf(_coverage_fout, "19\n");
#line 58
  fflush(_coverage_fout);
#line 61
  if (tmp___1 == 0) {
#line 61
    fprintf(_coverage_fout, "10\n");
#line 61
    fflush(_coverage_fout);
#line 62
    if (is_input) {
#line 62
      fprintf(_coverage_fout, "8\n");
#line 62
      fflush(_coverage_fout);
#line 62
      input = stdin;
    } else {
#line 62
      fprintf(_coverage_fout, "9\n");
#line 62
      fflush(_coverage_fout);
#line 62
      input = stdout;
    }
  } else {
#line 61
    fprintf(_coverage_fout, "16\n");
#line 61
    fflush(_coverage_fout);
#line 64
    input = fopen((char const   */* __restrict  */)filename,
                  (char const   */* __restrict  */)opt);
#line 61
    fprintf(_coverage_fout, "17\n");
#line 61
    fflush(_coverage_fout);
#line 65
    if (! input) {
#line 65
      fprintf(_coverage_fout, "11\n");
#line 65
      fflush(_coverage_fout);
#line 66
      tmp = __errno_location();
#line 65
      fprintf(_coverage_fout, "12\n");
#line 65
      fflush(_coverage_fout);
#line 66
      tmp___0 = strerror(*tmp);
#line 65
      fprintf(_coverage_fout, "13\n");
#line 65
      fflush(_coverage_fout);
#line 66
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"error: cannot open %s: %s",
              filename, tmp___0);
#line 65
      fprintf(_coverage_fout, "14\n");
#line 65
      fflush(_coverage_fout);
#line 67
      return ((FILE *)((void *)0));
    } else {
#line 65
      fprintf(_coverage_fout, "15\n");
#line 65
      fflush(_coverage_fout);

    }
  }
#line 58
  fprintf(_coverage_fout, "20\n");
#line 58
  fflush(_coverage_fout);
#line 70
  return (input);
}
}
#line 73 "jsonlint.c"
void close_filename(char const   *filename , FILE *file ) 
{ int tmp ;

  {
#line 73
  fprintf(_coverage_fout, "23\n");
#line 73
  fflush(_coverage_fout);
#line 75
  tmp = strcmp(filename, "-");
#line 73
  fprintf(_coverage_fout, "24\n");
#line 73
  fflush(_coverage_fout);
#line 75
  if (tmp != 0) {
#line 75
    fprintf(_coverage_fout, "21\n");
#line 75
    fflush(_coverage_fout);
#line 76
    fclose(file);
  } else {
#line 75
    fprintf(_coverage_fout, "22\n");
#line 75
    fflush(_coverage_fout);

  }
#line 73
  fprintf(_coverage_fout, "25\n");
#line 73
  fflush(_coverage_fout);
#line 77
  return;
}
}
#line 79 "jsonlint.c"
int process_file(json_parser *parser , FILE *input , int *retlines ,
                 int *retcols ) 
{ char buffer[4096] ;
  int ret ;
  int32_t read ;
  int lines ;
  int col ;
  int i ;
  uint32_t processed ;
  size_t tmp ;

  {
#line 79
  fprintf(_coverage_fout, "46\n");
#line 79
  fflush(_coverage_fout);
#line 82
  ret = 0;
#line 79
  fprintf(_coverage_fout, "47\n");
#line 79
  fflush(_coverage_fout);
#line 86
  lines = 1;
#line 79
  fprintf(_coverage_fout, "48\n");
#line 79
  fflush(_coverage_fout);
#line 87
  col = 0;
#line 79
  fprintf(_coverage_fout, "49\n");
#line 79
  fflush(_coverage_fout);
#line 88
  while (1) {
#line 88
    fprintf(_coverage_fout, "35\n");
#line 88
    fflush(_coverage_fout);
#line 90
    tmp = fread((void */* __restrict  */)(buffer), 1UL, 4096UL,
                (FILE */* __restrict  */)input);
#line 88
    fprintf(_coverage_fout, "36\n");
#line 88
    fflush(_coverage_fout);
#line 90
    read = (int )tmp;
#line 88
    fprintf(_coverage_fout, "37\n");
#line 88
    fflush(_coverage_fout);
#line 91
    if (read <= 0) {
#line 92
      break;
    } else {
#line 91
      fprintf(_coverage_fout, "26\n");
#line 91
      fflush(_coverage_fout);

    }
#line 88
    fprintf(_coverage_fout, "38\n");
#line 88
    fflush(_coverage_fout);
#line 93
    ret = json_parser_string(parser, (char const   *)(buffer),
                             (unsigned int )read, & processed);
#line 88
    fprintf(_coverage_fout, "39\n");
#line 88
    fflush(_coverage_fout);
#line 94
    i = 0;
#line 88
    fprintf(_coverage_fout, "40\n");
#line 88
    fflush(_coverage_fout);
#line 94
    while (1) {
#line 94
      fprintf(_coverage_fout, "31\n");
#line 94
      fflush(_coverage_fout);
#line 94
      if ((unsigned int )i < processed) {
#line 94
        fprintf(_coverage_fout, "27\n");
#line 94
        fflush(_coverage_fout);

      } else {
#line 94
        break;
      }
#line 94
      fprintf(_coverage_fout, "32\n");
#line 94
      fflush(_coverage_fout);
#line 95
      if ((int )buffer[i] == 10) {
#line 95
        fprintf(_coverage_fout, "28\n");
#line 95
        fflush(_coverage_fout);
#line 95
        col = 0;
#line 95
        fprintf(_coverage_fout, "29\n");
#line 95
        fflush(_coverage_fout);
#line 95
        lines ++;
      } else {
#line 95
        fprintf(_coverage_fout, "30\n");
#line 95
        fflush(_coverage_fout);
#line 95
        col ++;
      }
#line 94
      fprintf(_coverage_fout, "33\n");
#line 94
      fflush(_coverage_fout);
#line 94
      i ++;
    }
#line 88
    fprintf(_coverage_fout, "41\n");
#line 88
    fflush(_coverage_fout);
#line 97
    if (ret) {
#line 98
      break;
    } else {
#line 97
      fprintf(_coverage_fout, "34\n");
#line 97
      fflush(_coverage_fout);

    }
  }
#line 79
  fprintf(_coverage_fout, "50\n");
#line 79
  fflush(_coverage_fout);
#line 100
  if (retlines) {
#line 100
    fprintf(_coverage_fout, "42\n");
#line 100
    fflush(_coverage_fout);
#line 100
    *retlines = lines;
  } else {
#line 100
    fprintf(_coverage_fout, "43\n");
#line 100
    fflush(_coverage_fout);

  }
#line 79
  fprintf(_coverage_fout, "51\n");
#line 79
  fflush(_coverage_fout);
#line 101
  if (retcols) {
#line 101
    fprintf(_coverage_fout, "44\n");
#line 101
    fflush(_coverage_fout);
#line 101
    *retcols = col;
  } else {
#line 101
    fprintf(_coverage_fout, "45\n");
#line 101
    fflush(_coverage_fout);

  }
#line 79
  fprintf(_coverage_fout, "52\n");
#line 79
  fflush(_coverage_fout);
#line 102
  return (ret);
}
}
#line 105 "jsonlint.c"
static int do_verify(json_config *config , char const   *filename ) 
{ FILE *input ;
  json_parser parser ;
  int ret ;

  {
#line 105
  fprintf(_coverage_fout, "62\n");
#line 105
  fflush(_coverage_fout);
#line 111
  input = open_filename(filename, "r", 1);
#line 105
  fprintf(_coverage_fout, "63\n");
#line 105
  fflush(_coverage_fout);
#line 112
  if (! input) {
#line 112
    fprintf(_coverage_fout, "53\n");
#line 112
    fflush(_coverage_fout);
#line 113
    return (2);
  } else {
#line 112
    fprintf(_coverage_fout, "54\n");
#line 112
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "64\n");
#line 105
  fflush(_coverage_fout);
#line 116
  ret = json_parser_init(& parser, config,
                         (int (*)(void *userdata , int type ,
                                  char const   *data ,
                                  uint32_t length ))((void *)0), (void *)0);
#line 105
  fprintf(_coverage_fout, "65\n");
#line 105
  fflush(_coverage_fout);
#line 117
  if (ret) {
#line 117
    fprintf(_coverage_fout, "55\n");
#line 117
    fflush(_coverage_fout);
#line 118
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"error: initializing parser failed (code=%d): %s\n",
            ret, string_of_errors[ret]);
#line 117
    fprintf(_coverage_fout, "56\n");
#line 117
    fflush(_coverage_fout);
#line 119
    return (ret);
  } else {
#line 117
    fprintf(_coverage_fout, "57\n");
#line 117
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "66\n");
#line 105
  fflush(_coverage_fout);
#line 122
  ret = process_file(& parser, input, (int *)((void *)0), (int *)((void *)0));
#line 105
  fprintf(_coverage_fout, "67\n");
#line 105
  fflush(_coverage_fout);
#line 123
  if (ret) {
#line 123
    fprintf(_coverage_fout, "58\n");
#line 123
    fflush(_coverage_fout);
#line 124
    return (1);
  } else {
#line 123
    fprintf(_coverage_fout, "59\n");
#line 123
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "68\n");
#line 105
  fflush(_coverage_fout);
#line 126
  ret = json_parser_is_done(& parser);
#line 105
  fprintf(_coverage_fout, "69\n");
#line 105
  fflush(_coverage_fout);
#line 127
  if (! ret) {
#line 127
    fprintf(_coverage_fout, "60\n");
#line 127
    fflush(_coverage_fout);
#line 128
    return (1);
  } else {
#line 127
    fprintf(_coverage_fout, "61\n");
#line 127
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "70\n");
#line 105
  fflush(_coverage_fout);
#line 130
  close_filename(filename, input);
#line 105
  fprintf(_coverage_fout, "71\n");
#line 105
  fflush(_coverage_fout);
#line 131
  return (0);
}
}
#line 134 "jsonlint.c"
static int do_parse(json_config *config , char const   *filename ) 
{ FILE *input ;
  json_parser parser ;
  int ret ;
  int col ;
  int lines ;

  {
#line 134
  fprintf(_coverage_fout, "83\n");
#line 134
  fflush(_coverage_fout);
#line 141
  input = open_filename(filename, "r", 1);
#line 134
  fprintf(_coverage_fout, "84\n");
#line 134
  fflush(_coverage_fout);
#line 142
  if (! input) {
#line 142
    fprintf(_coverage_fout, "72\n");
#line 142
    fflush(_coverage_fout);
#line 143
    return (2);
  } else {
#line 142
    fprintf(_coverage_fout, "73\n");
#line 142
    fflush(_coverage_fout);

  }
#line 134
  fprintf(_coverage_fout, "85\n");
#line 134
  fflush(_coverage_fout);
#line 146
  ret = json_parser_init(& parser, config,
                         (int (*)(void *userdata , int type ,
                                  char const   *data ,
                                  uint32_t length ))((void *)0), (void *)0);
#line 134
  fprintf(_coverage_fout, "86\n");
#line 134
  fflush(_coverage_fout);
#line 147
  if (ret) {
#line 147
    fprintf(_coverage_fout, "74\n");
#line 147
    fflush(_coverage_fout);
#line 148
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"error: initializing parser failed (code=%d): %s\n",
            ret, string_of_errors[ret]);
#line 147
    fprintf(_coverage_fout, "75\n");
#line 147
    fflush(_coverage_fout);
#line 149
    return (ret);
  } else {
#line 147
    fprintf(_coverage_fout, "76\n");
#line 147
    fflush(_coverage_fout);

  }
#line 134
  fprintf(_coverage_fout, "87\n");
#line 134
  fflush(_coverage_fout);
#line 152
  ret = process_file(& parser, input, & lines, & col);
#line 134
  fprintf(_coverage_fout, "88\n");
#line 134
  fflush(_coverage_fout);
#line 153
  if (ret) {
#line 153
    fprintf(_coverage_fout, "77\n");
#line 153
    fflush(_coverage_fout);
#line 154
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"line %d, col %d: [code=%d] %s\n",
            lines, col, ret, string_of_errors[ret]);
#line 153
    fprintf(_coverage_fout, "78\n");
#line 153
    fflush(_coverage_fout);
#line 156
    return (1);
  } else {
#line 153
    fprintf(_coverage_fout, "79\n");
#line 153
    fflush(_coverage_fout);

  }
#line 134
  fprintf(_coverage_fout, "89\n");
#line 134
  fflush(_coverage_fout);
#line 159
  ret = json_parser_is_done(& parser);
#line 134
  fprintf(_coverage_fout, "90\n");
#line 134
  fflush(_coverage_fout);
#line 160
  if (! ret) {
#line 160
    fprintf(_coverage_fout, "80\n");
#line 160
    fflush(_coverage_fout);
#line 161
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"syntax error\n");
#line 160
    fprintf(_coverage_fout, "81\n");
#line 160
    fflush(_coverage_fout);
#line 162
    return (1);
  } else {
#line 160
    fprintf(_coverage_fout, "82\n");
#line 160
    fflush(_coverage_fout);

  }
#line 134
  fprintf(_coverage_fout, "91\n");
#line 134
  fflush(_coverage_fout);
#line 165
  close_filename(filename, input);
#line 134
  fprintf(_coverage_fout, "92\n");
#line 134
  fflush(_coverage_fout);
#line 166
  return (0);
}
}
#line 169 "jsonlint.c"
static int do_format(json_config *config , char const   *filename ,
                     char const   *outputfile ) 
{ FILE *input ;
  FILE *output ;
  json_parser parser ;
  json_printer printer ;
  int ret ;
  int col ;
  int lines ;

  {
#line 169
  fprintf(_coverage_fout, "111\n");
#line 169
  fflush(_coverage_fout);
#line 177
  input = open_filename(filename, "r", 1);
#line 169
  fprintf(_coverage_fout, "112\n");
#line 169
  fflush(_coverage_fout);
#line 178
  if (! input) {
#line 178
    fprintf(_coverage_fout, "93\n");
#line 178
    fflush(_coverage_fout);
#line 179
    return (2);
  } else {
#line 178
    fprintf(_coverage_fout, "94\n");
#line 178
    fflush(_coverage_fout);

  }
#line 169
  fprintf(_coverage_fout, "113\n");
#line 169
  fflush(_coverage_fout);
#line 181
  output = open_filename(outputfile, "a+", 0);
#line 169
  fprintf(_coverage_fout, "114\n");
#line 169
  fflush(_coverage_fout);
#line 182
  if (! output) {
#line 182
    fprintf(_coverage_fout, "95\n");
#line 182
    fflush(_coverage_fout);
#line 183
    return (2);
  } else {
#line 182
    fprintf(_coverage_fout, "96\n");
#line 182
    fflush(_coverage_fout);

  }
#line 169
  fprintf(_coverage_fout, "115\n");
#line 169
  fflush(_coverage_fout);
#line 186
  ret = json_print_init(& printer, & printchannel, (void *)stdout);
#line 169
  fprintf(_coverage_fout, "116\n");
#line 169
  fflush(_coverage_fout);
#line 187
  if (ret) {
#line 187
    fprintf(_coverage_fout, "97\n");
#line 187
    fflush(_coverage_fout);
#line 188
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"error: initializing printer failed: [code=%d] %s\n",
            ret, string_of_errors[ret]);
#line 187
    fprintf(_coverage_fout, "98\n");
#line 187
    fflush(_coverage_fout);
#line 189
    return (ret);
  } else {
#line 187
    fprintf(_coverage_fout, "99\n");
#line 187
    fflush(_coverage_fout);

  }
#line 169
  fprintf(_coverage_fout, "117\n");
#line 169
  fflush(_coverage_fout);
#line 191
  if (indent_string) {
#line 191
    fprintf(_coverage_fout, "100\n");
#line 191
    fflush(_coverage_fout);
#line 192
    printer.indentstr = indent_string;
  } else {
#line 191
    fprintf(_coverage_fout, "101\n");
#line 191
    fflush(_coverage_fout);

  }
#line 169
  fprintf(_coverage_fout, "118\n");
#line 169
  fflush(_coverage_fout);
#line 194
  ret = json_parser_init(& parser, config, & prettyprint, (void *)(& printer));
#line 169
  fprintf(_coverage_fout, "119\n");
#line 169
  fflush(_coverage_fout);
#line 195
  if (ret) {
#line 195
    fprintf(_coverage_fout, "102\n");
#line 195
    fflush(_coverage_fout);
#line 196
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"error: initializing parser failed: [code=%d] %s\n",
            ret, string_of_errors[ret]);
#line 195
    fprintf(_coverage_fout, "103\n");
#line 195
    fflush(_coverage_fout);
#line 197
    return (ret);
  } else {
#line 195
    fprintf(_coverage_fout, "104\n");
#line 195
    fflush(_coverage_fout);

  }
#line 169
  fprintf(_coverage_fout, "120\n");
#line 169
  fflush(_coverage_fout);
#line 200
  ret = process_file(& parser, input, & lines, & col);
#line 169
  fprintf(_coverage_fout, "121\n");
#line 169
  fflush(_coverage_fout);
#line 201
  if (ret) {
#line 201
    fprintf(_coverage_fout, "105\n");
#line 201
    fflush(_coverage_fout);
#line 202
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"line %d, col %d: [code=%d] %s\n",
            lines, col, ret, string_of_errors[ret]);
#line 201
    fprintf(_coverage_fout, "106\n");
#line 201
    fflush(_coverage_fout);
#line 204
    return (1);
  } else {
#line 201
    fprintf(_coverage_fout, "107\n");
#line 201
    fflush(_coverage_fout);

  }
#line 169
  fprintf(_coverage_fout, "122\n");
#line 169
  fflush(_coverage_fout);
#line 207
  ret = json_parser_is_done(& parser);
#line 169
  fprintf(_coverage_fout, "123\n");
#line 169
  fflush(_coverage_fout);
#line 208
  if (! ret) {
#line 208
    fprintf(_coverage_fout, "108\n");
#line 208
    fflush(_coverage_fout);
#line 209
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"syntax error\n");
#line 208
    fprintf(_coverage_fout, "109\n");
#line 208
    fflush(_coverage_fout);
#line 210
    return (1);
  } else {
#line 208
    fprintf(_coverage_fout, "110\n");
#line 208
    fflush(_coverage_fout);

  }
#line 169
  fprintf(_coverage_fout, "124\n");
#line 169
  fflush(_coverage_fout);
#line 214
  json_parser_free(& parser);
#line 169
  fprintf(_coverage_fout, "125\n");
#line 169
  fflush(_coverage_fout);
#line 215
  json_print_free(& printer);
#line 169
  fprintf(_coverage_fout, "126\n");
#line 169
  fflush(_coverage_fout);
#line 216
  fwrite((void const   */* __restrict  */)"\n", 1UL, 1UL,
         (FILE */* __restrict  */)stdout);
#line 169
  fprintf(_coverage_fout, "127\n");
#line 169
  fflush(_coverage_fout);
#line 217
  close_filename(filename, input);
#line 169
  fprintf(_coverage_fout, "128\n");
#line 169
  fflush(_coverage_fout);
#line 218
  return (0);
}
}
#line 238 "jsonlint.c"
static void *tree_create_structure(int nesting , int is_object ) 
{ json_val_t *v ;
  void *tmp ;

  {
#line 238
  fprintf(_coverage_fout, "136\n");
#line 238
  fflush(_coverage_fout);
#line 240
  tmp = malloc(sizeof(json_val_t ));
#line 238
  fprintf(_coverage_fout, "137\n");
#line 238
  fflush(_coverage_fout);
#line 240
  v = (json_val_t *)tmp;
#line 238
  fprintf(_coverage_fout, "138\n");
#line 238
  fflush(_coverage_fout);
#line 241
  if (v) {
#line 241
    fprintf(_coverage_fout, "133\n");
#line 241
    fflush(_coverage_fout);
#line 244
    if (is_object) {
#line 244
      fprintf(_coverage_fout, "129\n");
#line 244
      fflush(_coverage_fout);
#line 245
      v->type = 2;
#line 244
      fprintf(_coverage_fout, "130\n");
#line 244
      fflush(_coverage_fout);
#line 246
      v->u.object = (struct json_val_elem **)((void *)0);
    } else {
#line 244
      fprintf(_coverage_fout, "131\n");
#line 244
      fflush(_coverage_fout);
#line 248
      v->type = 1;
#line 244
      fprintf(_coverage_fout, "132\n");
#line 244
      fflush(_coverage_fout);
#line 249
      v->u.array = (struct json_val **)((void *)0);
    }
#line 241
    fprintf(_coverage_fout, "134\n");
#line 241
    fflush(_coverage_fout);
#line 251
    v->length = 0;
  } else {
#line 241
    fprintf(_coverage_fout, "135\n");
#line 241
    fflush(_coverage_fout);

  }
#line 238
  fprintf(_coverage_fout, "139\n");
#line 238
  fflush(_coverage_fout);
#line 253
  return ((void *)v);
}
}
#line 256 "jsonlint.c"
static char *memalloc_copy_length(char const   *src , uint32_t n ) 
{ char *dest ;
  void *tmp ;

  {
#line 256
  fprintf(_coverage_fout, "142\n");
#line 256
  fflush(_coverage_fout);
#line 260
  tmp = calloc((unsigned long )(n + 1U), sizeof(char ));
#line 256
  fprintf(_coverage_fout, "143\n");
#line 256
  fflush(_coverage_fout);
#line 260
  dest = (char *)tmp;
#line 256
  fprintf(_coverage_fout, "144\n");
#line 256
  fflush(_coverage_fout);
#line 261
  if (dest) {
#line 261
    fprintf(_coverage_fout, "140\n");
#line 261
    fflush(_coverage_fout);
#line 262
    memcpy((void */* __restrict  */)dest, (void const   */* __restrict  */)src,
           (unsigned long )n);
  } else {
#line 261
    fprintf(_coverage_fout, "141\n");
#line 261
    fflush(_coverage_fout);

  }
#line 256
  fprintf(_coverage_fout, "145\n");
#line 256
  fflush(_coverage_fout);
#line 263
  return (dest);
}
}
#line 266 "jsonlint.c"
static void *tree_create_data(int type , char const   *data , uint32_t length ) 
{ json_val_t *v ;
  void *tmp ;

  {
#line 266
  fprintf(_coverage_fout, "154\n");
#line 266
  fflush(_coverage_fout);
#line 270
  tmp = malloc(sizeof(json_val_t ));
#line 266
  fprintf(_coverage_fout, "155\n");
#line 266
  fflush(_coverage_fout);
#line 270
  v = (json_val_t *)tmp;
#line 266
  fprintf(_coverage_fout, "156\n");
#line 266
  fflush(_coverage_fout);
#line 271
  if (v) {
#line 271
    fprintf(_coverage_fout, "149\n");
#line 271
    fflush(_coverage_fout);
#line 272
    v->type = type;
#line 271
    fprintf(_coverage_fout, "150\n");
#line 271
    fflush(_coverage_fout);
#line 273
    v->length = (int )length;
#line 271
    fprintf(_coverage_fout, "151\n");
#line 271
    fflush(_coverage_fout);
#line 274
    v->u.data = memalloc_copy_length(data, length);
#line 271
    fprintf(_coverage_fout, "152\n");
#line 271
    fflush(_coverage_fout);
#line 275
    if (! v->u.data) {
#line 275
      fprintf(_coverage_fout, "146\n");
#line 275
      fflush(_coverage_fout);
#line 276
      free((void *)v);
#line 275
      fprintf(_coverage_fout, "147\n");
#line 275
      fflush(_coverage_fout);
#line 277
      return ((void *)0);
    } else {
#line 275
      fprintf(_coverage_fout, "148\n");
#line 275
      fflush(_coverage_fout);

    }
  } else {
#line 271
    fprintf(_coverage_fout, "153\n");
#line 271
    fflush(_coverage_fout);

  }
#line 266
  fprintf(_coverage_fout, "157\n");
#line 266
  fflush(_coverage_fout);
#line 280
  return ((void *)v);
}
}
#line 283 "jsonlint.c"
static int tree_append(void *structure , char *key , uint32_t key_length ,
                       void *obj ) 
{ json_val_t *parent ;
  struct json_val_elem *objelem ;
  void *tmp ;
  uint32_t newsize ;
  void *newptr ;
  void *tmp___0 ;
  int tmp___1 ;
  void *tmp___2 ;
  uint32_t newsize___0 ;
  void *newptr___0 ;
  int tmp___3 ;

  {
#line 283
  fprintf(_coverage_fout, "198\n");
#line 283
  fflush(_coverage_fout);
#line 285
  parent = (json_val_t *)structure;
#line 283
  fprintf(_coverage_fout, "199\n");
#line 283
  fflush(_coverage_fout);
#line 286
  if (key) {
#line 286
    fprintf(_coverage_fout, "171\n");
#line 286
    fflush(_coverage_fout);
#line 289
    if (parent->length == 0) {
#line 289
      fprintf(_coverage_fout, "160\n");
#line 289
      fflush(_coverage_fout);
#line 290
      tmp = calloc(2UL, sizeof(json_val_t *));
#line 289
      fprintf(_coverage_fout, "161\n");
#line 289
      fflush(_coverage_fout);
#line 290
      parent->u.object = (struct json_val_elem **)tmp;
#line 289
      fprintf(_coverage_fout, "162\n");
#line 289
      fflush(_coverage_fout);
#line 291
      if (! parent->u.object) {
#line 291
        fprintf(_coverage_fout, "158\n");
#line 291
        fflush(_coverage_fout);
#line 292
        return (1);
      } else {
#line 291
        fprintf(_coverage_fout, "159\n");
#line 291
        fflush(_coverage_fout);

      }
    } else {
#line 289
      fprintf(_coverage_fout, "165\n");
#line 289
      fflush(_coverage_fout);
#line 294
      newsize = (unsigned int )((parent->length + 1) + 1);
#line 289
      fprintf(_coverage_fout, "166\n");
#line 289
      fflush(_coverage_fout);
#line 297
      newptr = realloc((void *)parent->u.object,
                       (unsigned long )newsize * sizeof(json_val_t *));
#line 289
      fprintf(_coverage_fout, "167\n");
#line 289
      fflush(_coverage_fout);
#line 298
      if (! newptr) {
#line 298
        fprintf(_coverage_fout, "163\n");
#line 298
        fflush(_coverage_fout);
#line 299
        return (-1);
      } else {
#line 298
        fprintf(_coverage_fout, "164\n");
#line 298
        fflush(_coverage_fout);

      }
#line 289
      fprintf(_coverage_fout, "168\n");
#line 289
      fflush(_coverage_fout);
#line 300
      parent->u.object = (struct json_val_elem **)newptr;
    }
#line 286
    fprintf(_coverage_fout, "172\n");
#line 286
    fflush(_coverage_fout);
#line 303
    tmp___0 = malloc(sizeof(struct json_val_elem ));
#line 286
    fprintf(_coverage_fout, "173\n");
#line 286
    fflush(_coverage_fout);
#line 303
    objelem = (struct json_val_elem *)tmp___0;
#line 286
    fprintf(_coverage_fout, "174\n");
#line 286
    fflush(_coverage_fout);
#line 304
    if (! objelem) {
#line 304
      fprintf(_coverage_fout, "169\n");
#line 304
      fflush(_coverage_fout);
#line 305
      return (-1);
    } else {
#line 304
      fprintf(_coverage_fout, "170\n");
#line 304
      fflush(_coverage_fout);

    }
#line 286
    fprintf(_coverage_fout, "175\n");
#line 286
    fflush(_coverage_fout);
#line 307
    objelem->key = memalloc_copy_length((char const   *)key, key_length);
#line 286
    fprintf(_coverage_fout, "176\n");
#line 286
    fflush(_coverage_fout);
#line 308
    objelem->key_length = key_length;
#line 286
    fprintf(_coverage_fout, "177\n");
#line 286
    fflush(_coverage_fout);
#line 309
    objelem->val = (struct json_val *)obj;
#line 286
    fprintf(_coverage_fout, "178\n");
#line 286
    fflush(_coverage_fout);
#line 310
    tmp___1 = parent->length;
#line 286
    fprintf(_coverage_fout, "179\n");
#line 286
    fflush(_coverage_fout);
#line 310
    (parent->length) ++;
#line 286
    fprintf(_coverage_fout, "180\n");
#line 286
    fflush(_coverage_fout);
#line 310
    *(parent->u.object + tmp___1) = objelem;
#line 286
    fprintf(_coverage_fout, "181\n");
#line 286
    fflush(_coverage_fout);
#line 311
    *(parent->u.object + parent->length) = (struct json_val_elem *)((void *)0);
  } else {
#line 286
    fprintf(_coverage_fout, "193\n");
#line 286
    fflush(_coverage_fout);
#line 313
    if (parent->length == 0) {
#line 313
      fprintf(_coverage_fout, "184\n");
#line 313
      fflush(_coverage_fout);
#line 314
      tmp___2 = calloc(2UL, sizeof(json_val_t *));
#line 313
      fprintf(_coverage_fout, "185\n");
#line 313
      fflush(_coverage_fout);
#line 314
      parent->u.array = (struct json_val **)tmp___2;
#line 313
      fprintf(_coverage_fout, "186\n");
#line 313
      fflush(_coverage_fout);
#line 315
      if (! parent->u.array) {
#line 315
        fprintf(_coverage_fout, "182\n");
#line 315
        fflush(_coverage_fout);
#line 316
        return (1);
      } else {
#line 315
        fprintf(_coverage_fout, "183\n");
#line 315
        fflush(_coverage_fout);

      }
    } else {
#line 313
      fprintf(_coverage_fout, "189\n");
#line 313
      fflush(_coverage_fout);
#line 318
      newsize___0 = (unsigned int )((parent->length + 1) + 1);
#line 313
      fprintf(_coverage_fout, "190\n");
#line 313
      fflush(_coverage_fout);
#line 321
      newptr___0 = realloc((void *)parent->u.object,
                           (unsigned long )newsize___0 * sizeof(json_val_t *));
#line 313
      fprintf(_coverage_fout, "191\n");
#line 313
      fflush(_coverage_fout);
#line 322
      if (! newptr___0) {
#line 322
        fprintf(_coverage_fout, "187\n");
#line 322
        fflush(_coverage_fout);
#line 323
        return (-1);
      } else {
#line 322
        fprintf(_coverage_fout, "188\n");
#line 322
        fflush(_coverage_fout);

      }
#line 313
      fprintf(_coverage_fout, "192\n");
#line 313
      fflush(_coverage_fout);
#line 324
      parent->u.array = (struct json_val **)newptr___0;
    }
#line 286
    fprintf(_coverage_fout, "194\n");
#line 286
    fflush(_coverage_fout);
#line 326
    tmp___3 = parent->length;
#line 286
    fprintf(_coverage_fout, "195\n");
#line 286
    fflush(_coverage_fout);
#line 326
    (parent->length) ++;
#line 286
    fprintf(_coverage_fout, "196\n");
#line 286
    fflush(_coverage_fout);
#line 326
    *(parent->u.array + tmp___3) = (struct json_val *)obj;
#line 286
    fprintf(_coverage_fout, "197\n");
#line 286
    fflush(_coverage_fout);
#line 327
    *(parent->u.array + parent->length) = (struct json_val *)((void *)0);
  }
#line 283
  fprintf(_coverage_fout, "200\n");
#line 283
  fflush(_coverage_fout);
#line 329
  return (0);
}
}
#line 332 "jsonlint.c"
static int do_tree(json_config *config , char const   *filename ,
                   json_val_t **root_structure ) 
{ FILE *input ;
  json_parser parser ;
  json_parser_dom dom ;
  int ret ;
  int col ;
  int lines ;

  {
#line 332
  fprintf(_coverage_fout, "217\n");
#line 332
  fflush(_coverage_fout);
#line 340
  input = open_filename(filename, "r", 1);
#line 332
  fprintf(_coverage_fout, "218\n");
#line 332
  fflush(_coverage_fout);
#line 341
  if (! input) {
#line 341
    fprintf(_coverage_fout, "201\n");
#line 341
    fflush(_coverage_fout);
#line 342
    return (2);
  } else {
#line 341
    fprintf(_coverage_fout, "202\n");
#line 341
    fflush(_coverage_fout);

  }
#line 332
  fprintf(_coverage_fout, "219\n");
#line 332
  fflush(_coverage_fout);
#line 344
  ret = json_parser_dom_init(& dom, & tree_create_structure, & tree_create_data,
                             & tree_append);
#line 332
  fprintf(_coverage_fout, "220\n");
#line 332
  fflush(_coverage_fout);
#line 345
  if (ret) {
#line 345
    fprintf(_coverage_fout, "203\n");
#line 345
    fflush(_coverage_fout);
#line 346
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"error: initializing helper failed: [code=%d] %s\n",
            ret, string_of_errors[ret]);
#line 345
    fprintf(_coverage_fout, "204\n");
#line 345
    fflush(_coverage_fout);
#line 347
    return (ret);
  } else {
#line 345
    fprintf(_coverage_fout, "205\n");
#line 345
    fflush(_coverage_fout);

  }
#line 332
  fprintf(_coverage_fout, "221\n");
#line 332
  fflush(_coverage_fout);
#line 350
  ret = json_parser_init(& parser, config, & json_parser_dom_callback,
                         (void *)(& dom));
#line 332
  fprintf(_coverage_fout, "222\n");
#line 332
  fflush(_coverage_fout);
#line 351
  if (ret) {
#line 351
    fprintf(_coverage_fout, "206\n");
#line 351
    fflush(_coverage_fout);
#line 352
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"error: initializing parser failed: [code=%d] %s\n",
            ret, string_of_errors[ret]);
#line 351
    fprintf(_coverage_fout, "207\n");
#line 351
    fflush(_coverage_fout);
#line 353
    return (ret);
  } else {
#line 351
    fprintf(_coverage_fout, "208\n");
#line 351
    fflush(_coverage_fout);

  }
#line 332
  fprintf(_coverage_fout, "223\n");
#line 332
  fflush(_coverage_fout);
#line 356
  ret = process_file(& parser, input, & lines, & col);
#line 332
  fprintf(_coverage_fout, "224\n");
#line 332
  fflush(_coverage_fout);
#line 357
  if (ret) {
#line 357
    fprintf(_coverage_fout, "209\n");
#line 357
    fflush(_coverage_fout);
#line 358
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"line %d, col %d: [code=%d] %s\n",
            lines, col, ret, string_of_errors[ret]);
#line 357
    fprintf(_coverage_fout, "210\n");
#line 357
    fflush(_coverage_fout);
#line 361
    return (1);
  } else {
#line 357
    fprintf(_coverage_fout, "211\n");
#line 357
    fflush(_coverage_fout);

  }
#line 332
  fprintf(_coverage_fout, "225\n");
#line 332
  fflush(_coverage_fout);
#line 364
  ret = json_parser_is_done(& parser);
#line 332
  fprintf(_coverage_fout, "226\n");
#line 332
  fflush(_coverage_fout);
#line 365
  if (! ret) {
#line 365
    fprintf(_coverage_fout, "212\n");
#line 365
    fflush(_coverage_fout);
#line 366
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"syntax error\n");
#line 365
    fprintf(_coverage_fout, "213\n");
#line 365
    fflush(_coverage_fout);
#line 367
    return (1);
  } else {
#line 365
    fprintf(_coverage_fout, "214\n");
#line 365
    fflush(_coverage_fout);

  }
#line 332
  fprintf(_coverage_fout, "227\n");
#line 332
  fflush(_coverage_fout);
#line 370
  if (root_structure) {
#line 370
    fprintf(_coverage_fout, "215\n");
#line 370
    fflush(_coverage_fout);
#line 371
    *root_structure = (json_val_t *)dom.root_structure;
  } else {
#line 370
    fprintf(_coverage_fout, "216\n");
#line 370
    fflush(_coverage_fout);

  }
#line 332
  fprintf(_coverage_fout, "228\n");
#line 332
  fflush(_coverage_fout);
#line 374
  json_parser_free(& parser);
#line 332
  fprintf(_coverage_fout, "229\n");
#line 332
  fflush(_coverage_fout);
#line 375
  close_filename(filename, input);
#line 332
  fprintf(_coverage_fout, "230\n");
#line 332
  fflush(_coverage_fout);
#line 376
  return (0);
}
}
#line 379 "jsonlint.c"
static int print_tree_iter(json_val_t *element , FILE *output ) 
{ int i ;

  {
#line 379
  fprintf(_coverage_fout, "256\n");
#line 379
  fflush(_coverage_fout);
#line 382
  if (! element) {
#line 382
    fprintf(_coverage_fout, "231\n");
#line 382
    fflush(_coverage_fout);
#line 383
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"error: no element in print tree\n");
#line 382
    fprintf(_coverage_fout, "232\n");
#line 382
    fflush(_coverage_fout);
#line 384
    return (-1);
  } else {
#line 382
    fprintf(_coverage_fout, "233\n");
#line 382
    fflush(_coverage_fout);

  }
#line 387
  switch (element->type) {
#line 387
  fprintf(_coverage_fout, "243\n");
#line 387
  fflush(_coverage_fout);
  case 2: 
#line 389
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"object begin (%d element)\n",
          element->length);
#line 387
  fprintf(_coverage_fout, "244\n");
#line 387
  fflush(_coverage_fout);
#line 390
  i = 0;
#line 387
  fprintf(_coverage_fout, "245\n");
#line 387
  fflush(_coverage_fout);
#line 390
  while (1) {
#line 390
    fprintf(_coverage_fout, "235\n");
#line 390
    fflush(_coverage_fout);
#line 390
    if (i < element->length) {
#line 390
      fprintf(_coverage_fout, "234\n");
#line 390
      fflush(_coverage_fout);

    } else {
#line 390
      break;
    }
#line 390
    fprintf(_coverage_fout, "236\n");
#line 390
    fflush(_coverage_fout);
#line 391
    fprintf((FILE */* __restrict  */)output,
            (char const   */* __restrict  */)"key: %s\n",
            (*(element->u.object + i))->key);
#line 390
    fprintf(_coverage_fout, "237\n");
#line 390
    fflush(_coverage_fout);
#line 392
    print_tree_iter((*(element->u.object + i))->val, output);
#line 390
    fprintf(_coverage_fout, "238\n");
#line 390
    fflush(_coverage_fout);
#line 390
    i ++;
  }
#line 387
  fprintf(_coverage_fout, "246\n");
#line 387
  fflush(_coverage_fout);
#line 394
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"object end\n");
#line 395
  break;
#line 387
  fprintf(_coverage_fout, "247\n");
#line 387
  fflush(_coverage_fout);
  case 1: 
#line 397
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"array begin\n");
#line 387
  fprintf(_coverage_fout, "248\n");
#line 387
  fflush(_coverage_fout);
#line 398
  i = 0;
#line 387
  fprintf(_coverage_fout, "249\n");
#line 387
  fflush(_coverage_fout);
#line 398
  while (1) {
#line 398
    fprintf(_coverage_fout, "240\n");
#line 398
    fflush(_coverage_fout);
#line 398
    if (i < element->length) {
#line 398
      fprintf(_coverage_fout, "239\n");
#line 398
      fflush(_coverage_fout);

    } else {
#line 398
      break;
    }
#line 398
    fprintf(_coverage_fout, "241\n");
#line 398
    fflush(_coverage_fout);
#line 399
    print_tree_iter(*(element->u.array + i), output);
#line 398
    fprintf(_coverage_fout, "242\n");
#line 398
    fflush(_coverage_fout);
#line 398
    i ++;
  }
#line 387
  fprintf(_coverage_fout, "250\n");
#line 387
  fflush(_coverage_fout);
#line 401
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"array end\n");
#line 402
  break;
#line 387
  fprintf(_coverage_fout, "251\n");
#line 387
  fflush(_coverage_fout);
  case 10: 
  case 9: 
  case 11: 
#line 406
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"constant\n");
#line 407
  break;
#line 387
  fprintf(_coverage_fout, "252\n");
#line 387
  fflush(_coverage_fout);
  case 5: 
#line 409
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"integer: %s\n", element->u.data);
#line 410
  break;
#line 387
  fprintf(_coverage_fout, "253\n");
#line 387
  fflush(_coverage_fout);
  case 7: 
#line 412
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"string: %s\n", element->u.data);
#line 413
  break;
#line 387
  fprintf(_coverage_fout, "254\n");
#line 387
  fflush(_coverage_fout);
  case 6: 
#line 415
  fprintf((FILE */* __restrict  */)output,
          (char const   */* __restrict  */)"float: %s\n", element->u.data);
#line 416
  break;
#line 387
  fprintf(_coverage_fout, "255\n");
#line 387
  fflush(_coverage_fout);
  default: ;
#line 418
  break;
  }
#line 379
  fprintf(_coverage_fout, "257\n");
#line 379
  fflush(_coverage_fout);
#line 420
  return (0);
}
}
#line 423 "jsonlint.c"
static int print_tree(json_val_t *root_structure , char *outputfile ) 
{ FILE *output ;

  {
#line 423
  fprintf(_coverage_fout, "260\n");
#line 423
  fflush(_coverage_fout);
#line 427
  output = open_filename((char const   *)outputfile, "a+", 0);
#line 423
  fprintf(_coverage_fout, "261\n");
#line 423
  fflush(_coverage_fout);
#line 428
  if (! output) {
#line 428
    fprintf(_coverage_fout, "258\n");
#line 428
    fflush(_coverage_fout);
#line 429
    return (2);
  } else {
#line 428
    fprintf(_coverage_fout, "259\n");
#line 428
    fflush(_coverage_fout);

  }
#line 423
  fprintf(_coverage_fout, "262\n");
#line 423
  fflush(_coverage_fout);
#line 430
  print_tree_iter(root_structure, output);
#line 423
  fprintf(_coverage_fout, "263\n");
#line 423
  fflush(_coverage_fout);
#line 431
  close_filename((char const   *)outputfile, output);
#line 423
  fprintf(_coverage_fout, "264\n");
#line 423
  fflush(_coverage_fout);
#line 432
  return (0);
}
}
#line 435 "jsonlint.c"
int usage(char const   *argv0 ) 
{ 

  {
#line 435
  fprintf(_coverage_fout, "265\n");
#line 435
  fflush(_coverage_fout);
#line 437
  printf((char const   */* __restrict  */)"usage: %s [options] JSON-FILE(s)...\n",
         argv0);
#line 435
  fprintf(_coverage_fout, "266\n");
#line 435
  fflush(_coverage_fout);
#line 438
  printf((char const   */* __restrict  */)"\t--no-comments : disallow C and YAML comments in json file (default to both on)\n");
#line 435
  fprintf(_coverage_fout, "267\n");
#line 435
  fflush(_coverage_fout);
#line 439
  printf((char const   */* __restrict  */)"\t--no-yaml-comments : disallow YAML comment (default to on)\n");
#line 435
  fprintf(_coverage_fout, "268\n");
#line 435
  fflush(_coverage_fout);
#line 440
  printf((char const   */* __restrict  */)"\t--no-c-comments : disallow C comment (default to on)\n");
#line 435
  fprintf(_coverage_fout, "269\n");
#line 435
  fflush(_coverage_fout);
#line 441
  printf((char const   */* __restrict  */)"\t--format : pretty print the json file to stdout (unless -o specified)\n");
#line 435
  fprintf(_coverage_fout, "270\n");
#line 435
  fflush(_coverage_fout);
#line 442
  printf((char const   */* __restrict  */)"\t--verify : quietly verified if the json file is valid. exit 0 if valid, 1 if not\n");
#line 435
  fprintf(_coverage_fout, "271\n");
#line 435
  fflush(_coverage_fout);
#line 443
  printf((char const   */* __restrict  */)"\t--max-nesting : limit the number of nesting in structure (default to no limit)\n");
#line 435
  fprintf(_coverage_fout, "272\n");
#line 435
  fflush(_coverage_fout);
#line 444
  printf((char const   */* __restrict  */)"\t--max-data : limit the number of characters of data (string/int/float) (default to no limit)\n");
#line 435
  fprintf(_coverage_fout, "273\n");
#line 435
  fflush(_coverage_fout);
#line 445
  printf((char const   */* __restrict  */)"\t--indent-string : set the string to use for indenting one level (default to 1 tab)\n");
#line 435
  fprintf(_coverage_fout, "274\n");
#line 435
  fflush(_coverage_fout);
#line 446
  printf((char const   */* __restrict  */)"\t--tree : build a tree (DOM)\n");
#line 435
  fprintf(_coverage_fout, "275\n");
#line 435
  fflush(_coverage_fout);
#line 447
  printf((char const   */* __restrict  */)"\t-o : output to a specific file instead of stdout\n");
#line 435
  fprintf(_coverage_fout, "276\n");
#line 435
  fflush(_coverage_fout);
#line 448
  exit(0);
}
}
#line 451 "jsonlint.c"
int main(int argc , char **argv ) 
{ int format ;
  int verify ;
  int use_tree ;
  int ret ;
  int i ;
  json_config config ;
  char *output ;
  int option_index ;
  struct option long_options[11] ;
  int c ;
  int tmp ;
  char const   *name ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  int tmp___6 ;
  int tmp___7 ;
  int tmp___8 ;
  int tmp___9 ;
  int tmp___10 ;
  int tmp___11 ;
  json_val_t *root_structure ;

  {
  __globinit_jsonlint_comb();
#line 451
  fprintf(_coverage_fout, "385\n");
#line 451
  fflush(_coverage_fout);
#line 453
  format = 0;
#line 451
  fprintf(_coverage_fout, "386\n");
#line 451
  fflush(_coverage_fout);
#line 453
  verify = 0;
#line 451
  fprintf(_coverage_fout, "387\n");
#line 451
  fflush(_coverage_fout);
#line 453
  use_tree = 0;
#line 451
  fprintf(_coverage_fout, "388\n");
#line 451
  fflush(_coverage_fout);
#line 454
  ret = 0;
#line 451
  fprintf(_coverage_fout, "389\n");
#line 451
  fflush(_coverage_fout);
#line 456
  output = (char *)"-";
#line 451
  fprintf(_coverage_fout, "390\n");
#line 451
  fflush(_coverage_fout);
#line 458
  memset((void *)(& config), 0, sizeof(json_config ));
#line 451
  fprintf(_coverage_fout, "391\n");
#line 451
  fflush(_coverage_fout);
#line 459
  config.max_nesting = 0U;
#line 451
  fprintf(_coverage_fout, "392\n");
#line 451
  fflush(_coverage_fout);
#line 460
  config.max_data = 0U;
#line 451
  fprintf(_coverage_fout, "393\n");
#line 451
  fflush(_coverage_fout);
#line 461
  config.allow_c_comments = 1;
#line 451
  fprintf(_coverage_fout, "394\n");
#line 451
  fflush(_coverage_fout);
#line 462
  config.allow_yaml_comments = 1;
#line 451
  fprintf(_coverage_fout, "395\n");
#line 451
  fflush(_coverage_fout);
#line 464
  while (1) {
#line 464
    fprintf(_coverage_fout, "315\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[0].name = "no-comments";
#line 464
    fprintf(_coverage_fout, "316\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[0].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "317\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[0].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "318\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[0].val = 0;
#line 464
    fprintf(_coverage_fout, "319\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[1].name = "no-yaml-comments";
#line 464
    fprintf(_coverage_fout, "320\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[1].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "321\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[1].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "322\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[1].val = 0;
#line 464
    fprintf(_coverage_fout, "323\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[2].name = "no-c-comments";
#line 464
    fprintf(_coverage_fout, "324\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[2].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "325\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[2].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "326\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[2].val = 0;
#line 464
    fprintf(_coverage_fout, "327\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[3].name = "format";
#line 464
    fprintf(_coverage_fout, "328\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[3].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "329\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[3].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "330\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[3].val = 0;
#line 464
    fprintf(_coverage_fout, "331\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[4].name = "verify";
#line 464
    fprintf(_coverage_fout, "332\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[4].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "333\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[4].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "334\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[4].val = 0;
#line 464
    fprintf(_coverage_fout, "335\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[5].name = "help";
#line 464
    fprintf(_coverage_fout, "336\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[5].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "337\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[5].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "338\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[5].val = 0;
#line 464
    fprintf(_coverage_fout, "339\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[6].name = "max-nesting";
#line 464
    fprintf(_coverage_fout, "340\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[6].has_arg = 1;
#line 464
    fprintf(_coverage_fout, "341\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[6].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "342\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[6].val = 0;
#line 464
    fprintf(_coverage_fout, "343\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[7].name = "max-data";
#line 464
    fprintf(_coverage_fout, "344\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[7].has_arg = 1;
#line 464
    fprintf(_coverage_fout, "345\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[7].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "346\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[7].val = 0;
#line 464
    fprintf(_coverage_fout, "347\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[8].name = "indent-string";
#line 464
    fprintf(_coverage_fout, "348\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[8].has_arg = 1;
#line 464
    fprintf(_coverage_fout, "349\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[8].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "350\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[8].val = 0;
#line 464
    fprintf(_coverage_fout, "351\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[9].name = "tree";
#line 464
    fprintf(_coverage_fout, "352\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[9].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "353\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[9].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "354\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[9].val = 0;
#line 464
    fprintf(_coverage_fout, "355\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[10].name = (char const   *)0;
#line 464
    fprintf(_coverage_fout, "356\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[10].has_arg = 0;
#line 464
    fprintf(_coverage_fout, "357\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[10].flag = (int *)0;
#line 464
    fprintf(_coverage_fout, "358\n");
#line 464
    fflush(_coverage_fout);
#line 466
    long_options[10].val = 0;
#line 464
    fprintf(_coverage_fout, "359\n");
#line 464
    fflush(_coverage_fout);
#line 479
    tmp = getopt_long(argc, (char * const  *)argv, "o:",
                      (struct option  const  *)(long_options), & option_index);
#line 464
    fprintf(_coverage_fout, "360\n");
#line 464
    fflush(_coverage_fout);
#line 479
    c = tmp;
#line 464
    fprintf(_coverage_fout, "361\n");
#line 464
    fflush(_coverage_fout);
#line 480
    if (c == -1) {
#line 481
      break;
    } else {
#line 480
      fprintf(_coverage_fout, "277\n");
#line 480
      fflush(_coverage_fout);

    }
#line 482
    switch (c) {
#line 482
    fprintf(_coverage_fout, "310\n");
#line 482
    fflush(_coverage_fout);
    case 0: 
#line 484
    name = long_options[option_index].name;
#line 482
    fprintf(_coverage_fout, "311\n");
#line 482
    fflush(_coverage_fout);
#line 485
    tmp___11 = strcmp(name, "help");
#line 482
    fprintf(_coverage_fout, "312\n");
#line 482
    fflush(_coverage_fout);
#line 485
    if (tmp___11 == 0) {
#line 485
      fprintf(_coverage_fout, "278\n");
#line 485
      fflush(_coverage_fout);
#line 486
      usage((char const   *)*(argv + 0));
    } else {
#line 485
      fprintf(_coverage_fout, "308\n");
#line 485
      fflush(_coverage_fout);
#line 487
      tmp___10 = strcmp(name, "no-c-comments");
#line 485
      fprintf(_coverage_fout, "309\n");
#line 485
      fflush(_coverage_fout);
#line 487
      if (tmp___10 == 0) {
#line 487
        fprintf(_coverage_fout, "279\n");
#line 487
        fflush(_coverage_fout);
#line 488
        config.allow_c_comments = 0;
      } else {
#line 487
        fprintf(_coverage_fout, "306\n");
#line 487
        fflush(_coverage_fout);
#line 489
        tmp___9 = strcmp(name, "no-yaml-comments");
#line 487
        fprintf(_coverage_fout, "307\n");
#line 487
        fflush(_coverage_fout);
#line 489
        if (tmp___9 == 0) {
#line 489
          fprintf(_coverage_fout, "280\n");
#line 489
          fflush(_coverage_fout);
#line 490
          config.allow_yaml_comments = 0;
        } else {
#line 489
          fprintf(_coverage_fout, "304\n");
#line 489
          fflush(_coverage_fout);
#line 491
          tmp___8 = strcmp(name, "no-comments");
#line 489
          fprintf(_coverage_fout, "305\n");
#line 489
          fflush(_coverage_fout);
#line 491
          if (tmp___8 == 0) {
#line 491
            fprintf(_coverage_fout, "281\n");
#line 491
            fflush(_coverage_fout);
#line 492
            config.allow_yaml_comments = 0;
#line 491
            fprintf(_coverage_fout, "282\n");
#line 491
            fflush(_coverage_fout);
#line 492
            config.allow_c_comments = config.allow_yaml_comments;
          } else {
#line 491
            fprintf(_coverage_fout, "302\n");
#line 491
            fflush(_coverage_fout);
#line 493
            tmp___7 = strcmp(name, "format");
#line 491
            fprintf(_coverage_fout, "303\n");
#line 491
            fflush(_coverage_fout);
#line 493
            if (tmp___7 == 0) {
#line 493
              fprintf(_coverage_fout, "283\n");
#line 493
              fflush(_coverage_fout);
#line 494
              format = 1;
            } else {
#line 493
              fprintf(_coverage_fout, "300\n");
#line 493
              fflush(_coverage_fout);
#line 495
              tmp___6 = strcmp(name, "verify");
#line 493
              fprintf(_coverage_fout, "301\n");
#line 493
              fflush(_coverage_fout);
#line 495
              if (tmp___6 == 0) {
#line 495
                fprintf(_coverage_fout, "284\n");
#line 495
                fflush(_coverage_fout);
#line 496
                verify = 1;
              } else {
#line 495
                fprintf(_coverage_fout, "298\n");
#line 495
                fflush(_coverage_fout);
#line 497
                tmp___5 = strcmp(name, "max-nesting");
#line 495
                fprintf(_coverage_fout, "299\n");
#line 495
                fflush(_coverage_fout);
#line 497
                if (tmp___5 == 0) {
#line 497
                  fprintf(_coverage_fout, "285\n");
#line 497
                  fflush(_coverage_fout);
#line 498
                  tmp___0 = atoi((char const   *)optarg);
#line 497
                  fprintf(_coverage_fout, "286\n");
#line 497
                  fflush(_coverage_fout);
#line 498
                  config.max_nesting = (unsigned int )tmp___0;
                } else {
#line 497
                  fprintf(_coverage_fout, "296\n");
#line 497
                  fflush(_coverage_fout);
#line 499
                  tmp___4 = strcmp(name, "max-data");
#line 497
                  fprintf(_coverage_fout, "297\n");
#line 497
                  fflush(_coverage_fout);
#line 499
                  if (tmp___4 == 0) {
#line 499
                    fprintf(_coverage_fout, "287\n");
#line 499
                    fflush(_coverage_fout);
#line 500
                    tmp___1 = atoi((char const   *)optarg);
#line 499
                    fprintf(_coverage_fout, "288\n");
#line 499
                    fflush(_coverage_fout);
#line 500
                    config.max_data = (unsigned int )tmp___1;
                  } else {
#line 499
                    fprintf(_coverage_fout, "294\n");
#line 499
                    fflush(_coverage_fout);
#line 501
                    tmp___3 = strcmp(name, "indent-string");
#line 499
                    fprintf(_coverage_fout, "295\n");
#line 499
                    fflush(_coverage_fout);
#line 501
                    if (tmp___3 == 0) {
#line 501
                      fprintf(_coverage_fout, "289\n");
#line 501
                      fflush(_coverage_fout);
#line 502
                      indent_string = strdup((char const   *)optarg);
                    } else {
#line 501
                      fprintf(_coverage_fout, "292\n");
#line 501
                      fflush(_coverage_fout);
#line 503
                      tmp___2 = strcmp(name, "tree");
#line 501
                      fprintf(_coverage_fout, "293\n");
#line 501
                      fflush(_coverage_fout);
#line 503
                      if (tmp___2 == 0) {
#line 503
                        fprintf(_coverage_fout, "290\n");
#line 503
                        fflush(_coverage_fout);
#line 504
                        use_tree = 1;
                      } else {
#line 503
                        fprintf(_coverage_fout, "291\n");
#line 503
                        fflush(_coverage_fout);

                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
#line 505
    break;
#line 482
    fprintf(_coverage_fout, "313\n");
#line 482
    fflush(_coverage_fout);
    case 111: 
#line 508
    output = strdup((char const   *)optarg);
#line 509
    break;
#line 482
    fprintf(_coverage_fout, "314\n");
#line 482
    fflush(_coverage_fout);
    default: ;
#line 511
    break;
    }
  }
#line 451
  fprintf(_coverage_fout, "396\n");
#line 451
  fflush(_coverage_fout);
#line 514
  if (config.max_nesting < 0U) {
#line 514
    fprintf(_coverage_fout, "362\n");
#line 514
    fflush(_coverage_fout);
#line 515
    config.max_nesting = 0U;
  } else {
#line 514
    fprintf(_coverage_fout, "363\n");
#line 514
    fflush(_coverage_fout);

  }
#line 451
  fprintf(_coverage_fout, "397\n");
#line 451
  fflush(_coverage_fout);
#line 516
  if (! output) {
#line 516
    fprintf(_coverage_fout, "364\n");
#line 516
    fflush(_coverage_fout);
#line 517
    output = (char *)"-";
  } else {
#line 516
    fprintf(_coverage_fout, "365\n");
#line 516
    fflush(_coverage_fout);

  }
#line 451
  fprintf(_coverage_fout, "398\n");
#line 451
  fflush(_coverage_fout);
#line 518
  if (optind >= argc) {
#line 518
    fprintf(_coverage_fout, "366\n");
#line 518
    fflush(_coverage_fout);
#line 519
    usage((char const   *)*(argv + 0));
  } else {
#line 518
    fprintf(_coverage_fout, "367\n");
#line 518
    fflush(_coverage_fout);

  }
#line 451
  fprintf(_coverage_fout, "399\n");
#line 451
  fflush(_coverage_fout);
#line 521
  i = optind;
#line 451
  fprintf(_coverage_fout, "400\n");
#line 451
  fflush(_coverage_fout);
#line 521
  while (1) {
#line 521
    fprintf(_coverage_fout, "381\n");
#line 521
    fflush(_coverage_fout);
#line 521
    if (i < argc) {
#line 521
      fprintf(_coverage_fout, "368\n");
#line 521
      fflush(_coverage_fout);

    } else {
#line 521
      break;
    }
#line 521
    fprintf(_coverage_fout, "382\n");
#line 521
    fflush(_coverage_fout);
#line 522
    if (use_tree) {
#line 522
      fprintf(_coverage_fout, "371\n");
#line 522
      fflush(_coverage_fout);
#line 524
      ret = do_tree(& config, (char const   *)*(argv + i), & root_structure);
#line 522
      fprintf(_coverage_fout, "372\n");
#line 522
      fflush(_coverage_fout);
#line 525
      if (ret) {
#line 525
        fprintf(_coverage_fout, "369\n");
#line 525
        fflush(_coverage_fout);
#line 526
        exit(ret);
      } else {
#line 525
        fprintf(_coverage_fout, "370\n");
#line 525
        fflush(_coverage_fout);

      }
#line 522
      fprintf(_coverage_fout, "373\n");
#line 522
      fflush(_coverage_fout);
#line 527
      print_tree(root_structure, output);
    } else {
#line 522
      fprintf(_coverage_fout, "378\n");
#line 522
      fflush(_coverage_fout);
#line 529
      if (format) {
#line 529
        fprintf(_coverage_fout, "374\n");
#line 529
        fflush(_coverage_fout);
#line 530
        ret = do_format(& config, (char const   *)*(argv + i),
                        (char const   *)output);
      } else {
#line 529
        fprintf(_coverage_fout, "377\n");
#line 529
        fflush(_coverage_fout);
#line 531
        if (verify) {
#line 531
          fprintf(_coverage_fout, "375\n");
#line 531
          fflush(_coverage_fout);
#line 532
          ret = do_verify(& config, (char const   *)*(argv + i));
        } else {
#line 531
          fprintf(_coverage_fout, "376\n");
#line 531
          fflush(_coverage_fout);
#line 534
          ret = do_parse(& config, (char const   *)*(argv + i));
        }
      }
    }
#line 521
    fprintf(_coverage_fout, "383\n");
#line 521
    fflush(_coverage_fout);
#line 536
    if (ret) {
#line 536
      fprintf(_coverage_fout, "379\n");
#line 536
      fflush(_coverage_fout);
#line 537
      exit(ret);
    } else {
#line 536
      fprintf(_coverage_fout, "380\n");
#line 536
      fflush(_coverage_fout);

    }
#line 521
    fprintf(_coverage_fout, "384\n");
#line 521
    fflush(_coverage_fout);
#line 521
    i ++;
  }
#line 451
  fprintf(_coverage_fout, "401\n");
#line 451
  fflush(_coverage_fout);
#line 539
  return (ret);
}
}
#line 1 "json.o"
/* #pragma merger(0,"/tmp/cil-mkv59NCk.i","-Wall,-Os,-fPIC") */
#line 242 "/usr/include/string.h"
extern  __attribute__((__nothrow__)) size_t strlen(char const   *__s )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 139 "json.h"
int json_parser_char(json_parser *parser , unsigned char ch ) ;
#line 155
int json_print_raw(json_printer *printer , int type , char const   *data ,
                   uint32_t length ) ;
#line 162
int json_print_args(json_printer *printer ,
                    int (*f)(json_printer * , int  ,
                             char const   * , uint32_t  )  , ...) ;
#line 202
int json_parser_dom_free(json_parser_dom *dom ) ;
#line 58 "json.c"
static uint8_t character_class[128]  = 
#line 58 "json.c"
  {      (unsigned char)254,      (unsigned char)254,      (unsigned char)254,      (unsigned char)254, 
        (unsigned char)254,      (unsigned char)254,      (unsigned char)254,      (unsigned char)254, 
        (unsigned char)254,      (unsigned char)2,      (unsigned char)1,      (unsigned char)254, 
        (unsigned char)254,      (unsigned char)2,      (unsigned char)254,      (unsigned char)254, 
        (unsigned char)254,      (unsigned char)254,      (unsigned char)254,      (unsigned char)254, 
        (unsigned char)254,      (unsigned char)254,      (unsigned char)254,      (unsigned char)254, 
        (unsigned char)254,      (unsigned char)254,      (unsigned char)254,      (unsigned char)254, 
        (unsigned char)254,      (unsigned char)254,      (unsigned char)254,      (unsigned char)254, 
        (unsigned char)0,      (unsigned char)31,      (unsigned char)9,      (unsigned char)33, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)32,      (unsigned char)12, 
        (unsigned char)8,      (unsigned char)13,      (unsigned char)14,      (unsigned char)11, 
        (unsigned char)15,      (unsigned char)16,      (unsigned char)16,      (unsigned char)16, 
        (unsigned char)16,      (unsigned char)16,      (unsigned char)16,      (unsigned char)16, 
        (unsigned char)16,      (unsigned char)16,      (unsigned char)7,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)29,      (unsigned char)29,      (unsigned char)29, 
        (unsigned char)29,      (unsigned char)30,      (unsigned char)29,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)5, 
        (unsigned char)10,      (unsigned char)6,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)17,      (unsigned char)18,      (unsigned char)19, 
        (unsigned char)20,      (unsigned char)21,      (unsigned char)22,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)23,      (unsigned char)31,      (unsigned char)24,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)25,      (unsigned char)26, 
        (unsigned char)27,      (unsigned char)28,      (unsigned char)31,      (unsigned char)31, 
        (unsigned char)31,      (unsigned char)31,      (unsigned char)31,      (unsigned char)3, 
        (unsigned char)31,      (unsigned char)4,      (unsigned char)31,      (unsigned char)31};
#line 81 "json.c"
static char *character_escape[36]  = 
#line 81
  {      (char *)"\\u0000",      (char *)"\\u0001",      (char *)"\\u0002",      (char *)"\\u0003", 
        (char *)"\\u0004",      (char *)"\\u0005",      (char *)"\\u0006",      (char *)"\\u0007", 
        (char *)"\\b",      (char *)"\\t",      (char *)"\\n",      (char *)"\\u000b", 
        (char *)"\\f",      (char *)"\\r",      (char *)"\\u000e",      (char *)"\\u000f", 
        (char *)"\\u0010",      (char *)"\\u0011",      (char *)"\\u0012",      (char *)"\\u0013", 
        (char *)"\\u0014",      (char *)"\\u0015",      (char *)"\\u0016",      (char *)"\\u0017", 
        (char *)"\\u0018",      (char *)"\\u0019",      (char *)"\\u001a",      (char *)"\\u001b", 
        (char *)"\\u001c",      (char *)"\\u001d",      (char *)"\\u001e",      (char *)"\\u001f", 
        (char *)" ",      (char *)"!",      (char *)"\\\"",      (char *)"#"};
#line 157 "json.c"
static uint8_t const   state_transition_table[37][34]  = 
#line 157
  { {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )132, 
            (unsigned char const   )255,        (unsigned char const   )130,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )255, 
            (unsigned char const   )133,        (unsigned char const   )255,        (unsigned char const   )131,        (unsigned char const   )255, 
            (unsigned char const   )129,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )2,        (unsigned char const   )2,        (unsigned char const   )2,        (unsigned char const   )255, 
            (unsigned char const   )133,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )7,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )3,        (unsigned char const   )3,        (unsigned char const   )3,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )7,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )4,        (unsigned char const   )4,        (unsigned char const   )4,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )128, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )5,        (unsigned char const   )5,        (unsigned char const   )5,        (unsigned char const   )132, 
            (unsigned char const   )255,        (unsigned char const   )130,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )7,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )143,        (unsigned char const   )255,        (unsigned char const   )144, 
            (unsigned char const   )145,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )24,        (unsigned char const   )255, 
            (unsigned char const   )28,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )21, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )6,        (unsigned char const   )6,        (unsigned char const   )6,        (unsigned char const   )132, 
            (unsigned char const   )255,        (unsigned char const   )130,        (unsigned char const   )131,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )7,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )143,        (unsigned char const   )255,        (unsigned char const   )144, 
            (unsigned char const   )145,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )24,        (unsigned char const   )255, 
            (unsigned char const   )28,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )21, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )7,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )142,        (unsigned char const   )8,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7, 
            (unsigned char const   )7,        (unsigned char const   )7}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )7, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )7,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )7,        (unsigned char const   )255, 
            (unsigned char const   )7,        (unsigned char const   )7,        (unsigned char const   )255,        (unsigned char const   )7, 
            (unsigned char const   )9,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )10, 
            (unsigned char const   )10,        (unsigned char const   )10,        (unsigned char const   )10,        (unsigned char const   )10, 
            (unsigned char const   )10,        (unsigned char const   )10,        (unsigned char const   )10,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )10,        (unsigned char const   )10,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )11, 
            (unsigned char const   )11,        (unsigned char const   )11,        (unsigned char const   )11,        (unsigned char const   )11, 
            (unsigned char const   )11,        (unsigned char const   )11,        (unsigned char const   )11,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )11,        (unsigned char const   )11,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )12, 
            (unsigned char const   )12,        (unsigned char const   )12,        (unsigned char const   )12,        (unsigned char const   )12, 
            (unsigned char const   )12,        (unsigned char const   )12,        (unsigned char const   )12,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )12,        (unsigned char const   )12,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )146, 
            (unsigned char const   )146,        (unsigned char const   )146,        (unsigned char const   )146,        (unsigned char const   )146, 
            (unsigned char const   )146,        (unsigned char const   )146,        (unsigned char const   )146,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )146,        (unsigned char const   )146,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )14, 
            (unsigned char const   )15,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )255, 
            (unsigned char const   )133,        (unsigned char const   )255,        (unsigned char const   )131,        (unsigned char const   )255, 
            (unsigned char const   )129,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )141,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )255, 
            (unsigned char const   )133,        (unsigned char const   )255,        (unsigned char const   )131,        (unsigned char const   )255, 
            (unsigned char const   )129,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )141,        (unsigned char const   )15, 
            (unsigned char const   )15,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )140,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )140,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )17, 
            (unsigned char const   )17,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )255, 
            (unsigned char const   )133,        (unsigned char const   )255,        (unsigned char const   )131,        (unsigned char const   )255, 
            (unsigned char const   )129,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )134, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )17, 
            (unsigned char const   )17,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )18,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )18,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )135}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )19,        (unsigned char const   )19,        (unsigned char const   )255,        (unsigned char const   )20, 
            (unsigned char const   )20,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )20, 
            (unsigned char const   )20,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )255, 
            (unsigned char const   )133,        (unsigned char const   )255,        (unsigned char const   )131,        (unsigned char const   )255, 
            (unsigned char const   )129,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )20, 
            (unsigned char const   )20,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )22,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )23,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )138,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )25,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )26, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )27,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )137,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )29,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )30, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )139, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )32,        (unsigned char const   )255}, 
   {        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )33,        (unsigned char const   )32}, 
   {        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )136, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32,        (unsigned char const   )32, 
            (unsigned char const   )33,        (unsigned char const   )32}, 
   {        (unsigned char const   )34,        (unsigned char const   )136,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34,        (unsigned char const   )34, 
            (unsigned char const   )34,        (unsigned char const   )34}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )36,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}, 
   {        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )9,        (unsigned char const   )255,        (unsigned char const   )255,        (unsigned char const   )255, 
            (unsigned char const   )255,        (unsigned char const   )255}};
#line 206 "json.c"
static uint8_t const   buffer_policy_table[37][34]  = 
#line 206
  { {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )2,        (unsigned char const   )2,        (unsigned char const   )2, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )2,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )2,        (unsigned char const   )0, 
            (unsigned char const   )2,        (unsigned char const   )2,        (unsigned char const   )0,        (unsigned char const   )2, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )1,        (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )1, 
            (unsigned char const   )1,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}, 
   {        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0,        (unsigned char const   )0, 
            (unsigned char const   )0,        (unsigned char const   )0}};
#line 255 "json.c"
__inline static void *memory_realloc(void *(*realloc_fct)(void * , size_t  ) ,
                                     void *ptr , size_t size ) 
{ void *tmp___1 ;

  {
#line 255
  fprintf(_coverage_fout, "404\n");
#line 255
  fflush(_coverage_fout);
#line 257
  if (realloc_fct) {
#line 257
    fprintf(_coverage_fout, "402\n");
#line 257
    fflush(_coverage_fout);
#line 257
    tmp___1 = (*realloc_fct)(ptr, size);
  } else {
#line 257
    fprintf(_coverage_fout, "403\n");
#line 257
    fflush(_coverage_fout);
#line 257
    tmp___1 = realloc(ptr, size);
  }
#line 255
  fprintf(_coverage_fout, "405\n");
#line 255
  fflush(_coverage_fout);
#line 257
  return (tmp___1);
}
}
#line 260 "json.c"
__inline static void *memory_calloc(void *(*calloc_fct)(size_t  , size_t  ) ,
                                    size_t nmemb , size_t size ) 
{ void *tmp___1 ;

  {
#line 260
  fprintf(_coverage_fout, "408\n");
#line 260
  fflush(_coverage_fout);
#line 262
  if (calloc_fct) {
#line 262
    fprintf(_coverage_fout, "406\n");
#line 262
    fflush(_coverage_fout);
#line 262
    tmp___1 = (*calloc_fct)(nmemb, size);
  } else {
#line 262
    fprintf(_coverage_fout, "407\n");
#line 262
    fflush(_coverage_fout);
#line 262
    tmp___1 = calloc(nmemb, size);
  }
#line 260
  fprintf(_coverage_fout, "409\n");
#line 260
  fflush(_coverage_fout);
#line 262
  return (tmp___1);
}
}
#line 268 "json.c"
static int state_grow(json_parser *parser ) 
{ uint32_t newsize ;
  void *ptr ;

  {
#line 268
  fprintf(_coverage_fout, "414\n");
#line 268
  fflush(_coverage_fout);
#line 270
  newsize = parser->stack_size * 2U;
#line 268
  fprintf(_coverage_fout, "415\n");
#line 268
  fflush(_coverage_fout);
#line 273
  if (parser->config.max_nesting != 0U) {
#line 273
    fprintf(_coverage_fout, "410\n");
#line 273
    fflush(_coverage_fout);
#line 274
    return (5);
  } else {
#line 273
    fprintf(_coverage_fout, "411\n");
#line 273
    fflush(_coverage_fout);

  }
#line 268
  fprintf(_coverage_fout, "416\n");
#line 268
  fflush(_coverage_fout);
#line 276
  ptr = memory_realloc(parser->config.user_realloc, (void *)parser->stack,
                       (unsigned long )newsize * sizeof(uint8_t ));
#line 268
  fprintf(_coverage_fout, "417\n");
#line 268
  fflush(_coverage_fout);
#line 277
  if (! ptr) {
#line 277
    fprintf(_coverage_fout, "412\n");
#line 277
    fflush(_coverage_fout);
#line 278
    return (1);
  } else {
#line 277
    fprintf(_coverage_fout, "413\n");
#line 277
    fflush(_coverage_fout);

  }
#line 268
  fprintf(_coverage_fout, "418\n");
#line 268
  fflush(_coverage_fout);
#line 279
  parser->stack = (uint8_t *)ptr;
#line 268
  fprintf(_coverage_fout, "419\n");
#line 268
  fflush(_coverage_fout);
#line 280
  parser->stack_size = newsize;
#line 268
  fprintf(_coverage_fout, "420\n");
#line 268
  fflush(_coverage_fout);
#line 281
  return (0);
}
}
#line 284 "json.c"
static int state_push(json_parser *parser , int mode ) 
{ int ret ;
  int tmp ;
  uint32_t tmp___0 ;

  {
#line 284
  fprintf(_coverage_fout, "427\n");
#line 284
  fflush(_coverage_fout);
#line 286
  if (parser->stack_offset >= parser->stack_size) {
#line 286
    fprintf(_coverage_fout, "423\n");
#line 286
    fflush(_coverage_fout);
#line 287
    tmp = state_grow(parser);
#line 286
    fprintf(_coverage_fout, "424\n");
#line 286
    fflush(_coverage_fout);
#line 287
    ret = tmp;
#line 286
    fprintf(_coverage_fout, "425\n");
#line 286
    fflush(_coverage_fout);
#line 288
    if (ret) {
#line 288
      fprintf(_coverage_fout, "421\n");
#line 288
      fflush(_coverage_fout);
#line 289
      return (ret);
    } else {
#line 288
      fprintf(_coverage_fout, "422\n");
#line 288
      fflush(_coverage_fout);

    }
  } else {
#line 286
    fprintf(_coverage_fout, "426\n");
#line 286
    fflush(_coverage_fout);

  }
#line 284
  fprintf(_coverage_fout, "428\n");
#line 284
  fflush(_coverage_fout);
#line 291
  tmp___0 = parser->stack_offset;
#line 284
  fprintf(_coverage_fout, "429\n");
#line 284
  fflush(_coverage_fout);
#line 291
  (parser->stack_offset) ++;
#line 284
  fprintf(_coverage_fout, "430\n");
#line 284
  fflush(_coverage_fout);
#line 291
  *(parser->stack + tmp___0) = (unsigned char )mode;
#line 284
  fprintf(_coverage_fout, "431\n");
#line 284
  fflush(_coverage_fout);
#line 292
  return (0);
}
}
#line 295 "json.c"
static int state_pop(json_parser *parser , int mode ) 
{ 

  {
#line 295
  fprintf(_coverage_fout, "436\n");
#line 295
  fflush(_coverage_fout);
#line 297
  if (parser->stack_offset == 0U) {
#line 297
    fprintf(_coverage_fout, "432\n");
#line 297
    fflush(_coverage_fout);
#line 298
    return (3);
  } else {
#line 297
    fprintf(_coverage_fout, "433\n");
#line 297
    fflush(_coverage_fout);

  }
#line 295
  fprintf(_coverage_fout, "437\n");
#line 295
  fflush(_coverage_fout);
#line 299
  (parser->stack_offset) --;
#line 295
  fprintf(_coverage_fout, "438\n");
#line 295
  fflush(_coverage_fout);
#line 300
  if ((int )*(parser->stack + parser->stack_offset) != mode) {
#line 300
    fprintf(_coverage_fout, "434\n");
#line 300
    fflush(_coverage_fout);
#line 301
    return (4);
  } else {
#line 300
    fprintf(_coverage_fout, "435\n");
#line 300
    fflush(_coverage_fout);

  }
#line 295
  fprintf(_coverage_fout, "439\n");
#line 295
  fflush(_coverage_fout);
#line 302
  return (0);
}
}
#line 305 "json.c"
static int buffer_grow(json_parser *parser ) 
{ uint32_t newsize ;
  void *ptr ;
  int max ;

  {
#line 305
  fprintf(_coverage_fout, "450\n");
#line 305
  fflush(_coverage_fout);
#line 309
  max = (int )parser->config.max_data;
#line 305
  fprintf(_coverage_fout, "451\n");
#line 305
  fflush(_coverage_fout);
#line 311
  if (max > 0) {
#line 311
    fprintf(_coverage_fout, "442\n");
#line 311
    fflush(_coverage_fout);
#line 311
    if (parser->buffer_size == (unsigned int )max) {
#line 311
      fprintf(_coverage_fout, "440\n");
#line 311
      fflush(_coverage_fout);
#line 312
      return (6);
    } else {
#line 311
      fprintf(_coverage_fout, "441\n");
#line 311
      fflush(_coverage_fout);

    }
  } else {
#line 311
    fprintf(_coverage_fout, "443\n");
#line 311
    fflush(_coverage_fout);

  }
#line 305
  fprintf(_coverage_fout, "452\n");
#line 305
  fflush(_coverage_fout);
#line 313
  newsize = parser->buffer_size * 2U;
#line 305
  fprintf(_coverage_fout, "453\n");
#line 305
  fflush(_coverage_fout);
#line 314
  if (max > 0) {
#line 314
    fprintf(_coverage_fout, "446\n");
#line 314
    fflush(_coverage_fout);
#line 314
    if (newsize > (unsigned int )max) {
#line 314
      fprintf(_coverage_fout, "444\n");
#line 314
      fflush(_coverage_fout);
#line 315
      newsize = (unsigned int )max;
    } else {
#line 314
      fprintf(_coverage_fout, "445\n");
#line 314
      fflush(_coverage_fout);

    }
  } else {
#line 314
    fprintf(_coverage_fout, "447\n");
#line 314
    fflush(_coverage_fout);

  }
#line 305
  fprintf(_coverage_fout, "454\n");
#line 305
  fflush(_coverage_fout);
#line 317
  ptr = memory_realloc(parser->config.user_realloc, (void *)parser->buffer,
                       (unsigned long )newsize * sizeof(char ));
#line 305
  fprintf(_coverage_fout, "455\n");
#line 305
  fflush(_coverage_fout);
#line 318
  if (! ptr) {
#line 318
    fprintf(_coverage_fout, "448\n");
#line 318
    fflush(_coverage_fout);
#line 319
    return (1);
  } else {
#line 318
    fprintf(_coverage_fout, "449\n");
#line 318
    fflush(_coverage_fout);

  }
#line 305
  fprintf(_coverage_fout, "456\n");
#line 305
  fflush(_coverage_fout);
#line 320
  parser->buffer = (char *)ptr;
#line 305
  fprintf(_coverage_fout, "457\n");
#line 305
  fflush(_coverage_fout);
#line 321
  parser->buffer_size = newsize;
#line 305
  fprintf(_coverage_fout, "458\n");
#line 305
  fflush(_coverage_fout);
#line 322
  return (0);
}
}
#line 325 "json.c"
static int buffer_push(json_parser *parser , unsigned char c ) 
{ int ret ;
  uint32_t tmp ;

  {
#line 325
  fprintf(_coverage_fout, "464\n");
#line 325
  fflush(_coverage_fout);
#line 329
  if (parser->buffer_offset + 1U >= parser->buffer_size) {
#line 329
    fprintf(_coverage_fout, "461\n");
#line 329
    fflush(_coverage_fout);
#line 330
    ret = buffer_grow(parser);
#line 329
    fprintf(_coverage_fout, "462\n");
#line 329
    fflush(_coverage_fout);
#line 331
    if (ret) {
#line 331
      fprintf(_coverage_fout, "459\n");
#line 331
      fflush(_coverage_fout);
#line 332
      return (ret);
    } else {
#line 331
      fprintf(_coverage_fout, "460\n");
#line 331
      fflush(_coverage_fout);

    }
  } else {
#line 329
    fprintf(_coverage_fout, "463\n");
#line 329
    fflush(_coverage_fout);

  }
#line 325
  fprintf(_coverage_fout, "465\n");
#line 325
  fflush(_coverage_fout);
#line 334
  tmp = parser->buffer_offset;
#line 325
  fprintf(_coverage_fout, "466\n");
#line 325
  fflush(_coverage_fout);
#line 334
  (parser->buffer_offset) ++;
#line 325
  fprintf(_coverage_fout, "467\n");
#line 325
  fflush(_coverage_fout);
#line 334
  *(parser->buffer + tmp) = (char )c;
#line 325
  fprintf(_coverage_fout, "468\n");
#line 325
  fflush(_coverage_fout);
#line 335
  return (0);
}
}
#line 338 "json.c"
static int do_callback_withbuf(json_parser *parser , int type ) 
{ int tmp ;

  {
#line 338
  fprintf(_coverage_fout, "471\n");
#line 338
  fflush(_coverage_fout);
#line 340
  if (! parser->callback) {
#line 340
    fprintf(_coverage_fout, "469\n");
#line 340
    fflush(_coverage_fout);
#line 341
    return (0);
  } else {
#line 340
    fprintf(_coverage_fout, "470\n");
#line 340
    fflush(_coverage_fout);

  }
#line 338
  fprintf(_coverage_fout, "472\n");
#line 338
  fflush(_coverage_fout);
#line 342
  *(parser->buffer + parser->buffer_offset) = (char )'\000';
#line 338
  fprintf(_coverage_fout, "473\n");
#line 338
  fflush(_coverage_fout);
#line 343
  tmp = (*(parser->callback))(parser->userdata, type,
                              (char const   *)parser->buffer,
                              parser->buffer_offset);
#line 338
  fprintf(_coverage_fout, "474\n");
#line 338
  fflush(_coverage_fout);
#line 343
  return (tmp);
}
}
#line 346 "json.c"
static int do_callback(json_parser *parser , int type ) 
{ int tmp ;

  {
#line 346
  fprintf(_coverage_fout, "477\n");
#line 346
  fflush(_coverage_fout);
#line 348
  if (! parser->callback) {
#line 348
    fprintf(_coverage_fout, "475\n");
#line 348
    fflush(_coverage_fout);
#line 349
    return (0);
  } else {
#line 348
    fprintf(_coverage_fout, "476\n");
#line 348
    fflush(_coverage_fout);

  }
#line 346
  fprintf(_coverage_fout, "478\n");
#line 346
  fflush(_coverage_fout);
#line 350
  tmp = (*(parser->callback))(parser->userdata, type,
                              (char const   *)((void *)0), 0U);
#line 346
  fprintf(_coverage_fout, "479\n");
#line 346
  fflush(_coverage_fout);
#line 350
  return (tmp);
}
}
#line 353 "json.c"
static int do_buffer(json_parser *parser ) 
{ int ret ;

  {
#line 353
  fprintf(_coverage_fout, "485\n");
#line 353
  fflush(_coverage_fout);
#line 355
  ret = 0;
#line 357
  switch ((int )parser->type) {
#line 357
  fprintf(_coverage_fout, "482\n");
#line 357
  fflush(_coverage_fout);
  case 8: 
  case 7: 
  case 6: 
  case 5: 
  case 11: 
  case 9: 
  case 10: 
#line 361
  ret = do_callback_withbuf(parser, (int )parser->type);
#line 357
  fprintf(_coverage_fout, "483\n");
#line 357
  fflush(_coverage_fout);
#line 362
  if (ret) {
#line 362
    fprintf(_coverage_fout, "480\n");
#line 362
    fflush(_coverage_fout);
#line 363
    return (ret);
  } else {
#line 362
    fprintf(_coverage_fout, "481\n");
#line 362
    fflush(_coverage_fout);

  }
#line 364
  break;
#line 357
  fprintf(_coverage_fout, "484\n");
#line 357
  fflush(_coverage_fout);
  default: ;
#line 366
  break;
  }
#line 353
  fprintf(_coverage_fout, "486\n");
#line 353
  fflush(_coverage_fout);
#line 368
  parser->buffer_offset = 0U;
#line 353
  fprintf(_coverage_fout, "487\n");
#line 353
  fflush(_coverage_fout);
#line 369
  return (ret);
}
}
#line 372 "json.c"
static uint8_t const   hextable[128]  = 
#line 372
  {      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )0,      (unsigned char const   )1,      (unsigned char const   )2,      (unsigned char const   )3, 
        (unsigned char const   )4,      (unsigned char const   )5,      (unsigned char const   )6,      (unsigned char const   )7, 
        (unsigned char const   )8,      (unsigned char const   )9,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )10,      (unsigned char const   )11,      (unsigned char const   )12, 
        (unsigned char const   )13,      (unsigned char const   )14,      (unsigned char const   )15,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )10,      (unsigned char const   )11,      (unsigned char const   )12, 
        (unsigned char const   )13,      (unsigned char const   )14,      (unsigned char const   )15,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1, 
        (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1,      (unsigned char const   )-1};
#line 391 "json.c"
static int decode_unicode_char(json_parser *parser ) 
{ uint32_t uval ;
  char *b ;
  int offset ;
  uint32_t tmp ;
  uint32_t tmp___0 ;
  uint32_t tmp___1 ;
  uint32_t tmp___2 ;
  uint32_t tmp___3 ;
  uint32_t tmp___4 ;
  uint32_t tmp___5 ;
  uint32_t tmp___6 ;
  uint32_t tmp___7 ;
  uint32_t tmp___8 ;

  {
#line 391
  fprintf(_coverage_fout, "534\n");
#line 391
  fflush(_coverage_fout);
#line 394
  b = parser->buffer;
#line 391
  fprintf(_coverage_fout, "535\n");
#line 391
  fflush(_coverage_fout);
#line 395
  offset = (int )parser->buffer_offset;
#line 391
  fprintf(_coverage_fout, "536\n");
#line 391
  fflush(_coverage_fout);
#line 397
  uval = (unsigned int )(((((int const   )hextable[(unsigned char )*(b + (offset - 4))] << 12) | ((int const   )hextable[(unsigned char )*(b + (offset - 3))] << 8)) | ((int const   )hextable[(unsigned char )*(b + (offset - 2))] << 4)) | (int const   )hextable[(unsigned char )*(b + (offset - 1))]);
#line 391
  fprintf(_coverage_fout, "537\n");
#line 391
  fflush(_coverage_fout);
#line 400
  parser->buffer_offset -= 4U;
#line 391
  fprintf(_coverage_fout, "538\n");
#line 391
  fflush(_coverage_fout);
#line 403
  if (! parser->unicode_multi) {
#line 403
    fprintf(_coverage_fout, "493\n");
#line 403
    fflush(_coverage_fout);
#line 403
    if (uval < 128U) {
#line 403
      fprintf(_coverage_fout, "488\n");
#line 403
      fflush(_coverage_fout);
#line 404
      tmp = parser->buffer_offset;
#line 403
      fprintf(_coverage_fout, "489\n");
#line 403
      fflush(_coverage_fout);
#line 404
      (parser->buffer_offset) ++;
#line 403
      fprintf(_coverage_fout, "490\n");
#line 403
      fflush(_coverage_fout);
#line 404
      *(b + tmp) = (char )uval;
#line 403
      fprintf(_coverage_fout, "491\n");
#line 403
      fflush(_coverage_fout);
#line 405
      return (0);
    } else {
#line 403
      fprintf(_coverage_fout, "492\n");
#line 403
      fflush(_coverage_fout);

    }
  } else {
#line 403
    fprintf(_coverage_fout, "494\n");
#line 403
    fflush(_coverage_fout);

  }
#line 391
  fprintf(_coverage_fout, "539\n");
#line 391
  fflush(_coverage_fout);
#line 408
  if (parser->unicode_multi) {
#line 408
    fprintf(_coverage_fout, "497\n");
#line 408
    fflush(_coverage_fout);
#line 409
    if (! ((uval & 64512U) == 56320U)) {
#line 409
      fprintf(_coverage_fout, "495\n");
#line 409
      fflush(_coverage_fout);
#line 410
      return (9);
    } else {
#line 409
      fprintf(_coverage_fout, "496\n");
#line 409
      fflush(_coverage_fout);

    }
#line 408
    fprintf(_coverage_fout, "498\n");
#line 408
    fflush(_coverage_fout);
#line 412
    uval = (unsigned int )(65536 + (((int )parser->unicode_multi & 1023) << 10)) + (uval & 1023U);
#line 408
    fprintf(_coverage_fout, "499\n");
#line 408
    fflush(_coverage_fout);
#line 413
    tmp___0 = parser->buffer_offset;
#line 408
    fprintf(_coverage_fout, "500\n");
#line 408
    fflush(_coverage_fout);
#line 413
    (parser->buffer_offset) ++;
#line 408
    fprintf(_coverage_fout, "501\n");
#line 408
    fflush(_coverage_fout);
#line 413
    *(b + tmp___0) = (char )((uval >> 18) | 240U);
#line 408
    fprintf(_coverage_fout, "502\n");
#line 408
    fflush(_coverage_fout);
#line 414
    tmp___1 = parser->buffer_offset;
#line 408
    fprintf(_coverage_fout, "503\n");
#line 408
    fflush(_coverage_fout);
#line 414
    (parser->buffer_offset) ++;
#line 408
    fprintf(_coverage_fout, "504\n");
#line 408
    fflush(_coverage_fout);
#line 414
    *(b + tmp___1) = (char )(((uval >> 12) & 63U) | 128U);
#line 408
    fprintf(_coverage_fout, "505\n");
#line 408
    fflush(_coverage_fout);
#line 415
    tmp___2 = parser->buffer_offset;
#line 408
    fprintf(_coverage_fout, "506\n");
#line 408
    fflush(_coverage_fout);
#line 415
    (parser->buffer_offset) ++;
#line 408
    fprintf(_coverage_fout, "507\n");
#line 408
    fflush(_coverage_fout);
#line 415
    *(b + tmp___2) = (char )(((uval >> 6) & 63U) | 128U);
#line 408
    fprintf(_coverage_fout, "508\n");
#line 408
    fflush(_coverage_fout);
#line 416
    tmp___3 = parser->buffer_offset;
#line 408
    fprintf(_coverage_fout, "509\n");
#line 408
    fflush(_coverage_fout);
#line 416
    (parser->buffer_offset) ++;
#line 408
    fprintf(_coverage_fout, "510\n");
#line 408
    fflush(_coverage_fout);
#line 416
    *(b + tmp___3) = (char )((uval & 63U) | 128U);
#line 408
    fprintf(_coverage_fout, "511\n");
#line 408
    fflush(_coverage_fout);
#line 417
    parser->unicode_multi = (unsigned short)0;
#line 408
    fprintf(_coverage_fout, "512\n");
#line 408
    fflush(_coverage_fout);
#line 418
    return (0);
  } else {
#line 408
    fprintf(_coverage_fout, "513\n");
#line 408
    fflush(_coverage_fout);

  }
#line 391
  fprintf(_coverage_fout, "540\n");
#line 391
  fflush(_coverage_fout);
#line 421
  if ((uval & 64512U) == 56320U) {
#line 421
    fprintf(_coverage_fout, "514\n");
#line 421
    fflush(_coverage_fout);
#line 422
    return (10);
  } else {
#line 421
    fprintf(_coverage_fout, "515\n");
#line 421
    fflush(_coverage_fout);

  }
#line 391
  fprintf(_coverage_fout, "541\n");
#line 391
  fflush(_coverage_fout);
#line 423
  if ((uval & 64512U) == 55296U) {
#line 423
    fprintf(_coverage_fout, "516\n");
#line 423
    fflush(_coverage_fout);
#line 424
    parser->unicode_multi = (unsigned short )uval;
#line 423
    fprintf(_coverage_fout, "517\n");
#line 423
    fflush(_coverage_fout);
#line 425
    return (0);
  } else {
#line 423
    fprintf(_coverage_fout, "518\n");
#line 423
    fflush(_coverage_fout);

  }
#line 391
  fprintf(_coverage_fout, "542\n");
#line 391
  fflush(_coverage_fout);
#line 428
  if (uval < 2048U) {
#line 428
    fprintf(_coverage_fout, "519\n");
#line 428
    fflush(_coverage_fout);
#line 429
    tmp___4 = parser->buffer_offset;
#line 428
    fprintf(_coverage_fout, "520\n");
#line 428
    fflush(_coverage_fout);
#line 429
    (parser->buffer_offset) ++;
#line 428
    fprintf(_coverage_fout, "521\n");
#line 428
    fflush(_coverage_fout);
#line 429
    *(b + tmp___4) = (char )((uval >> 6) | 192U);
#line 428
    fprintf(_coverage_fout, "522\n");
#line 428
    fflush(_coverage_fout);
#line 430
    tmp___5 = parser->buffer_offset;
#line 428
    fprintf(_coverage_fout, "523\n");
#line 428
    fflush(_coverage_fout);
#line 430
    (parser->buffer_offset) ++;
#line 428
    fprintf(_coverage_fout, "524\n");
#line 428
    fflush(_coverage_fout);
#line 430
    *(b + tmp___5) = (char )((uval & 63U) | 128U);
  } else {
#line 428
    fprintf(_coverage_fout, "525\n");
#line 428
    fflush(_coverage_fout);
#line 432
    tmp___6 = parser->buffer_offset;
#line 428
    fprintf(_coverage_fout, "526\n");
#line 428
    fflush(_coverage_fout);
#line 432
    (parser->buffer_offset) ++;
#line 428
    fprintf(_coverage_fout, "527\n");
#line 428
    fflush(_coverage_fout);
#line 432
    *(b + tmp___6) = (char )((uval >> 12) | 224U);
#line 428
    fprintf(_coverage_fout, "528\n");
#line 428
    fflush(_coverage_fout);
#line 433
    tmp___7 = parser->buffer_offset;
#line 428
    fprintf(_coverage_fout, "529\n");
#line 428
    fflush(_coverage_fout);
#line 433
    (parser->buffer_offset) ++;
#line 428
    fprintf(_coverage_fout, "530\n");
#line 428
    fflush(_coverage_fout);
#line 433
    *(b + tmp___7) = (char )(((uval >> 6) & 63U) | 128U);
#line 428
    fprintf(_coverage_fout, "531\n");
#line 428
    fflush(_coverage_fout);
#line 434
    tmp___8 = parser->buffer_offset;
#line 428
    fprintf(_coverage_fout, "532\n");
#line 428
    fflush(_coverage_fout);
#line 434
    (parser->buffer_offset) ++;
#line 428
    fprintf(_coverage_fout, "533\n");
#line 428
    fflush(_coverage_fout);
#line 434
    *(b + tmp___8) = (char )((uval & 63U) | 128U);
  }
#line 391
  fprintf(_coverage_fout, "543\n");
#line 391
  fflush(_coverage_fout);
#line 436
  return (0);
}
}
#line 439 "json.c"
static int buffer_push_escape(json_parser *parser , unsigned char next ) 
{ char c ;
  int tmp ;

  {
#line 439
  fprintf(_coverage_fout, "552\n");
#line 439
  fflush(_coverage_fout);
#line 441
  c = (char )'\000';
#line 443
  switch ((int )next) {
#line 443
  fprintf(_coverage_fout, "544\n");
#line 443
  fflush(_coverage_fout);
  case 98: 
#line 444
  c = (char )'\b';
#line 444
  break;
#line 443
  fprintf(_coverage_fout, "545\n");
#line 443
  fflush(_coverage_fout);
  case 102: 
#line 445
  c = (char )'\f';
#line 445
  break;
#line 443
  fprintf(_coverage_fout, "546\n");
#line 443
  fflush(_coverage_fout);
  case 110: 
#line 446
  c = (char )'\n';
#line 446
  break;
#line 443
  fprintf(_coverage_fout, "547\n");
#line 443
  fflush(_coverage_fout);
  case 114: 
#line 447
  c = (char )'\r';
#line 447
  break;
#line 443
  fprintf(_coverage_fout, "548\n");
#line 443
  fflush(_coverage_fout);
  case 116: 
#line 448
  c = (char )'\t';
#line 448
  break;
#line 443
  fprintf(_coverage_fout, "549\n");
#line 443
  fflush(_coverage_fout);
  case 34: 
#line 449
  c = (char )'\"';
#line 449
  break;
#line 443
  fprintf(_coverage_fout, "550\n");
#line 443
  fflush(_coverage_fout);
  case 47: 
#line 450
  c = (char )'/';
#line 450
  break;
#line 443
  fprintf(_coverage_fout, "551\n");
#line 443
  fflush(_coverage_fout);
  case 92: 
#line 451
  c = (char )'\\';
#line 451
  break;
  }
#line 439
  fprintf(_coverage_fout, "553\n");
#line 439
  fflush(_coverage_fout);
#line 454
  tmp = buffer_push(parser, (unsigned char )c);
#line 439
  fprintf(_coverage_fout, "554\n");
#line 439
  fflush(_coverage_fout);
#line 454
  return (tmp);
}
}
#line 459 "json.c"
int act_uc(json_parser *parser ) 
{ int ret ;

  {
#line 459
  fprintf(_coverage_fout, "559\n");
#line 459
  fflush(_coverage_fout);
#line 462
  ret = decode_unicode_char(parser);
#line 459
  fprintf(_coverage_fout, "560\n");
#line 459
  fflush(_coverage_fout);
#line 462
  if (ret) {
#line 462
    fprintf(_coverage_fout, "555\n");
#line 462
    fflush(_coverage_fout);
#line 462
    return (ret);
  } else {
#line 462
    fprintf(_coverage_fout, "556\n");
#line 462
    fflush(_coverage_fout);

  }
#line 459
  fprintf(_coverage_fout, "561\n");
#line 459
  fflush(_coverage_fout);
#line 463
  if (parser->unicode_multi) {
#line 463
    fprintf(_coverage_fout, "557\n");
#line 463
    fflush(_coverage_fout);
#line 463
    parser->state = (unsigned char)35;
  } else {
#line 463
    fprintf(_coverage_fout, "558\n");
#line 463
    fflush(_coverage_fout);
#line 463
    parser->state = (unsigned char)7;
  }
#line 459
  fprintf(_coverage_fout, "562\n");
#line 459
  fflush(_coverage_fout);
#line 464
  return (0);
}
}
#line 467 "json.c"
int act_yb(json_parser *parser ) 
{ 

  {
#line 467
  fprintf(_coverage_fout, "565\n");
#line 467
  fflush(_coverage_fout);
#line 469
  if (! parser->config.allow_yaml_comments) {
#line 469
    fprintf(_coverage_fout, "563\n");
#line 469
    fflush(_coverage_fout);
#line 470
    return (7);
  } else {
#line 469
    fprintf(_coverage_fout, "564\n");
#line 469
    fflush(_coverage_fout);

  }
#line 467
  fprintf(_coverage_fout, "566\n");
#line 467
  fflush(_coverage_fout);
#line 471
  parser->save_state = parser->state;
#line 467
  fprintf(_coverage_fout, "567\n");
#line 467
  fflush(_coverage_fout);
#line 472
  return (0);
}
}
#line 475 "json.c"
int act_cb(json_parser *parser ) 
{ 

  {
#line 475
  fprintf(_coverage_fout, "570\n");
#line 475
  fflush(_coverage_fout);
#line 477
  if (! parser->config.allow_c_comments) {
#line 477
    fprintf(_coverage_fout, "568\n");
#line 477
    fflush(_coverage_fout);
#line 478
    return (7);
  } else {
#line 477
    fprintf(_coverage_fout, "569\n");
#line 477
    fflush(_coverage_fout);

  }
#line 475
  fprintf(_coverage_fout, "571\n");
#line 475
  fflush(_coverage_fout);
#line 479
  parser->save_state = parser->state;
#line 475
  fprintf(_coverage_fout, "572\n");
#line 475
  fflush(_coverage_fout);
#line 480
  return (0);
}
}
#line 483 "json.c"
int act_ce(json_parser *parser ) 
{ 

  {
#line 483
  fprintf(_coverage_fout, "575\n");
#line 483
  fflush(_coverage_fout);
#line 485
  if ((int )parser->save_state > 6) {
#line 485
    fprintf(_coverage_fout, "573\n");
#line 485
    fflush(_coverage_fout);
#line 485
    parser->state = (unsigned char)1;
  } else {
#line 485
    fprintf(_coverage_fout, "574\n");
#line 485
    fflush(_coverage_fout);
#line 485
    parser->state = parser->save_state;
  }
#line 483
  fprintf(_coverage_fout, "576\n");
#line 483
  fflush(_coverage_fout);
#line 486
  return (0);
}
}
#line 489 "json.c"
int act_ob(json_parser *parser ) 
{ int ret ;

  {
#line 489
  fprintf(_coverage_fout, "581\n");
#line 489
  fflush(_coverage_fout);
#line 492
  ret = do_callback(parser, 2);
#line 489
  fprintf(_coverage_fout, "582\n");
#line 489
  fflush(_coverage_fout);
#line 492
  if (ret) {
#line 492
    fprintf(_coverage_fout, "577\n");
#line 492
    fflush(_coverage_fout);
#line 492
    return (ret);
  } else {
#line 492
    fprintf(_coverage_fout, "578\n");
#line 492
    fflush(_coverage_fout);

  }
#line 489
  fprintf(_coverage_fout, "583\n");
#line 489
  fflush(_coverage_fout);
#line 493
  ret = state_push(parser, 1);
#line 489
  fprintf(_coverage_fout, "584\n");
#line 489
  fflush(_coverage_fout);
#line 493
  if (ret) {
#line 493
    fprintf(_coverage_fout, "579\n");
#line 493
    fflush(_coverage_fout);
#line 493
    return (ret);
  } else {
#line 493
    fprintf(_coverage_fout, "580\n");
#line 493
    fflush(_coverage_fout);

  }
#line 489
  fprintf(_coverage_fout, "585\n");
#line 489
  fflush(_coverage_fout);
#line 494
  parser->expecting_key = (unsigned char)1;
#line 489
  fprintf(_coverage_fout, "586\n");
#line 489
  fflush(_coverage_fout);
#line 495
  return (0);
}
}
#line 498 "json.c"
int act_oe(json_parser *parser ) 
{ int ret ;

  {
#line 498
  fprintf(_coverage_fout, "591\n");
#line 498
  fflush(_coverage_fout);
#line 501
  ret = do_callback(parser, 4);
#line 498
  fprintf(_coverage_fout, "592\n");
#line 498
  fflush(_coverage_fout);
#line 501
  if (ret) {
#line 501
    fprintf(_coverage_fout, "587\n");
#line 501
    fflush(_coverage_fout);
#line 501
    return (ret);
  } else {
#line 501
    fprintf(_coverage_fout, "588\n");
#line 501
    fflush(_coverage_fout);

  }
#line 498
  fprintf(_coverage_fout, "593\n");
#line 498
  fflush(_coverage_fout);
#line 502
  ret = state_pop(parser, 1);
#line 498
  fprintf(_coverage_fout, "594\n");
#line 498
  fflush(_coverage_fout);
#line 502
  if (ret) {
#line 502
    fprintf(_coverage_fout, "589\n");
#line 502
    fflush(_coverage_fout);
#line 502
    return (ret);
  } else {
#line 502
    fprintf(_coverage_fout, "590\n");
#line 502
    fflush(_coverage_fout);

  }
#line 498
  fprintf(_coverage_fout, "595\n");
#line 498
  fflush(_coverage_fout);
#line 503
  parser->expecting_key = (unsigned char)0;
#line 498
  fprintf(_coverage_fout, "596\n");
#line 498
  fflush(_coverage_fout);
#line 504
  return (0);
}
}
#line 507 "json.c"
int act_ab(json_parser *parser ) 
{ int ret ;

  {
#line 507
  fprintf(_coverage_fout, "601\n");
#line 507
  fflush(_coverage_fout);
#line 510
  ret = do_callback(parser, 1);
#line 507
  fprintf(_coverage_fout, "602\n");
#line 507
  fflush(_coverage_fout);
#line 510
  if (ret) {
#line 510
    fprintf(_coverage_fout, "597\n");
#line 510
    fflush(_coverage_fout);
#line 510
    return (ret);
  } else {
#line 510
    fprintf(_coverage_fout, "598\n");
#line 510
    fflush(_coverage_fout);

  }
#line 507
  fprintf(_coverage_fout, "603\n");
#line 507
  fflush(_coverage_fout);
#line 511
  ret = state_push(parser, 0);
#line 507
  fprintf(_coverage_fout, "604\n");
#line 507
  fflush(_coverage_fout);
#line 511
  if (ret) {
#line 511
    fprintf(_coverage_fout, "599\n");
#line 511
    fflush(_coverage_fout);
#line 511
    return (ret);
  } else {
#line 511
    fprintf(_coverage_fout, "600\n");
#line 511
    fflush(_coverage_fout);

  }
#line 507
  fprintf(_coverage_fout, "605\n");
#line 507
  fflush(_coverage_fout);
#line 512
  return (0);
}
}
#line 514 "json.c"
int act_ae(json_parser *parser ) 
{ int ret ;

  {
#line 514
  fprintf(_coverage_fout, "610\n");
#line 514
  fflush(_coverage_fout);
#line 517
  ret = do_callback(parser, 3);
#line 514
  fprintf(_coverage_fout, "611\n");
#line 514
  fflush(_coverage_fout);
#line 517
  if (ret) {
#line 517
    fprintf(_coverage_fout, "606\n");
#line 517
    fflush(_coverage_fout);
#line 517
    return (ret);
  } else {
#line 517
    fprintf(_coverage_fout, "607\n");
#line 517
    fflush(_coverage_fout);

  }
#line 514
  fprintf(_coverage_fout, "612\n");
#line 514
  fflush(_coverage_fout);
#line 518
  ret = state_pop(parser, 0);
#line 514
  fprintf(_coverage_fout, "613\n");
#line 514
  fflush(_coverage_fout);
#line 518
  if (ret) {
#line 518
    fprintf(_coverage_fout, "608\n");
#line 518
    fflush(_coverage_fout);
#line 518
    return (ret);
  } else {
#line 518
    fprintf(_coverage_fout, "609\n");
#line 518
    fflush(_coverage_fout);

  }
#line 514
  fprintf(_coverage_fout, "614\n");
#line 514
  fflush(_coverage_fout);
#line 519
  return (0);
}
}
#line 522 "json.c"
int act_se(json_parser *parser ) 
{ int ret ;
  int tmp ;

  {
#line 522
  fprintf(_coverage_fout, "621\n");
#line 522
  fflush(_coverage_fout);
#line 525
  if (parser->expecting_key) {
#line 525
    fprintf(_coverage_fout, "615\n");
#line 525
    fflush(_coverage_fout);
#line 525
    tmp = 8;
  } else {
#line 525
    fprintf(_coverage_fout, "616\n");
#line 525
    fflush(_coverage_fout);
#line 525
    tmp = 7;
  }
#line 522
  fprintf(_coverage_fout, "622\n");
#line 522
  fflush(_coverage_fout);
#line 525
  ret = do_callback_withbuf(parser, tmp);
#line 522
  fprintf(_coverage_fout, "623\n");
#line 522
  fflush(_coverage_fout);
#line 525
  if (ret) {
#line 525
    fprintf(_coverage_fout, "617\n");
#line 525
    fflush(_coverage_fout);
#line 525
    return (ret);
  } else {
#line 525
    fprintf(_coverage_fout, "618\n");
#line 525
    fflush(_coverage_fout);

  }
#line 522
  fprintf(_coverage_fout, "624\n");
#line 522
  fflush(_coverage_fout);
#line 526
  parser->buffer_offset = 0U;
#line 522
  fprintf(_coverage_fout, "625\n");
#line 522
  fflush(_coverage_fout);
#line 527
  if (parser->expecting_key) {
#line 527
    fprintf(_coverage_fout, "619\n");
#line 527
    fflush(_coverage_fout);
#line 527
    parser->state = (unsigned char)4;
  } else {
#line 527
    fprintf(_coverage_fout, "620\n");
#line 527
    fflush(_coverage_fout);
#line 527
    parser->state = (unsigned char)1;
  }
#line 522
  fprintf(_coverage_fout, "626\n");
#line 522
  fflush(_coverage_fout);
#line 528
  parser->expecting_key = (unsigned char)0;
#line 522
  fprintf(_coverage_fout, "627\n");
#line 522
  fflush(_coverage_fout);
#line 529
  return (0);
}
}
#line 532 "json.c"
int act_sp(json_parser *parser ) 
{ 

  {
#line 532
  fprintf(_coverage_fout, "633\n");
#line 532
  fflush(_coverage_fout);
#line 534
  if (parser->stack_offset == 0U) {
#line 534
    fprintf(_coverage_fout, "628\n");
#line 534
    fflush(_coverage_fout);
#line 535
    return (11);
  } else {
#line 534
    fprintf(_coverage_fout, "629\n");
#line 534
    fflush(_coverage_fout);

  }
#line 532
  fprintf(_coverage_fout, "634\n");
#line 532
  fflush(_coverage_fout);
#line 536
  if ((int )*(parser->stack + (parser->stack_offset - 1U)) == 1) {
#line 536
    fprintf(_coverage_fout, "630\n");
#line 536
    fflush(_coverage_fout);
#line 537
    parser->expecting_key = (unsigned char)1;
#line 536
    fprintf(_coverage_fout, "631\n");
#line 536
    fflush(_coverage_fout);
#line 538
    parser->state = (unsigned char)3;
  } else {
#line 536
    fprintf(_coverage_fout, "632\n");
#line 536
    fflush(_coverage_fout);
#line 540
    parser->state = (unsigned char)5;
  }
#line 532
  fprintf(_coverage_fout, "635\n");
#line 532
  fflush(_coverage_fout);
#line 541
  return (0);
}
}
#line 552 "json.c"
static struct action_descr actions_map[19]  = 
#line 552
  {      {(int (*)(json_parser *parser ))((void *)0), (unsigned char)0,
      (unsigned char)5, (unsigned char)0}, 
        {& act_sp, (unsigned char)0, (unsigned char)0, (unsigned char)1}, 
        {& act_ab, (unsigned char)0, (unsigned char)6, (unsigned char)0}, 
        {& act_ae, (unsigned char)0, (unsigned char)1, (unsigned char)1}, 
        {& act_ob, (unsigned char)0, (unsigned char)2, (unsigned char)0}, 
        {& act_oe, (unsigned char)0, (unsigned char)1, (unsigned char)1}, 
        {& act_cb, (unsigned char)0, (unsigned char)31, (unsigned char)1}, 
        {& act_yb, (unsigned char)0, (unsigned char)34, (unsigned char)1}, 
        {& act_ce, (unsigned char)0, (unsigned char)0, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)10,
      (unsigned char)1, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)9,
      (unsigned char)1, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)11,
      (unsigned char)1, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)6,
      (unsigned char)18, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)6,
      (unsigned char)16, (unsigned char)0}, 
        {& act_se, (unsigned char)0, (unsigned char)0, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)5,
      (unsigned char)13, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)5,
      (unsigned char)14, (unsigned char)0}, 
        {(int (*)(json_parser *parser ))((void *)0), (unsigned char)5,
      (unsigned char)15, (unsigned char)0}, 
        {& act_uc, (unsigned char)0, (unsigned char)0, (unsigned char)0}};
#line 574 "json.c"
static int do_action(json_parser *parser , int next_state ) 
{ struct action_descr *descr ;
  int ret ;

  {
#line 574
  fprintf(_coverage_fout, "649\n");
#line 574
  fflush(_coverage_fout);
#line 576
  descr = & actions_map[next_state & -129];
#line 574
  fprintf(_coverage_fout, "650\n");
#line 574
  fflush(_coverage_fout);
#line 579
  if (descr->call) {
#line 579
    fprintf(_coverage_fout, "643\n");
#line 579
    fflush(_coverage_fout);
#line 580
    if (descr->dobuffer) {
#line 580
      fprintf(_coverage_fout, "638\n");
#line 580
      fflush(_coverage_fout);
#line 581
      ret = do_buffer(parser);
#line 580
      fprintf(_coverage_fout, "639\n");
#line 580
      fflush(_coverage_fout);
#line 581
      if (ret) {
#line 581
        fprintf(_coverage_fout, "636\n");
#line 581
        fflush(_coverage_fout);
#line 581
        return (ret);
      } else {
#line 581
        fprintf(_coverage_fout, "637\n");
#line 581
        fflush(_coverage_fout);

      }
    } else {
#line 580
      fprintf(_coverage_fout, "640\n");
#line 580
      fflush(_coverage_fout);

    }
#line 579
    fprintf(_coverage_fout, "644\n");
#line 579
    fflush(_coverage_fout);
#line 582
    ret = (*(descr->call))(parser);
#line 579
    fprintf(_coverage_fout, "645\n");
#line 579
    fflush(_coverage_fout);
#line 582
    if (ret) {
#line 582
      fprintf(_coverage_fout, "641\n");
#line 582
      fflush(_coverage_fout);
#line 582
      return (ret);
    } else {
#line 582
      fprintf(_coverage_fout, "642\n");
#line 582
      fflush(_coverage_fout);

    }
  } else {
#line 579
    fprintf(_coverage_fout, "646\n");
#line 579
    fflush(_coverage_fout);

  }
#line 574
  fprintf(_coverage_fout, "651\n");
#line 574
  fflush(_coverage_fout);
#line 584
  if (descr->state) {
#line 584
    fprintf(_coverage_fout, "647\n");
#line 584
    fflush(_coverage_fout);
#line 585
    parser->state = descr->state;
  } else {
#line 584
    fprintf(_coverage_fout, "648\n");
#line 584
    fflush(_coverage_fout);

  }
#line 574
  fprintf(_coverage_fout, "652\n");
#line 574
  fflush(_coverage_fout);
#line 586
  parser->type = (enum __anonenum_json_type_26 )descr->type;
#line 574
  fprintf(_coverage_fout, "653\n");
#line 574
  fflush(_coverage_fout);
#line 587
  return (ret);
}
}
#line 594 "json.c"
int json_parser_init(json_parser *parser , json_config *config ,
                     int (*callback)(void *userdata , int type ,
                                     char const   *data , uint32_t length ) ,
                     void *userdata ) 
{ void *tmp ;
  void *tmp___0 ;

  {
#line 594
  fprintf(_coverage_fout, "669\n");
#line 594
  fflush(_coverage_fout);
#line 597
  memset((void *)parser, 0, sizeof(*parser));
#line 594
  fprintf(_coverage_fout, "670\n");
#line 594
  fflush(_coverage_fout);
#line 599
  if (config) {
#line 599
    fprintf(_coverage_fout, "654\n");
#line 599
    fflush(_coverage_fout);
#line 600
    memcpy((void */* __restrict  */)(& parser->config),
           (void const   */* __restrict  */)config, sizeof(json_config ));
  } else {
#line 599
    fprintf(_coverage_fout, "655\n");
#line 599
    fflush(_coverage_fout);

  }
#line 594
  fprintf(_coverage_fout, "671\n");
#line 594
  fflush(_coverage_fout);
#line 601
  parser->callback = callback;
#line 594
  fprintf(_coverage_fout, "672\n");
#line 594
  fflush(_coverage_fout);
#line 602
  parser->userdata = userdata;
#line 594
  fprintf(_coverage_fout, "673\n");
#line 594
  fflush(_coverage_fout);
#line 605
  parser->stack_offset = 0U;
#line 594
  fprintf(_coverage_fout, "674\n");
#line 594
  fflush(_coverage_fout);
#line 606
  parser->state = (unsigned char)0;
#line 594
  fprintf(_coverage_fout, "675\n");
#line 594
  fflush(_coverage_fout);
#line 609
  if (parser->config.max_nesting > 0U) {
#line 609
    fprintf(_coverage_fout, "656\n");
#line 609
    fflush(_coverage_fout);
#line 609
    parser->stack_size = parser->config.max_nesting;
  } else {
#line 609
    fprintf(_coverage_fout, "657\n");
#line 609
    fflush(_coverage_fout);
#line 609
    parser->stack_size = 256U;
  }
#line 594
  fprintf(_coverage_fout, "676\n");
#line 594
  fflush(_coverage_fout);
#line 613
  tmp = memory_calloc(parser->config.user_calloc,
                      (unsigned long )parser->stack_size,
                      sizeof(*(parser->stack + 0)));
#line 594
  fprintf(_coverage_fout, "677\n");
#line 594
  fflush(_coverage_fout);
#line 613
  parser->stack = (uint8_t *)tmp;
#line 594
  fprintf(_coverage_fout, "678\n");
#line 594
  fflush(_coverage_fout);
#line 614
  if (! parser->stack) {
#line 614
    fprintf(_coverage_fout, "658\n");
#line 614
    fflush(_coverage_fout);
#line 615
    return (1);
  } else {
#line 614
    fprintf(_coverage_fout, "659\n");
#line 614
    fflush(_coverage_fout);

  }
#line 594
  fprintf(_coverage_fout, "679\n");
#line 594
  fflush(_coverage_fout);
#line 618
  if (parser->config.buffer_initial_size > 0U) {
#line 618
    fprintf(_coverage_fout, "660\n");
#line 618
    fflush(_coverage_fout);
#line 618
    parser->buffer_size = parser->config.buffer_initial_size;
  } else {
#line 618
    fprintf(_coverage_fout, "661\n");
#line 618
    fflush(_coverage_fout);
#line 618
    parser->buffer_size = 4096U;
  }
#line 594
  fprintf(_coverage_fout, "680\n");
#line 594
  fflush(_coverage_fout);
#line 622
  if (parser->config.max_data > 0U) {
#line 622
    fprintf(_coverage_fout, "664\n");
#line 622
    fflush(_coverage_fout);
#line 622
    if (parser->buffer_size > parser->config.max_data) {
#line 622
      fprintf(_coverage_fout, "662\n");
#line 622
      fflush(_coverage_fout);
#line 623
      parser->buffer_size = parser->config.max_data;
    } else {
#line 622
      fprintf(_coverage_fout, "663\n");
#line 622
      fflush(_coverage_fout);

    }
  } else {
#line 622
    fprintf(_coverage_fout, "665\n");
#line 622
    fflush(_coverage_fout);

  }
#line 594
  fprintf(_coverage_fout, "681\n");
#line 594
  fflush(_coverage_fout);
#line 625
  tmp___0 = memory_calloc(parser->config.user_calloc,
                          (unsigned long )parser->buffer_size, sizeof(char ));
#line 594
  fprintf(_coverage_fout, "682\n");
#line 594
  fflush(_coverage_fout);
#line 625
  parser->buffer = (char *)tmp___0;
#line 594
  fprintf(_coverage_fout, "683\n");
#line 594
  fflush(_coverage_fout);
#line 626
  if (! parser->buffer) {
#line 626
    fprintf(_coverage_fout, "666\n");
#line 626
    fflush(_coverage_fout);
#line 627
    free((void *)parser->stack);
#line 626
    fprintf(_coverage_fout, "667\n");
#line 626
    fflush(_coverage_fout);
#line 628
    return (1);
  } else {
#line 626
    fprintf(_coverage_fout, "668\n");
#line 626
    fflush(_coverage_fout);

  }
#line 594
  fprintf(_coverage_fout, "684\n");
#line 594
  fflush(_coverage_fout);
#line 630
  return (0);
}
}
#line 634 "json.c"
int json_parser_free(json_parser *parser ) 
{ 

  {
#line 634
  fprintf(_coverage_fout, "687\n");
#line 634
  fflush(_coverage_fout);
#line 636
  if (! parser) {
#line 636
    fprintf(_coverage_fout, "685\n");
#line 636
    fflush(_coverage_fout);
#line 637
    return (0);
  } else {
#line 636
    fprintf(_coverage_fout, "686\n");
#line 636
    fflush(_coverage_fout);

  }
#line 634
  fprintf(_coverage_fout, "688\n");
#line 634
  fflush(_coverage_fout);
#line 638
  free((void *)parser->stack);
#line 634
  fprintf(_coverage_fout, "689\n");
#line 634
  fflush(_coverage_fout);
#line 639
  free((void *)parser->buffer);
#line 634
  fprintf(_coverage_fout, "690\n");
#line 634
  fflush(_coverage_fout);
#line 640
  parser->stack = (uint8_t *)((void *)0);
#line 634
  fprintf(_coverage_fout, "691\n");
#line 634
  fflush(_coverage_fout);
#line 641
  parser->buffer = (char *)((void *)0);
#line 634
  fprintf(_coverage_fout, "692\n");
#line 634
  fflush(_coverage_fout);
#line 642
  return (0);
}
}
#line 646 "json.c"
int json_parser_is_done(json_parser *parser ) 
{ int tmp ;

  {
#line 646
  fprintf(_coverage_fout, "697\n");
#line 646
  fflush(_coverage_fout);
#line 649
  if (parser->stack_offset == 0U) {
#line 649
    fprintf(_coverage_fout, "695\n");
#line 649
    fflush(_coverage_fout);
#line 649
    if ((int )parser->state != 0) {
#line 649
      fprintf(_coverage_fout, "693\n");
#line 649
      fflush(_coverage_fout);
#line 649
      tmp = 1;
    } else {
#line 649
      fprintf(_coverage_fout, "694\n");
#line 649
      fflush(_coverage_fout);
#line 649
      tmp = 0;
    }
  } else {
#line 649
    fprintf(_coverage_fout, "696\n");
#line 649
    fflush(_coverage_fout);
#line 649
    tmp = 0;
  }
#line 646
  fprintf(_coverage_fout, "698\n");
#line 646
  fflush(_coverage_fout);
#line 649
  return (tmp);
}
}
#line 656 "json.c"
int json_parser_string(json_parser *parser , char const   *s , uint32_t length ,
                       uint32_t *processed ) 
{ int ret ;
  int next_class ;
  int next_state ;
  int buffer_policy ;
  uint32_t i ;
  unsigned char ch ;

  {
#line 656
  fprintf(_coverage_fout, "729\n");
#line 656
  fflush(_coverage_fout);
#line 664
  ret = 0;
#line 656
  fprintf(_coverage_fout, "730\n");
#line 656
  fflush(_coverage_fout);
#line 665
  i = 0U;
#line 656
  fprintf(_coverage_fout, "731\n");
#line 656
  fflush(_coverage_fout);
#line 665
  while (1) {
#line 665
    fprintf(_coverage_fout, "715\n");
#line 665
    fflush(_coverage_fout);
#line 665
    if (i < length) {
#line 665
      fprintf(_coverage_fout, "699\n");
#line 665
      fflush(_coverage_fout);

    } else {
#line 665
      break;
    }
#line 665
    fprintf(_coverage_fout, "716\n");
#line 665
    fflush(_coverage_fout);
#line 666
    ch = (unsigned char )*(s + i);
#line 665
    fprintf(_coverage_fout, "717\n");
#line 665
    fflush(_coverage_fout);
#line 668
    ret = 0;
#line 665
    fprintf(_coverage_fout, "718\n");
#line 665
    fflush(_coverage_fout);
#line 669
    if ((int )ch >= 128) {
#line 669
      fprintf(_coverage_fout, "700\n");
#line 669
      fflush(_coverage_fout);
#line 669
      next_class = 31;
    } else {
#line 669
      fprintf(_coverage_fout, "701\n");
#line 669
      fflush(_coverage_fout);
#line 669
      next_class = (int )character_class[ch];
    }
#line 665
    fprintf(_coverage_fout, "719\n");
#line 665
    fflush(_coverage_fout);
#line 670
    if (next_class == 254) {
#line 670
      fprintf(_coverage_fout, "702\n");
#line 670
      fflush(_coverage_fout);
#line 671
      ret = 2;
#line 672
      break;
    } else {
#line 670
      fprintf(_coverage_fout, "703\n");
#line 670
      fflush(_coverage_fout);

    }
#line 665
    fprintf(_coverage_fout, "720\n");
#line 665
    fflush(_coverage_fout);
#line 675
    next_state = (int )state_transition_table[parser->state][next_class];
#line 665
    fprintf(_coverage_fout, "721\n");
#line 665
    fflush(_coverage_fout);
#line 676
    buffer_policy = (int )buffer_policy_table[parser->state][next_class];
#line 665
    fprintf(_coverage_fout, "722\n");
#line 665
    fflush(_coverage_fout);
#line 679
    if (next_state == 255) {
#line 679
      fprintf(_coverage_fout, "704\n");
#line 679
      fflush(_coverage_fout);
#line 680
      ret = 8;
#line 681
      break;
    } else {
#line 679
      fprintf(_coverage_fout, "705\n");
#line 679
      fflush(_coverage_fout);

    }
#line 665
    fprintf(_coverage_fout, "723\n");
#line 665
    fflush(_coverage_fout);
#line 685
    if (buffer_policy) {
#line 685
      fprintf(_coverage_fout, "709\n");
#line 685
      fflush(_coverage_fout);
#line 686
      if (buffer_policy == 2) {
#line 686
        fprintf(_coverage_fout, "706\n");
#line 686
        fflush(_coverage_fout);
#line 686
        ret = buffer_push_escape(parser, ch);
      } else {
#line 686
        fprintf(_coverage_fout, "707\n");
#line 686
        fflush(_coverage_fout);
#line 686
        ret = buffer_push(parser, ch);
      }
#line 685
      fprintf(_coverage_fout, "710\n");
#line 685
      fflush(_coverage_fout);
#line 689
      if (ret) {
#line 690
        break;
      } else {
#line 689
        fprintf(_coverage_fout, "708\n");
#line 689
        fflush(_coverage_fout);

      }
    } else {
#line 685
      fprintf(_coverage_fout, "711\n");
#line 685
      fflush(_coverage_fout);

    }
#line 665
    fprintf(_coverage_fout, "724\n");
#line 665
    fflush(_coverage_fout);
#line 694
    if (next_state & 128) {
#line 694
      fprintf(_coverage_fout, "712\n");
#line 694
      fflush(_coverage_fout);
#line 695
      ret = do_action(parser, next_state);
    } else {
#line 694
      fprintf(_coverage_fout, "713\n");
#line 694
      fflush(_coverage_fout);
#line 697
      parser->state = (unsigned char )next_state;
    }
#line 665
    fprintf(_coverage_fout, "725\n");
#line 665
    fflush(_coverage_fout);
#line 698
    if (ret) {
#line 699
      break;
    } else {
#line 698
      fprintf(_coverage_fout, "714\n");
#line 698
      fflush(_coverage_fout);

    }
#line 665
    fprintf(_coverage_fout, "726\n");
#line 665
    fflush(_coverage_fout);
#line 665
    i ++;
  }
#line 656
  fprintf(_coverage_fout, "732\n");
#line 656
  fflush(_coverage_fout);
#line 701
  if (processed) {
#line 701
    fprintf(_coverage_fout, "727\n");
#line 701
    fflush(_coverage_fout);
#line 702
    *processed = i;
  } else {
#line 701
    fprintf(_coverage_fout, "728\n");
#line 701
    fflush(_coverage_fout);

  }
#line 656
  fprintf(_coverage_fout, "733\n");
#line 656
  fflush(_coverage_fout);
#line 703
  return (ret);
}
}
#line 708 "json.c"
int json_parser_char(json_parser *parser , unsigned char ch ) 
{ int tmp ;

  {
#line 708
  fprintf(_coverage_fout, "734\n");
#line 708
  fflush(_coverage_fout);
#line 710
  tmp = json_parser_string(parser, (char const   *)((char *)(& ch)), 1U,
                           (uint32_t *)((void *)0));
#line 708
  fprintf(_coverage_fout, "735\n");
#line 708
  fflush(_coverage_fout);
#line 710
  return (tmp);
}
}
#line 714 "json.c"
int json_print_init(json_printer *printer ,
                    int (*callback)(void *userdata , char const   *s ,
                                    uint32_t length ) , void *userdata ) 
{ 

  {
#line 714
  fprintf(_coverage_fout, "736\n");
#line 714
  fflush(_coverage_fout);
#line 716
  memset((void *)printer, '\000', sizeof(*printer));
#line 714
  fprintf(_coverage_fout, "737\n");
#line 714
  fflush(_coverage_fout);
#line 717
  printer->callback = callback;
#line 714
  fprintf(_coverage_fout, "738\n");
#line 714
  fflush(_coverage_fout);
#line 718
  printer->userdata = userdata;
#line 714
  fprintf(_coverage_fout, "739\n");
#line 714
  fflush(_coverage_fout);
#line 720
  printer->indentstr = (char *)"\t";
#line 714
  fprintf(_coverage_fout, "740\n");
#line 714
  fflush(_coverage_fout);
#line 721
  printer->indentlevel = 0;
#line 714
  fprintf(_coverage_fout, "741\n");
#line 714
  fflush(_coverage_fout);
#line 722
  printer->enter_object = 1;
#line 714
  fprintf(_coverage_fout, "742\n");
#line 714
  fflush(_coverage_fout);
#line 723
  printer->first = 1;
#line 714
  fprintf(_coverage_fout, "743\n");
#line 714
  fflush(_coverage_fout);
#line 724
  return (0);
}
}
#line 729 "json.c"
int json_print_free(json_printer *printer ) 
{ 

  {
#line 729
  fprintf(_coverage_fout, "744\n");
#line 729
  fflush(_coverage_fout);
#line 731
  return (0);
}
}
#line 736 "json.c"
static int print_string(json_printer *printer , char const   *data ,
                        uint32_t length ) 
{ int i ;
  unsigned char c ;
  char *esc ;
  size_t tmp ;

  {
#line 736
  fprintf(_coverage_fout, "756\n");
#line 736
  fflush(_coverage_fout);
#line 740
  (*(printer->callback))(printer->userdata, "\"", 1U);
#line 736
  fprintf(_coverage_fout, "757\n");
#line 736
  fflush(_coverage_fout);
#line 741
  i = 0;
#line 736
  fprintf(_coverage_fout, "758\n");
#line 736
  fflush(_coverage_fout);
#line 741
  while (1) {
#line 741
    fprintf(_coverage_fout, "752\n");
#line 741
    fflush(_coverage_fout);
#line 741
    if ((unsigned int )i < length) {
#line 741
      fprintf(_coverage_fout, "745\n");
#line 741
      fflush(_coverage_fout);

    } else {
#line 741
      break;
    }
#line 741
    fprintf(_coverage_fout, "753\n");
#line 741
    fflush(_coverage_fout);
#line 742
    c = (unsigned char )*(data + i);
#line 741
    fprintf(_coverage_fout, "754\n");
#line 741
    fflush(_coverage_fout);
#line 743
    if ((int )c < 36) {
#line 743
      fprintf(_coverage_fout, "746\n");
#line 743
      fflush(_coverage_fout);
#line 744
      esc = character_escape[c];
#line 743
      fprintf(_coverage_fout, "747\n");
#line 743
      fflush(_coverage_fout);
#line 745
      tmp = strlen((char const   *)esc);
#line 743
      fprintf(_coverage_fout, "748\n");
#line 743
      fflush(_coverage_fout);
#line 745
      (*(printer->callback))(printer->userdata, (char const   *)esc,
                             (unsigned int )tmp);
    } else {
#line 743
      fprintf(_coverage_fout, "751\n");
#line 743
      fflush(_coverage_fout);
#line 746
      if ((int )c == 92) {
#line 746
        fprintf(_coverage_fout, "749\n");
#line 746
        fflush(_coverage_fout);
#line 747
        (*(printer->callback))(printer->userdata, "\\\\", 2U);
      } else {
#line 746
        fprintf(_coverage_fout, "750\n");
#line 746
        fflush(_coverage_fout);
#line 749
        (*(printer->callback))(printer->userdata, data + i, 1U);
      }
    }
#line 741
    fprintf(_coverage_fout, "755\n");
#line 741
    fflush(_coverage_fout);
#line 741
    i ++;
  }
#line 736
  fprintf(_coverage_fout, "759\n");
#line 736
  fflush(_coverage_fout);
#line 751
  (*(printer->callback))(printer->userdata, "\"", 1U);
#line 736
  fprintf(_coverage_fout, "760\n");
#line 736
  fflush(_coverage_fout);
#line 752
  return (0);
}
}
#line 755 "json.c"
static int print_indent(json_printer *printer ) 
{ int i ;
  size_t tmp ;

  {
#line 755
  fprintf(_coverage_fout, "766\n");
#line 755
  fflush(_coverage_fout);
#line 758
  (*(printer->callback))(printer->userdata, "\n", 1U);
#line 755
  fprintf(_coverage_fout, "767\n");
#line 755
  fflush(_coverage_fout);
#line 759
  i = 0;
#line 755
  fprintf(_coverage_fout, "768\n");
#line 755
  fflush(_coverage_fout);
#line 759
  while (1) {
#line 759
    fprintf(_coverage_fout, "762\n");
#line 759
    fflush(_coverage_fout);
#line 759
    if (i < printer->indentlevel) {
#line 759
      fprintf(_coverage_fout, "761\n");
#line 759
      fflush(_coverage_fout);

    } else {
#line 759
      break;
    }
#line 759
    fprintf(_coverage_fout, "763\n");
#line 759
    fflush(_coverage_fout);
#line 760
    tmp = strlen((char const   *)printer->indentstr);
#line 759
    fprintf(_coverage_fout, "764\n");
#line 759
    fflush(_coverage_fout);
#line 760
    (*(printer->callback))(printer->userdata,
                           (char const   *)printer->indentstr,
                           (unsigned int )tmp);
#line 759
    fprintf(_coverage_fout, "765\n");
#line 759
    fflush(_coverage_fout);
#line 759
    i ++;
  }
#line 755
  fprintf(_coverage_fout, "769\n");
#line 755
  fflush(_coverage_fout);
#line 761
  return (0);
}
}
#line 764 "json.c"
int json_print_mode(json_printer *printer , int type , char const   *data ,
                    uint32_t length , int pretty ) 
{ int enterobj ;
  char const   *tmp ;
  int tmp___0 ;

  {
#line 764
  fprintf(_coverage_fout, "820\n");
#line 764
  fflush(_coverage_fout);
#line 766
  enterobj = printer->enter_object;
#line 764
  fprintf(_coverage_fout, "821\n");
#line 764
  fflush(_coverage_fout);
#line 768
  if (! enterobj) {
#line 768
    fprintf(_coverage_fout, "779\n");
#line 768
    fflush(_coverage_fout);
#line 768
    if (! printer->afterkey) {
#line 768
      fprintf(_coverage_fout, "777\n");
#line 768
      fflush(_coverage_fout);
#line 768
      if (type != 3) {
#line 768
        fprintf(_coverage_fout, "775\n");
#line 768
        fflush(_coverage_fout);
#line 768
        if (type != 4) {
#line 768
          fprintf(_coverage_fout, "772\n");
#line 768
          fflush(_coverage_fout);
#line 769
          (*(printer->callback))(printer->userdata, ",", 1U);
#line 768
          fprintf(_coverage_fout, "773\n");
#line 768
          fflush(_coverage_fout);
#line 770
          if (pretty) {
#line 770
            fprintf(_coverage_fout, "770\n");
#line 770
            fflush(_coverage_fout);
#line 770
            print_indent(printer);
          } else {
#line 770
            fprintf(_coverage_fout, "771\n");
#line 770
            fflush(_coverage_fout);

          }
        } else {
#line 768
          fprintf(_coverage_fout, "774\n");
#line 768
          fflush(_coverage_fout);

        }
      } else {
#line 768
        fprintf(_coverage_fout, "776\n");
#line 768
        fflush(_coverage_fout);

      }
    } else {
#line 768
      fprintf(_coverage_fout, "778\n");
#line 768
      fflush(_coverage_fout);

    }
  } else {
#line 768
    fprintf(_coverage_fout, "780\n");
#line 768
    fflush(_coverage_fout);

  }
#line 764
  fprintf(_coverage_fout, "822\n");
#line 764
  fflush(_coverage_fout);
#line 773
  if (pretty) {
#line 773
    fprintf(_coverage_fout, "789\n");
#line 773
    fflush(_coverage_fout);
#line 773
    if (enterobj) {
#line 773
      fprintf(_coverage_fout, "787\n");
#line 773
      fflush(_coverage_fout);
#line 773
      if (! printer->first) {
#line 773
        fprintf(_coverage_fout, "785\n");
#line 773
        fflush(_coverage_fout);
#line 773
        if (type != 3) {
#line 773
          fprintf(_coverage_fout, "783\n");
#line 773
          fflush(_coverage_fout);
#line 773
          if (type != 4) {
#line 773
            fprintf(_coverage_fout, "781\n");
#line 773
            fflush(_coverage_fout);
#line 774
            print_indent(printer);
          } else {
#line 773
            fprintf(_coverage_fout, "782\n");
#line 773
            fflush(_coverage_fout);

          }
        } else {
#line 773
          fprintf(_coverage_fout, "784\n");
#line 773
          fflush(_coverage_fout);

        }
      } else {
#line 773
        fprintf(_coverage_fout, "786\n");
#line 773
        fflush(_coverage_fout);

      }
    } else {
#line 773
      fprintf(_coverage_fout, "788\n");
#line 773
      fflush(_coverage_fout);

    }
  } else {
#line 773
    fprintf(_coverage_fout, "790\n");
#line 773
    fflush(_coverage_fout);

  }
#line 764
  fprintf(_coverage_fout, "823\n");
#line 764
  fflush(_coverage_fout);
#line 777
  printer->first = 0;
#line 764
  fprintf(_coverage_fout, "824\n");
#line 764
  fflush(_coverage_fout);
#line 778
  printer->enter_object = 0;
#line 764
  fprintf(_coverage_fout, "825\n");
#line 764
  fflush(_coverage_fout);
#line 779
  printer->afterkey = 0;
#line 780
  switch (type) {
#line 780
  fprintf(_coverage_fout, "799\n");
#line 780
  fflush(_coverage_fout);
  case 1: 
#line 782
  (*(printer->callback))(printer->userdata, "[", 1U);
#line 780
  fprintf(_coverage_fout, "800\n");
#line 780
  fflush(_coverage_fout);
#line 783
  (printer->indentlevel) ++;
#line 780
  fprintf(_coverage_fout, "801\n");
#line 780
  fflush(_coverage_fout);
#line 784
  printer->enter_object = 1;
#line 785
  break;
#line 780
  fprintf(_coverage_fout, "802\n");
#line 780
  fflush(_coverage_fout);
  case 2: 
#line 787
  (*(printer->callback))(printer->userdata, "{", 1U);
#line 780
  fprintf(_coverage_fout, "803\n");
#line 780
  fflush(_coverage_fout);
#line 788
  (printer->indentlevel) ++;
#line 780
  fprintf(_coverage_fout, "804\n");
#line 780
  fflush(_coverage_fout);
#line 789
  printer->enter_object = 1;
#line 790
  break;
#line 780
  fprintf(_coverage_fout, "805\n");
#line 780
  fflush(_coverage_fout);
  case 3: 
  case 4: 
#line 793
  (printer->indentlevel) --;
#line 780
  fprintf(_coverage_fout, "806\n");
#line 780
  fflush(_coverage_fout);
#line 794
  if (pretty) {
#line 794
    fprintf(_coverage_fout, "793\n");
#line 794
    fflush(_coverage_fout);
#line 794
    if (! enterobj) {
#line 794
      fprintf(_coverage_fout, "791\n");
#line 794
      fflush(_coverage_fout);
#line 794
      print_indent(printer);
    } else {
#line 794
      fprintf(_coverage_fout, "792\n");
#line 794
      fflush(_coverage_fout);

    }
  } else {
#line 794
    fprintf(_coverage_fout, "794\n");
#line 794
    fflush(_coverage_fout);

  }
#line 780
  fprintf(_coverage_fout, "807\n");
#line 780
  fflush(_coverage_fout);
#line 795
  if (type == 4) {
#line 795
    fprintf(_coverage_fout, "795\n");
#line 795
    fflush(_coverage_fout);
#line 795
    tmp = "}";
  } else {
#line 795
    fprintf(_coverage_fout, "796\n");
#line 795
    fflush(_coverage_fout);
#line 795
    tmp = "]";
  }
#line 780
  fprintf(_coverage_fout, "808\n");
#line 780
  fflush(_coverage_fout);
#line 795
  (*(printer->callback))(printer->userdata, tmp, 1U);
#line 796
  break;
#line 780
  fprintf(_coverage_fout, "809\n");
#line 780
  fflush(_coverage_fout);
  case 5: 
#line 797
  (*(printer->callback))(printer->userdata, data, length);
#line 797
  break;
#line 780
  fprintf(_coverage_fout, "810\n");
#line 780
  fflush(_coverage_fout);
  case 6: 
#line 798
  (*(printer->callback))(printer->userdata, data, length);
#line 798
  break;
#line 780
  fprintf(_coverage_fout, "811\n");
#line 780
  fflush(_coverage_fout);
  case 11: 
#line 799
  (*(printer->callback))(printer->userdata, "null", 4U);
#line 799
  break;
#line 780
  fprintf(_coverage_fout, "812\n");
#line 780
  fflush(_coverage_fout);
  case 9: 
#line 800
  (*(printer->callback))(printer->userdata, "true", 4U);
#line 800
  break;
#line 780
  fprintf(_coverage_fout, "813\n");
#line 780
  fflush(_coverage_fout);
  case 10: 
#line 801
  (*(printer->callback))(printer->userdata, "false", 5U);
#line 801
  break;
#line 780
  fprintf(_coverage_fout, "814\n");
#line 780
  fflush(_coverage_fout);
  case 8: 
#line 803
  print_string(printer, data, length);
#line 780
  fprintf(_coverage_fout, "815\n");
#line 780
  fflush(_coverage_fout);
#line 804
  if (pretty) {
#line 804
    fprintf(_coverage_fout, "797\n");
#line 804
    fflush(_coverage_fout);
#line 804
    tmp___0 = 2;
  } else {
#line 804
    fprintf(_coverage_fout, "798\n");
#line 804
    fflush(_coverage_fout);
#line 804
    tmp___0 = 1;
  }
#line 780
  fprintf(_coverage_fout, "816\n");
#line 780
  fflush(_coverage_fout);
#line 804
  (*(printer->callback))(printer->userdata, ": ", (unsigned int )tmp___0);
#line 780
  fprintf(_coverage_fout, "817\n");
#line 780
  fflush(_coverage_fout);
#line 805
  printer->afterkey = 1;
#line 806
  break;
#line 780
  fprintf(_coverage_fout, "818\n");
#line 780
  fflush(_coverage_fout);
  case 7: 
#line 808
  print_string(printer, data, length);
#line 809
  break;
#line 780
  fprintf(_coverage_fout, "819\n");
#line 780
  fflush(_coverage_fout);
  default: ;
#line 811
  break;
  }
#line 764
  fprintf(_coverage_fout, "826\n");
#line 764
  fflush(_coverage_fout);
#line 814
  return (0);
}
}
#line 818 "json.c"
int json_print_pretty(json_printer *printer , int type , char const   *data ,
                      uint32_t length ) 
{ int tmp ;

  {
#line 818
  fprintf(_coverage_fout, "827\n");
#line 818
  fflush(_coverage_fout);
#line 820
  tmp = json_print_mode(printer, type, data, length, 1);
#line 818
  fprintf(_coverage_fout, "828\n");
#line 818
  fflush(_coverage_fout);
#line 820
  return (tmp);
}
}
#line 824 "json.c"
int json_print_raw(json_printer *printer , int type , char const   *data ,
                   uint32_t length ) 
{ int tmp ;

  {
#line 824
  fprintf(_coverage_fout, "829\n");
#line 824
  fflush(_coverage_fout);
#line 826
  tmp = json_print_mode(printer, type, data, length, 0);
#line 824
  fprintf(_coverage_fout, "830\n");
#line 824
  fflush(_coverage_fout);
#line 826
  return (tmp);
}
}
#line 830 "json.c"
int json_print_args(json_printer *printer ,
                    int (*f)(json_printer * , int  ,
                             char const   * , uint32_t  )  , ...) 
{ va_list ap ;
  char *data ;
  uint32_t length ;
  int type ;
  int ret ;
  size_t tmp ;

  {
#line 830
  fprintf(_coverage_fout, "844\n");
#line 830
  fflush(_coverage_fout);
#line 839
  ret = 0;
#line 830
  fprintf(_coverage_fout, "845\n");
#line 830
  fflush(_coverage_fout);
#line 840
  __builtin_va_start(ap, f);
#line 830
  fprintf(_coverage_fout, "846\n");
#line 830
  fflush(_coverage_fout);
#line 841
  while (1) {
#line 841
    fprintf(_coverage_fout, "841\n");
#line 841
    fflush(_coverage_fout);
#line 841
    type = __builtin_va_arg(ap, int );
#line 841
    fprintf(_coverage_fout, "842\n");
#line 841
    fflush(_coverage_fout);
#line 841
    if (! (type != -1)) {
#line 841
      break;
    } else {
#line 841
      fprintf(_coverage_fout, "831\n");
#line 841
      fflush(_coverage_fout);

    }
#line 842
    switch (type) {
#line 842
    fprintf(_coverage_fout, "835\n");
#line 842
    fflush(_coverage_fout);
    case 1: 
    case 3: 
    case 2: 
    case 4: 
    case 11: 
    case 9: 
    case 10: 
#line 850
    ret = (*f)(printer, type, (char const   *)((void *)0), 0U);
#line 851
    break;
#line 842
    fprintf(_coverage_fout, "836\n");
#line 842
    fflush(_coverage_fout);
    case 5: 
    case 6: 
    case 8: 
    case 7: 
#line 856
    data = __builtin_va_arg(ap, char *);
#line 842
    fprintf(_coverage_fout, "837\n");
#line 842
    fflush(_coverage_fout);
#line 857
    length = __builtin_va_arg(ap, uint32_t );
#line 842
    fprintf(_coverage_fout, "838\n");
#line 842
    fflush(_coverage_fout);
#line 858
    if (length == 4294967295U) {
#line 858
      fprintf(_coverage_fout, "832\n");
#line 858
      fflush(_coverage_fout);
#line 859
      tmp = strlen((char const   *)data);
#line 858
      fprintf(_coverage_fout, "833\n");
#line 858
      fflush(_coverage_fout);
#line 859
      length = (unsigned int )tmp;
    } else {
#line 858
      fprintf(_coverage_fout, "834\n");
#line 858
      fflush(_coverage_fout);

    }
#line 842
    fprintf(_coverage_fout, "839\n");
#line 842
    fflush(_coverage_fout);
#line 860
    ret = (*f)(printer, type, (char const   *)data, length);
#line 861
    break;
    }
#line 841
    fprintf(_coverage_fout, "843\n");
#line 841
    fflush(_coverage_fout);
#line 863
    if (ret) {
#line 864
      break;
    } else {
#line 863
      fprintf(_coverage_fout, "840\n");
#line 863
      fflush(_coverage_fout);

    }
  }
#line 830
  fprintf(_coverage_fout, "847\n");
#line 830
  fflush(_coverage_fout);
#line 866
  __builtin_va_end(ap);
#line 830
  fprintf(_coverage_fout, "848\n");
#line 830
  fflush(_coverage_fout);
#line 867
  return (ret);
}
}
#line 870 "json.c"
static int dom_push(struct json_parser_dom *ctx , void *val ) 
{ void *ptr ;
  uint32_t newsize ;

  {
#line 870
  fprintf(_coverage_fout, "857\n");
#line 870
  fflush(_coverage_fout);
#line 872
  if (ctx->stack_offset == ctx->stack_size) {
#line 872
    fprintf(_coverage_fout, "851\n");
#line 872
    fflush(_coverage_fout);
#line 874
    newsize = ctx->stack_size * 2U;
#line 872
    fprintf(_coverage_fout, "852\n");
#line 872
    fflush(_coverage_fout);
#line 875
    ptr = memory_realloc(ctx->user_realloc, (void *)ctx->stack,
                         (unsigned long )newsize);
#line 872
    fprintf(_coverage_fout, "853\n");
#line 872
    fflush(_coverage_fout);
#line 876
    if (! ptr) {
#line 876
      fprintf(_coverage_fout, "849\n");
#line 876
      fflush(_coverage_fout);
#line 877
      return (1);
    } else {
#line 876
      fprintf(_coverage_fout, "850\n");
#line 876
      fflush(_coverage_fout);

    }
#line 872
    fprintf(_coverage_fout, "854\n");
#line 872
    fflush(_coverage_fout);
#line 878
    ctx->stack = (struct stack_elem *)ptr;
#line 872
    fprintf(_coverage_fout, "855\n");
#line 872
    fflush(_coverage_fout);
#line 879
    ctx->stack_size = newsize;
  } else {
#line 872
    fprintf(_coverage_fout, "856\n");
#line 872
    fflush(_coverage_fout);

  }
#line 870
  fprintf(_coverage_fout, "858\n");
#line 870
  fflush(_coverage_fout);
#line 881
  (ctx->stack + ctx->stack_offset)->val = val;
#line 870
  fprintf(_coverage_fout, "859\n");
#line 870
  fflush(_coverage_fout);
#line 882
  (ctx->stack + ctx->stack_offset)->key = (char *)((void *)0);
#line 870
  fprintf(_coverage_fout, "860\n");
#line 870
  fflush(_coverage_fout);
#line 883
  (ctx->stack + ctx->stack_offset)->key_length = 0U;
#line 870
  fprintf(_coverage_fout, "861\n");
#line 870
  fflush(_coverage_fout);
#line 884
  (ctx->stack_offset) ++;
#line 870
  fprintf(_coverage_fout, "862\n");
#line 870
  fflush(_coverage_fout);
#line 885
  return (0);
}
}
#line 888 "json.c"
static int dom_pop(struct json_parser_dom *ctx , void **val ) 
{ 

  {
#line 888
  fprintf(_coverage_fout, "863\n");
#line 888
  fflush(_coverage_fout);
#line 890
  (ctx->stack_offset) --;
#line 888
  fprintf(_coverage_fout, "864\n");
#line 888
  fflush(_coverage_fout);
#line 891
  *val = (ctx->stack + ctx->stack_offset)->val;
#line 888
  fprintf(_coverage_fout, "865\n");
#line 888
  fflush(_coverage_fout);
#line 892
  return (0);
}
}
#line 895 "json.c"
int json_parser_dom_init(json_parser_dom *dom ,
                         void *(*create_structure)(int  , int  ) ,
                         void *(*create_data)(int  , char const   * , uint32_t  ) ,
                         int (*append)(void * , char * , uint32_t  , void * ) ) 
{ void *tmp ;

  {
#line 895
  fprintf(_coverage_fout, "868\n");
#line 895
  fflush(_coverage_fout);
#line 900
  memset((void *)dom, 0, sizeof(*dom));
#line 895
  fprintf(_coverage_fout, "869\n");
#line 895
  fflush(_coverage_fout);
#line 901
  dom->stack_size = 1024U;
#line 895
  fprintf(_coverage_fout, "870\n");
#line 895
  fflush(_coverage_fout);
#line 902
  dom->stack_offset = 0U;
#line 895
  fprintf(_coverage_fout, "871\n");
#line 895
  fflush(_coverage_fout);
#line 903
  tmp = memory_calloc(dom->user_calloc, (unsigned long )dom->stack_size,
                      sizeof(*(dom->stack)));
#line 895
  fprintf(_coverage_fout, "872\n");
#line 895
  fflush(_coverage_fout);
#line 903
  dom->stack = (struct stack_elem *)tmp;
#line 895
  fprintf(_coverage_fout, "873\n");
#line 895
  fflush(_coverage_fout);
#line 904
  if (! dom->stack) {
#line 904
    fprintf(_coverage_fout, "866\n");
#line 904
    fflush(_coverage_fout);
#line 905
    return (1);
  } else {
#line 904
    fprintf(_coverage_fout, "867\n");
#line 904
    fflush(_coverage_fout);

  }
#line 895
  fprintf(_coverage_fout, "874\n");
#line 895
  fflush(_coverage_fout);
#line 906
  dom->append = append;
#line 895
  fprintf(_coverage_fout, "875\n");
#line 895
  fflush(_coverage_fout);
#line 907
  dom->create_structure = create_structure;
#line 895
  fprintf(_coverage_fout, "876\n");
#line 895
  fflush(_coverage_fout);
#line 908
  dom->create_data = create_data;
#line 895
  fprintf(_coverage_fout, "877\n");
#line 895
  fflush(_coverage_fout);
#line 909
  return (0);
}
}
#line 912 "json.c"
int json_parser_dom_free(json_parser_dom *dom ) 
{ 

  {
#line 912
  fprintf(_coverage_fout, "878\n");
#line 912
  fflush(_coverage_fout);
#line 914
  free((void *)dom->stack);
#line 912
  fprintf(_coverage_fout, "879\n");
#line 912
  fflush(_coverage_fout);
#line 915
  return (0);
}
}
#line 918 "json.c"
int json_parser_dom_callback(void *userdata , int type , char const   *data ,
                             uint32_t length ) 
{ struct json_parser_dom *ctx ;
  void *v ;
  struct stack_elem *stack ;
  void *tmp ;

  {
#line 918
  fprintf(_coverage_fout, "903\n");
#line 918
  fflush(_coverage_fout);
#line 920
  ctx = (struct json_parser_dom *)userdata;
#line 918
  fprintf(_coverage_fout, "904\n");
#line 918
  fflush(_coverage_fout);
#line 922
  stack = (struct stack_elem *)((void *)0);
#line 924
  switch (type) {
#line 924
  fprintf(_coverage_fout, "888\n");
#line 924
  fflush(_coverage_fout);
  case 1: 
  case 2: 
#line 927
  v = (*(ctx->create_structure))((int )ctx->stack_offset, type == 2);
#line 924
  fprintf(_coverage_fout, "889\n");
#line 924
  fflush(_coverage_fout);
#line 928
  if (! v) {
#line 928
    fprintf(_coverage_fout, "880\n");
#line 928
    fflush(_coverage_fout);
#line 929
    return (12);
  } else {
#line 928
    fprintf(_coverage_fout, "881\n");
#line 928
    fflush(_coverage_fout);

  }
#line 924
  fprintf(_coverage_fout, "890\n");
#line 924
  fflush(_coverage_fout);
#line 930
  dom_push(ctx, v);
#line 931
  break;
#line 924
  fprintf(_coverage_fout, "891\n");
#line 924
  fflush(_coverage_fout);
  case 4: 
  case 3: 
#line 934
  dom_pop(ctx, & v);
#line 924
  fprintf(_coverage_fout, "892\n");
#line 924
  fflush(_coverage_fout);
#line 935
  if (ctx->stack_offset > 0U) {
#line 935
    fprintf(_coverage_fout, "882\n");
#line 935
    fflush(_coverage_fout);
#line 936
    stack = ctx->stack + (ctx->stack_offset - 1U);
#line 935
    fprintf(_coverage_fout, "883\n");
#line 935
    fflush(_coverage_fout);
#line 937
    (*(ctx->append))(stack->val, stack->key, stack->key_length, v);
#line 935
    fprintf(_coverage_fout, "884\n");
#line 935
    fflush(_coverage_fout);
#line 938
    free((void *)stack->key);
  } else {
#line 935
    fprintf(_coverage_fout, "885\n");
#line 935
    fflush(_coverage_fout);
#line 940
    ctx->root_structure = v;
  }
#line 941
  break;
#line 924
  fprintf(_coverage_fout, "893\n");
#line 924
  fflush(_coverage_fout);
  case 8: 
#line 943
  stack = ctx->stack + (ctx->stack_offset - 1U);
#line 924
  fprintf(_coverage_fout, "894\n");
#line 924
  fflush(_coverage_fout);
#line 944
  tmp = memory_calloc(ctx->user_calloc, (unsigned long )(length + 1U),
                      sizeof(char ));
#line 924
  fprintf(_coverage_fout, "895\n");
#line 924
  fflush(_coverage_fout);
#line 944
  stack->key = (char *)tmp;
#line 924
  fprintf(_coverage_fout, "896\n");
#line 924
  fflush(_coverage_fout);
#line 945
  stack->key_length = length;
#line 924
  fprintf(_coverage_fout, "897\n");
#line 924
  fflush(_coverage_fout);
#line 946
  if (! stack->key) {
#line 946
    fprintf(_coverage_fout, "886\n");
#line 946
    fflush(_coverage_fout);
#line 947
    return (1);
  } else {
#line 946
    fprintf(_coverage_fout, "887\n");
#line 946
    fflush(_coverage_fout);

  }
#line 924
  fprintf(_coverage_fout, "898\n");
#line 924
  fflush(_coverage_fout);
#line 948
  memcpy((void */* __restrict  */)stack->key,
         (void const   */* __restrict  */)data, (unsigned long )length);
#line 949
  break;
#line 924
  fprintf(_coverage_fout, "899\n");
#line 924
  fflush(_coverage_fout);
  case 7: 
  case 5: 
  case 6: 
  case 11: 
  case 9: 
  case 10: 
#line 956
  stack = ctx->stack + (ctx->stack_offset - 1U);
#line 924
  fprintf(_coverage_fout, "900\n");
#line 924
  fflush(_coverage_fout);
#line 957
  v = (*(ctx->create_data))(type, data, length);
#line 924
  fprintf(_coverage_fout, "901\n");
#line 924
  fflush(_coverage_fout);
#line 958
  (*(ctx->append))(stack->val, stack->key, stack->key_length, v);
#line 924
  fprintf(_coverage_fout, "902\n");
#line 924
  fflush(_coverage_fout);
#line 959
  free((void *)stack->key);
#line 960
  break;
  }
#line 918
  fprintf(_coverage_fout, "905\n");
#line 918
  fflush(_coverage_fout);
#line 962
  return (0);
}
}
void __globinit_jsonlint_comb(void) 
{ 

  {
#line 918
  _coverage_fout = fopen("coverage.path", "wb");
}
}
