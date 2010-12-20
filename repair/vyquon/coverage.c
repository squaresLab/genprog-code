#line 9 "Vyquon.c"
void __globinit_vyquon_comb(void) ;
#line 51 "external/Bool.c"
void *_coverage_fout ;
#line 13 "include/Vyquon.h"
typedef int bool;
#line 214 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stddef.h"
typedef unsigned long size_t;
#line 144 "/usr/include/bits/types.h"
typedef long __off_t;
#line 145 "/usr/include/bits/types.h"
typedef long __off64_t;
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
#line 10 "include/Obj.h"
struct _VySymbol;
#line 10
struct _VySymbol;
#line 11 "include/Obj.h"
struct _VyType {
   int size ;
   struct _VySymbol *name ;
};
#line 11 "include/Obj.h"
typedef struct _VyType VyType;
#line 16 "include/Obj.h"
struct _VyObj {
   VyType type ;
   void *obj ;
};
#line 16 "include/Obj.h"
typedef struct _VyObj VyObj;
#line 12 "include/Symbol.h"
struct _VySymbol {
   char *symb ;
};
#line 10 "include/Lexer.h"
struct _Token {
   char type ;
   char *data ;
};
#line 10 "include/Lexer.h"
typedef struct _Token Token;
#line 32
struct _TokenList;
#line 32
struct _TokenList;
#line 32 "include/Lexer.h"
typedef struct _TokenList TokenList;
#line 33 "include/Lexer.h"
struct _TokenList {
   Token token ;
   int count ;
   TokenList *next ;
};
#line 14 "include/Cons.h"
struct _VyCons {
   VyObj car ;
   VyObj cdr ;
};
#line 14 "include/Cons.h"
typedef struct _VyCons VyCons;
#line 7 "include/String.h"
struct _VyString {
   char *str ;
};
#line 7 "include/String.h"
typedef struct _VyString VyString;
#line 44 "/usr/include/glib-2.0/glib/gtypes.h"
typedef int gint;
#line 45 "/usr/include/glib-2.0/glib/gtypes.h"
typedef gint gboolean;
#line 50 "/usr/include/glib-2.0/glib/gtypes.h"
typedef unsigned int guint;
#line 72 "/usr/include/glib-2.0/glib/gtypes.h"
typedef void *gpointer;
#line 73 "/usr/include/glib-2.0/glib/gtypes.h"
typedef void const   *gconstpointer;
#line 34 "/usr/include/glib-2.0/glib/ghash.h"
struct _GHashTable;
#line 34
struct _GHashTable;
#line 34 "/usr/include/glib-2.0/glib/ghash.h"
typedef struct _GHashTable GHashTable;
#line 12 "include/Symbol.h"
typedef struct _VySymbol VySymbol;
#line 22 "include/VM.h"
union __anonunion_data_65 {
   VyObj obj ;
   int num ;
};
#line 22 "include/VM.h"
struct _Instruction {
   int opcode ;
   union __anonunion_data_65 data ;
};
#line 22 "include/VM.h"
typedef struct _Instruction Instruction;
#line 37 "include/VM.h"
struct _Bytecode {
   int size ;
   int used ;
   Instruction *instructions ;
};
#line 37 "include/VM.h"
typedef struct _Bytecode Bytecode;
#line 18 "include/Scope.h"
struct _Scope;
#line 18
struct _Scope;
#line 18 "include/Scope.h"
typedef struct _Scope Scope;
#line 9 "include/Function.h"
struct _Param;
#line 9
struct _Param;
#line 9 "include/Function.h"
typedef struct _Param Param;
#line 18 "include/Function.h"
struct _ArgList {
   int num_params ;
   Param *params ;
};
#line 18 "include/Function.h"
typedef struct _ArgList ArgList;
#line 41 "include/Function.h"
union __anonunion_code_66 {
   Bytecode *bytecode ;
   VyObj (*native)(VyObj * , int  ) ;
};
#line 41 "include/Function.h"
struct _VyFunction {
   ArgList arguments ;
   union __anonunion_code_66 code ;
   bool native ;
   Scope *creation_scope ;
};
#line 41 "include/Function.h"
typedef struct _VyFunction VyFunction;
#line 10 "types/Function.c"
struct _Param {
   bool optional ;
   bool rest ;
   VySymbol *name ;
   VyObj default_value ;
};
#line 31 "include/External.h"
struct _VyInt {
   int val ;
};
#line 31 "include/External.h"
typedef struct _VyInt VyInt;
#line 34 "include/External.h"
struct _VyFloat {
   double val ;
};
#line 34 "include/External.h"
typedef struct _VyFloat VyFloat;
#line 4 "vm/Scope.c"
struct _Scope {
   Scope *parent ;
   GHashTable *var_values ;
   GHashTable *type_values ;
   GHashTable *size_values ;
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
   float __attribute__((____vector_size____(16)))  __builtin_ia32_unpckhps(float __attribute__((____vector_size____(16)))   ,
                                                                           float __attribute__((____vector_size____(16)))   ) ;  */
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
   float __attribute__((____vector_size____(16)))  __builtin_ia32_subps(float __attribute__((____vector_size____(16)))   ,
                                                                        float __attribute__((____vector_size____(16)))   ) ;  */
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
   unsigned long __builtin_strlen(char const   * ) ;  */
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
   float __attribute__((____vector_size____(16)))  __builtin_ia32_mulps(float __attribute__((____vector_size____(16)))   ,
                                                                        float __attribute__((____vector_size____(16)))   ) ;  */
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
   float __attribute__((____vector_size____(16)))  __builtin_ia32_maxps(float __attribute__((____vector_size____(16)))   ,
                                                                        float __attribute__((____vector_size____(16)))   ) ;  */
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
   void __builtin_bcopy(void const   * , void * , unsigned long  ) ;  */
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
   float __attribute__((____vector_size____(16)))  __builtin_ia32_unpcklps(float __attribute__((____vector_size____(16)))   ,
                                                                           float __attribute__((____vector_size____(16)))   ) ;  */
/* compiler builtin: 
   double __builtin_tanh(double  ) ;  */
/* compiler builtin: 
   int __builtin_constant_p(int  ) ;  */
/* compiler builtin: 
   long double __builtin_ceill(long double  ) ;  */
/* compiler builtin: 
   int __builtin_va_arg_pack_len(void) ;  */
/* compiler builtin: 
   void *__builtin_apply(void (*)() , void * , unsigned long  ) ;  */
/* compiler builtin: 
   long double __builtin_tanl(long double  ) ;  */
/* compiler builtin: 
   double __builtin_log(double  ) ;  */
/* compiler builtin: 
   long double __builtin_ldexpl(long double  , int  ) ;  */
/* compiler builtin: 
   int __builtin_popcountl(unsigned long  ) ;  */
/* compiler builtin: 
   long double __builtin_expl(long double  ) ;  */
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
   void *__builtin_apply_args(void) ;  */
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
   void __builtin_bzero(void * , unsigned long  ) ;  */
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
   int __builtin_va_arg_pack(void) ;  */
/* compiler builtin: 
   float __attribute__((____vector_size____(16)))  __builtin_ia32_addps(float __attribute__((____vector_size____(16)))   ,
                                                                        float __attribute__((____vector_size____(16)))   ) ;  */
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
#line 1 "Lexer.o"
/* #pragma merger(0,"/tmp/cil-2qQ9LqDq.i","-Wall,-g") */
#line 213 "/usr/include/stdio.h"
extern int fclose(FILE *__stream ) ;
#line 327
extern int fprintf(FILE * __restrict  __stream ,
                   char const   * __restrict  __format  , ...) ;
#line 441
extern int fgetc(FILE *__stream ) ;
#line 610
extern size_t fread(void * __restrict  __ptr , size_t __size , size_t __n ,
                    FILE * __restrict  __stream ) ;
#line 650
extern int fseek(FILE *__stream , long __off , int __whence ) ;
#line 729
extern  __attribute__((__nothrow__)) int feof(FILE *__stream ) ;
#line 148 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) int atoi(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 165
extern  __attribute__((__nothrow__)) double strtod(char const   * __restrict  __nptr ,
                                                   char ** __restrict  __endptr )  __attribute__((__nonnull__(1))) ;
#line 242 "/usr/include/string.h"
extern  __attribute__((__nothrow__)) size_t strlen(char const   *__s )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 15 "include/Mem.h"
void *VyMalloc(size_t size ) ;
#line 19
void VyFree(void *ptr ) ;
#line 54 "include/Obj.h"
VyType TypeCons  ;
#line 54 "include/Obj.h"
VyType TypeString  ;
#line 54 "include/Obj.h"
VyType TypeSymbol  ;
#line 54 "include/Obj.h"
VyType TypeFloat  ;
#line 54 "include/Obj.h"
VyType TypeInt  ;
#line 54 "include/Obj.h"
VyType TypeFunction  ;
#line 54 "include/Obj.h"
VyType TypeNone  ;
#line 26 "include/Symbol.h"
VyObj SymbolFalse  ;
#line 26 "include/Symbol.h"
VyObj SymbolIf  ;
#line 26 "include/Symbol.h"
VyObj SymbolSetvar  ;
#line 26 "include/Symbol.h"
VyObj SymbolWhile  ;
#line 26 "include/Symbol.h"
VyObj SymbolNil  ;
#line 26 "include/Symbol.h"
VyObj SymbolQuote  ;
#line 26 "include/Symbol.h"
VyObj SymbolFn  ;
#line 41 "include/Lexer.h"
TokenList *LexFile(FILE *file ) ;
#line 42
TokenList *LexString(char *string ) ;
#line 45
void PrintTokens(FILE *out , TokenList *tokens ) ;
#line 48
void FreeTokens(TokenList *tokens ) ;
#line 4 "parser/Lexer.c"
__inline bool IsWhitespace(char c ) 
{ int tmp ;

  {
#line 4
  fprintf(_coverage_fout, "7\n");
#line 4
  fflush(_coverage_fout);
#line 5
  if ((int )c == 32) {
#line 5
    fprintf(_coverage_fout, "1\n");
#line 5
    fflush(_coverage_fout);
#line 5
    tmp = 1;
  } else {
#line 5
    fprintf(_coverage_fout, "6\n");
#line 5
    fflush(_coverage_fout);
#line 5
    if ((int )c == 9) {
#line 5
      fprintf(_coverage_fout, "2\n");
#line 5
      fflush(_coverage_fout);
#line 5
      tmp = 1;
    } else {
#line 5
      fprintf(_coverage_fout, "5\n");
#line 5
      fflush(_coverage_fout);
#line 5
      if ((int )c == 10) {
#line 5
        fprintf(_coverage_fout, "3\n");
#line 5
        fflush(_coverage_fout);
#line 5
        tmp = 1;
      } else {
#line 5
        fprintf(_coverage_fout, "4\n");
#line 5
        fflush(_coverage_fout);
#line 5
        tmp = 0;
      }
    }
  }
#line 4
  fprintf(_coverage_fout, "8\n");
#line 4
  fflush(_coverage_fout);
#line 5
  return (tmp);
}
}
#line 9 "parser/Lexer.c"
__inline bool IsNumeric(char c ) 
{ int tmp ;

  {
#line 9
  fprintf(_coverage_fout, "13\n");
#line 9
  fflush(_coverage_fout);
#line 10
  if (48 <= (int )c) {
#line 10
    fprintf(_coverage_fout, "11\n");
#line 10
    fflush(_coverage_fout);
#line 10
    if ((int )c <= 57) {
#line 10
      fprintf(_coverage_fout, "9\n");
#line 10
      fflush(_coverage_fout);
#line 10
      tmp = 1;
    } else {
#line 10
      fprintf(_coverage_fout, "10\n");
#line 10
      fflush(_coverage_fout);
#line 10
      tmp = 0;
    }
  } else {
#line 10
    fprintf(_coverage_fout, "12\n");
#line 10
    fflush(_coverage_fout);
#line 10
    tmp = 0;
  }
#line 9
  fprintf(_coverage_fout, "14\n");
#line 9
  fflush(_coverage_fout);
#line 10
  return (tmp);
}
}
#line 14 "parser/Lexer.c"
TokenList *CreateTokenList(void) 
{ Token token ;
  TokenList *tok_list ;
  void *tmp ;

  {
#line 14
  fprintf(_coverage_fout, "15\n");
#line 14
  fflush(_coverage_fout);
#line 15
  token.type = (char)60;
#line 14
  fprintf(_coverage_fout, "16\n");
#line 14
  fflush(_coverage_fout);
#line 15
  token.data = (char *)((void *)0);
#line 14
  fprintf(_coverage_fout, "17\n");
#line 14
  fflush(_coverage_fout);
#line 16
  tmp = VyMalloc(sizeof(TokenList ));
#line 14
  fprintf(_coverage_fout, "18\n");
#line 14
  fflush(_coverage_fout);
#line 16
  tok_list = (TokenList *)tmp;
#line 14
  fprintf(_coverage_fout, "19\n");
#line 14
  fflush(_coverage_fout);
#line 17
  tok_list->count = 0;
#line 14
  fprintf(_coverage_fout, "20\n");
#line 14
  fflush(_coverage_fout);
#line 18
  tok_list->token = token;
#line 14
  fprintf(_coverage_fout, "21\n");
#line 14
  fflush(_coverage_fout);
#line 19
  tok_list->next = (TokenList *)((void *)0);
#line 14
  fprintf(_coverage_fout, "22\n");
#line 14
  fflush(_coverage_fout);
#line 20
  return (tok_list);
}
}
#line 24 "parser/Lexer.c"
TokenList *AppendToken(TokenList *list , Token token ) 
{ TokenList *next ;
  void *tmp ;

  {
#line 24
  fprintf(_coverage_fout, "26\n");
#line 24
  fflush(_coverage_fout);
#line 26
  if ((int )list->token.type == 60) {
#line 26
    fprintf(_coverage_fout, "23\n");
#line 26
    fflush(_coverage_fout);
#line 27
    list->token = token;
#line 26
    fprintf(_coverage_fout, "24\n");
#line 26
    fflush(_coverage_fout);
#line 28
    return (list);
  } else {
#line 26
    fprintf(_coverage_fout, "25\n");
#line 26
    fflush(_coverage_fout);

  }
#line 24
  fprintf(_coverage_fout, "27\n");
#line 24
  fflush(_coverage_fout);
#line 32
  tmp = VyMalloc(sizeof(TokenList ));
#line 24
  fprintf(_coverage_fout, "28\n");
#line 24
  fflush(_coverage_fout);
#line 32
  next = (TokenList *)tmp;
#line 24
  fprintf(_coverage_fout, "29\n");
#line 24
  fflush(_coverage_fout);
#line 33
  next->count = list->count + 1;
#line 24
  fprintf(_coverage_fout, "30\n");
#line 24
  fflush(_coverage_fout);
#line 34
  next->token = token;
#line 24
  fprintf(_coverage_fout, "31\n");
#line 24
  fflush(_coverage_fout);
#line 35
  next->next = (TokenList *)((void *)0);
#line 24
  fprintf(_coverage_fout, "32\n");
#line 24
  fflush(_coverage_fout);
#line 36
  list->next = next;
#line 24
  fprintf(_coverage_fout, "33\n");
#line 24
  fflush(_coverage_fout);
#line 37
  return (next);
}
}
#line 43 "parser/Lexer.c"
void PrintTokens(FILE *out , TokenList *tokens ) 
{ char *null_str ;
  char *str_oparen ;
  char *str_cparen ;
  char *str_symbol ;
  char *str_quoted ;
  char *str_string ;
  char *str_numflt ;
  char *str_numint ;
  char *type ;
  char *data ;

  {
#line 43
  fprintf(_coverage_fout, "49\n");
#line 43
  fflush(_coverage_fout);
#line 45
  null_str = (char *)"NULL";
#line 43
  fprintf(_coverage_fout, "50\n");
#line 43
  fflush(_coverage_fout);
#line 46
  str_oparen = (char *)"TOKEN_OPAREN";
#line 43
  fprintf(_coverage_fout, "51\n");
#line 43
  fflush(_coverage_fout);
#line 47
  str_cparen = (char *)"TOKEN_CPAREN";
#line 43
  fprintf(_coverage_fout, "52\n");
#line 43
  fflush(_coverage_fout);
#line 48
  str_symbol = (char *)"TOKEN_SYMBOL";
#line 43
  fprintf(_coverage_fout, "53\n");
#line 43
  fflush(_coverage_fout);
#line 49
  str_quoted = (char *)"TOKEN_QUOTED";
#line 43
  fprintf(_coverage_fout, "54\n");
#line 43
  fflush(_coverage_fout);
#line 50
  str_string = (char *)"TOKEN_STRING";
#line 43
  fprintf(_coverage_fout, "55\n");
#line 43
  fflush(_coverage_fout);
#line 51
  str_numflt = (char *)"TOKEN_NUMFLT";
#line 43
  fprintf(_coverage_fout, "56\n");
#line 43
  fflush(_coverage_fout);
#line 52
  str_numint = (char *)"TOKEN_NUMINT";
#line 43
  fprintf(_coverage_fout, "57\n");
#line 43
  fflush(_coverage_fout);
#line 55
  while (1) {
#line 55
    fprintf(_coverage_fout, "45\n");
#line 55
    fflush(_coverage_fout);
#line 55
    if ((unsigned long )tokens != (unsigned long )((void *)0)) {
#line 55
      fprintf(_coverage_fout, "34\n");
#line 55
      fflush(_coverage_fout);

    } else {
#line 55
      break;
    }
#line 58
    switch ((int )tokens->token.type) {
#line 58
    fprintf(_coverage_fout, "35\n");
#line 58
    fflush(_coverage_fout);
    case 10: 
#line 59
    type = str_oparen;
#line 59
    break;
#line 58
    fprintf(_coverage_fout, "36\n");
#line 58
    fflush(_coverage_fout);
    case 20: 
#line 60
    type = str_cparen;
#line 60
    break;
#line 58
    fprintf(_coverage_fout, "37\n");
#line 58
    fflush(_coverage_fout);
    case 30: 
#line 61
    type = str_symbol;
#line 61
    break;
#line 58
    fprintf(_coverage_fout, "38\n");
#line 58
    fflush(_coverage_fout);
    case 40: 
#line 62
    type = str_quoted;
#line 62
    break;
#line 58
    fprintf(_coverage_fout, "39\n");
#line 58
    fflush(_coverage_fout);
    case 50: 
#line 63
    type = str_string;
#line 63
    break;
#line 58
    fprintf(_coverage_fout, "40\n");
#line 58
    fflush(_coverage_fout);
    case 26: 
#line 64
    type = str_numint;
#line 64
    break;
#line 58
    fprintf(_coverage_fout, "41\n");
#line 58
    fflush(_coverage_fout);
    case 25: 
#line 65
    type = str_numflt;
#line 65
    break;
#line 58
    fprintf(_coverage_fout, "42\n");
#line 58
    fflush(_coverage_fout);
    default: 
#line 66
    type = null_str;
    }
#line 55
    fprintf(_coverage_fout, "46\n");
#line 55
    fflush(_coverage_fout);
#line 71
    if ((unsigned long )tokens->token.data == (unsigned long )((void *)0)) {
#line 71
      fprintf(_coverage_fout, "43\n");
#line 71
      fflush(_coverage_fout);
#line 71
      data = null_str;
    } else {
#line 71
      fprintf(_coverage_fout, "44\n");
#line 71
      fflush(_coverage_fout);
#line 72
      data = tokens->token.data;
    }
#line 55
    fprintf(_coverage_fout, "47\n");
#line 55
    fflush(_coverage_fout);
#line 75
    fprintf((FILE */* __restrict  */)out,
            (char const   */* __restrict  */)"%d: {%s, \"%s\"}\n",
            tokens->count, type, data);
#line 55
    fprintf(_coverage_fout, "48\n");
#line 55
    fflush(_coverage_fout);
#line 76
    tokens = tokens->next;
  }
#line 43
  fprintf(_coverage_fout, "58\n");
#line 43
  fflush(_coverage_fout);
#line 78
  return;
}
}
#line 82 "parser/Lexer.c"
void FreeTokens(TokenList *tokens ) 
{ char *data ;
  TokenList *next ;

  {
#line 82
  fprintf(_coverage_fout, "68\n");
#line 82
  fflush(_coverage_fout);
#line 83
  while (1) {
#line 83
    fprintf(_coverage_fout, "62\n");
#line 83
    fflush(_coverage_fout);
#line 83
    if ((unsigned long )tokens != (unsigned long )((void *)0)) {
#line 83
      fprintf(_coverage_fout, "59\n");
#line 83
      fflush(_coverage_fout);

    } else {
#line 83
      break;
    }
#line 83
    fprintf(_coverage_fout, "63\n");
#line 83
    fflush(_coverage_fout);
#line 84
    data = tokens->token.data;
#line 83
    fprintf(_coverage_fout, "64\n");
#line 83
    fflush(_coverage_fout);
#line 85
    if (data) {
#line 85
      fprintf(_coverage_fout, "60\n");
#line 85
      fflush(_coverage_fout);
#line 85
      VyFree((void *)data);
    } else {
#line 85
      fprintf(_coverage_fout, "61\n");
#line 85
      fflush(_coverage_fout);

    }
#line 83
    fprintf(_coverage_fout, "65\n");
#line 83
    fflush(_coverage_fout);
#line 87
    next = tokens->next;
#line 83
    fprintf(_coverage_fout, "66\n");
#line 83
    fflush(_coverage_fout);
#line 88
    VyFree((void *)tokens);
#line 83
    fprintf(_coverage_fout, "67\n");
#line 83
    fflush(_coverage_fout);
#line 89
    tokens = next;
  }
#line 82
  fprintf(_coverage_fout, "69\n");
#line 82
  fflush(_coverage_fout);
#line 91
  return;
}
}
#line 94 "parser/Lexer.c"
int FindTokenType(char *data ) 
{ char *end_ptr ;
  double val ;
  double tmp ;
  int int_val ;
  int tmp___0 ;
  int i ;
  size_t tmp___1 ;

  {
#line 94
  fprintf(_coverage_fout, "85\n");
#line 94
  fflush(_coverage_fout);
#line 97
  tmp = strtod((char const   */* __restrict  */)data,
               (char **/* __restrict  */)(& end_ptr));
#line 94
  fprintf(_coverage_fout, "86\n");
#line 94
  fflush(_coverage_fout);
#line 97
  val = tmp;
#line 94
  fprintf(_coverage_fout, "87\n");
#line 94
  fflush(_coverage_fout);
#line 100
  if ((int )*(end_ptr + 0) != 0) {
#line 100
    fprintf(_coverage_fout, "70\n");
#line 100
    fflush(_coverage_fout);
#line 101
    return (30);
  } else {
#line 100
    fprintf(_coverage_fout, "82\n");
#line 100
    fflush(_coverage_fout);
#line 104
    tmp___0 = atoi((char const   *)data);
#line 100
    fprintf(_coverage_fout, "83\n");
#line 100
    fflush(_coverage_fout);
#line 104
    int_val = tmp___0;
#line 100
    fprintf(_coverage_fout, "84\n");
#line 100
    fflush(_coverage_fout);
#line 105
    if ((double )int_val == val) {
#line 105
      fprintf(_coverage_fout, "78\n");
#line 105
      fflush(_coverage_fout);
#line 109
      i = 0;
#line 105
      fprintf(_coverage_fout, "79\n");
#line 105
      fflush(_coverage_fout);
#line 109
      while (1) {
#line 109
        fprintf(_coverage_fout, "74\n");
#line 109
        fflush(_coverage_fout);
#line 109
        tmp___1 = strlen((char const   *)data);
#line 109
        fprintf(_coverage_fout, "75\n");
#line 109
        fflush(_coverage_fout);
#line 109
        if (! ((unsigned long )i < tmp___1)) {
#line 109
          break;
        } else {
#line 109
          fprintf(_coverage_fout, "71\n");
#line 109
          fflush(_coverage_fout);

        }
#line 109
        fprintf(_coverage_fout, "76\n");
#line 109
        fflush(_coverage_fout);
#line 110
        if ((int )*(data + i) == 46) {
#line 110
          fprintf(_coverage_fout, "72\n");
#line 110
          fflush(_coverage_fout);
#line 111
          return (25);
        } else {
#line 110
          fprintf(_coverage_fout, "73\n");
#line 110
          fflush(_coverage_fout);

        }
#line 109
        fprintf(_coverage_fout, "77\n");
#line 109
        fflush(_coverage_fout);
#line 109
        i ++;
      }
#line 105
      fprintf(_coverage_fout, "80\n");
#line 105
      fflush(_coverage_fout);
#line 113
      return (26);
    } else {
#line 105
      fprintf(_coverage_fout, "81\n");
#line 105
      fflush(_coverage_fout);
#line 116
      return (25);
    }
  }
}
}
#line 122 "parser/Lexer.c"
TokenList *LexFile(FILE *file ) 
{ TokenList *tokens ;
  TokenList *tmp ;
  TokenList *list_start ;
  char next ;
  int tmp___0 ;
  Token t ;
  int char_count ;
  char prev ;
  char *str_data ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  char *str_data___0 ;
  void *tmp___5 ;
  bool tmp___6 ;
  bool tmp___7 ;
  int tmp___8 ;
  int tmp___9 ;
  void *tmp___10 ;
  int tmp___11 ;
  int tmp___12 ;
  int tmp___13 ;

  {
#line 122
  fprintf(_coverage_fout, "159\n");
#line 122
  fflush(_coverage_fout);
#line 124
  tmp = CreateTokenList();
#line 122
  fprintf(_coverage_fout, "160\n");
#line 122
  fflush(_coverage_fout);
#line 124
  tokens = tmp;
#line 122
  fprintf(_coverage_fout, "161\n");
#line 122
  fflush(_coverage_fout);
#line 125
  list_start = tokens;
#line 122
  fprintf(_coverage_fout, "162\n");
#line 122
  fflush(_coverage_fout);
#line 126
  tmp___0 = fgetc(file);
#line 122
  fprintf(_coverage_fout, "163\n");
#line 122
  fflush(_coverage_fout);
#line 126
  next = (char )tmp___0;
#line 122
  fprintf(_coverage_fout, "164\n");
#line 122
  fflush(_coverage_fout);
#line 129
  while (1) {
#line 129
    fprintf(_coverage_fout, "154\n");
#line 129
    fflush(_coverage_fout);
#line 129
    tmp___13 = feof(file);
#line 129
    fprintf(_coverage_fout, "155\n");
#line 129
    fflush(_coverage_fout);
#line 129
    if (tmp___13) {
#line 129
      break;
    } else {
#line 129
      fprintf(_coverage_fout, "88\n");
#line 129
      fflush(_coverage_fout);

    }
#line 129
    fprintf(_coverage_fout, "156\n");
#line 129
    fflush(_coverage_fout);
#line 139
    str_data = (char *)((void *)0);
#line 141
    switch ((int )next) {
#line 141
    fprintf(_coverage_fout, "117\n");
#line 141
    fflush(_coverage_fout);
    case 59: 
#line 144
    while (1) {
#line 144
      fprintf(_coverage_fout, "90\n");
#line 144
      fflush(_coverage_fout);
#line 144
      if ((int )next != 10) {
#line 144
        fprintf(_coverage_fout, "89\n");
#line 144
        fflush(_coverage_fout);

      } else {
#line 144
        break;
      }
#line 144
      fprintf(_coverage_fout, "91\n");
#line 144
      fflush(_coverage_fout);
#line 145
      tmp___1 = fgetc(file);
#line 144
      fprintf(_coverage_fout, "92\n");
#line 144
      fflush(_coverage_fout);
#line 145
      next = (char )tmp___1;
    }
#line 147
    break;
#line 141
    fprintf(_coverage_fout, "118\n");
#line 141
    fflush(_coverage_fout);
    case 40: 
#line 150
    t.type = (char)10;
#line 141
    fprintf(_coverage_fout, "119\n");
#line 141
    fflush(_coverage_fout);
#line 150
    t.data = str_data;
#line 141
    fprintf(_coverage_fout, "120\n");
#line 141
    fflush(_coverage_fout);
#line 151
    tokens = AppendToken(tokens, t);
#line 152
    break;
#line 141
    fprintf(_coverage_fout, "121\n");
#line 141
    fflush(_coverage_fout);
    case 41: 
#line 154
    t.type = (char)20;
#line 141
    fprintf(_coverage_fout, "122\n");
#line 141
    fflush(_coverage_fout);
#line 154
    t.data = str_data;
#line 141
    fprintf(_coverage_fout, "123\n");
#line 141
    fflush(_coverage_fout);
#line 155
    tokens = AppendToken(tokens, t);
#line 156
    break;
#line 141
    fprintf(_coverage_fout, "124\n");
#line 141
    fflush(_coverage_fout);
    case 39: 
#line 158
    t.type = (char)40;
#line 141
    fprintf(_coverage_fout, "125\n");
#line 141
    fflush(_coverage_fout);
#line 158
    t.data = str_data;
#line 141
    fprintf(_coverage_fout, "126\n");
#line 141
    fflush(_coverage_fout);
#line 159
    tokens = AppendToken(tokens, t);
#line 160
    break;
#line 141
    fprintf(_coverage_fout, "127\n");
#line 141
    fflush(_coverage_fout);
    case 34: 
#line 165
    char_count = 0;
#line 141
    fprintf(_coverage_fout, "128\n");
#line 141
    fflush(_coverage_fout);
#line 166
    prev = next;
#line 141
    fprintf(_coverage_fout, "129\n");
#line 141
    fflush(_coverage_fout);
#line 167
    tmp___2 = fgetc(file);
#line 141
    fprintf(_coverage_fout, "130\n");
#line 141
    fflush(_coverage_fout);
#line 167
    next = (char )tmp___2;
#line 141
    fprintf(_coverage_fout, "131\n");
#line 141
    fflush(_coverage_fout);
#line 168
    while (1) {
#line 168
      fprintf(_coverage_fout, "97\n");
#line 168
      fflush(_coverage_fout);
#line 170
      if ((int )next == 34) {
#line 170
        fprintf(_coverage_fout, "94\n");
#line 170
        fflush(_coverage_fout);
#line 170
        if ((int )prev != 92) {
#line 170
          break;
        } else {
#line 170
          fprintf(_coverage_fout, "93\n");
#line 170
          fflush(_coverage_fout);

        }
      } else {
#line 170
        fprintf(_coverage_fout, "95\n");
#line 170
        fflush(_coverage_fout);

      }
#line 168
      fprintf(_coverage_fout, "98\n");
#line 168
      fflush(_coverage_fout);
#line 171
      tmp___3 = feof(file);
#line 168
      fprintf(_coverage_fout, "99\n");
#line 168
      fflush(_coverage_fout);
#line 171
      if (tmp___3) {
#line 171
        break;
      } else {
#line 171
        fprintf(_coverage_fout, "96\n");
#line 171
        fflush(_coverage_fout);

      }
#line 168
      fprintf(_coverage_fout, "100\n");
#line 168
      fflush(_coverage_fout);
#line 173
      char_count ++;
#line 168
      fprintf(_coverage_fout, "101\n");
#line 168
      fflush(_coverage_fout);
#line 174
      prev = next;
#line 168
      fprintf(_coverage_fout, "102\n");
#line 168
      fflush(_coverage_fout);
#line 175
      tmp___4 = fgetc(file);
#line 168
      fprintf(_coverage_fout, "103\n");
#line 168
      fflush(_coverage_fout);
#line 175
      next = (char )tmp___4;
    }
#line 141
    fprintf(_coverage_fout, "132\n");
#line 141
    fflush(_coverage_fout);
#line 179
    tmp___5 = VyMalloc(sizeof(char ) * (unsigned long )(char_count + 1));
#line 141
    fprintf(_coverage_fout, "133\n");
#line 141
    fflush(_coverage_fout);
#line 179
    str_data___0 = (char *)tmp___5;
#line 141
    fprintf(_coverage_fout, "134\n");
#line 141
    fflush(_coverage_fout);
#line 180
    fseek(file, (long )(- char_count - 1), 1);
#line 141
    fprintf(_coverage_fout, "135\n");
#line 141
    fflush(_coverage_fout);
#line 181
    fread((void */* __restrict  */)str_data___0, sizeof(char ),
          (unsigned long )char_count, (FILE */* __restrict  */)file);
#line 141
    fprintf(_coverage_fout, "136\n");
#line 141
    fflush(_coverage_fout);
#line 184
    *(str_data___0 + char_count) = (char )'\000';
#line 141
    fprintf(_coverage_fout, "137\n");
#line 141
    fflush(_coverage_fout);
#line 187
    fgetc(file);
#line 141
    fprintf(_coverage_fout, "138\n");
#line 141
    fflush(_coverage_fout);
#line 189
    t.type = (char)50;
#line 141
    fprintf(_coverage_fout, "139\n");
#line 141
    fflush(_coverage_fout);
#line 189
    t.data = str_data___0;
#line 141
    fprintf(_coverage_fout, "140\n");
#line 141
    fflush(_coverage_fout);
#line 190
    tokens = AppendToken(tokens, t);
#line 191
    break;
#line 141
    fprintf(_coverage_fout, "141\n");
#line 141
    fflush(_coverage_fout);
    default: 
#line 196
    tmp___6 = IsWhitespace(next);
#line 141
    fprintf(_coverage_fout, "142\n");
#line 141
    fflush(_coverage_fout);
#line 196
    if (tmp___6) {
#line 196
      break;
    } else {
#line 196
      fprintf(_coverage_fout, "104\n");
#line 196
      fflush(_coverage_fout);

    }
#line 141
    fprintf(_coverage_fout, "143\n");
#line 141
    fflush(_coverage_fout);
#line 198
    char_count = 0;
#line 141
    fprintf(_coverage_fout, "144\n");
#line 141
    fflush(_coverage_fout);
#line 201
    while (1) {
#line 201
      fprintf(_coverage_fout, "111\n");
#line 201
      fflush(_coverage_fout);
#line 202
      if ((int )next == 41) {
#line 202
        break;
      } else {
#line 202
        fprintf(_coverage_fout, "109\n");
#line 202
        fflush(_coverage_fout);
#line 202
        if ((int )next == 39) {
#line 202
          break;
        } else {
#line 202
          fprintf(_coverage_fout, "108\n");
#line 202
          fflush(_coverage_fout);
#line 202
          if ((int )next == 40) {
#line 202
            break;
          } else {
#line 202
            fprintf(_coverage_fout, "106\n");
#line 202
            fflush(_coverage_fout);
#line 202
            tmp___7 = IsWhitespace(next);
#line 202
            fprintf(_coverage_fout, "107\n");
#line 202
            fflush(_coverage_fout);
#line 202
            if (tmp___7) {
#line 202
              break;
            } else {
#line 202
              fprintf(_coverage_fout, "105\n");
#line 202
              fflush(_coverage_fout);

            }
          }
        }
      }
#line 201
      fprintf(_coverage_fout, "112\n");
#line 201
      fflush(_coverage_fout);
#line 203
      tmp___8 = feof(file);
#line 201
      fprintf(_coverage_fout, "113\n");
#line 201
      fflush(_coverage_fout);
#line 203
      if (tmp___8) {
#line 203
        break;
      } else {
#line 203
        fprintf(_coverage_fout, "110\n");
#line 203
        fflush(_coverage_fout);

      }
#line 201
      fprintf(_coverage_fout, "114\n");
#line 201
      fflush(_coverage_fout);
#line 205
      char_count ++;
#line 201
      fprintf(_coverage_fout, "115\n");
#line 201
      fflush(_coverage_fout);
#line 207
      tmp___9 = fgetc(file);
#line 201
      fprintf(_coverage_fout, "116\n");
#line 201
      fflush(_coverage_fout);
#line 207
      next = (char )tmp___9;
    }
#line 141
    fprintf(_coverage_fout, "145\n");
#line 141
    fflush(_coverage_fout);
#line 211
    tmp___10 = VyMalloc(sizeof(char ) * (unsigned long )(char_count + 1));
#line 141
    fprintf(_coverage_fout, "146\n");
#line 141
    fflush(_coverage_fout);
#line 211
    str_data___0 = (char *)tmp___10;
#line 141
    fprintf(_coverage_fout, "147\n");
#line 141
    fflush(_coverage_fout);
#line 212
    fseek(file, (long )(- char_count - 1), 1);
#line 141
    fprintf(_coverage_fout, "148\n");
#line 141
    fflush(_coverage_fout);
#line 213
    fread((void */* __restrict  */)str_data___0, sizeof(char ),
          (unsigned long )(char_count - 0), (FILE */* __restrict  */)file);
#line 141
    fprintf(_coverage_fout, "149\n");
#line 141
    fflush(_coverage_fout);
#line 215
    *(str_data___0 + char_count) = (char )'\000';
#line 141
    fprintf(_coverage_fout, "150\n");
#line 141
    fflush(_coverage_fout);
#line 220
    tmp___11 = FindTokenType(str_data___0);
#line 141
    fprintf(_coverage_fout, "151\n");
#line 141
    fflush(_coverage_fout);
#line 220
    t.type = (char )tmp___11;
#line 141
    fprintf(_coverage_fout, "152\n");
#line 141
    fflush(_coverage_fout);
#line 220
    t.data = str_data___0;
#line 141
    fprintf(_coverage_fout, "153\n");
#line 141
    fflush(_coverage_fout);
#line 221
    tokens = AppendToken(tokens, t);
#line 222
    break;
    }
#line 129
    fprintf(_coverage_fout, "157\n");
#line 129
    fflush(_coverage_fout);
#line 226
    tmp___12 = fgetc(file);
#line 129
    fprintf(_coverage_fout, "158\n");
#line 129
    fflush(_coverage_fout);
#line 226
    next = (char )tmp___12;
  }
#line 122
  fprintf(_coverage_fout, "165\n");
#line 122
  fflush(_coverage_fout);
#line 230
  return (list_start);
}
}
#line 236
extern int fmemopen() ;
#line 234 "parser/Lexer.c"
TokenList *LexString(char *string ) 
{ FILE *file ;
  size_t tmp ;
  int tmp___0 ;
  TokenList *tokens ;
  TokenList *tmp___1 ;

  {
#line 234
  fprintf(_coverage_fout, "166\n");
#line 234
  fflush(_coverage_fout);
#line 236
  tmp = strlen((char const   *)string);
#line 234
  fprintf(_coverage_fout, "167\n");
#line 234
  fflush(_coverage_fout);
#line 236
  tmp___0 = fmemopen(string, tmp, "r");
#line 234
  fprintf(_coverage_fout, "168\n");
#line 234
  fflush(_coverage_fout);
#line 236
  file = (FILE *)tmp___0;
#line 234
  fprintf(_coverage_fout, "169\n");
#line 234
  fflush(_coverage_fout);
#line 237
  tmp___1 = LexFile(file);
#line 234
  fprintf(_coverage_fout, "170\n");
#line 234
  fflush(_coverage_fout);
#line 237
  tokens = tmp___1;
#line 234
  fprintf(_coverage_fout, "171\n");
#line 234
  fflush(_coverage_fout);
#line 239
  fclose(file);
#line 234
  fprintf(_coverage_fout, "172\n");
#line 234
  fflush(_coverage_fout);
#line 240
  return (tokens);
}
}
#line 1 "Mem.o"
/* #pragma merger(0,"/tmp/cil-4chi4D1f.i","-Wall,-g") */
#line 589 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) void *malloc(size_t __size )  __attribute__((__malloc__)) ;
#line 600
extern  __attribute__((__nothrow__)) void *realloc(void *__ptr , size_t __size )  __attribute__((__warn_unused_result__,
__malloc__)) ;
#line 603
extern  __attribute__((__nothrow__)) void free(void *__ptr ) ;
#line 16 "include/Mem.h"
void *VyRealloc(void *ptr , size_t size ) ;
#line 4 "mem/Mem.c"
void *VyMalloc(size_t size ) 
{ void *tmp ;

  {
#line 4
  fprintf(_coverage_fout, "173\n");
#line 4
  fflush(_coverage_fout);
#line 5
  tmp = malloc(size);
#line 4
  fprintf(_coverage_fout, "174\n");
#line 4
  fflush(_coverage_fout);
#line 5
  return (tmp);
}
}
#line 7 "mem/Mem.c"
void *VyRealloc(void *ptr , size_t size ) 
{ void *tmp ;

  {
#line 7
  fprintf(_coverage_fout, "175\n");
#line 7
  fflush(_coverage_fout);
#line 8
  tmp = realloc(ptr, size);
#line 7
  fprintf(_coverage_fout, "176\n");
#line 7
  fflush(_coverage_fout);
#line 8
  return (tmp);
}
}
#line 10 "mem/Mem.c"
void VyFree(void *ptr ) 
{ 

  {
#line 10
  fprintf(_coverage_fout, "177\n");
#line 10
  fflush(_coverage_fout);
#line 11
  free(ptr);
#line 10
  fprintf(_coverage_fout, "178\n");
#line 10
  fflush(_coverage_fout);
#line 12
  return;
}
}
#line 1 "Vyquon.o"
/* #pragma merger(0,"/tmp/cil-lYUq1Esk.i","-Wall,-g") */
#line 143 "/usr/include/stdio.h"
extern struct _IO_FILE *stdout ;
#line 248
extern FILE *fopen(char const   * __restrict  __filename ,
                   char const   * __restrict  __modes ) ;
#line 333
extern int printf(char const   * __restrict  __format  , ...) ;
#line 51 "include/Obj.h"
void PrintObj(FILE *file , VyObj obj ) ;
#line 29 "include/Symbol.h"
void DeleteInternedSymbols(void) ;
#line 15 "include/VM.h"
VyObj Eval(VyObj sexp ) ;
#line 14 "include/Parser.h"
VyObj Parse(TokenList *tokens , TokenList **used ) ;
#line 6 "Vyquon.c"
void LoadCoreLibrary(void) ;
#line 7
void FinishRuntime(void) ;
#line 9 "Vyquon.c"
int main(void) 
{ FILE *file ;
  FILE *tmp ;
  TokenList *tokens ;
  TokenList *tmp___0 ;
  TokenList *last ;
  VyObj parsed ;
  VyObj tmp___1 ;
  VyObj val ;
  VyObj tmp___2 ;

  {
  __globinit_vyquon_comb();
#line 9
  fprintf(_coverage_fout, "190\n");
#line 9
  fflush(_coverage_fout);
#line 11
  LoadCoreLibrary();
#line 9
  fprintf(_coverage_fout, "191\n");
#line 9
  fflush(_coverage_fout);
#line 14
  tmp = fopen((char const   */* __restrict  */)"test/test.vy",
              (char const   */* __restrict  */)"r");
#line 9
  fprintf(_coverage_fout, "192\n");
#line 9
  fflush(_coverage_fout);
#line 14
  file = tmp;
#line 9
  fprintf(_coverage_fout, "193\n");
#line 9
  fflush(_coverage_fout);
#line 15
  tmp___0 = LexFile(file);
#line 9
  fprintf(_coverage_fout, "194\n");
#line 9
  fflush(_coverage_fout);
#line 15
  tokens = tmp___0;
#line 9
  fprintf(_coverage_fout, "195\n");
#line 9
  fflush(_coverage_fout);
#line 17
  last = tokens;
#line 9
  fprintf(_coverage_fout, "196\n");
#line 9
  fflush(_coverage_fout);
#line 18
  while (1) {
#line 18
    fprintf(_coverage_fout, "180\n");
#line 18
    fflush(_coverage_fout);
#line 18
    if (last) {
#line 18
      fprintf(_coverage_fout, "179\n");
#line 18
      fflush(_coverage_fout);

    } else {
#line 18
      break;
    }
#line 18
    fprintf(_coverage_fout, "181\n");
#line 18
    fflush(_coverage_fout);
#line 19
    tmp___1 = Parse(last, & last);
#line 18
    fprintf(_coverage_fout, "182\n");
#line 18
    fflush(_coverage_fout);
#line 19
    parsed = tmp___1;
#line 18
    fprintf(_coverage_fout, "183\n");
#line 18
    fflush(_coverage_fout);
#line 21
    printf((char const   */* __restrict  */)"|");
#line 18
    fprintf(_coverage_fout, "184\n");
#line 18
    fflush(_coverage_fout);
#line 22
    PrintObj(stdout, parsed);
#line 18
    fprintf(_coverage_fout, "185\n");
#line 18
    fflush(_coverage_fout);
#line 23
    printf((char const   */* __restrict  */)"|\nevaluates to\n");
#line 18
    fprintf(_coverage_fout, "186\n");
#line 18
    fflush(_coverage_fout);
#line 25
    tmp___2 = Eval(parsed);
#line 18
    fprintf(_coverage_fout, "187\n");
#line 18
    fflush(_coverage_fout);
#line 25
    val = tmp___2;
#line 18
    fprintf(_coverage_fout, "188\n");
#line 18
    fflush(_coverage_fout);
#line 27
    PrintObj(stdout, val);
#line 18
    fprintf(_coverage_fout, "189\n");
#line 18
    fflush(_coverage_fout);
#line 28
    printf((char const   */* __restrict  */)"\n\n");
  }
#line 9
  fprintf(_coverage_fout, "197\n");
#line 9
  fflush(_coverage_fout);
#line 31
  FreeTokens(tokens);
#line 9
  fprintf(_coverage_fout, "198\n");
#line 9
  fflush(_coverage_fout);
#line 34
  FinishRuntime();
#line 9
  fprintf(_coverage_fout, "199\n");
#line 9
  fflush(_coverage_fout);
#line 37
  return (0);
}
}
#line 41 "Vyquon.c"
void FinishRuntime(void) 
{ 

  {
#line 41
  fprintf(_coverage_fout, "200\n");
#line 41
  fflush(_coverage_fout);
#line 42
  DeleteInternedSymbols();
#line 41
  fprintf(_coverage_fout, "201\n");
#line 41
  fflush(_coverage_fout);
#line 43
  return;
}
}
#line 1 "Parser.o"
/* #pragma merger(0,"/tmp/cil-iV0mzugx.i","-Wall,-g") */
#line 144 "/usr/include/stdio.h"
extern struct _IO_FILE *stderr ;
#line 646 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__, __noreturn__)) void exit(int __status ) ;
#line 24 "include/Cons.h"
VyObj Nil(void) ;
#line 31
VyObj Cons(VyObj car , VyObj cdr ) ;
#line 12 "include/String.h"
VyObj CreateString(char *str ) ;
#line 17 "include/Symbol.h"
VyObj CreateSymbol(char *str ) ;
#line 6 "parser/Parser.c"
VyObj ParseList(TokenList *tokens , TokenList **used ) 
{ VyObj tmp ;
  TokenList *last ;
  VyObj car ;
  VyObj tmp___0 ;
  VyObj cdr ;
  VyObj tmp___1 ;
  VyObj tmp___2 ;

  {
#line 6
  fprintf(_coverage_fout, "211\n");
#line 6
  fflush(_coverage_fout);
#line 8
  if ((int )tokens->token.type == 20) {
#line 8
    fprintf(_coverage_fout, "202\n");
#line 8
    fflush(_coverage_fout);
#line 10
    *used = tokens->next;
#line 8
    fprintf(_coverage_fout, "203\n");
#line 8
    fflush(_coverage_fout);
#line 13
    tmp = Nil();
#line 8
    fprintf(_coverage_fout, "204\n");
#line 8
    fflush(_coverage_fout);
#line 13
    return (tmp);
  } else {
#line 8
    fprintf(_coverage_fout, "205\n");
#line 8
    fflush(_coverage_fout);
#line 20
    tmp___0 = Parse(tokens, & last);
#line 8
    fprintf(_coverage_fout, "206\n");
#line 8
    fflush(_coverage_fout);
#line 20
    car = tmp___0;
#line 8
    fprintf(_coverage_fout, "207\n");
#line 8
    fflush(_coverage_fout);
#line 21
    tmp___1 = ParseList(last, used);
#line 8
    fprintf(_coverage_fout, "208\n");
#line 8
    fflush(_coverage_fout);
#line 21
    cdr = tmp___1;
#line 8
    fprintf(_coverage_fout, "209\n");
#line 8
    fflush(_coverage_fout);
#line 24
    tmp___2 = Cons(car, cdr);
#line 8
    fprintf(_coverage_fout, "210\n");
#line 8
    fflush(_coverage_fout);
#line 24
    return (tmp___2);
  }
}
}
#line 29 "parser/Parser.c"
VyObj ParseQuoted(TokenList *tokens , TokenList **used ) 
{ VyObj tmp ;
  VyObj tmp___0 ;
  VyObj tmp___1 ;
  VyObj tmp___2 ;
  VyObj tmp___3 ;

  {
#line 29
  fprintf(_coverage_fout, "212\n");
#line 29
  fflush(_coverage_fout);
#line 30
  *used = tokens;
#line 29
  fprintf(_coverage_fout, "213\n");
#line 29
  fflush(_coverage_fout);
#line 32
  tmp = Nil();
#line 29
  fprintf(_coverage_fout, "214\n");
#line 29
  fflush(_coverage_fout);
#line 32
  tmp___0 = Parse(tokens, used);
#line 29
  fprintf(_coverage_fout, "215\n");
#line 29
  fflush(_coverage_fout);
#line 32
  tmp___1 = Cons(tmp___0, tmp);
#line 29
  fprintf(_coverage_fout, "216\n");
#line 29
  fflush(_coverage_fout);
#line 32
  tmp___2 = CreateSymbol((char *)"quote");
#line 29
  fprintf(_coverage_fout, "217\n");
#line 29
  fflush(_coverage_fout);
#line 32
  tmp___3 = Cons(tmp___2, tmp___1);
#line 29
  fprintf(_coverage_fout, "218\n");
#line 29
  fflush(_coverage_fout);
#line 32
  return (tmp___3);
}
}
#line 35
VyObj CreateFloatFromStr(char *str ) ;
#line 36
VyObj CreateIntFromStr(char *str ) ;
#line 39 "parser/Parser.c"
VyObj Parse(TokenList *tokens , TokenList **used ) 
{ Token first ;
  VyObj tmp ;
  VyObj tmp___0 ;
  VyObj tmp___1 ;
  VyObj tmp___2 ;
  VyObj tmp___3 ;
  VyObj tmp___4 ;
  VyObj tmp___5 ;

  {
#line 39
  fprintf(_coverage_fout, "239\n");
#line 39
  fflush(_coverage_fout);
#line 41
  first = tokens->token;
#line 42
  switch ((int )first.type) {
#line 42
  fprintf(_coverage_fout, "219\n");
#line 42
  fflush(_coverage_fout);
  case 10: 
#line 45
  tmp = ParseList(tokens->next, used);
#line 42
  fprintf(_coverage_fout, "220\n");
#line 42
  fflush(_coverage_fout);
#line 45
  return (tmp);
#line 42
  fprintf(_coverage_fout, "221\n");
#line 42
  fflush(_coverage_fout);
  case 40: 
#line 49
  tmp___0 = ParseQuoted(tokens->next, used);
#line 42
  fprintf(_coverage_fout, "222\n");
#line 42
  fflush(_coverage_fout);
#line 49
  return (tmp___0);
#line 42
  fprintf(_coverage_fout, "223\n");
#line 42
  fflush(_coverage_fout);
  case 30: 
#line 53
  *used = tokens->next;
#line 42
  fprintf(_coverage_fout, "224\n");
#line 42
  fflush(_coverage_fout);
#line 54
  tmp___1 = CreateSymbol(first.data);
#line 42
  fprintf(_coverage_fout, "225\n");
#line 42
  fflush(_coverage_fout);
#line 54
  return (tmp___1);
#line 42
  fprintf(_coverage_fout, "226\n");
#line 42
  fflush(_coverage_fout);
  case 50: 
#line 56
  *used = tokens->next;
#line 42
  fprintf(_coverage_fout, "227\n");
#line 42
  fflush(_coverage_fout);
#line 57
  tmp___2 = CreateString(first.data);
#line 42
  fprintf(_coverage_fout, "228\n");
#line 42
  fflush(_coverage_fout);
#line 57
  return (tmp___2);
#line 42
  fprintf(_coverage_fout, "229\n");
#line 42
  fflush(_coverage_fout);
  case 25: 
#line 59
  *used = tokens->next;
#line 42
  fprintf(_coverage_fout, "230\n");
#line 42
  fflush(_coverage_fout);
#line 60
  tmp___3 = CreateFloatFromStr(first.data);
#line 42
  fprintf(_coverage_fout, "231\n");
#line 42
  fflush(_coverage_fout);
#line 60
  return (tmp___3);
#line 42
  fprintf(_coverage_fout, "232\n");
#line 42
  fflush(_coverage_fout);
  case 26: 
#line 62
  *used = tokens->next;
#line 42
  fprintf(_coverage_fout, "233\n");
#line 42
  fflush(_coverage_fout);
#line 63
  tmp___4 = CreateIntFromStr(first.data);
#line 42
  fprintf(_coverage_fout, "234\n");
#line 42
  fflush(_coverage_fout);
#line 63
  return (tmp___4);
#line 42
  fprintf(_coverage_fout, "235\n");
#line 42
  fflush(_coverage_fout);
  case 60: 
#line 67
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"Error: No code in the file.\n");
#line 42
  fprintf(_coverage_fout, "236\n");
#line 42
  fflush(_coverage_fout);
#line 68
  exit(0);
#line 42
  fprintf(_coverage_fout, "237\n");
#line 42
  fflush(_coverage_fout);
  default: 
#line 72
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"Error: Unexpected token type\n");
#line 42
  fprintf(_coverage_fout, "238\n");
#line 42
  fflush(_coverage_fout);
#line 73
  exit(0);
  }
#line 39
  fprintf(_coverage_fout, "240\n");
#line 39
  fflush(_coverage_fout);
#line 77
  tmp___5 = Nil();
#line 39
  fprintf(_coverage_fout, "241\n");
#line 39
  fflush(_coverage_fout);
#line 77
  return (tmp___5);
}
}
#line 1 "Cons.o"
/* #pragma merger(0,"/tmp/cil-A7dek07T.i","-Wall,-g") */
#line 22 "include/Obj.h"
void *Obj(VyObj object ) ;
#line 28
bool IsType(VyObj obj , VyType type ) ;
#line 36
VyObj WrapObj(void *data , VyType type ) ;
#line 48
bool IsNone(VyObj obj ) ;
#line 27 "include/Cons.h"
bool IsNil(VyObj obj ) ;
#line 34
VyObj Car(VyObj cons ) ;
#line 35
VyObj Cdr(VyObj cons ) ;
#line 40
VyObj ListGet(VyCons *list , int index___0 ) ;
#line 45
int ListLen(VyObj list ) ;
#line 4 "types/Cons.c"
VyObj nil_symbol  =    {{0, (struct _VySymbol *)((void *)0)}, (void *)0};
#line 6 "types/Cons.c"
VyObj Nil(void) 
{ bool tmp ;

  {
#line 6
  fprintf(_coverage_fout, "244\n");
#line 6
  fflush(_coverage_fout);
#line 7
  tmp = IsNone(nil_symbol);
#line 6
  fprintf(_coverage_fout, "245\n");
#line 6
  fflush(_coverage_fout);
#line 7
  if (tmp) {
#line 7
    fprintf(_coverage_fout, "242\n");
#line 7
    fflush(_coverage_fout);
#line 8
    nil_symbol = CreateSymbol((char *)"nil");
  } else {
#line 7
    fprintf(_coverage_fout, "243\n");
#line 7
    fflush(_coverage_fout);

  }
#line 6
  fprintf(_coverage_fout, "246\n");
#line 6
  fflush(_coverage_fout);
#line 9
  return (nil_symbol);
}
}
#line 13 "types/Cons.c"
VyObj Cons(VyObj car , VyObj cdr ) 
{ VyCons *cons ;
  void *tmp ;
  VyObj tmp___0 ;

  {
#line 13
  fprintf(_coverage_fout, "247\n");
#line 13
  fflush(_coverage_fout);
#line 14
  tmp = VyMalloc(sizeof(VyCons ));
#line 13
  fprintf(_coverage_fout, "248\n");
#line 13
  fflush(_coverage_fout);
#line 14
  cons = (VyCons *)tmp;
#line 13
  fprintf(_coverage_fout, "249\n");
#line 13
  fflush(_coverage_fout);
#line 15
  cons->car = car;
#line 13
  fprintf(_coverage_fout, "250\n");
#line 13
  fflush(_coverage_fout);
#line 16
  cons->cdr = cdr;
#line 13
  fprintf(_coverage_fout, "251\n");
#line 13
  fflush(_coverage_fout);
#line 17
  tmp___0 = WrapObj((void *)cons, TypeCons);
#line 13
  fprintf(_coverage_fout, "252\n");
#line 13
  fflush(_coverage_fout);
#line 17
  return (tmp___0);
}
}
#line 21 "types/Cons.c"
VyObj Cdr(VyObj cons ) 
{ void *tmp ;

  {
#line 21
  fprintf(_coverage_fout, "253\n");
#line 21
  fflush(_coverage_fout);
#line 22
  tmp = Obj(cons);
#line 21
  fprintf(_coverage_fout, "254\n");
#line 21
  fflush(_coverage_fout);
#line 22
  return (((VyCons *)tmp)->cdr);
}
}
#line 24 "types/Cons.c"
VyObj Car(VyObj cons ) 
{ void *tmp ;

  {
#line 24
  fprintf(_coverage_fout, "255\n");
#line 24
  fflush(_coverage_fout);
#line 25
  tmp = Obj(cons);
#line 24
  fprintf(_coverage_fout, "256\n");
#line 24
  fflush(_coverage_fout);
#line 25
  return (((VyCons *)tmp)->car);
}
}
#line 29 "types/Cons.c"
bool IsNil(VyObj obj ) 
{ VyObj tmp ;
  bool tmp___0 ;
  VyObj tmp___1 ;
  bool tmp___2 ;
  int tmp___3 ;
  bool tmp___4 ;
  bool tmp___5 ;
  void *tmp___6 ;
  void *tmp___7 ;

  {
#line 29
  fprintf(_coverage_fout, "270\n");
#line 29
  fflush(_coverage_fout);
#line 31
  tmp___4 = IsType(obj, TypeCons);
#line 29
  fprintf(_coverage_fout, "271\n");
#line 29
  fflush(_coverage_fout);
#line 31
  if (tmp___4) {
#line 31
    fprintf(_coverage_fout, "263\n");
#line 31
    fflush(_coverage_fout);
#line 32
    tmp = Car(obj);
#line 31
    fprintf(_coverage_fout, "264\n");
#line 31
    fflush(_coverage_fout);
#line 32
    tmp___0 = IsNone(tmp);
#line 31
    fprintf(_coverage_fout, "265\n");
#line 31
    fflush(_coverage_fout);
#line 32
    if (tmp___0) {
#line 32
      fprintf(_coverage_fout, "259\n");
#line 32
      fflush(_coverage_fout);
#line 32
      tmp___1 = Cdr(obj);
#line 32
      fprintf(_coverage_fout, "260\n");
#line 32
      fflush(_coverage_fout);
#line 32
      tmp___2 = IsNone(tmp___1);
#line 32
      fprintf(_coverage_fout, "261\n");
#line 32
      fflush(_coverage_fout);
#line 32
      if (tmp___2) {
#line 32
        fprintf(_coverage_fout, "257\n");
#line 32
        fflush(_coverage_fout);
#line 32
        tmp___3 = 1;
      } else {
#line 32
        fprintf(_coverage_fout, "258\n");
#line 32
        fflush(_coverage_fout);
#line 32
        tmp___3 = 0;
      }
    } else {
#line 32
      fprintf(_coverage_fout, "262\n");
#line 32
      fflush(_coverage_fout);
#line 32
      tmp___3 = 0;
    }
#line 31
    fprintf(_coverage_fout, "266\n");
#line 31
    fflush(_coverage_fout);
#line 32
    return (tmp___3);
  } else {
#line 31
    fprintf(_coverage_fout, "267\n");
#line 31
    fflush(_coverage_fout);

  }
#line 29
  fprintf(_coverage_fout, "272\n");
#line 29
  fflush(_coverage_fout);
#line 36
  tmp___5 = IsNone(nil_symbol);
#line 29
  fprintf(_coverage_fout, "273\n");
#line 29
  fflush(_coverage_fout);
#line 36
  if (tmp___5) {
#line 36
    fprintf(_coverage_fout, "268\n");
#line 36
    fflush(_coverage_fout);
#line 37
    nil_symbol = CreateSymbol((char *)"nil");
  } else {
#line 36
    fprintf(_coverage_fout, "269\n");
#line 36
    fflush(_coverage_fout);

  }
#line 29
  fprintf(_coverage_fout, "274\n");
#line 29
  fflush(_coverage_fout);
#line 40
  tmp___6 = Obj(obj);
#line 29
  fprintf(_coverage_fout, "275\n");
#line 29
  fflush(_coverage_fout);
#line 40
  tmp___7 = Obj(nil_symbol);
#line 29
  fprintf(_coverage_fout, "276\n");
#line 29
  fflush(_coverage_fout);
#line 40
  return ((unsigned long )tmp___6 == (unsigned long )tmp___7);
}
}
#line 44 "types/Cons.c"
VyObj ListGet(VyCons *list , int index___0 ) 
{ void *tmp ;
  VyObj tmp___0 ;

  {
#line 44
  fprintf(_coverage_fout, "281\n");
#line 44
  fflush(_coverage_fout);
#line 46
  if (index___0 == 0) {
#line 46
    fprintf(_coverage_fout, "277\n");
#line 46
    fflush(_coverage_fout);
#line 46
    return (list->car);
  } else {
#line 46
    fprintf(_coverage_fout, "278\n");
#line 46
    fflush(_coverage_fout);
#line 49
    tmp = Obj(list->cdr);
#line 46
    fprintf(_coverage_fout, "279\n");
#line 46
    fflush(_coverage_fout);
#line 49
    tmp___0 = ListGet((VyCons *)tmp, index___0 - 1);
#line 46
    fprintf(_coverage_fout, "280\n");
#line 46
    fflush(_coverage_fout);
#line 49
    return (tmp___0);
  }
}
}
#line 53 "types/Cons.c"
int ListLen(VyObj list ) 
{ VyObj tmp ;
  int tmp___0 ;
  bool tmp___1 ;

  {
#line 53
  fprintf(_coverage_fout, "286\n");
#line 53
  fflush(_coverage_fout);
#line 55
  tmp___1 = IsNil(list);
#line 53
  fprintf(_coverage_fout, "287\n");
#line 53
  fflush(_coverage_fout);
#line 55
  if (tmp___1) {
#line 55
    fprintf(_coverage_fout, "282\n");
#line 55
    fflush(_coverage_fout);
#line 55
    return (0);
  } else {
#line 55
    fprintf(_coverage_fout, "283\n");
#line 55
    fflush(_coverage_fout);
#line 58
    tmp = Cdr(list);
#line 55
    fprintf(_coverage_fout, "284\n");
#line 55
    fflush(_coverage_fout);
#line 58
    tmp___0 = ListLen(tmp);
#line 55
    fprintf(_coverage_fout, "285\n");
#line 55
    fflush(_coverage_fout);
#line 58
    return (1 + tmp___0);
  }
}
}
#line 1 "String.o"
/* #pragma merger(0,"/tmp/cil-vsU71NsC.i","-Wall,-g") */
#line 130 "/usr/include/string.h"
extern  __attribute__((__nothrow__)) char *strdup(char const   *__s )  __attribute__((__nonnull__(1),
__malloc__)) ;
#line 4 "types/String.c"
VyObj CreateString(char *str ) 
{ VyString *string ;
  void *tmp ;
  VyObj tmp___0 ;

  {
#line 4
  fprintf(_coverage_fout, "288\n");
#line 4
  fflush(_coverage_fout);
#line 5
  tmp = VyMalloc(sizeof(VyString ));
#line 4
  fprintf(_coverage_fout, "289\n");
#line 4
  fflush(_coverage_fout);
#line 5
  string = (VyString *)tmp;
#line 4
  fprintf(_coverage_fout, "290\n");
#line 4
  fflush(_coverage_fout);
#line 9
  string->str = strdup((char const   *)str);
#line 4
  fprintf(_coverage_fout, "291\n");
#line 4
  fflush(_coverage_fout);
#line 10
  tmp___0 = WrapObj((void *)string, TypeString);
#line 4
  fprintf(_coverage_fout, "292\n");
#line 4
  fflush(_coverage_fout);
#line 10
  return (tmp___0);
}
}
#line 1 "Symbol.o"
/* #pragma merger(0,"/tmp/cil-isSWbIAO.i","-Wall,-g") */
#line 42 "/usr/include/glib-2.0/glib/ghash.h"
extern GHashTable *g_hash_table_new(guint (*hash_func)(gconstpointer key ) ,
                                    gboolean (*key_equal_func)(gconstpointer a ,
                                                               gconstpointer b ) ) ;
#line 49
extern void g_hash_table_insert(GHashTable *hash_table , gpointer key ,
                                gpointer value ) ;
#line 61
extern gpointer g_hash_table_lookup(GHashTable *hash_table , gconstpointer key ) ;
#line 83
extern void g_hash_table_unref(GHashTable *hash_table ) ;
#line 96
extern gboolean g_str_equal(gconstpointer v1 , gconstpointer v2 ) ;
#line 98
extern guint g_str_hash(gconstpointer v ) ;
#line 20 "include/Symbol.h"
VySymbol *CreateSymbol_NoObj(char *str ) ;
#line 23
bool SymbolEq(VySymbol *symb , VySymbol *data ) ;
#line 3 "types/Symbol.c"
GHashTable *symbol_hash  =    (GHashTable *)((void *)0);
#line 6 "types/Symbol.c"
VySymbol *CreateSymbol_NoObj(char *str ) 
{ VySymbol *symbol ;
  gpointer tmp ;
  void *tmp___0 ;

  {
#line 6
  fprintf(_coverage_fout, "300\n");
#line 6
  fflush(_coverage_fout);
#line 8
  if (! symbol_hash) {
#line 8
    fprintf(_coverage_fout, "293\n");
#line 8
    fflush(_coverage_fout);
#line 9
    symbol_hash = g_hash_table_new(& g_str_hash, & g_str_equal);
  } else {
#line 8
    fprintf(_coverage_fout, "294\n");
#line 8
    fflush(_coverage_fout);

  }
#line 6
  fprintf(_coverage_fout, "301\n");
#line 6
  fflush(_coverage_fout);
#line 12
  tmp = g_hash_table_lookup(symbol_hash, (void const   *)str);
#line 6
  fprintf(_coverage_fout, "302\n");
#line 6
  fflush(_coverage_fout);
#line 12
  symbol = (VySymbol *)tmp;
#line 6
  fprintf(_coverage_fout, "303\n");
#line 6
  fflush(_coverage_fout);
#line 13
  if (! symbol) {
#line 13
    fprintf(_coverage_fout, "295\n");
#line 13
    fflush(_coverage_fout);
#line 14
    tmp___0 = VyMalloc(sizeof(VySymbol ));
#line 13
    fprintf(_coverage_fout, "296\n");
#line 13
    fflush(_coverage_fout);
#line 14
    symbol = (VySymbol *)tmp___0;
#line 13
    fprintf(_coverage_fout, "297\n");
#line 13
    fflush(_coverage_fout);
#line 17
    symbol->symb = strdup((char const   *)str);
#line 13
    fprintf(_coverage_fout, "298\n");
#line 13
    fflush(_coverage_fout);
#line 18
    g_hash_table_insert(symbol_hash, (void *)str, (void *)symbol);
  } else {
#line 13
    fprintf(_coverage_fout, "299\n");
#line 13
    fflush(_coverage_fout);

  }
#line 6
  fprintf(_coverage_fout, "304\n");
#line 6
  fflush(_coverage_fout);
#line 21
  return (symbol);
}
}
#line 25 "types/Symbol.c"
VyObj CreateSymbol(char *str ) 
{ VySymbol *tmp ;
  VyObj tmp___0 ;

  {
#line 25
  fprintf(_coverage_fout, "305\n");
#line 25
  fflush(_coverage_fout);
#line 26
  tmp = CreateSymbol_NoObj(str);
#line 25
  fprintf(_coverage_fout, "306\n");
#line 25
  fflush(_coverage_fout);
#line 26
  tmp___0 = WrapObj((void *)tmp, TypeSymbol);
#line 25
  fprintf(_coverage_fout, "307\n");
#line 25
  fflush(_coverage_fout);
#line 26
  return (tmp___0);
}
}
#line 30 "types/Symbol.c"
bool SymbolEq(VySymbol *symb , VySymbol *data ) 
{ 

  {
#line 30
  fprintf(_coverage_fout, "308\n");
#line 30
  fflush(_coverage_fout);
#line 31
  return ((unsigned long )symb == (unsigned long )data);
}
}
#line 35 "types/Symbol.c"
void DeleteInternedSymbols(void) 
{ 

  {
#line 35
  fprintf(_coverage_fout, "309\n");
#line 35
  fflush(_coverage_fout);
#line 36
  g_hash_table_unref(symbol_hash);
#line 35
  fprintf(_coverage_fout, "310\n");
#line 35
  fflush(_coverage_fout);
#line 37
  return;
}
}
#line 1 "Function.o"
/* #pragma merger(0,"/tmp/cil-hMZFlkSf.i","-Wall,-g") */
#line 39 "include/Obj.h"
bool ObjEq(VyObj one , VyObj two ) ;
#line 45
VyObj None(void) ;
#line 34 "include/Scope.h"
void VariableBind(VySymbol *symb , VyObj obj ) ;
#line 53 "include/Function.h"
VyFunction *CreateFunction(ArgList args , Bytecode *code ) ;
#line 54
VyFunction *CreateNativeFunction(ArgList args ,
                                 VyObj (*native_code)(VyObj * , int  ) ) ;
#line 57
ArgList ParseArgList(VyObj list ) ;
#line 63
void BindArguments(ArgList arguments , VyObj *values , int num_args ) ;
#line 17 "types/Function.c"
VyFunction *CreateFunction(ArgList args , Bytecode *code ) 
{ VyFunction *func ;
  void *tmp ;

  {
#line 17
  fprintf(_coverage_fout, "311\n");
#line 17
  fflush(_coverage_fout);
#line 18
  tmp = VyMalloc(sizeof(VyFunction ));
#line 17
  fprintf(_coverage_fout, "312\n");
#line 17
  fflush(_coverage_fout);
#line 18
  func = (VyFunction *)tmp;
#line 17
  fprintf(_coverage_fout, "313\n");
#line 17
  fflush(_coverage_fout);
#line 19
  func->arguments = args;
#line 17
  fprintf(_coverage_fout, "314\n");
#line 17
  fflush(_coverage_fout);
#line 20
  func->code.bytecode = code;
#line 17
  fprintf(_coverage_fout, "315\n");
#line 17
  fflush(_coverage_fout);
#line 21
  func->native = 0;
#line 17
  fprintf(_coverage_fout, "316\n");
#line 17
  fflush(_coverage_fout);
#line 22
  return (func);
}
}
#line 26 "types/Function.c"
VyFunction *CreateNativeFunction(ArgList args ,
                                 VyObj (*native_code)(VyObj * , int  ) ) 
{ VyFunction *func ;
  void *tmp ;

  {
#line 26
  fprintf(_coverage_fout, "317\n");
#line 26
  fflush(_coverage_fout);
#line 27
  tmp = VyMalloc(sizeof(VyFunction ));
#line 26
  fprintf(_coverage_fout, "318\n");
#line 26
  fflush(_coverage_fout);
#line 27
  func = (VyFunction *)tmp;
#line 26
  fprintf(_coverage_fout, "319\n");
#line 26
  fflush(_coverage_fout);
#line 28
  func->arguments = args;
#line 26
  fprintf(_coverage_fout, "320\n");
#line 26
  fflush(_coverage_fout);
#line 29
  func->code.native = native_code;
#line 26
  fprintf(_coverage_fout, "321\n");
#line 26
  fflush(_coverage_fout);
#line 30
  func->native = 1;
#line 26
  fprintf(_coverage_fout, "322\n");
#line 26
  fflush(_coverage_fout);
#line 31
  return (func);
}
}
#line 35 "types/Function.c"
Param ParseParam(VyObj param , bool opt , bool rest ) 
{ VyObj default_val ;
  VyObj tmp ;
  VyObj name ;
  void *tmp___0 ;
  VyObj tmp___1 ;
  void *tmp___2 ;
  bool tmp___3 ;
  Param p ;
  void *tmp___4 ;

  {
#line 35
  fprintf(_coverage_fout, "333\n");
#line 35
  fflush(_coverage_fout);
#line 36
  tmp = None();
#line 35
  fprintf(_coverage_fout, "334\n");
#line 35
  fflush(_coverage_fout);
#line 36
  default_val = tmp;
#line 35
  fprintf(_coverage_fout, "335\n");
#line 35
  fflush(_coverage_fout);
#line 37
  if (opt) {
#line 37
    fprintf(_coverage_fout, "330\n");
#line 37
    fflush(_coverage_fout);
#line 38
    tmp___3 = IsType(param, TypeCons);
#line 37
    fprintf(_coverage_fout, "331\n");
#line 37
    fflush(_coverage_fout);
#line 38
    if (tmp___3) {
#line 38
      fprintf(_coverage_fout, "323\n");
#line 38
      fflush(_coverage_fout);
#line 39
      tmp___0 = Obj(param);
#line 38
      fprintf(_coverage_fout, "324\n");
#line 38
      fflush(_coverage_fout);
#line 39
      tmp___1 = ListGet((VyCons *)tmp___0, 0);
#line 38
      fprintf(_coverage_fout, "325\n");
#line 38
      fflush(_coverage_fout);
#line 39
      name = tmp___1;
#line 38
      fprintf(_coverage_fout, "326\n");
#line 38
      fflush(_coverage_fout);
#line 41
      tmp___2 = Obj(param);
#line 38
      fprintf(_coverage_fout, "327\n");
#line 38
      fflush(_coverage_fout);
#line 41
      default_val = ListGet((VyCons *)tmp___2, 0);
#line 38
      fprintf(_coverage_fout, "328\n");
#line 38
      fflush(_coverage_fout);
#line 42
      param = name;
    } else {
#line 38
      fprintf(_coverage_fout, "329\n");
#line 38
      fflush(_coverage_fout);

    }
  } else {
#line 37
    fprintf(_coverage_fout, "332\n");
#line 37
    fflush(_coverage_fout);

  }
#line 35
  fprintf(_coverage_fout, "336\n");
#line 35
  fflush(_coverage_fout);
#line 46
  tmp___4 = Obj(param);
#line 35
  fprintf(_coverage_fout, "337\n");
#line 35
  fflush(_coverage_fout);
#line 46
  p.optional = opt;
#line 35
  fprintf(_coverage_fout, "338\n");
#line 35
  fflush(_coverage_fout);
#line 46
  p.rest = rest;
#line 35
  fprintf(_coverage_fout, "339\n");
#line 35
  fflush(_coverage_fout);
#line 46
  p.name = (VySymbol *)tmp___4;
#line 35
  fprintf(_coverage_fout, "340\n");
#line 35
  fflush(_coverage_fout);
#line 46
  p.default_value = default_val;
#line 35
  fprintf(_coverage_fout, "341\n");
#line 35
  fflush(_coverage_fout);
#line 50
  return (p);
}
}
#line 54 "types/Function.c"
int CountParams(VyObj list ) 
{ int count ;
  VyObj tmp ;
  VyObj tmp___0 ;
  bool tmp___1 ;
  VyObj tmp___2 ;
  VyObj tmp___3 ;
  bool tmp___4 ;
  bool tmp___5 ;

  {
#line 54
  fprintf(_coverage_fout, "357\n");
#line 54
  fflush(_coverage_fout);
#line 55
  count = 0;
#line 54
  fprintf(_coverage_fout, "358\n");
#line 54
  fflush(_coverage_fout);
#line 56
  while (1) {
#line 56
    fprintf(_coverage_fout, "350\n");
#line 56
    fflush(_coverage_fout);
#line 56
    tmp___5 = IsNil(list);
#line 56
    fprintf(_coverage_fout, "351\n");
#line 56
    fflush(_coverage_fout);
#line 56
    if (tmp___5) {
#line 56
      break;
    } else {
#line 56
      fprintf(_coverage_fout, "342\n");
#line 56
      fflush(_coverage_fout);

    }
#line 56
    fprintf(_coverage_fout, "352\n");
#line 56
    fflush(_coverage_fout);
#line 57
    tmp = CreateSymbol((char *)"?");
#line 56
    fprintf(_coverage_fout, "353\n");
#line 56
    fflush(_coverage_fout);
#line 57
    tmp___0 = Car(list);
#line 56
    fprintf(_coverage_fout, "354\n");
#line 56
    fflush(_coverage_fout);
#line 57
    tmp___1 = ObjEq(tmp___0, tmp);
#line 56
    fprintf(_coverage_fout, "355\n");
#line 56
    fflush(_coverage_fout);
#line 57
    if (! tmp___1) {
#line 57
      fprintf(_coverage_fout, "345\n");
#line 57
      fflush(_coverage_fout);
#line 57
      tmp___2 = CreateSymbol((char *)"..");
#line 57
      fprintf(_coverage_fout, "346\n");
#line 57
      fflush(_coverage_fout);
#line 57
      tmp___3 = Car(list);
#line 57
      fprintf(_coverage_fout, "347\n");
#line 57
      fflush(_coverage_fout);
#line 57
      tmp___4 = ObjEq(tmp___3, tmp___2);
#line 57
      fprintf(_coverage_fout, "348\n");
#line 57
      fflush(_coverage_fout);
#line 57
      if (! tmp___4) {
#line 57
        fprintf(_coverage_fout, "343\n");
#line 57
        fflush(_coverage_fout);
#line 59
        count ++;
      } else {
#line 57
        fprintf(_coverage_fout, "344\n");
#line 57
        fflush(_coverage_fout);

      }
    } else {
#line 57
      fprintf(_coverage_fout, "349\n");
#line 57
      fflush(_coverage_fout);

    }
#line 56
    fprintf(_coverage_fout, "356\n");
#line 56
    fflush(_coverage_fout);
#line 61
    list = Cdr(list);
  }
#line 54
  fprintf(_coverage_fout, "359\n");
#line 54
  fflush(_coverage_fout);
#line 64
  return (count);
}
}
#line 68 "types/Function.c"
ArgList ParseArgList(VyObj list ) 
{ int num ;
  int tmp ;
  Param *params ;
  void *tmp___0 ;
  int i ;
  VyObj opt_arg_set ;
  VyObj tmp___1 ;
  VyObj rest_arg_set ;
  VyObj tmp___2 ;
  bool opt ;
  bool rest ;
  int param_num ;
  int arg_list_len ;
  int tmp___3 ;
  VyObj next ;
  void *tmp___4 ;
  VyObj tmp___5 ;
  bool tmp___6 ;
  bool tmp___7 ;
  ArgList args ;

  {
#line 68
  fprintf(_coverage_fout, "375\n");
#line 68
  fflush(_coverage_fout);
#line 69
  tmp = CountParams(list);
#line 68
  fprintf(_coverage_fout, "376\n");
#line 68
  fflush(_coverage_fout);
#line 69
  num = tmp;
#line 68
  fprintf(_coverage_fout, "377\n");
#line 68
  fflush(_coverage_fout);
#line 70
  tmp___0 = VyMalloc(sizeof(Param ) * (unsigned long )num);
#line 68
  fprintf(_coverage_fout, "378\n");
#line 68
  fflush(_coverage_fout);
#line 70
  params = (Param *)tmp___0;
#line 68
  fprintf(_coverage_fout, "379\n");
#line 68
  fflush(_coverage_fout);
#line 76
  tmp___1 = CreateSymbol((char *)"?");
#line 68
  fprintf(_coverage_fout, "380\n");
#line 68
  fflush(_coverage_fout);
#line 76
  opt_arg_set = tmp___1;
#line 68
  fprintf(_coverage_fout, "381\n");
#line 68
  fflush(_coverage_fout);
#line 77
  tmp___2 = CreateSymbol((char *)"..");
#line 68
  fprintf(_coverage_fout, "382\n");
#line 68
  fflush(_coverage_fout);
#line 77
  rest_arg_set = tmp___2;
#line 68
  fprintf(_coverage_fout, "383\n");
#line 68
  fflush(_coverage_fout);
#line 80
  rest = 0;
#line 68
  fprintf(_coverage_fout, "384\n");
#line 68
  fflush(_coverage_fout);
#line 80
  opt = rest;
#line 68
  fprintf(_coverage_fout, "385\n");
#line 68
  fflush(_coverage_fout);
#line 82
  param_num = 0;
#line 68
  fprintf(_coverage_fout, "386\n");
#line 68
  fflush(_coverage_fout);
#line 83
  tmp___3 = ListLen(list);
#line 68
  fprintf(_coverage_fout, "387\n");
#line 68
  fflush(_coverage_fout);
#line 83
  arg_list_len = tmp___3;
#line 68
  fprintf(_coverage_fout, "388\n");
#line 68
  fflush(_coverage_fout);
#line 84
  i = 0;
#line 68
  fprintf(_coverage_fout, "389\n");
#line 68
  fflush(_coverage_fout);
#line 84
  while (1) {
#line 84
    fprintf(_coverage_fout, "368\n");
#line 84
    fflush(_coverage_fout);
#line 84
    if (i < arg_list_len) {
#line 84
      fprintf(_coverage_fout, "360\n");
#line 84
      fflush(_coverage_fout);

    } else {
#line 84
      break;
    }
#line 84
    fprintf(_coverage_fout, "369\n");
#line 84
    fflush(_coverage_fout);
#line 85
    tmp___4 = Obj(list);
#line 84
    fprintf(_coverage_fout, "370\n");
#line 84
    fflush(_coverage_fout);
#line 85
    tmp___5 = ListGet((VyCons *)tmp___4, i);
#line 84
    fprintf(_coverage_fout, "371\n");
#line 84
    fflush(_coverage_fout);
#line 85
    next = tmp___5;
#line 84
    fprintf(_coverage_fout, "372\n");
#line 84
    fflush(_coverage_fout);
#line 87
    tmp___7 = ObjEq(next, opt_arg_set);
#line 84
    fprintf(_coverage_fout, "373\n");
#line 84
    fflush(_coverage_fout);
#line 87
    if (tmp___7) {
#line 87
      fprintf(_coverage_fout, "361\n");
#line 87
      fflush(_coverage_fout);
#line 88
      opt = 1;
    } else {
#line 87
      fprintf(_coverage_fout, "366\n");
#line 87
      fflush(_coverage_fout);
#line 90
      tmp___6 = ObjEq(next, rest_arg_set);
#line 87
      fprintf(_coverage_fout, "367\n");
#line 87
      fflush(_coverage_fout);
#line 90
      if (tmp___6) {
#line 90
        fprintf(_coverage_fout, "362\n");
#line 90
        fflush(_coverage_fout);
#line 91
        rest = 1;
#line 90
        fprintf(_coverage_fout, "363\n");
#line 90
        fflush(_coverage_fout);
#line 91
        opt = rest;
      } else {
#line 90
        fprintf(_coverage_fout, "364\n");
#line 90
        fflush(_coverage_fout);
#line 94
        *(params + param_num) = ParseParam(next, opt, rest);
#line 90
        fprintf(_coverage_fout, "365\n");
#line 90
        fflush(_coverage_fout);
#line 95
        param_num ++;
      }
    }
#line 84
    fprintf(_coverage_fout, "374\n");
#line 84
    fflush(_coverage_fout);
#line 84
    i ++;
  }
#line 68
  fprintf(_coverage_fout, "390\n");
#line 68
  fflush(_coverage_fout);
#line 100
  args.num_params = num;
#line 68
  fprintf(_coverage_fout, "391\n");
#line 68
  fflush(_coverage_fout);
#line 100
  args.params = params;
#line 68
  fprintf(_coverage_fout, "392\n");
#line 68
  fflush(_coverage_fout);
#line 101
  return (args);
}
}
#line 105 "types/Function.c"
void BindArguments(ArgList arguments , VyObj *values , int num_args ) 
{ int i ;
  Param cur ;
  VyObj list ;
  VyObj tmp ;
  int j ;
  Param cur___0 ;
  VyObj tmp___0 ;
  bool tmp___1 ;

  {
#line 105
  fprintf(_coverage_fout, "421\n");
#line 105
  fflush(_coverage_fout);
#line 107
  i = 0;
#line 105
  fprintf(_coverage_fout, "422\n");
#line 105
  fflush(_coverage_fout);
#line 107
  while (1) {
#line 107
    fprintf(_coverage_fout, "405\n");
#line 107
    fflush(_coverage_fout);
#line 107
    if (i < num_args) {
#line 107
      fprintf(_coverage_fout, "393\n");
#line 107
      fflush(_coverage_fout);

    } else {
#line 107
      break;
    }
#line 107
    fprintf(_coverage_fout, "406\n");
#line 107
    fflush(_coverage_fout);
#line 108
    cur = *(arguments.params + i);
#line 107
    fprintf(_coverage_fout, "407\n");
#line 107
    fflush(_coverage_fout);
#line 110
    if (! cur.rest) {
#line 110
      fprintf(_coverage_fout, "394\n");
#line 110
      fflush(_coverage_fout);
#line 111
      VariableBind(cur.name, *(values + i));
    } else {
#line 110
      fprintf(_coverage_fout, "399\n");
#line 110
      fflush(_coverage_fout);
#line 113
      tmp = Nil();
#line 110
      fprintf(_coverage_fout, "400\n");
#line 110
      fflush(_coverage_fout);
#line 113
      list = tmp;
#line 110
      fprintf(_coverage_fout, "401\n");
#line 110
      fflush(_coverage_fout);
#line 115
      j = num_args - 1;
#line 110
      fprintf(_coverage_fout, "402\n");
#line 110
      fflush(_coverage_fout);
#line 115
      while (1) {
#line 115
        fprintf(_coverage_fout, "396\n");
#line 115
        fflush(_coverage_fout);
#line 115
        if (j >= i) {
#line 115
          fprintf(_coverage_fout, "395\n");
#line 115
          fflush(_coverage_fout);

        } else {
#line 115
          break;
        }
#line 115
        fprintf(_coverage_fout, "397\n");
#line 115
        fflush(_coverage_fout);
#line 116
        list = Cons(*(values + j), list);
#line 115
        fprintf(_coverage_fout, "398\n");
#line 115
        fflush(_coverage_fout);
#line 115
        j --;
      }
#line 110
      fprintf(_coverage_fout, "403\n");
#line 110
      fflush(_coverage_fout);
#line 117
      VariableBind(cur.name, list);
#line 110
      fprintf(_coverage_fout, "404\n");
#line 110
      fflush(_coverage_fout);
#line 118
      i = num_args;
    }
#line 107
    fprintf(_coverage_fout, "408\n");
#line 107
    fflush(_coverage_fout);
#line 107
    i ++;
  }
#line 105
  fprintf(_coverage_fout, "423\n");
#line 105
  fflush(_coverage_fout);
#line 123
  while (1) {
#line 123
    fprintf(_coverage_fout, "417\n");
#line 123
    fflush(_coverage_fout);
#line 123
    if (i < arguments.num_params) {
#line 123
      fprintf(_coverage_fout, "409\n");
#line 123
      fflush(_coverage_fout);

    } else {
#line 123
      break;
    }
#line 123
    fprintf(_coverage_fout, "418\n");
#line 123
    fflush(_coverage_fout);
#line 124
    cur___0 = *(arguments.params + i);
#line 123
    fprintf(_coverage_fout, "419\n");
#line 123
    fflush(_coverage_fout);
#line 125
    if (cur___0.rest) {
#line 125
      fprintf(_coverage_fout, "410\n");
#line 125
      fflush(_coverage_fout);
#line 126
      tmp___0 = Nil();
#line 125
      fprintf(_coverage_fout, "411\n");
#line 125
      fflush(_coverage_fout);
#line 126
      VariableBind(cur___0.name, tmp___0);
    } else {
#line 125
      fprintf(_coverage_fout, "414\n");
#line 125
      fflush(_coverage_fout);
#line 128
      tmp___1 = IsNone(cur___0.default_value);
#line 125
      fprintf(_coverage_fout, "415\n");
#line 125
      fflush(_coverage_fout);
#line 128
      if (tmp___1) {
#line 128
        fprintf(_coverage_fout, "412\n");
#line 128
        fflush(_coverage_fout);
#line 129
        cur___0.default_value = Nil();
      } else {
#line 128
        fprintf(_coverage_fout, "413\n");
#line 128
        fflush(_coverage_fout);

      }
#line 125
      fprintf(_coverage_fout, "416\n");
#line 125
      fflush(_coverage_fout);
#line 130
      VariableBind(cur___0.name, cur___0.default_value);
    }
#line 123
    fprintf(_coverage_fout, "420\n");
#line 123
    fflush(_coverage_fout);
#line 123
    i ++;
  }
#line 105
  fprintf(_coverage_fout, "424\n");
#line 105
  fflush(_coverage_fout);
#line 133
  return;
}
}
#line 1 "Obj.o"
/* #pragma merger(0,"/tmp/cil-SE3r1GGu.i","-Wall,-g") */
#line 25 "include/Obj.h"
VyType Type(VyObj object ) ;
#line 33
VyType CreateType(int size , struct _VySymbol *type_name ) ;
#line 5 "types/Obj.c"
VyObj WrapObj(void *data , VyType type ) 
{ VyObj object ;

  {
#line 5
  fprintf(_coverage_fout, "425\n");
#line 5
  fflush(_coverage_fout);
#line 6
  object.type = type;
#line 5
  fprintf(_coverage_fout, "426\n");
#line 5
  fflush(_coverage_fout);
#line 6
  object.obj = data;
#line 5
  fprintf(_coverage_fout, "427\n");
#line 5
  fflush(_coverage_fout);
#line 7
  return (object);
}
}
#line 11 "types/Obj.c"
void *Obj(VyObj object ) 
{ 

  {
#line 11
  fprintf(_coverage_fout, "428\n");
#line 11
  fflush(_coverage_fout);
#line 12
  return (object.obj);
}
}
#line 14 "types/Obj.c"
VyType Type(VyObj object ) 
{ 

  {
#line 14
  fprintf(_coverage_fout, "429\n");
#line 14
  fflush(_coverage_fout);
#line 15
  return (object.type);
}
}
#line 18 "types/Obj.c"
bool IsType(VyObj obj , VyType type ) 
{ VyType tmp ;

  {
#line 18
  fprintf(_coverage_fout, "430\n");
#line 18
  fflush(_coverage_fout);
#line 19
  tmp = Type(obj);
#line 18
  fprintf(_coverage_fout, "431\n");
#line 18
  fflush(_coverage_fout);
#line 19
  return ((unsigned long )tmp.name == (unsigned long )type.name);
}
}
#line 23 "types/Obj.c"
VyType CreateType(int size , struct _VySymbol *type_name ) 
{ VyType new_type ;

  {
#line 23
  fprintf(_coverage_fout, "432\n");
#line 23
  fflush(_coverage_fout);
#line 24
  new_type.size = size;
#line 23
  fprintf(_coverage_fout, "433\n");
#line 23
  fflush(_coverage_fout);
#line 24
  new_type.name = type_name;
#line 23
  fprintf(_coverage_fout, "434\n");
#line 23
  fflush(_coverage_fout);
#line 25
  return (new_type);
}
}
#line 29 "types/Obj.c"
bool ObjEq(VyObj one , VyObj two ) 
{ void *tmp ;
  void *tmp___0 ;
  VyType tmp___1 ;
  VyType tmp___2 ;
  bool tmp___3 ;
  int tmp___4 ;

  {
#line 29
  fprintf(_coverage_fout, "442\n");
#line 29
  fflush(_coverage_fout);
#line 30
  tmp = Obj(one);
#line 29
  fprintf(_coverage_fout, "443\n");
#line 29
  fflush(_coverage_fout);
#line 30
  tmp___0 = Obj(two);
#line 29
  fprintf(_coverage_fout, "444\n");
#line 29
  fflush(_coverage_fout);
#line 30
  if ((unsigned long )tmp == (unsigned long )tmp___0) {
#line 30
    fprintf(_coverage_fout, "437\n");
#line 30
    fflush(_coverage_fout);
#line 30
    tmp___1 = Type(two);
#line 30
    fprintf(_coverage_fout, "438\n");
#line 30
    fflush(_coverage_fout);
#line 30
    tmp___2 = Type(one);
#line 30
    fprintf(_coverage_fout, "439\n");
#line 30
    fflush(_coverage_fout);
#line 30
    tmp___3 = SymbolEq(tmp___2.name, tmp___1.name);
#line 30
    fprintf(_coverage_fout, "440\n");
#line 30
    fflush(_coverage_fout);
#line 30
    if (tmp___3) {
#line 30
      fprintf(_coverage_fout, "435\n");
#line 30
      fflush(_coverage_fout);
#line 30
      tmp___4 = 1;
    } else {
#line 30
      fprintf(_coverage_fout, "436\n");
#line 30
      fflush(_coverage_fout);
#line 30
      tmp___4 = 0;
    }
  } else {
#line 30
    fprintf(_coverage_fout, "441\n");
#line 30
    fflush(_coverage_fout);
#line 30
    tmp___4 = 0;
  }
#line 29
  fprintf(_coverage_fout, "445\n");
#line 29
  fflush(_coverage_fout);
#line 30
  return (tmp___4);
}
}
#line 35 "types/Obj.c"
VyObj None(void) 
{ VyObj object ;

  {
#line 35
  fprintf(_coverage_fout, "446\n");
#line 35
  fflush(_coverage_fout);
#line 36
  object.type = TypeNone;
#line 35
  fprintf(_coverage_fout, "447\n");
#line 35
  fflush(_coverage_fout);
#line 36
  object.obj = (void *)0;
#line 35
  fprintf(_coverage_fout, "448\n");
#line 35
  fflush(_coverage_fout);
#line 37
  return (object);
}
}
#line 40 "types/Obj.c"
bool IsNone(VyObj obj ) 
{ bool tmp ;

  {
#line 40
  fprintf(_coverage_fout, "449\n");
#line 40
  fflush(_coverage_fout);
#line 41
  tmp = IsType(obj, TypeNone);
#line 40
  fprintf(_coverage_fout, "450\n");
#line 40
  fflush(_coverage_fout);
#line 41
  return (tmp);
}
}
#line 48 "types/Obj.c"
void PrintCons(FILE *file , VyCons *cons ) 
{ VyObj cdr ;
  void *tmp ;
  bool tmp___0 ;
  bool tmp___1 ;
  VyObj tmp___2 ;
  bool tmp___3 ;

  {
#line 48
  fprintf(_coverage_fout, "465\n");
#line 48
  fflush(_coverage_fout);
#line 49
  fprintf((FILE */* __restrict  */)file, (char const   */* __restrict  */)"(");
#line 48
  fprintf(_coverage_fout, "466\n");
#line 48
  fflush(_coverage_fout);
#line 52
  while (1) {
#line 52
    fprintf(_coverage_fout, "458\n");
#line 52
    fflush(_coverage_fout);
#line 52
    tmp___2 = WrapObj((void *)cons, TypeCons);
#line 52
    fprintf(_coverage_fout, "459\n");
#line 52
    fflush(_coverage_fout);
#line 52
    tmp___3 = IsNil(tmp___2);
#line 52
    fprintf(_coverage_fout, "460\n");
#line 52
    fflush(_coverage_fout);
#line 52
    if (tmp___3) {
#line 52
      break;
    } else {
#line 52
      fprintf(_coverage_fout, "451\n");
#line 52
      fflush(_coverage_fout);

    }
#line 52
    fprintf(_coverage_fout, "461\n");
#line 52
    fflush(_coverage_fout);
#line 53
    PrintObj(file, cons->car);
#line 52
    fprintf(_coverage_fout, "462\n");
#line 52
    fflush(_coverage_fout);
#line 54
    cdr = cons->cdr;
#line 52
    fprintf(_coverage_fout, "463\n");
#line 52
    fflush(_coverage_fout);
#line 57
    tmp___1 = IsType(cdr, TypeCons);
#line 52
    fprintf(_coverage_fout, "464\n");
#line 52
    fflush(_coverage_fout);
#line 57
    if (tmp___1) {
#line 57
      fprintf(_coverage_fout, "452\n");
#line 57
      fflush(_coverage_fout);
#line 58
      tmp = Obj(cdr);
#line 57
      fprintf(_coverage_fout, "453\n");
#line 57
      fflush(_coverage_fout);
#line 58
      cons = (VyCons *)tmp;
    } else {
#line 57
      fprintf(_coverage_fout, "456\n");
#line 57
      fflush(_coverage_fout);
#line 61
      tmp___0 = IsNil(cdr);
#line 57
      fprintf(_coverage_fout, "457\n");
#line 57
      fflush(_coverage_fout);
#line 61
      if (tmp___0) {
#line 62
        break;
      } else {
#line 61
        fprintf(_coverage_fout, "454\n");
#line 61
        fflush(_coverage_fout);
#line 66
        fprintf((FILE */* __restrict  */)file,
                (char const   */* __restrict  */)". ");
#line 61
        fprintf(_coverage_fout, "455\n");
#line 61
        fflush(_coverage_fout);
#line 67
        PrintObj(file, cdr);
#line 68
        break;
      }
    }
  }
#line 48
  fprintf(_coverage_fout, "467\n");
#line 48
  fflush(_coverage_fout);
#line 71
  fprintf((FILE */* __restrict  */)file, (char const   */* __restrict  */)"\b) ");
#line 48
  fprintf(_coverage_fout, "468\n");
#line 48
  fflush(_coverage_fout);
#line 72
  return;
}
}
#line 75 "types/Obj.c"
void PrintObj(FILE *file , VyObj obj ) 
{ void *tmp ;
  void *tmp___0 ;
  void *tmp___1 ;
  void *tmp___2 ;
  void *tmp___3 ;
  void *tmp___4 ;
  bool tmp___5 ;
  bool tmp___6 ;
  bool tmp___7 ;
  bool tmp___8 ;
  bool tmp___9 ;
  bool tmp___10 ;
  bool tmp___11 ;

  {
#line 75
  fprintf(_coverage_fout, "497\n");
#line 75
  fflush(_coverage_fout);
#line 77
  tmp___11 = IsType(obj, TypeString);
#line 75
  fprintf(_coverage_fout, "498\n");
#line 75
  fflush(_coverage_fout);
#line 77
  if (tmp___11) {
#line 77
    fprintf(_coverage_fout, "469\n");
#line 77
    fflush(_coverage_fout);
#line 78
    tmp = Obj(obj);
#line 77
    fprintf(_coverage_fout, "470\n");
#line 77
    fflush(_coverage_fout);
#line 78
    fprintf((FILE */* __restrict  */)file,
            (char const   */* __restrict  */)"\"%s\" ", ((VyString *)tmp)->str);
  } else {
#line 77
    fprintf(_coverage_fout, "495\n");
#line 77
    fflush(_coverage_fout);
#line 79
    tmp___10 = IsType(obj, TypeSymbol);
#line 77
    fprintf(_coverage_fout, "496\n");
#line 77
    fflush(_coverage_fout);
#line 79
    if (tmp___10) {
#line 79
      fprintf(_coverage_fout, "471\n");
#line 79
      fflush(_coverage_fout);
#line 80
      tmp___0 = Obj(obj);
#line 79
      fprintf(_coverage_fout, "472\n");
#line 79
      fflush(_coverage_fout);
#line 80
      fprintf((FILE */* __restrict  */)file,
              (char const   */* __restrict  */)"%s ",
              ((VySymbol *)tmp___0)->symb);
    } else {
#line 79
      fprintf(_coverage_fout, "493\n");
#line 79
      fflush(_coverage_fout);
#line 83
      tmp___9 = IsType(obj, TypeInt);
#line 79
      fprintf(_coverage_fout, "494\n");
#line 79
      fflush(_coverage_fout);
#line 83
      if (tmp___9) {
#line 83
        fprintf(_coverage_fout, "473\n");
#line 83
        fflush(_coverage_fout);
#line 84
        tmp___1 = Obj(obj);
#line 83
        fprintf(_coverage_fout, "474\n");
#line 83
        fflush(_coverage_fout);
#line 84
        fprintf((FILE */* __restrict  */)file,
                (char const   */* __restrict  */)"%d ", ((VyInt *)tmp___1)->val);
      } else {
#line 83
        fprintf(_coverage_fout, "491\n");
#line 83
        fflush(_coverage_fout);
#line 85
        tmp___8 = IsType(obj, TypeFloat);
#line 83
        fprintf(_coverage_fout, "492\n");
#line 83
        fflush(_coverage_fout);
#line 85
        if (tmp___8) {
#line 85
          fprintf(_coverage_fout, "475\n");
#line 85
          fflush(_coverage_fout);
#line 86
          tmp___2 = Obj(obj);
#line 85
          fprintf(_coverage_fout, "476\n");
#line 85
          fflush(_coverage_fout);
#line 86
          fprintf((FILE */* __restrict  */)file,
                  (char const   */* __restrict  */)"%f ",
                  ((VyFloat *)tmp___2)->val);
        } else {
#line 85
          fprintf(_coverage_fout, "489\n");
#line 85
          fflush(_coverage_fout);
#line 89
          tmp___7 = IsType(obj, TypeCons);
#line 85
          fprintf(_coverage_fout, "490\n");
#line 85
          fflush(_coverage_fout);
#line 89
          if (tmp___7) {
#line 89
            fprintf(_coverage_fout, "477\n");
#line 89
            fflush(_coverage_fout);
#line 90
            tmp___3 = Obj(obj);
#line 89
            fprintf(_coverage_fout, "478\n");
#line 89
            fflush(_coverage_fout);
#line 90
            PrintCons(file, (VyCons *)tmp___3);
          } else {
#line 89
            fprintf(_coverage_fout, "487\n");
#line 89
            fflush(_coverage_fout);
#line 93
            tmp___6 = IsType(obj, TypeFunction);
#line 89
            fprintf(_coverage_fout, "488\n");
#line 89
            fflush(_coverage_fout);
#line 93
            if (tmp___6) {
#line 93
              fprintf(_coverage_fout, "479\n");
#line 93
              fflush(_coverage_fout);
#line 94
              tmp___4 = Obj(obj);
#line 93
              fprintf(_coverage_fout, "480\n");
#line 93
              fflush(_coverage_fout);
#line 94
              fprintf((FILE */* __restrict  */)file,
                      (char const   */* __restrict  */)"Unnamed function lambda at (%p)",
                      tmp___4);
            } else {
#line 93
              fprintf(_coverage_fout, "485\n");
#line 93
              fflush(_coverage_fout);
#line 95
              tmp___5 = IsType(obj, TypeNone);
#line 93
              fprintf(_coverage_fout, "486\n");
#line 93
              fflush(_coverage_fout);
#line 95
              if (tmp___5) {
#line 95
                fprintf(_coverage_fout, "481\n");
#line 95
                fflush(_coverage_fout);
#line 96
                fprintf((FILE */* __restrict  */)file,
                        (char const   */* __restrict  */)"Obj-type-NONE. Error!");
#line 95
                fprintf(_coverage_fout, "482\n");
#line 95
                fflush(_coverage_fout);
#line 97
                exit(0);
              } else {
#line 95
                fprintf(_coverage_fout, "483\n");
#line 95
                fflush(_coverage_fout);
#line 100
                printf((char const   */* __restrict  */)"Unknown object type.\n");
#line 95
                fprintf(_coverage_fout, "484\n");
#line 95
                fflush(_coverage_fout);
#line 101
                exit(0);
              }
            }
          }
        }
      }
    }
  }
#line 75
  fprintf(_coverage_fout, "499\n");
#line 75
  fflush(_coverage_fout);
#line 103
  return;
}
}
#line 1 "Eval.o"
/* #pragma merger(0,"/tmp/cil-fV9QVGX3.i","-Wall,-g") */
#line 44 "include/VM.h"
Bytecode *Compile(VyObj compileObj ) ;
#line 47
void FreeBytecode(Bytecode *bytecode ) ;
#line 50
VyObj EvalBytecode(Bytecode *bytecode ) ;
#line 60
void StackPush(VyObj obj ) ;
#line 61
VyObj StackPop(void) ;
#line 62
VyObj StackPeek(void) ;
#line 21 "include/Scope.h"
Scope *CurrentScope(void) ;
#line 24
Scope *CreateScope(Scope *parent ) ;
#line 27
void EnterScope(Scope *scope ) ;
#line 30
void DeleteScope(Scope *scope ) ;
#line 33
VyObj VariableValue(VySymbol *symb ) ;
#line 23 "include/External.h"
bool IsTrue(VyObj x ) ;
#line 4 "vm/Eval.c"
VyObj Eval(VyObj sexp ) 
{ Bytecode *bytecode ;
  Bytecode *tmp ;
  VyObj result ;
  VyObj tmp___0 ;

  {
#line 4
  fprintf(_coverage_fout, "500\n");
#line 4
  fflush(_coverage_fout);
#line 5
  tmp = Compile(sexp);
#line 4
  fprintf(_coverage_fout, "501\n");
#line 4
  fflush(_coverage_fout);
#line 5
  bytecode = tmp;
#line 4
  fprintf(_coverage_fout, "502\n");
#line 4
  fflush(_coverage_fout);
#line 6
  tmp___0 = EvalBytecode(bytecode);
#line 4
  fprintf(_coverage_fout, "503\n");
#line 4
  fflush(_coverage_fout);
#line 6
  result = tmp___0;
#line 4
  fprintf(_coverage_fout, "504\n");
#line 4
  fflush(_coverage_fout);
#line 7
  FreeBytecode(bytecode);
#line 4
  fprintf(_coverage_fout, "505\n");
#line 4
  fflush(_coverage_fout);
#line 8
  return (result);
}
}
#line 12
void PushInstr(VyObj obj ) ;
#line 13
void PopInstr(void) ;
#line 14
void BindInstr(void) ;
#line 15
void ValueInstr(VyObj obj ) ;
#line 16
void FuncInstr(void) ;
#line 17
void CallInstr(int num_args ) ;
#line 18
int IfJmpInstr(int ifFalse ) ;
#line 21 "vm/Eval.c"
VyObj EvalBytecode(Bytecode *bytecode ) 
{ int nextInstr ;
  int i ;
  Instruction instr ;
  VyObj tmp ;

  {
#line 21
  fprintf(_coverage_fout, "526\n");
#line 21
  fflush(_coverage_fout);
#line 25
  nextInstr = -1;
#line 21
  fprintf(_coverage_fout, "527\n");
#line 21
  fflush(_coverage_fout);
#line 28
  i = 0;
#line 21
  fprintf(_coverage_fout, "528\n");
#line 21
  fflush(_coverage_fout);
#line 28
  while (1) {
#line 28
    fprintf(_coverage_fout, "521\n");
#line 28
    fflush(_coverage_fout);
#line 28
    if (i < bytecode->used) {
#line 28
      fprintf(_coverage_fout, "506\n");
#line 28
      fflush(_coverage_fout);

    } else {
#line 28
      break;
    }
#line 28
    fprintf(_coverage_fout, "522\n");
#line 28
    fflush(_coverage_fout);
#line 31
    if (nextInstr >= 0) {
#line 31
      fprintf(_coverage_fout, "507\n");
#line 31
      fflush(_coverage_fout);
#line 32
      i = nextInstr;
#line 31
      fprintf(_coverage_fout, "508\n");
#line 31
      fflush(_coverage_fout);
#line 33
      nextInstr = -1;
    } else {
#line 31
      fprintf(_coverage_fout, "509\n");
#line 31
      fflush(_coverage_fout);

    }
#line 28
    fprintf(_coverage_fout, "523\n");
#line 28
    fflush(_coverage_fout);
#line 37
    if (i >= bytecode->used) {
#line 38
      break;
    } else {
#line 37
      fprintf(_coverage_fout, "510\n");
#line 37
      fflush(_coverage_fout);

    }
#line 28
    fprintf(_coverage_fout, "524\n");
#line 28
    fflush(_coverage_fout);
#line 41
    instr = *(bytecode->instructions + i);
#line 52
    switch (instr.opcode) {
#line 52
    fprintf(_coverage_fout, "511\n");
#line 52
    fflush(_coverage_fout);
    case 20: 
#line 55
    PushInstr(instr.data.obj);
#line 56
    break;
#line 52
    fprintf(_coverage_fout, "512\n");
#line 52
    fflush(_coverage_fout);
    case 10: 
#line 58
    PopInstr();
#line 59
    break;
#line 52
    fprintf(_coverage_fout, "513\n");
#line 52
    fflush(_coverage_fout);
    case 70: 
#line 61
    BindInstr();
#line 62
    break;
#line 52
    fprintf(_coverage_fout, "514\n");
#line 52
    fflush(_coverage_fout);
    case 60: 
#line 65
    ValueInstr(instr.data.obj);
#line 66
    break;
#line 52
    fprintf(_coverage_fout, "515\n");
#line 52
    fflush(_coverage_fout);
    case 80: 
#line 68
    FuncInstr();
#line 69
    break;
#line 52
    fprintf(_coverage_fout, "516\n");
#line 52
    fflush(_coverage_fout);
    case 30: 
#line 72
    CallInstr(instr.data.num);
#line 73
    break;
#line 52
    fprintf(_coverage_fout, "517\n");
#line 52
    fflush(_coverage_fout);
    case 50: 
#line 77
    nextInstr = instr.data.num;
#line 78
    break;
#line 52
    fprintf(_coverage_fout, "518\n");
#line 52
    fflush(_coverage_fout);
    case 40: 
#line 80
    nextInstr = IfJmpInstr(instr.data.num);
#line 81
    break;
#line 52
    fprintf(_coverage_fout, "519\n");
#line 52
    fflush(_coverage_fout);
    default: 
#line 85
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"Unknown opcode type (%d), I\'m confused.\n",
            instr.opcode);
#line 52
    fprintf(_coverage_fout, "520\n");
#line 52
    fflush(_coverage_fout);
#line 86
    exit(0);
    }
#line 28
    fprintf(_coverage_fout, "525\n");
#line 28
    fflush(_coverage_fout);
#line 28
    i ++;
  }
#line 21
  fprintf(_coverage_fout, "529\n");
#line 21
  fflush(_coverage_fout);
#line 91
  tmp = StackPop();
#line 21
  fprintf(_coverage_fout, "530\n");
#line 21
  fflush(_coverage_fout);
#line 91
  return (tmp);
}
}
#line 95 "vm/Eval.c"
void PushInstr(VyObj obj ) 
{ 

  {
#line 95
  fprintf(_coverage_fout, "531\n");
#line 95
  fflush(_coverage_fout);
#line 96
  StackPush(obj);
#line 95
  fprintf(_coverage_fout, "532\n");
#line 95
  fflush(_coverage_fout);
#line 97
  return;
}
}
#line 98 "vm/Eval.c"
void PopInstr(void) 
{ 

  {
#line 98
  fprintf(_coverage_fout, "533\n");
#line 98
  fflush(_coverage_fout);
#line 99
  StackPop();
#line 98
  fprintf(_coverage_fout, "534\n");
#line 98
  fflush(_coverage_fout);
#line 100
  return;
}
}
#line 102 "vm/Eval.c"
void BindInstr(void) 
{ VyObj name_obj ;
  VyObj tmp ;
  VyObj val ;
  VyObj tmp___0 ;
  VySymbol *name ;
  void *tmp___1 ;

  {
#line 102
  fprintf(_coverage_fout, "535\n");
#line 102
  fflush(_coverage_fout);
#line 103
  tmp = StackPop();
#line 102
  fprintf(_coverage_fout, "536\n");
#line 102
  fflush(_coverage_fout);
#line 103
  name_obj = tmp;
#line 102
  fprintf(_coverage_fout, "537\n");
#line 102
  fflush(_coverage_fout);
#line 104
  tmp___0 = StackPeek();
#line 102
  fprintf(_coverage_fout, "538\n");
#line 102
  fflush(_coverage_fout);
#line 104
  val = tmp___0;
#line 102
  fprintf(_coverage_fout, "539\n");
#line 102
  fflush(_coverage_fout);
#line 105
  tmp___1 = Obj(name_obj);
#line 102
  fprintf(_coverage_fout, "540\n");
#line 102
  fflush(_coverage_fout);
#line 105
  name = (VySymbol *)tmp___1;
#line 102
  fprintf(_coverage_fout, "541\n");
#line 102
  fflush(_coverage_fout);
#line 106
  VariableBind(name, val);
#line 102
  fprintf(_coverage_fout, "542\n");
#line 102
  fflush(_coverage_fout);
#line 107
  return;
}
}
#line 108 "vm/Eval.c"
void ValueInstr(VyObj obj ) 
{ VySymbol *name ;
  void *tmp ;
  VyObj val ;
  VyObj tmp___0 ;

  {
#line 108
  fprintf(_coverage_fout, "543\n");
#line 108
  fflush(_coverage_fout);
#line 109
  tmp = Obj(obj);
#line 108
  fprintf(_coverage_fout, "544\n");
#line 108
  fflush(_coverage_fout);
#line 109
  name = (VySymbol *)tmp;
#line 108
  fprintf(_coverage_fout, "545\n");
#line 108
  fflush(_coverage_fout);
#line 110
  tmp___0 = VariableValue(name);
#line 108
  fprintf(_coverage_fout, "546\n");
#line 108
  fflush(_coverage_fout);
#line 110
  val = tmp___0;
#line 108
  fprintf(_coverage_fout, "547\n");
#line 108
  fflush(_coverage_fout);
#line 111
  StackPush(val);
#line 108
  fprintf(_coverage_fout, "548\n");
#line 108
  fflush(_coverage_fout);
#line 112
  return;
}
}
#line 113 "vm/Eval.c"
void FuncInstr(void) 
{ VyObj func_obj ;
  VyObj tmp ;
  VyFunction *func ;
  void *tmp___0 ;

  {
#line 113
  fprintf(_coverage_fout, "549\n");
#line 113
  fflush(_coverage_fout);
#line 114
  tmp = StackPeek();
#line 113
  fprintf(_coverage_fout, "550\n");
#line 113
  fflush(_coverage_fout);
#line 114
  func_obj = tmp;
#line 113
  fprintf(_coverage_fout, "551\n");
#line 113
  fflush(_coverage_fout);
#line 115
  tmp___0 = Obj(func_obj);
#line 113
  fprintf(_coverage_fout, "552\n");
#line 113
  fflush(_coverage_fout);
#line 115
  func = (VyFunction *)tmp___0;
#line 113
  fprintf(_coverage_fout, "553\n");
#line 113
  fflush(_coverage_fout);
#line 116
  func->creation_scope = CurrentScope();
#line 113
  fprintf(_coverage_fout, "554\n");
#line 113
  fflush(_coverage_fout);
#line 117
  return;
}
}
#line 118 "vm/Eval.c"
int IfJmpInstr(int ifFalse ) 
{ VyObj condition_value ;
  VyObj tmp ;
  bool tmp___0 ;

  {
#line 118
  fprintf(_coverage_fout, "557\n");
#line 118
  fflush(_coverage_fout);
#line 119
  tmp = StackPop();
#line 118
  fprintf(_coverage_fout, "558\n");
#line 118
  fflush(_coverage_fout);
#line 119
  condition_value = tmp;
#line 118
  fprintf(_coverage_fout, "559\n");
#line 118
  fflush(_coverage_fout);
#line 121
  tmp___0 = IsTrue(condition_value);
#line 118
  fprintf(_coverage_fout, "560\n");
#line 118
  fflush(_coverage_fout);
#line 121
  if (tmp___0) {
#line 121
    fprintf(_coverage_fout, "555\n");
#line 121
    fflush(_coverage_fout);
#line 122
    return (-1);
  } else {
#line 121
    fprintf(_coverage_fout, "556\n");
#line 121
    fflush(_coverage_fout);
#line 125
    return (ifFalse);
  }
}
}
#line 128 "vm/Eval.c"
void CallInstr(int num_args ) 
{ VyObj func_obj ;
  VyObj tmp ;
  VyFunction *func ;
  void *tmp___0 ;
  VyObj *arguments ;
  unsigned long __lengthofarguments ;
  void *tmp___1 ;
  int i ;
  Scope *caller_scope ;
  Scope *tmp___2 ;
  Scope *new_scope ;
  Scope *tmp___3 ;
  VyObj return_val ;
  VyObj tmp___4 ;
  VyObj return_val___0 ;
  VyObj tmp___5 ;

  {
#line 128
  fprintf(_coverage_fout, "579\n");
#line 128
  fflush(_coverage_fout);
#line 129
  tmp = StackPop();
#line 128
  fprintf(_coverage_fout, "580\n");
#line 128
  fflush(_coverage_fout);
#line 129
  func_obj = tmp;
#line 128
  fprintf(_coverage_fout, "581\n");
#line 128
  fflush(_coverage_fout);
#line 130
  tmp___0 = Obj(func_obj);
#line 128
  fprintf(_coverage_fout, "582\n");
#line 128
  fflush(_coverage_fout);
#line 130
  func = (VyFunction *)tmp___0;
#line 128
  fprintf(_coverage_fout, "583\n");
#line 128
  fflush(_coverage_fout);
#line 132
  __lengthofarguments = (unsigned long )num_args;
#line 128
  fprintf(_coverage_fout, "584\n");
#line 128
  fflush(_coverage_fout);
#line 132
  tmp___1 = __builtin_alloca(sizeof(*arguments) * __lengthofarguments);
#line 128
  fprintf(_coverage_fout, "585\n");
#line 128
  fflush(_coverage_fout);
#line 132
  arguments = (VyObj *)tmp___1;
#line 128
  fprintf(_coverage_fout, "586\n");
#line 128
  fflush(_coverage_fout);
#line 134
  i = 0;
#line 128
  fprintf(_coverage_fout, "587\n");
#line 128
  fflush(_coverage_fout);
#line 134
  while (1) {
#line 134
    fprintf(_coverage_fout, "562\n");
#line 134
    fflush(_coverage_fout);
#line 134
    if (i < num_args) {
#line 134
      fprintf(_coverage_fout, "561\n");
#line 134
      fflush(_coverage_fout);

    } else {
#line 134
      break;
    }
#line 134
    fprintf(_coverage_fout, "563\n");
#line 134
    fflush(_coverage_fout);
#line 135
    *(arguments + i) = StackPop();
#line 134
    fprintf(_coverage_fout, "564\n");
#line 134
    fflush(_coverage_fout);
#line 134
    i ++;
  }
#line 128
  fprintf(_coverage_fout, "588\n");
#line 128
  fflush(_coverage_fout);
#line 150
  if (! func->native) {
#line 150
    fprintf(_coverage_fout, "565\n");
#line 150
    fflush(_coverage_fout);
#line 151
    tmp___2 = CurrentScope();
#line 150
    fprintf(_coverage_fout, "566\n");
#line 150
    fflush(_coverage_fout);
#line 151
    caller_scope = tmp___2;
#line 150
    fprintf(_coverage_fout, "567\n");
#line 150
    fflush(_coverage_fout);
#line 152
    tmp___3 = CreateScope(func->creation_scope);
#line 150
    fprintf(_coverage_fout, "568\n");
#line 150
    fflush(_coverage_fout);
#line 152
    new_scope = tmp___3;
#line 150
    fprintf(_coverage_fout, "569\n");
#line 150
    fflush(_coverage_fout);
#line 153
    EnterScope(new_scope);
#line 150
    fprintf(_coverage_fout, "570\n");
#line 150
    fflush(_coverage_fout);
#line 155
    BindArguments(func->arguments, arguments, num_args);
#line 150
    fprintf(_coverage_fout, "571\n");
#line 150
    fflush(_coverage_fout);
#line 157
    tmp___4 = EvalBytecode(func->code.bytecode);
#line 150
    fprintf(_coverage_fout, "572\n");
#line 150
    fflush(_coverage_fout);
#line 157
    return_val = tmp___4;
#line 150
    fprintf(_coverage_fout, "573\n");
#line 150
    fflush(_coverage_fout);
#line 158
    DeleteScope(new_scope);
#line 150
    fprintf(_coverage_fout, "574\n");
#line 150
    fflush(_coverage_fout);
#line 159
    EnterScope(caller_scope);
#line 150
    fprintf(_coverage_fout, "575\n");
#line 150
    fflush(_coverage_fout);
#line 160
    StackPush(return_val);
  } else {
#line 150
    fprintf(_coverage_fout, "576\n");
#line 150
    fflush(_coverage_fout);
#line 162
    tmp___5 = (*(func->code.native))(arguments + 0, num_args);
#line 150
    fprintf(_coverage_fout, "577\n");
#line 150
    fflush(_coverage_fout);
#line 162
    return_val___0 = tmp___5;
#line 150
    fprintf(_coverage_fout, "578\n");
#line 150
    fflush(_coverage_fout);
#line 163
    StackPush(return_val___0);
  }
#line 128
  fprintf(_coverage_fout, "589\n");
#line 128
  fflush(_coverage_fout);
#line 165
  return;
}
}
#line 1 "Stack.o"
/* #pragma merger(0,"/tmp/cil-VGY3wYyw.i","-Wall,-g") */
#line 4 "vm/Stack.c"
VyObj *stack  =    (VyObj *)((void *)0);
#line 7 "vm/Stack.c"
int stack_size  =    100;
#line 8 "vm/Stack.c"
int stack_index  =    0;
#line 11 "vm/Stack.c"
void InitStack(void) 
{ void *tmp ;

  {
#line 11
  fprintf(_coverage_fout, "590\n");
#line 11
  fflush(_coverage_fout);
#line 12
  tmp = VyMalloc(sizeof(VyObj ) * (unsigned long )stack_size);
#line 11
  fprintf(_coverage_fout, "591\n");
#line 11
  fflush(_coverage_fout);
#line 12
  stack = (VyObj *)tmp;
#line 11
  fprintf(_coverage_fout, "592\n");
#line 11
  fflush(_coverage_fout);
#line 13
  stack_index = 0;
#line 11
  fprintf(_coverage_fout, "593\n");
#line 11
  fflush(_coverage_fout);
#line 14
  return;
}
}
#line 17 "vm/Stack.c"
void ExpandStack(void) 
{ void *tmp ;

  {
#line 17
  fprintf(_coverage_fout, "594\n");
#line 17
  fflush(_coverage_fout);
#line 18
  stack_size *= 2;
#line 17
  fprintf(_coverage_fout, "595\n");
#line 17
  fflush(_coverage_fout);
#line 19
  tmp = VyRealloc((void *)stack, sizeof(VyObj ) * (unsigned long )stack_size);
#line 17
  fprintf(_coverage_fout, "596\n");
#line 17
  fflush(_coverage_fout);
#line 19
  stack = (VyObj *)tmp;
#line 17
  fprintf(_coverage_fout, "597\n");
#line 17
  fflush(_coverage_fout);
#line 20
  return;
}
}
#line 23 "vm/Stack.c"
void StackPush(VyObj obj ) 
{ 

  {
#line 23
  fprintf(_coverage_fout, "602\n");
#line 23
  fflush(_coverage_fout);
#line 25
  if (! stack) {
#line 25
    fprintf(_coverage_fout, "598\n");
#line 25
    fflush(_coverage_fout);
#line 26
    InitStack();
  } else {
#line 25
    fprintf(_coverage_fout, "601\n");
#line 25
    fflush(_coverage_fout);
#line 27
    if (stack_index == stack_size) {
#line 27
      fprintf(_coverage_fout, "599\n");
#line 27
      fflush(_coverage_fout);
#line 28
      ExpandStack();
    } else {
#line 27
      fprintf(_coverage_fout, "600\n");
#line 27
      fflush(_coverage_fout);

    }
  }
#line 23
  fprintf(_coverage_fout, "603\n");
#line 23
  fflush(_coverage_fout);
#line 31
  *(stack + stack_index) = obj;
#line 23
  fprintf(_coverage_fout, "604\n");
#line 23
  fflush(_coverage_fout);
#line 32
  stack_index ++;
#line 23
  fprintf(_coverage_fout, "605\n");
#line 23
  fflush(_coverage_fout);
#line 33
  return;
}
}
#line 36 "vm/Stack.c"
VyObj StackPop(void) 
{ VyObj val ;

  {
#line 36
  fprintf(_coverage_fout, "606\n");
#line 36
  fflush(_coverage_fout);
#line 37
  stack_index --;
#line 36
  fprintf(_coverage_fout, "607\n");
#line 36
  fflush(_coverage_fout);
#line 38
  val = *(stack + stack_index);
#line 36
  fprintf(_coverage_fout, "608\n");
#line 36
  fflush(_coverage_fout);
#line 41
  *(stack + stack_index) = None();
#line 36
  fprintf(_coverage_fout, "609\n");
#line 36
  fflush(_coverage_fout);
#line 43
  return (val);
}
}
#line 47 "vm/Stack.c"
VyObj StackPeek(void) 
{ 

  {
#line 47
  fprintf(_coverage_fout, "610\n");
#line 47
  fflush(_coverage_fout);
#line 48
  return (*(stack + (stack_index - 1)));
}
}
#line 1 "Bytecode.o"
/* #pragma merger(0,"/tmp/cil-hWEs12Mz.i","-Wall,-g") */
#line 4 "vm/Bytecode.c"
Bytecode *CreateBytecode(void) 
{ Bytecode *bytecode ;
  void *tmp ;
  void *tmp___0 ;

  {
#line 4
  fprintf(_coverage_fout, "611\n");
#line 4
  fflush(_coverage_fout);
#line 5
  tmp = VyMalloc(sizeof(Bytecode ));
#line 4
  fprintf(_coverage_fout, "612\n");
#line 4
  fflush(_coverage_fout);
#line 5
  bytecode = (Bytecode *)tmp;
#line 4
  fprintf(_coverage_fout, "613\n");
#line 4
  fflush(_coverage_fout);
#line 8
  bytecode->size = 1024;
#line 4
  fprintf(_coverage_fout, "614\n");
#line 4
  fflush(_coverage_fout);
#line 9
  bytecode->used = 0;
#line 4
  fprintf(_coverage_fout, "615\n");
#line 4
  fflush(_coverage_fout);
#line 10
  tmp___0 = VyMalloc(sizeof(Instruction ) * (unsigned long )bytecode->size);
#line 4
  fprintf(_coverage_fout, "616\n");
#line 4
  fflush(_coverage_fout);
#line 10
  bytecode->instructions = (Instruction *)tmp___0;
#line 4
  fprintf(_coverage_fout, "617\n");
#line 4
  fflush(_coverage_fout);
#line 11
  return (bytecode);
}
}
#line 13 "vm/Bytecode.c"
void FreeBytecode(Bytecode *bytecode ) 
{ 

  {
#line 13
  fprintf(_coverage_fout, "618\n");
#line 13
  fflush(_coverage_fout);
#line 14
  VyFree((void *)bytecode->instructions);
#line 13
  fprintf(_coverage_fout, "619\n");
#line 13
  fflush(_coverage_fout);
#line 15
  VyFree((void *)bytecode);
#line 13
  fprintf(_coverage_fout, "620\n");
#line 13
  fflush(_coverage_fout);
#line 16
  return;
}
}
#line 19 "vm/Bytecode.c"
void ExpandBytecode(Bytecode *bytecode ) 
{ void *tmp ;

  {
#line 19
  fprintf(_coverage_fout, "621\n");
#line 19
  fflush(_coverage_fout);
#line 21
  bytecode->size *= 2;
#line 19
  fprintf(_coverage_fout, "622\n");
#line 19
  fflush(_coverage_fout);
#line 24
  tmp = VyRealloc((void *)bytecode->instructions,
                  sizeof(Instruction ) * (unsigned long )bytecode->size);
#line 19
  fprintf(_coverage_fout, "623\n");
#line 19
  fflush(_coverage_fout);
#line 24
  bytecode->instructions = (Instruction *)tmp;
#line 19
  fprintf(_coverage_fout, "624\n");
#line 19
  fflush(_coverage_fout);
#line 25
  return;
}
}
#line 28 "vm/Bytecode.c"
void EmitInstruction(Bytecode *bytecode , Instruction instr ) 
{ int index___0 ;

  {
#line 28
  fprintf(_coverage_fout, "627\n");
#line 28
  fflush(_coverage_fout);
#line 29
  index___0 = bytecode->used;
#line 28
  fprintf(_coverage_fout, "628\n");
#line 28
  fflush(_coverage_fout);
#line 32
  (bytecode->used) ++;
#line 28
  fprintf(_coverage_fout, "629\n");
#line 28
  fflush(_coverage_fout);
#line 33
  if (bytecode->used > bytecode->size) {
#line 33
    fprintf(_coverage_fout, "625\n");
#line 33
    fflush(_coverage_fout);
#line 34
    ExpandBytecode(bytecode);
  } else {
#line 33
    fprintf(_coverage_fout, "626\n");
#line 33
    fflush(_coverage_fout);

  }
#line 28
  fprintf(_coverage_fout, "630\n");
#line 28
  fflush(_coverage_fout);
#line 36
  *(bytecode->instructions + index___0) = instr;
#line 28
  fprintf(_coverage_fout, "631\n");
#line 28
  fflush(_coverage_fout);
#line 37
  return;
}
}
#line 40 "vm/Bytecode.c"
__inline Instruction Push(VyObj data ) 
{ Instruction instr ;

  {
#line 40
  fprintf(_coverage_fout, "632\n");
#line 40
  fflush(_coverage_fout);
#line 41
  instr.opcode = 20;
#line 40
  fprintf(_coverage_fout, "633\n");
#line 40
  fflush(_coverage_fout);
#line 41
  instr.data.obj = data;
#line 40
  fprintf(_coverage_fout, "634\n");
#line 40
  fflush(_coverage_fout);
#line 42
  return (instr);
}
}
#line 44 "vm/Bytecode.c"
__inline Instruction Pop(void) 
{ Instruction instr ;

  {
#line 44
  fprintf(_coverage_fout, "635\n");
#line 44
  fflush(_coverage_fout);
#line 45
  instr.opcode = 10;
#line 44
  fprintf(_coverage_fout, "636\n");
#line 44
  fflush(_coverage_fout);
#line 45
  instr.data.num = 0;
#line 44
  fprintf(_coverage_fout, "637\n");
#line 44
  fflush(_coverage_fout);
#line 46
  return (instr);
}
}
#line 48 "vm/Bytecode.c"
__inline Instruction Bind(void) 
{ Instruction instr ;

  {
#line 48
  fprintf(_coverage_fout, "638\n");
#line 48
  fflush(_coverage_fout);
#line 49
  instr.opcode = 70;
#line 48
  fprintf(_coverage_fout, "639\n");
#line 48
  fflush(_coverage_fout);
#line 49
  instr.data.num = 0;
#line 48
  fprintf(_coverage_fout, "640\n");
#line 48
  fflush(_coverage_fout);
#line 50
  return (instr);
}
}
#line 52 "vm/Bytecode.c"
__inline Instruction Value(VyObj name ) 
{ Instruction instr ;

  {
#line 52
  fprintf(_coverage_fout, "641\n");
#line 52
  fflush(_coverage_fout);
#line 53
  instr.opcode = 60;
#line 52
  fprintf(_coverage_fout, "642\n");
#line 52
  fflush(_coverage_fout);
#line 53
  instr.data.obj = name;
#line 52
  fprintf(_coverage_fout, "643\n");
#line 52
  fflush(_coverage_fout);
#line 54
  return (instr);
}
}
#line 56 "vm/Bytecode.c"
__inline Instruction Func(void) 
{ Instruction instr ;

  {
#line 56
  fprintf(_coverage_fout, "644\n");
#line 56
  fflush(_coverage_fout);
#line 57
  instr.opcode = 80;
#line 56
  fprintf(_coverage_fout, "645\n");
#line 56
  fflush(_coverage_fout);
#line 57
  instr.data.num = 0;
#line 56
  fprintf(_coverage_fout, "646\n");
#line 56
  fflush(_coverage_fout);
#line 58
  return (instr);
}
}
#line 60 "vm/Bytecode.c"
__inline Instruction Call(int num_args ) 
{ Instruction instr ;

  {
#line 60
  fprintf(_coverage_fout, "647\n");
#line 60
  fflush(_coverage_fout);
#line 61
  instr.opcode = 30;
#line 60
  fprintf(_coverage_fout, "648\n");
#line 60
  fflush(_coverage_fout);
#line 61
  instr.data.num = num_args;
#line 60
  fprintf(_coverage_fout, "649\n");
#line 60
  fflush(_coverage_fout);
#line 62
  return (instr);
}
}
#line 66
Bytecode *CompileExpr(Bytecode *bytecode , VyObj expr ) ;
#line 67
VyObj CompileFunctionObj(VyObj arg_list , VyObj statement_list ) ;
#line 70 "vm/Bytecode.c"
Bytecode *Compile(VyObj compileObj ) 
{ Bytecode *bytecode ;
  Bytecode *tmp ;
  Bytecode *tmp___0 ;

  {
#line 70
  fprintf(_coverage_fout, "650\n");
#line 70
  fflush(_coverage_fout);
#line 71
  tmp = CreateBytecode();
#line 70
  fprintf(_coverage_fout, "651\n");
#line 70
  fflush(_coverage_fout);
#line 71
  bytecode = tmp;
#line 70
  fprintf(_coverage_fout, "652\n");
#line 70
  fflush(_coverage_fout);
#line 72
  tmp___0 = CompileExpr(bytecode, compileObj);
#line 70
  fprintf(_coverage_fout, "653\n");
#line 70
  fflush(_coverage_fout);
#line 72
  return (tmp___0);
}
}
#line 76 "vm/Bytecode.c"
Bytecode *CompileExpr(Bytecode *bytecode , VyObj expr ) 
{ VyObj obj ;
  Instruction tmp ;
  bool tmp___0 ;
  bool tmp___1 ;
  bool tmp___2 ;
  Instruction tmp___3 ;
  bool tmp___4 ;
  VyCons *cons ;
  void *tmp___5 ;
  bool special_form ;
  VyObj symbol ;
  VyObj tmp___6 ;
  VyObj quoted ;
  VyObj tmp___7 ;
  VyObj tmp___8 ;
  Instruction tmp___9 ;
  VyObj name ;
  VyObj tmp___10 ;
  VyObj tmp___11 ;
  Instruction tmp___12 ;
  VyObj arg_list ;
  VyObj tmp___13 ;
  VyObj statements ;
  VyObj tmp___14 ;
  VyObj tmp___15 ;
  VyObj function_obj ;
  VyObj tmp___16 ;
  Instruction tmp___17 ;
  Instruction tmp___18 ;
  bool has_else_clause ;
  int tmp___19 ;
  VyObj tmp___20 ;
  Instruction if_jmp ;
  Instruction *if_jmp_placeholder ;
  VyObj tmp___21 ;
  Instruction jmp ;
  Instruction *jmp_placeholder ;
  int else_index ;
  VyObj tmp___22 ;
  VyObj tmp___23 ;
  Instruction tmp___24 ;
  int end_index ;
  int start_index ;
  VyObj tmp___25 ;
  Instruction if_jmp___0 ;
  Instruction *if_jmp_placeholder___0 ;
  int i ;
  int len ;
  int tmp___26 ;
  VyObj tmp___27 ;
  Instruction tmp___28 ;
  Instruction jmp___0 ;
  int end_index___0 ;
  VyObj tmp___29 ;
  Instruction tmp___30 ;
  bool tmp___31 ;
  bool tmp___32 ;
  bool tmp___33 ;
  bool tmp___34 ;
  bool tmp___35 ;
  VyObj tmp___36 ;
  bool tmp___37 ;
  int num_args ;
  int tmp___38 ;
  int cur_arg ;
  VyObj tmp___39 ;
  VyObj tmp___40 ;
  Instruction tmp___41 ;
  bool tmp___42 ;

  {
#line 76
  fprintf(_coverage_fout, "784\n");
#line 76
  fflush(_coverage_fout);
#line 78
  obj = expr;
#line 76
  fprintf(_coverage_fout, "785\n");
#line 76
  fflush(_coverage_fout);
#line 84
  tmp___0 = IsType(obj, TypeString);
#line 76
  fprintf(_coverage_fout, "786\n");
#line 76
  fflush(_coverage_fout);
#line 84
  if (tmp___0) {
#line 84
    fprintf(_coverage_fout, "654\n");
#line 84
    fflush(_coverage_fout);
#line 85
    tmp = Push(expr);
#line 84
    fprintf(_coverage_fout, "655\n");
#line 84
    fflush(_coverage_fout);
#line 85
    EmitInstruction(bytecode, tmp);
  } else {
#line 84
    fprintf(_coverage_fout, "663\n");
#line 84
    fflush(_coverage_fout);
#line 84
    tmp___1 = IsType(obj, TypeFloat);
#line 84
    fprintf(_coverage_fout, "664\n");
#line 84
    fflush(_coverage_fout);
#line 84
    if (tmp___1) {
#line 84
      fprintf(_coverage_fout, "656\n");
#line 84
      fflush(_coverage_fout);
#line 85
      tmp = Push(expr);
#line 84
      fprintf(_coverage_fout, "657\n");
#line 84
      fflush(_coverage_fout);
#line 85
      EmitInstruction(bytecode, tmp);
    } else {
#line 84
      fprintf(_coverage_fout, "661\n");
#line 84
      fflush(_coverage_fout);
#line 84
      tmp___2 = IsType(obj, TypeInt);
#line 84
      fprintf(_coverage_fout, "662\n");
#line 84
      fflush(_coverage_fout);
#line 84
      if (tmp___2) {
#line 84
        fprintf(_coverage_fout, "658\n");
#line 84
        fflush(_coverage_fout);
#line 85
        tmp = Push(expr);
#line 84
        fprintf(_coverage_fout, "659\n");
#line 84
        fflush(_coverage_fout);
#line 85
        EmitInstruction(bytecode, tmp);
      } else {
#line 84
        fprintf(_coverage_fout, "660\n");
#line 84
        fflush(_coverage_fout);

      }
    }
  }
#line 76
  fprintf(_coverage_fout, "787\n");
#line 76
  fflush(_coverage_fout);
#line 88
  tmp___4 = IsType(obj, TypeSymbol);
#line 76
  fprintf(_coverage_fout, "788\n");
#line 76
  fflush(_coverage_fout);
#line 88
  if (tmp___4) {
#line 88
    fprintf(_coverage_fout, "665\n");
#line 88
    fflush(_coverage_fout);
#line 89
    tmp___3 = Value(expr);
#line 88
    fprintf(_coverage_fout, "666\n");
#line 88
    fflush(_coverage_fout);
#line 89
    EmitInstruction(bytecode, tmp___3);
  } else {
#line 88
    fprintf(_coverage_fout, "667\n");
#line 88
    fflush(_coverage_fout);

  }
#line 76
  fprintf(_coverage_fout, "789\n");
#line 76
  fflush(_coverage_fout);
#line 93
  tmp___42 = IsType(obj, TypeCons);
#line 76
  fprintf(_coverage_fout, "790\n");
#line 76
  fflush(_coverage_fout);
#line 93
  if (tmp___42) {
#line 93
    fprintf(_coverage_fout, "776\n");
#line 93
    fflush(_coverage_fout);
#line 94
    tmp___5 = Obj(obj);
#line 93
    fprintf(_coverage_fout, "777\n");
#line 93
    fflush(_coverage_fout);
#line 94
    cons = (VyCons *)tmp___5;
#line 93
    fprintf(_coverage_fout, "778\n");
#line 93
    fflush(_coverage_fout);
#line 96
    special_form = 0;
#line 93
    fprintf(_coverage_fout, "779\n");
#line 93
    fflush(_coverage_fout);
#line 99
    tmp___36 = Car(obj);
#line 93
    fprintf(_coverage_fout, "780\n");
#line 93
    fflush(_coverage_fout);
#line 99
    tmp___37 = IsType(tmp___36, TypeSymbol);
#line 93
    fprintf(_coverage_fout, "781\n");
#line 93
    fflush(_coverage_fout);
#line 99
    if (tmp___37) {
#line 99
      fprintf(_coverage_fout, "756\n");
#line 99
      fflush(_coverage_fout);
#line 100
      tmp___6 = Car(obj);
#line 99
      fprintf(_coverage_fout, "757\n");
#line 99
      fflush(_coverage_fout);
#line 100
      symbol = tmp___6;
#line 99
      fprintf(_coverage_fout, "758\n");
#line 99
      fflush(_coverage_fout);
#line 103
      tmp___35 = ObjEq(symbol, SymbolQuote);
#line 99
      fprintf(_coverage_fout, "759\n");
#line 99
      fflush(_coverage_fout);
#line 103
      if (tmp___35) {
#line 103
        fprintf(_coverage_fout, "668\n");
#line 103
        fflush(_coverage_fout);
#line 104
        special_form = 1;
#line 103
        fprintf(_coverage_fout, "669\n");
#line 103
        fflush(_coverage_fout);
#line 107
        tmp___7 = Cdr(obj);
#line 103
        fprintf(_coverage_fout, "670\n");
#line 103
        fflush(_coverage_fout);
#line 107
        tmp___8 = Car(tmp___7);
#line 103
        fprintf(_coverage_fout, "671\n");
#line 103
        fflush(_coverage_fout);
#line 107
        quoted = tmp___8;
#line 103
        fprintf(_coverage_fout, "672\n");
#line 103
        fflush(_coverage_fout);
#line 108
        tmp___9 = Push(quoted);
#line 103
        fprintf(_coverage_fout, "673\n");
#line 103
        fflush(_coverage_fout);
#line 108
        EmitInstruction(bytecode, tmp___9);
      } else {
#line 103
        fprintf(_coverage_fout, "754\n");
#line 103
        fflush(_coverage_fout);
#line 112
        tmp___34 = ObjEq(symbol, SymbolSetvar);
#line 103
        fprintf(_coverage_fout, "755\n");
#line 103
        fflush(_coverage_fout);
#line 112
        if (tmp___34) {
#line 112
          fprintf(_coverage_fout, "674\n");
#line 112
          fflush(_coverage_fout);
#line 113
          special_form = 1;
#line 112
          fprintf(_coverage_fout, "675\n");
#line 112
          fflush(_coverage_fout);
#line 117
          tmp___10 = ListGet(cons, 1);
#line 112
          fprintf(_coverage_fout, "676\n");
#line 112
          fflush(_coverage_fout);
#line 117
          name = tmp___10;
#line 112
          fprintf(_coverage_fout, "677\n");
#line 112
          fflush(_coverage_fout);
#line 120
          tmp___11 = ListGet(cons, 2);
#line 112
          fprintf(_coverage_fout, "678\n");
#line 112
          fflush(_coverage_fout);
#line 120
          CompileExpr(bytecode, tmp___11);
#line 112
          fprintf(_coverage_fout, "679\n");
#line 112
          fflush(_coverage_fout);
#line 123
          CompileExpr(bytecode, name);
#line 112
          fprintf(_coverage_fout, "680\n");
#line 112
          fflush(_coverage_fout);
#line 126
          tmp___12 = Bind();
#line 112
          fprintf(_coverage_fout, "681\n");
#line 112
          fflush(_coverage_fout);
#line 126
          EmitInstruction(bytecode, tmp___12);
        } else {
#line 112
          fprintf(_coverage_fout, "752\n");
#line 112
          fflush(_coverage_fout);
#line 130
          tmp___33 = ObjEq(symbol, SymbolFn);
#line 112
          fprintf(_coverage_fout, "753\n");
#line 112
          fflush(_coverage_fout);
#line 130
          if (tmp___33) {
#line 130
            fprintf(_coverage_fout, "682\n");
#line 130
            fflush(_coverage_fout);
#line 131
            special_form = 1;
#line 130
            fprintf(_coverage_fout, "683\n");
#line 130
            fflush(_coverage_fout);
#line 133
            tmp___13 = ListGet(cons, 1);
#line 130
            fprintf(_coverage_fout, "684\n");
#line 130
            fflush(_coverage_fout);
#line 133
            arg_list = tmp___13;
#line 130
            fprintf(_coverage_fout, "685\n");
#line 130
            fflush(_coverage_fout);
#line 134
            tmp___14 = Cdr(obj);
#line 130
            fprintf(_coverage_fout, "686\n");
#line 130
            fflush(_coverage_fout);
#line 134
            tmp___15 = Cdr(tmp___14);
#line 130
            fprintf(_coverage_fout, "687\n");
#line 130
            fflush(_coverage_fout);
#line 134
            statements = tmp___15;
#line 130
            fprintf(_coverage_fout, "688\n");
#line 130
            fflush(_coverage_fout);
#line 137
            tmp___16 = CompileFunctionObj(arg_list, statements);
#line 130
            fprintf(_coverage_fout, "689\n");
#line 130
            fflush(_coverage_fout);
#line 137
            function_obj = tmp___16;
#line 130
            fprintf(_coverage_fout, "690\n");
#line 130
            fflush(_coverage_fout);
#line 138
            tmp___17 = Push(function_obj);
#line 130
            fprintf(_coverage_fout, "691\n");
#line 130
            fflush(_coverage_fout);
#line 138
            EmitInstruction(bytecode, tmp___17);
#line 130
            fprintf(_coverage_fout, "692\n");
#line 130
            fflush(_coverage_fout);
#line 141
            tmp___18 = Func();
#line 130
            fprintf(_coverage_fout, "693\n");
#line 130
            fflush(_coverage_fout);
#line 141
            EmitInstruction(bytecode, tmp___18);
          } else {
#line 130
            fprintf(_coverage_fout, "750\n");
#line 130
            fflush(_coverage_fout);
#line 145
            tmp___32 = ObjEq(symbol, SymbolIf);
#line 130
            fprintf(_coverage_fout, "751\n");
#line 130
            fflush(_coverage_fout);
#line 145
            if (tmp___32) {
#line 145
              fprintf(_coverage_fout, "699\n");
#line 145
              fflush(_coverage_fout);
#line 146
              special_form = 1;
#line 145
              fprintf(_coverage_fout, "700\n");
#line 145
              fflush(_coverage_fout);
#line 161
              tmp___19 = ListLen(obj);
#line 145
              fprintf(_coverage_fout, "701\n");
#line 145
              fflush(_coverage_fout);
#line 161
              has_else_clause = tmp___19 == 4;
#line 145
              fprintf(_coverage_fout, "702\n");
#line 145
              fflush(_coverage_fout);
#line 164
              tmp___20 = ListGet(cons, 1);
#line 145
              fprintf(_coverage_fout, "703\n");
#line 145
              fflush(_coverage_fout);
#line 164
              bytecode = CompileExpr(bytecode, tmp___20);
#line 145
              fprintf(_coverage_fout, "704\n");
#line 145
              fflush(_coverage_fout);
#line 168
              if_jmp.opcode = 40;
#line 145
              fprintf(_coverage_fout, "705\n");
#line 145
              fflush(_coverage_fout);
#line 168
              if_jmp.data.num = 0;
#line 145
              fprintf(_coverage_fout, "706\n");
#line 145
              fflush(_coverage_fout);
#line 169
              EmitInstruction(bytecode, if_jmp);
#line 145
              fprintf(_coverage_fout, "707\n");
#line 145
              fflush(_coverage_fout);
#line 172
              if_jmp_placeholder = bytecode->instructions + (bytecode->used - 1);
#line 145
              fprintf(_coverage_fout, "708\n");
#line 145
              fflush(_coverage_fout);
#line 175
              tmp___21 = ListGet(cons, 2);
#line 145
              fprintf(_coverage_fout, "709\n");
#line 145
              fflush(_coverage_fout);
#line 175
              bytecode = CompileExpr(bytecode, tmp___21);
#line 145
              fprintf(_coverage_fout, "710\n");
#line 145
              fflush(_coverage_fout);
#line 178
              jmp.opcode = 50;
#line 145
              fprintf(_coverage_fout, "711\n");
#line 145
              fflush(_coverage_fout);
#line 178
              jmp.data.num = 0;
#line 145
              fprintf(_coverage_fout, "712\n");
#line 145
              fflush(_coverage_fout);
#line 179
              EmitInstruction(bytecode, jmp);
#line 145
              fprintf(_coverage_fout, "713\n");
#line 145
              fflush(_coverage_fout);
#line 180
              jmp_placeholder = bytecode->instructions + (bytecode->used - 1);
#line 145
              fprintf(_coverage_fout, "714\n");
#line 145
              fflush(_coverage_fout);
#line 183
              else_index = bytecode->used;
#line 145
              fprintf(_coverage_fout, "715\n");
#line 145
              fflush(_coverage_fout);
#line 186
              if (has_else_clause) {
#line 186
                fprintf(_coverage_fout, "694\n");
#line 186
                fflush(_coverage_fout);
#line 187
                tmp___22 = ListGet(cons, 3);
#line 186
                fprintf(_coverage_fout, "695\n");
#line 186
                fflush(_coverage_fout);
#line 187
                bytecode = CompileExpr(bytecode, tmp___22);
              } else {
#line 186
                fprintf(_coverage_fout, "696\n");
#line 186
                fflush(_coverage_fout);
#line 192
                tmp___23 = Nil();
#line 186
                fprintf(_coverage_fout, "697\n");
#line 186
                fflush(_coverage_fout);
#line 192
                tmp___24 = Push(tmp___23);
#line 186
                fprintf(_coverage_fout, "698\n");
#line 186
                fflush(_coverage_fout);
#line 192
                EmitInstruction(bytecode, tmp___24);
              }
#line 145
              fprintf(_coverage_fout, "716\n");
#line 145
              fflush(_coverage_fout);
#line 196
              end_index = bytecode->used;
#line 145
              fprintf(_coverage_fout, "717\n");
#line 145
              fflush(_coverage_fout);
#line 197
              jmp_placeholder->data.num = end_index;
#line 145
              fprintf(_coverage_fout, "718\n");
#line 145
              fflush(_coverage_fout);
#line 198
              if_jmp_placeholder->data.num = else_index;
            } else {
#line 145
              fprintf(_coverage_fout, "748\n");
#line 145
              fflush(_coverage_fout);
#line 202
              tmp___31 = ObjEq(symbol, SymbolWhile);
#line 145
              fprintf(_coverage_fout, "749\n");
#line 145
              fflush(_coverage_fout);
#line 202
              if (tmp___31) {
#line 202
                fprintf(_coverage_fout, "726\n");
#line 202
                fflush(_coverage_fout);
#line 203
                special_form = 1;
#line 202
                fprintf(_coverage_fout, "727\n");
#line 202
                fflush(_coverage_fout);
#line 218
                start_index = bytecode->used;
#line 202
                fprintf(_coverage_fout, "728\n");
#line 202
                fflush(_coverage_fout);
#line 221
                tmp___25 = ListGet(cons, 1);
#line 202
                fprintf(_coverage_fout, "729\n");
#line 202
                fflush(_coverage_fout);
#line 221
                bytecode = CompileExpr(bytecode, tmp___25);
#line 202
                fprintf(_coverage_fout, "730\n");
#line 202
                fflush(_coverage_fout);
#line 224
                if_jmp___0.opcode = 40;
#line 202
                fprintf(_coverage_fout, "731\n");
#line 202
                fflush(_coverage_fout);
#line 224
                if_jmp___0.data.num = 0;
#line 202
                fprintf(_coverage_fout, "732\n");
#line 202
                fflush(_coverage_fout);
#line 225
                EmitInstruction(bytecode, if_jmp___0);
#line 202
                fprintf(_coverage_fout, "733\n");
#line 202
                fflush(_coverage_fout);
#line 226
                if_jmp_placeholder___0 = bytecode->instructions + (bytecode->used - 1);
#line 202
                fprintf(_coverage_fout, "734\n");
#line 202
                fflush(_coverage_fout);
#line 229
                i = 2;
#line 202
                fprintf(_coverage_fout, "735\n");
#line 202
                fflush(_coverage_fout);
#line 229
                tmp___26 = ListLen(obj);
#line 202
                fprintf(_coverage_fout, "736\n");
#line 202
                fflush(_coverage_fout);
#line 229
                len = tmp___26;
#line 202
                fprintf(_coverage_fout, "737\n");
#line 202
                fflush(_coverage_fout);
#line 230
                i = 2;
#line 202
                fprintf(_coverage_fout, "738\n");
#line 202
                fflush(_coverage_fout);
#line 230
                while (1) {
#line 230
                  fprintf(_coverage_fout, "720\n");
#line 230
                  fflush(_coverage_fout);
#line 230
                  if (i < len) {
#line 230
                    fprintf(_coverage_fout, "719\n");
#line 230
                    fflush(_coverage_fout);

                  } else {
#line 230
                    break;
                  }
#line 230
                  fprintf(_coverage_fout, "721\n");
#line 230
                  fflush(_coverage_fout);
#line 231
                  tmp___27 = ListGet(cons, i);
#line 230
                  fprintf(_coverage_fout, "722\n");
#line 230
                  fflush(_coverage_fout);
#line 231
                  bytecode = CompileExpr(bytecode, tmp___27);
#line 230
                  fprintf(_coverage_fout, "723\n");
#line 230
                  fflush(_coverage_fout);
#line 235
                  tmp___28 = Pop();
#line 230
                  fprintf(_coverage_fout, "724\n");
#line 230
                  fflush(_coverage_fout);
#line 235
                  EmitInstruction(bytecode, tmp___28);
#line 230
                  fprintf(_coverage_fout, "725\n");
#line 230
                  fflush(_coverage_fout);
#line 230
                  i ++;
                }
#line 202
                fprintf(_coverage_fout, "739\n");
#line 202
                fflush(_coverage_fout);
#line 239
                jmp___0.opcode = 50;
#line 202
                fprintf(_coverage_fout, "740\n");
#line 202
                fflush(_coverage_fout);
#line 239
                jmp___0.data.num = start_index;
#line 202
                fprintf(_coverage_fout, "741\n");
#line 202
                fflush(_coverage_fout);
#line 240
                EmitInstruction(bytecode, jmp___0);
#line 202
                fprintf(_coverage_fout, "742\n");
#line 202
                fflush(_coverage_fout);
#line 243
                end_index___0 = bytecode->used;
#line 202
                fprintf(_coverage_fout, "743\n");
#line 202
                fflush(_coverage_fout);
#line 244
                if_jmp_placeholder___0->data.num = end_index___0;
#line 202
                fprintf(_coverage_fout, "744\n");
#line 202
                fflush(_coverage_fout);
#line 247
                tmp___29 = Nil();
#line 202
                fprintf(_coverage_fout, "745\n");
#line 202
                fflush(_coverage_fout);
#line 247
                tmp___30 = Push(tmp___29);
#line 202
                fprintf(_coverage_fout, "746\n");
#line 202
                fflush(_coverage_fout);
#line 247
                EmitInstruction(bytecode, tmp___30);
              } else {
#line 202
                fprintf(_coverage_fout, "747\n");
#line 202
                fflush(_coverage_fout);

              }
            }
          }
        }
      }
    } else {
#line 99
      fprintf(_coverage_fout, "760\n");
#line 99
      fflush(_coverage_fout);

    }
#line 93
    fprintf(_coverage_fout, "782\n");
#line 93
    fflush(_coverage_fout);
#line 254
    if (! special_form) {
#line 254
      fprintf(_coverage_fout, "766\n");
#line 254
      fflush(_coverage_fout);
#line 255
      tmp___38 = ListLen(obj);
#line 254
      fprintf(_coverage_fout, "767\n");
#line 254
      fflush(_coverage_fout);
#line 255
      num_args = tmp___38 - 1;
#line 254
      fprintf(_coverage_fout, "768\n");
#line 254
      fflush(_coverage_fout);
#line 258
      cur_arg = num_args;
#line 254
      fprintf(_coverage_fout, "769\n");
#line 254
      fflush(_coverage_fout);
#line 259
      cur_arg = num_args;
#line 254
      fprintf(_coverage_fout, "770\n");
#line 254
      fflush(_coverage_fout);
#line 259
      while (1) {
#line 259
        fprintf(_coverage_fout, "762\n");
#line 259
        fflush(_coverage_fout);
#line 259
        if (cur_arg > 0) {
#line 259
          fprintf(_coverage_fout, "761\n");
#line 259
          fflush(_coverage_fout);

        } else {
#line 259
          break;
        }
#line 259
        fprintf(_coverage_fout, "763\n");
#line 259
        fflush(_coverage_fout);
#line 260
        tmp___39 = ListGet(cons, cur_arg);
#line 259
        fprintf(_coverage_fout, "764\n");
#line 259
        fflush(_coverage_fout);
#line 260
        bytecode = CompileExpr(bytecode, tmp___39);
#line 259
        fprintf(_coverage_fout, "765\n");
#line 259
        fflush(_coverage_fout);
#line 259
        cur_arg --;
      }
#line 254
      fprintf(_coverage_fout, "771\n");
#line 254
      fflush(_coverage_fout);
#line 264
      tmp___40 = ListGet(cons, 0);
#line 254
      fprintf(_coverage_fout, "772\n");
#line 254
      fflush(_coverage_fout);
#line 264
      bytecode = CompileExpr(bytecode, tmp___40);
#line 254
      fprintf(_coverage_fout, "773\n");
#line 254
      fflush(_coverage_fout);
#line 267
      tmp___41 = Call(num_args);
#line 254
      fprintf(_coverage_fout, "774\n");
#line 254
      fflush(_coverage_fout);
#line 267
      EmitInstruction(bytecode, tmp___41);
    } else {
#line 254
      fprintf(_coverage_fout, "775\n");
#line 254
      fflush(_coverage_fout);

    }
  } else {
#line 93
    fprintf(_coverage_fout, "783\n");
#line 93
    fflush(_coverage_fout);

  }
#line 76
  fprintf(_coverage_fout, "791\n");
#line 76
  fflush(_coverage_fout);
#line 274
  return (bytecode);
}
}
#line 278 "vm/Bytecode.c"
VyObj CompileFunctionObj(VyObj arg_list , VyObj statement_list ) 
{ Bytecode *func_bytecode ;
  Bytecode *tmp ;
  VyCons *statement_cons ;
  void *tmp___0 ;
  int statement_ind ;
  int len ;
  int tmp___1 ;
  VyObj statement ;
  VyObj tmp___2 ;
  Instruction tmp___3 ;
  VyFunction *func ;
  ArgList tmp___4 ;
  VyFunction *tmp___5 ;
  VyObj tmp___6 ;

  {
#line 278
  fprintf(_coverage_fout, "802\n");
#line 278
  fflush(_coverage_fout);
#line 280
  tmp = CreateBytecode();
#line 278
  fprintf(_coverage_fout, "803\n");
#line 278
  fflush(_coverage_fout);
#line 280
  func_bytecode = tmp;
#line 278
  fprintf(_coverage_fout, "804\n");
#line 278
  fflush(_coverage_fout);
#line 281
  tmp___0 = Obj(statement_list);
#line 278
  fprintf(_coverage_fout, "805\n");
#line 278
  fflush(_coverage_fout);
#line 281
  statement_cons = (VyCons *)tmp___0;
#line 278
  fprintf(_coverage_fout, "806\n");
#line 278
  fflush(_coverage_fout);
#line 283
  statement_ind = 0;
#line 278
  fprintf(_coverage_fout, "807\n");
#line 278
  fflush(_coverage_fout);
#line 284
  tmp___1 = ListLen(statement_list);
#line 278
  fprintf(_coverage_fout, "808\n");
#line 278
  fflush(_coverage_fout);
#line 284
  len = tmp___1;
#line 278
  fprintf(_coverage_fout, "809\n");
#line 278
  fflush(_coverage_fout);
#line 285
  while (1) {
#line 285
    fprintf(_coverage_fout, "796\n");
#line 285
    fflush(_coverage_fout);
#line 285
    if (statement_ind < len) {
#line 285
      fprintf(_coverage_fout, "792\n");
#line 285
      fflush(_coverage_fout);

    } else {
#line 285
      break;
    }
#line 285
    fprintf(_coverage_fout, "797\n");
#line 285
    fflush(_coverage_fout);
#line 287
    tmp___2 = ListGet(statement_cons, statement_ind);
#line 285
    fprintf(_coverage_fout, "798\n");
#line 285
    fflush(_coverage_fout);
#line 287
    statement = tmp___2;
#line 285
    fprintf(_coverage_fout, "799\n");
#line 285
    fflush(_coverage_fout);
#line 288
    func_bytecode = CompileExpr(func_bytecode, statement);
#line 285
    fprintf(_coverage_fout, "800\n");
#line 285
    fflush(_coverage_fout);
#line 290
    statement_ind ++;
#line 285
    fprintf(_coverage_fout, "801\n");
#line 285
    fflush(_coverage_fout);
#line 293
    if (statement_ind != len) {
#line 293
      fprintf(_coverage_fout, "793\n");
#line 293
      fflush(_coverage_fout);
#line 294
      tmp___3 = Pop();
#line 293
      fprintf(_coverage_fout, "794\n");
#line 293
      fflush(_coverage_fout);
#line 294
      EmitInstruction(func_bytecode, tmp___3);
    } else {
#line 293
      fprintf(_coverage_fout, "795\n");
#line 293
      fflush(_coverage_fout);

    }
  }
#line 278
  fprintf(_coverage_fout, "810\n");
#line 278
  fflush(_coverage_fout);
#line 299
  tmp___4 = ParseArgList(arg_list);
#line 278
  fprintf(_coverage_fout, "811\n");
#line 278
  fflush(_coverage_fout);
#line 299
  tmp___5 = CreateFunction(tmp___4, func_bytecode);
#line 278
  fprintf(_coverage_fout, "812\n");
#line 278
  fflush(_coverage_fout);
#line 299
  func = tmp___5;
#line 278
  fprintf(_coverage_fout, "813\n");
#line 278
  fflush(_coverage_fout);
#line 300
  tmp___6 = WrapObj((void *)func, TypeFunction);
#line 278
  fprintf(_coverage_fout, "814\n");
#line 278
  fflush(_coverage_fout);
#line 300
  return (tmp___6);
}
}
#line 1 "Scope.o"
/* #pragma merger(0,"/tmp/cil-dJXxQycw.i","-Wall,-g") */
#line 218 "/usr/include/stdio.h"
extern int fflush(FILE *__stream ) ;
#line 48 "/usr/include/glib-2.0/glib/ghash.h"
extern void g_hash_table_destroy(GHashTable *hash_table ) ;
#line 18 "vm/Scope.c"
Scope *current_scope  =    (Scope *)((void *)0);
#line 21 "vm/Scope.c"
Scope *CreateScope(Scope *parent ) 
{ Scope *scope ;
  void *tmp ;

  {
#line 21
  fprintf(_coverage_fout, "815\n");
#line 21
  fflush(_coverage_fout);
#line 22
  tmp = VyMalloc(sizeof(Scope ));
#line 21
  fprintf(_coverage_fout, "816\n");
#line 21
  fflush(_coverage_fout);
#line 22
  scope = (Scope *)tmp;
#line 21
  fprintf(_coverage_fout, "817\n");
#line 21
  fflush(_coverage_fout);
#line 23
  scope->parent = parent;
#line 21
  fprintf(_coverage_fout, "818\n");
#line 21
  fflush(_coverage_fout);
#line 24
  scope->var_values = g_hash_table_new(& g_str_hash, & g_str_equal);
#line 21
  fprintf(_coverage_fout, "819\n");
#line 21
  fflush(_coverage_fout);
#line 25
  scope->type_values = g_hash_table_new(& g_str_hash, & g_str_equal);
#line 21
  fprintf(_coverage_fout, "820\n");
#line 21
  fflush(_coverage_fout);
#line 26
  scope->size_values = g_hash_table_new(& g_str_hash, & g_str_equal);
#line 21
  fprintf(_coverage_fout, "821\n");
#line 21
  fflush(_coverage_fout);
#line 27
  return (scope);
}
}
#line 31 "vm/Scope.c"
Scope *CurrentScope(void) 
{ 

  {
#line 31
  fprintf(_coverage_fout, "824\n");
#line 31
  fflush(_coverage_fout);
#line 33
  if (! current_scope) {
#line 33
    fprintf(_coverage_fout, "822\n");
#line 33
    fflush(_coverage_fout);
#line 33
    current_scope = CreateScope((Scope *)((void *)0));
  } else {
#line 33
    fprintf(_coverage_fout, "823\n");
#line 33
    fflush(_coverage_fout);

  }
#line 31
  fprintf(_coverage_fout, "825\n");
#line 31
  fflush(_coverage_fout);
#line 35
  return (current_scope);
}
}
#line 39 "vm/Scope.c"
VyObj VariableValue(VySymbol *symb ) 
{ Scope *current ;
  Scope *tmp ;
  gpointer tmp___0 ;
  gpointer value ;
  gpointer tmp___1 ;
  gpointer name ;
  gpointer tmp___2 ;
  gpointer size ;
  gpointer tmp___3 ;
  VyType type ;
  VyObj obj ;
  VyObj tmp___4 ;

  {
#line 39
  fprintf(_coverage_fout, "834\n");
#line 39
  fflush(_coverage_fout);
#line 43
  tmp = CurrentScope();
#line 39
  fprintf(_coverage_fout, "835\n");
#line 39
  fflush(_coverage_fout);
#line 43
  current = tmp;
#line 39
  fprintf(_coverage_fout, "836\n");
#line 39
  fflush(_coverage_fout);
#line 44
  while (1) {
#line 44
    fprintf(_coverage_fout, "830\n");
#line 44
    fflush(_coverage_fout);
#line 44
    tmp___0 = g_hash_table_lookup(current->var_values,
                                  (void const   *)symb->symb);
#line 44
    fprintf(_coverage_fout, "831\n");
#line 44
    fflush(_coverage_fout);
#line 44
    if (tmp___0) {
#line 44
      break;
    } else {
#line 44
      fprintf(_coverage_fout, "826\n");
#line 44
      fflush(_coverage_fout);

    }
#line 44
    fprintf(_coverage_fout, "832\n");
#line 44
    fflush(_coverage_fout);
#line 45
    current = current->parent;
#line 44
    fprintf(_coverage_fout, "833\n");
#line 44
    fflush(_coverage_fout);
#line 46
    if (! current) {
#line 46
      fprintf(_coverage_fout, "827\n");
#line 46
      fflush(_coverage_fout);
#line 47
      printf((char const   */* __restrict  */)"Failed to find: %s\n", symb->symb);
#line 46
      fprintf(_coverage_fout, "828\n");
#line 46
      fflush(_coverage_fout);
#line 48
      fflush(stdout);
    } else {
#line 46
      fprintf(_coverage_fout, "829\n");
#line 46
      fflush(_coverage_fout);

    }
  }
#line 39
  fprintf(_coverage_fout, "837\n");
#line 39
  fflush(_coverage_fout);
#line 53
  tmp___1 = g_hash_table_lookup(current->var_values, (void const   *)symb->symb);
#line 39
  fprintf(_coverage_fout, "838\n");
#line 39
  fflush(_coverage_fout);
#line 53
  value = tmp___1;
#line 39
  fprintf(_coverage_fout, "839\n");
#line 39
  fflush(_coverage_fout);
#line 54
  tmp___2 = g_hash_table_lookup(current->type_values, (void const   *)symb->symb);
#line 39
  fprintf(_coverage_fout, "840\n");
#line 39
  fflush(_coverage_fout);
#line 54
  name = tmp___2;
#line 39
  fprintf(_coverage_fout, "841\n");
#line 39
  fflush(_coverage_fout);
#line 55
  tmp___3 = g_hash_table_lookup(current->size_values, (void const   *)symb->symb);
#line 39
  fprintf(_coverage_fout, "842\n");
#line 39
  fflush(_coverage_fout);
#line 55
  size = tmp___3;
#line 39
  fprintf(_coverage_fout, "843\n");
#line 39
  fflush(_coverage_fout);
#line 58
  type.size = (int )size;
#line 39
  fprintf(_coverage_fout, "844\n");
#line 39
  fflush(_coverage_fout);
#line 58
  type.name = (struct _VySymbol *)name;
#line 39
  fprintf(_coverage_fout, "845\n");
#line 39
  fflush(_coverage_fout);
#line 59
  tmp___4 = WrapObj(value, type);
#line 39
  fprintf(_coverage_fout, "846\n");
#line 39
  fflush(_coverage_fout);
#line 59
  obj = tmp___4;
#line 39
  fprintf(_coverage_fout, "847\n");
#line 39
  fflush(_coverage_fout);
#line 60
  return (obj);
}
}
#line 64 "vm/Scope.c"
void VariableBind(VySymbol *symb , VyObj obj ) 
{ Scope *current ;
  Scope *tmp ;
  void *tmp___0 ;
  VyType tmp___1 ;
  VyType tmp___2 ;

  {
#line 64
  fprintf(_coverage_fout, "848\n");
#line 64
  fflush(_coverage_fout);
#line 65
  tmp = CurrentScope();
#line 64
  fprintf(_coverage_fout, "849\n");
#line 64
  fflush(_coverage_fout);
#line 65
  current = tmp;
#line 64
  fprintf(_coverage_fout, "850\n");
#line 64
  fflush(_coverage_fout);
#line 66
  tmp___0 = Obj(obj);
#line 64
  fprintf(_coverage_fout, "851\n");
#line 64
  fflush(_coverage_fout);
#line 66
  g_hash_table_insert(current->var_values, (void *)symb->symb, tmp___0);
#line 64
  fprintf(_coverage_fout, "852\n");
#line 64
  fflush(_coverage_fout);
#line 67
  tmp___1 = Type(obj);
#line 64
  fprintf(_coverage_fout, "853\n");
#line 64
  fflush(_coverage_fout);
#line 67
  g_hash_table_insert(current->type_values, (void *)symb->symb,
                      (void *)tmp___1.name);
#line 64
  fprintf(_coverage_fout, "854\n");
#line 64
  fflush(_coverage_fout);
#line 68
  tmp___2 = Type(obj);
#line 64
  fprintf(_coverage_fout, "855\n");
#line 64
  fflush(_coverage_fout);
#line 68
  g_hash_table_insert(current->size_values, (void *)symb->symb,
                      (void *)tmp___2.size);
#line 64
  fprintf(_coverage_fout, "856\n");
#line 64
  fflush(_coverage_fout);
#line 69
  return;
}
}
#line 71 "vm/Scope.c"
void EnterScope(Scope *scope ) 
{ 

  {
#line 71
  fprintf(_coverage_fout, "857\n");
#line 71
  fflush(_coverage_fout);
#line 72
  current_scope = scope;
#line 71
  fprintf(_coverage_fout, "858\n");
#line 71
  fflush(_coverage_fout);
#line 73
  return;
}
}
#line 74 "vm/Scope.c"
void DeleteScope(Scope *scope ) 
{ 

  {
#line 74
  fprintf(_coverage_fout, "859\n");
#line 74
  fflush(_coverage_fout);
#line 75
  g_hash_table_destroy(scope->var_values);
#line 74
  fprintf(_coverage_fout, "860\n");
#line 74
  fflush(_coverage_fout);
#line 76
  g_hash_table_destroy(scope->type_values);
#line 74
  fprintf(_coverage_fout, "861\n");
#line 74
  fflush(_coverage_fout);
#line 77
  g_hash_table_destroy(scope->size_values);
#line 74
  fprintf(_coverage_fout, "862\n");
#line 74
  fflush(_coverage_fout);
#line 78
  VyFree((void *)scope);
#line 74
  fprintf(_coverage_fout, "863\n");
#line 74
  fflush(_coverage_fout);
#line 79
  return;
}
}
#line 1 "External.o"
/* #pragma merger(0,"/tmp/cil-qUeMMPki.i","-Wall,-g") */
#line 13 "include/External.h"
void NewFunction(char *name , char *argument_list ,
                 VyObj (*function)(VyObj * , int  ) ) ;
#line 4 "external/External.c"
void NewFunction(char *name , char *argument_list ,
                 VyObj (*function)(VyObj * , int  ) ) 
{ VySymbol *symbol_name ;
  VyObj tmp ;
  void *tmp___0 ;
  TokenList *tokens ;
  TokenList *tmp___1 ;
  TokenList *end ;
  VyObj parsed ;
  VyObj tmp___2 ;
  ArgList arguments ;
  ArgList tmp___3 ;
  VyFunction *func ;
  VyFunction *tmp___4 ;
  VyObj func_obj ;
  VyObj tmp___5 ;

  {
#line 4
  fprintf(_coverage_fout, "864\n");
#line 4
  fflush(_coverage_fout);
#line 6
  tmp = CreateSymbol(name);
#line 4
  fprintf(_coverage_fout, "865\n");
#line 4
  fflush(_coverage_fout);
#line 6
  tmp___0 = Obj(tmp);
#line 4
  fprintf(_coverage_fout, "866\n");
#line 4
  fflush(_coverage_fout);
#line 6
  symbol_name = (VySymbol *)tmp___0;
#line 4
  fprintf(_coverage_fout, "867\n");
#line 4
  fflush(_coverage_fout);
#line 9
  tmp___1 = LexString(argument_list);
#line 4
  fprintf(_coverage_fout, "868\n");
#line 4
  fflush(_coverage_fout);
#line 9
  tokens = tmp___1;
#line 4
  fprintf(_coverage_fout, "869\n");
#line 4
  fflush(_coverage_fout);
#line 11
  tmp___2 = Parse(tokens, & end);
#line 4
  fprintf(_coverage_fout, "870\n");
#line 4
  fflush(_coverage_fout);
#line 11
  parsed = tmp___2;
#line 4
  fprintf(_coverage_fout, "871\n");
#line 4
  fflush(_coverage_fout);
#line 12
  tmp___3 = ParseArgList(parsed);
#line 4
  fprintf(_coverage_fout, "872\n");
#line 4
  fflush(_coverage_fout);
#line 12
  arguments = tmp___3;
#line 4
  fprintf(_coverage_fout, "873\n");
#line 4
  fflush(_coverage_fout);
#line 13
  FreeTokens(tokens);
#line 4
  fprintf(_coverage_fout, "874\n");
#line 4
  fflush(_coverage_fout);
#line 16
  tmp___4 = CreateNativeFunction(arguments, function);
#line 4
  fprintf(_coverage_fout, "875\n");
#line 4
  fflush(_coverage_fout);
#line 16
  func = tmp___4;
#line 4
  fprintf(_coverage_fout, "876\n");
#line 4
  fflush(_coverage_fout);
#line 17
  tmp___5 = WrapObj((void *)func, TypeFunction);
#line 4
  fprintf(_coverage_fout, "877\n");
#line 4
  fflush(_coverage_fout);
#line 17
  func_obj = tmp___5;
#line 4
  fprintf(_coverage_fout, "878\n");
#line 4
  fflush(_coverage_fout);
#line 20
  VariableBind(symbol_name, func_obj);
#line 4
  fprintf(_coverage_fout, "879\n");
#line 4
  fflush(_coverage_fout);
#line 21
  return;
}
}
#line 1 "Lib.o"
/* #pragma merger(0,"/tmp/cil-2Uj2pJDK.i","-Wall,-g") */
#line 145 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) double atof(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 38 "include/External.h"
VyObj CreateInt(int d ) ;
#line 39
VyObj CreateFloat(double d ) ;
#line 4 "external/Lib.c"
void LoadIO(void) ;
#line 5
void LoadMath(void) ;
#line 6
void LoadList(void) ;
#line 7
void LoadBool(void) ;
#line 12 "external/Lib.c"
VyObj CreateFloat(double d ) 
{ VyFloat *flt ;
  void *tmp ;
  VyObj tmp___0 ;

  {
#line 12
  fprintf(_coverage_fout, "880\n");
#line 12
  fflush(_coverage_fout);
#line 13
  tmp = VyMalloc((unsigned long )TypeFloat.size);
#line 12
  fprintf(_coverage_fout, "881\n");
#line 12
  fflush(_coverage_fout);
#line 13
  flt = (VyFloat *)tmp;
#line 12
  fprintf(_coverage_fout, "882\n");
#line 12
  fflush(_coverage_fout);
#line 14
  flt->val = d;
#line 12
  fprintf(_coverage_fout, "883\n");
#line 12
  fflush(_coverage_fout);
#line 15
  tmp___0 = WrapObj((void *)flt, TypeFloat);
#line 12
  fprintf(_coverage_fout, "884\n");
#line 12
  fflush(_coverage_fout);
#line 15
  return (tmp___0);
}
}
#line 17 "external/Lib.c"
VyObj CreateInt(int d ) 
{ VyInt *i ;
  void *tmp ;
  VyObj tmp___0 ;

  {
#line 17
  fprintf(_coverage_fout, "885\n");
#line 17
  fflush(_coverage_fout);
#line 18
  tmp = VyMalloc((unsigned long )TypeInt.size);
#line 17
  fprintf(_coverage_fout, "886\n");
#line 17
  fflush(_coverage_fout);
#line 18
  i = (VyInt *)tmp;
#line 17
  fprintf(_coverage_fout, "887\n");
#line 17
  fflush(_coverage_fout);
#line 19
  i->val = d;
#line 17
  fprintf(_coverage_fout, "888\n");
#line 17
  fflush(_coverage_fout);
#line 20
  tmp___0 = WrapObj((void *)i, TypeInt);
#line 17
  fprintf(_coverage_fout, "889\n");
#line 17
  fflush(_coverage_fout);
#line 20
  return (tmp___0);
}
}
#line 23 "external/Lib.c"
VyObj CreateFloatFromStr(char *str ) 
{ double tmp ;
  VyObj tmp___0 ;

  {
#line 23
  fprintf(_coverage_fout, "890\n");
#line 23
  fflush(_coverage_fout);
#line 24
  tmp = atof((char const   *)str);
#line 23
  fprintf(_coverage_fout, "891\n");
#line 23
  fflush(_coverage_fout);
#line 24
  tmp___0 = CreateFloat(tmp);
#line 23
  fprintf(_coverage_fout, "892\n");
#line 23
  fflush(_coverage_fout);
#line 24
  return (tmp___0);
}
}
#line 26 "external/Lib.c"
VyObj CreateIntFromStr(char *str ) 
{ int tmp ;
  VyObj tmp___0 ;

  {
#line 26
  fprintf(_coverage_fout, "893\n");
#line 26
  fflush(_coverage_fout);
#line 27
  tmp = atoi((char const   *)str);
#line 26
  fprintf(_coverage_fout, "894\n");
#line 26
  fflush(_coverage_fout);
#line 27
  tmp___0 = CreateInt(tmp);
#line 26
  fprintf(_coverage_fout, "895\n");
#line 26
  fflush(_coverage_fout);
#line 27
  return (tmp___0);
}
}
#line 31 "external/Lib.c"
void CreateTypes(void) 
{ VySymbol *tmp ;
  VySymbol *tmp___0 ;
  VySymbol *tmp___1 ;
  VySymbol *tmp___2 ;
  VySymbol *tmp___3 ;
  VySymbol *tmp___4 ;

  {
#line 31
  fprintf(_coverage_fout, "896\n");
#line 31
  fflush(_coverage_fout);
#line 32
  TypeNone = CreateType(0, (struct _VySymbol *)((void *)0));
#line 31
  fprintf(_coverage_fout, "897\n");
#line 31
  fflush(_coverage_fout);
#line 33
  tmp = CreateSymbol_NoObj((char *)"cons");
#line 31
  fprintf(_coverage_fout, "898\n");
#line 31
  fflush(_coverage_fout);
#line 33
  TypeCons = CreateType((int )sizeof(VyCons ), tmp);
#line 31
  fprintf(_coverage_fout, "899\n");
#line 31
  fflush(_coverage_fout);
#line 34
  tmp___0 = CreateSymbol_NoObj((char *)"string");
#line 31
  fprintf(_coverage_fout, "900\n");
#line 31
  fflush(_coverage_fout);
#line 34
  TypeString = CreateType((int )sizeof(VyString ), tmp___0);
#line 31
  fprintf(_coverage_fout, "901\n");
#line 31
  fflush(_coverage_fout);
#line 35
  tmp___1 = CreateSymbol_NoObj((char *)"symbol");
#line 31
  fprintf(_coverage_fout, "902\n");
#line 31
  fflush(_coverage_fout);
#line 35
  TypeSymbol = CreateType((int )sizeof(VySymbol ), tmp___1);
#line 31
  fprintf(_coverage_fout, "903\n");
#line 31
  fflush(_coverage_fout);
#line 36
  tmp___2 = CreateSymbol_NoObj((char *)"float");
#line 31
  fprintf(_coverage_fout, "904\n");
#line 31
  fflush(_coverage_fout);
#line 36
  TypeFloat = CreateType((int )sizeof(VyFloat ), tmp___2);
#line 31
  fprintf(_coverage_fout, "905\n");
#line 31
  fflush(_coverage_fout);
#line 37
  tmp___3 = CreateSymbol_NoObj((char *)"int");
#line 31
  fprintf(_coverage_fout, "906\n");
#line 31
  fflush(_coverage_fout);
#line 37
  TypeInt = CreateType((int )sizeof(VyInt ), tmp___3);
#line 31
  fprintf(_coverage_fout, "907\n");
#line 31
  fflush(_coverage_fout);
#line 38
  tmp___4 = CreateSymbol_NoObj((char *)"function");
#line 31
  fprintf(_coverage_fout, "908\n");
#line 31
  fflush(_coverage_fout);
#line 38
  TypeFunction = CreateType((int )sizeof(VyFunction ), tmp___4);
#line 31
  fprintf(_coverage_fout, "909\n");
#line 31
  fflush(_coverage_fout);
#line 40
  return;
}
}
#line 44 "external/Lib.c"
void CreateSymbols(void) 
{ 

  {
#line 44
  fprintf(_coverage_fout, "910\n");
#line 44
  fflush(_coverage_fout);
#line 45
  SymbolFn = CreateSymbol((char *)"fn");
#line 44
  fprintf(_coverage_fout, "911\n");
#line 44
  fflush(_coverage_fout);
#line 46
  SymbolFalse = CreateSymbol((char *)"false");
#line 44
  fprintf(_coverage_fout, "912\n");
#line 44
  fflush(_coverage_fout);
#line 47
  SymbolIf = CreateSymbol((char *)"if");
#line 44
  fprintf(_coverage_fout, "913\n");
#line 44
  fflush(_coverage_fout);
#line 48
  SymbolSetvar = CreateSymbol((char *)"setvar");
#line 44
  fprintf(_coverage_fout, "914\n");
#line 44
  fflush(_coverage_fout);
#line 49
  SymbolWhile = CreateSymbol((char *)"while");
#line 44
  fprintf(_coverage_fout, "915\n");
#line 44
  fflush(_coverage_fout);
#line 50
  SymbolNil = CreateSymbol((char *)"nil");
#line 44
  fprintf(_coverage_fout, "916\n");
#line 44
  fflush(_coverage_fout);
#line 51
  SymbolQuote = CreateSymbol((char *)"quote");
#line 44
  fprintf(_coverage_fout, "917\n");
#line 44
  fflush(_coverage_fout);
#line 52
  return;
}
}
#line 55 "external/Lib.c"
void LoadCoreLibrary(void) 
{ 

  {
#line 55
  fprintf(_coverage_fout, "918\n");
#line 55
  fflush(_coverage_fout);
#line 56
  CreateTypes();
#line 55
  fprintf(_coverage_fout, "919\n");
#line 55
  fflush(_coverage_fout);
#line 58
  CreateSymbols();
#line 55
  fprintf(_coverage_fout, "920\n");
#line 55
  fflush(_coverage_fout);
#line 60
  LoadMath();
#line 55
  fprintf(_coverage_fout, "921\n");
#line 55
  fflush(_coverage_fout);
#line 61
  LoadBool();
#line 55
  fprintf(_coverage_fout, "922\n");
#line 55
  fflush(_coverage_fout);
#line 62
  LoadList();
#line 55
  fprintf(_coverage_fout, "923\n");
#line 55
  fflush(_coverage_fout);
#line 63
  LoadIO();
#line 55
  fprintf(_coverage_fout, "924\n");
#line 55
  fflush(_coverage_fout);
#line 64
  return;
}
}
#line 1 "IO.o"
/* #pragma merger(0,"/tmp/cil-uXu87rXN.i","-Wall,-g") */
#line 3 "external/IO.c"
VyObj PrintObjFun(VyObj *args , int num_args ) 
{ 

  {
#line 3
  fprintf(_coverage_fout, "925\n");
#line 3
  fflush(_coverage_fout);
#line 4
  PrintObj(stdout, *(args + 0));
#line 3
  fprintf(_coverage_fout, "926\n");
#line 3
  fflush(_coverage_fout);
#line 5
  printf((char const   */* __restrict  */)" ");
#line 3
  fprintf(_coverage_fout, "927\n");
#line 3
  fflush(_coverage_fout);
#line 6
  fflush(stdout);
#line 3
  fprintf(_coverage_fout, "928\n");
#line 3
  fflush(_coverage_fout);
#line 7
  return (*(args + 0));
}
}
#line 11 "external/IO.c"
void LoadIO(void) 
{ 

  {
#line 11
  fprintf(_coverage_fout, "929\n");
#line 11
  fflush(_coverage_fout);
#line 12
  NewFunction((char *)"print", (char *)"(a)", & PrintObjFun);
#line 11
  fprintf(_coverage_fout, "930\n");
#line 11
  fflush(_coverage_fout);
#line 13
  return;
}
}
#line 1 "Math.o"
/* #pragma merger(0,"/tmp/cil-noPKtXYJ.i","-Wall,-g") */
#line 16 "include/External.h"
VyObj TrueObj(void) ;
#line 17
VyObj FalseObj(void) ;
#line 42
bool NumEq(VyObj one , VyObj two ) ;
#line 4 "external/Math.c"
VyObj IsInt(VyObj *objs , int num ) 
{ bool is_int ;
  bool tmp ;
  VyObj tmp___2 ;

  {
#line 4
  fprintf(_coverage_fout, "933\n");
#line 4
  fflush(_coverage_fout);
#line 5
  tmp = IsType(*(objs + 0), TypeInt);
#line 4
  fprintf(_coverage_fout, "934\n");
#line 4
  fflush(_coverage_fout);
#line 5
  is_int = tmp;
#line 4
  fprintf(_coverage_fout, "935\n");
#line 4
  fflush(_coverage_fout);
#line 6
  if (is_int) {
#line 6
    fprintf(_coverage_fout, "931\n");
#line 6
    fflush(_coverage_fout);
#line 6
    tmp___2 = TrueObj();
  } else {
#line 6
    fprintf(_coverage_fout, "932\n");
#line 6
    fflush(_coverage_fout);
#line 6
    tmp___2 = FalseObj();
  }
#line 4
  fprintf(_coverage_fout, "936\n");
#line 4
  fflush(_coverage_fout);
#line 6
  return (tmp___2);
}
}
#line 8 "external/Math.c"
VyObj IsFloat(VyObj *objs , int num ) 
{ bool is_float ;
  bool tmp ;
  VyObj tmp___2 ;

  {
#line 8
  fprintf(_coverage_fout, "939\n");
#line 8
  fflush(_coverage_fout);
#line 9
  tmp = IsType(*(objs + 0), TypeFloat);
#line 8
  fprintf(_coverage_fout, "940\n");
#line 8
  fflush(_coverage_fout);
#line 9
  is_float = tmp;
#line 8
  fprintf(_coverage_fout, "941\n");
#line 8
  fflush(_coverage_fout);
#line 10
  if (is_float) {
#line 10
    fprintf(_coverage_fout, "937\n");
#line 10
    fflush(_coverage_fout);
#line 10
    tmp___2 = TrueObj();
  } else {
#line 10
    fprintf(_coverage_fout, "938\n");
#line 10
    fflush(_coverage_fout);
#line 10
    tmp___2 = FalseObj();
  }
#line 8
  fprintf(_coverage_fout, "942\n");
#line 8
  fflush(_coverage_fout);
#line 10
  return (tmp___2);
}
}
#line 14 "external/Math.c"
VyObj AddFun(VyObj *objs , int num ) 
{ double sum ;
  bool integral ;
  int i ;
  void *tmp ;
  bool tmp___0 ;
  void *tmp___1 ;
  bool tmp___2 ;
  VyObj tmp___3 ;
  VyObj tmp___4 ;

  {
#line 14
  fprintf(_coverage_fout, "961\n");
#line 14
  fflush(_coverage_fout);
#line 15
  sum = (double )0;
#line 14
  fprintf(_coverage_fout, "962\n");
#line 14
  fflush(_coverage_fout);
#line 16
  integral = 1;
#line 14
  fprintf(_coverage_fout, "963\n");
#line 14
  fflush(_coverage_fout);
#line 18
  i = 0;
#line 14
  fprintf(_coverage_fout, "964\n");
#line 14
  fflush(_coverage_fout);
#line 18
  while (1) {
#line 18
    fprintf(_coverage_fout, "951\n");
#line 18
    fflush(_coverage_fout);
#line 18
    if (i < num) {
#line 18
      fprintf(_coverage_fout, "943\n");
#line 18
      fflush(_coverage_fout);

    } else {
#line 18
      break;
    }
#line 18
    fprintf(_coverage_fout, "952\n");
#line 18
    fflush(_coverage_fout);
#line 19
    tmp___0 = IsType(*(objs + i), TypeInt);
#line 18
    fprintf(_coverage_fout, "953\n");
#line 18
    fflush(_coverage_fout);
#line 19
    if (tmp___0) {
#line 19
      fprintf(_coverage_fout, "944\n");
#line 19
      fflush(_coverage_fout);
#line 20
      tmp = Obj(*(objs + i));
#line 19
      fprintf(_coverage_fout, "945\n");
#line 19
      fflush(_coverage_fout);
#line 20
      sum += (double )((VyInt *)tmp)->val;
    } else {
#line 19
      fprintf(_coverage_fout, "946\n");
#line 19
      fflush(_coverage_fout);

    }
#line 18
    fprintf(_coverage_fout, "954\n");
#line 18
    fflush(_coverage_fout);
#line 21
    tmp___2 = IsType(*(objs + i), TypeFloat);
#line 18
    fprintf(_coverage_fout, "955\n");
#line 18
    fflush(_coverage_fout);
#line 21
    if (tmp___2) {
#line 21
      fprintf(_coverage_fout, "947\n");
#line 21
      fflush(_coverage_fout);
#line 22
      tmp___1 = Obj(*(objs + i));
#line 21
      fprintf(_coverage_fout, "948\n");
#line 21
      fflush(_coverage_fout);
#line 22
      sum += ((VyFloat *)tmp___1)->val;
#line 21
      fprintf(_coverage_fout, "949\n");
#line 21
      fflush(_coverage_fout);
#line 23
      integral = 0;
    } else {
#line 21
      fprintf(_coverage_fout, "950\n");
#line 21
      fflush(_coverage_fout);

    }
#line 18
    fprintf(_coverage_fout, "956\n");
#line 18
    fflush(_coverage_fout);
#line 18
    i ++;
  }
#line 14
  fprintf(_coverage_fout, "965\n");
#line 14
  fflush(_coverage_fout);
#line 27
  if (integral) {
#line 27
    fprintf(_coverage_fout, "957\n");
#line 27
    fflush(_coverage_fout);
#line 28
    tmp___3 = CreateInt((int )sum);
#line 27
    fprintf(_coverage_fout, "958\n");
#line 27
    fflush(_coverage_fout);
#line 28
    return (tmp___3);
  } else {
#line 27
    fprintf(_coverage_fout, "959\n");
#line 27
    fflush(_coverage_fout);
#line 30
    tmp___4 = CreateFloat(sum);
#line 27
    fprintf(_coverage_fout, "960\n");
#line 27
    fflush(_coverage_fout);
#line 30
    return (tmp___4);
  }
}
}
#line 32 "external/Math.c"
VyObj SubFun(VyObj *objs , int num ) 
{ VyObj tmp ;
  void *tmp___0 ;
  bool tmp___1 ;
  void *tmp___2 ;
  bool tmp___3 ;
  double sum ;
  void *tmp___4 ;
  bool tmp___5 ;
  void *tmp___6 ;
  bool tmp___7 ;
  bool integral ;
  bool tmp___8 ;
  int i ;
  void *tmp___9 ;
  bool tmp___10 ;
  void *tmp___11 ;
  bool tmp___12 ;
  VyObj tmp___13 ;
  VyObj tmp___14 ;

  {
#line 32
  fprintf(_coverage_fout, "1005\n");
#line 32
  fflush(_coverage_fout);
#line 34
  if (num == 0) {
#line 34
    fprintf(_coverage_fout, "966\n");
#line 34
    fflush(_coverage_fout);
#line 35
    tmp = CreateInt(0);
#line 34
    fprintf(_coverage_fout, "967\n");
#line 34
    fflush(_coverage_fout);
#line 35
    return (tmp);
  } else {
#line 34
    fprintf(_coverage_fout, "968\n");
#line 34
    fflush(_coverage_fout);

  }
#line 32
  fprintf(_coverage_fout, "1006\n");
#line 32
  fflush(_coverage_fout);
#line 39
  if (num == 1) {
#line 39
    fprintf(_coverage_fout, "975\n");
#line 39
    fflush(_coverage_fout);
#line 40
    tmp___1 = IsType(*(objs + 0), TypeInt);
#line 39
    fprintf(_coverage_fout, "976\n");
#line 39
    fflush(_coverage_fout);
#line 40
    if (tmp___1) {
#line 40
      fprintf(_coverage_fout, "969\n");
#line 40
      fflush(_coverage_fout);
#line 41
      tmp___0 = Obj(*(objs + 0));
#line 40
      fprintf(_coverage_fout, "970\n");
#line 40
      fflush(_coverage_fout);
#line 41
      ((VyInt *)tmp___0)->val *= -1;
    } else {
#line 40
      fprintf(_coverage_fout, "971\n");
#line 40
      fflush(_coverage_fout);

    }
#line 39
    fprintf(_coverage_fout, "977\n");
#line 39
    fflush(_coverage_fout);
#line 42
    tmp___3 = IsType(*(objs + 0), TypeFloat);
#line 39
    fprintf(_coverage_fout, "978\n");
#line 39
    fflush(_coverage_fout);
#line 42
    if (tmp___3) {
#line 42
      fprintf(_coverage_fout, "972\n");
#line 42
      fflush(_coverage_fout);
#line 43
      tmp___2 = Obj(*(objs + 0));
#line 42
      fprintf(_coverage_fout, "973\n");
#line 42
      fflush(_coverage_fout);
#line 43
      ((VyFloat *)tmp___2)->val *= (double )-1;
    } else {
#line 42
      fprintf(_coverage_fout, "974\n");
#line 42
      fflush(_coverage_fout);

    }
#line 39
    fprintf(_coverage_fout, "979\n");
#line 39
    fflush(_coverage_fout);
#line 45
    return (*(objs + 0));
  } else {
#line 39
    fprintf(_coverage_fout, "980\n");
#line 39
    fflush(_coverage_fout);

  }
#line 32
  fprintf(_coverage_fout, "1007\n");
#line 32
  fflush(_coverage_fout);
#line 49
  sum = (double )0;
#line 32
  fprintf(_coverage_fout, "1008\n");
#line 32
  fflush(_coverage_fout);
#line 50
  tmp___5 = IsType(*(objs + 0), TypeInt);
#line 32
  fprintf(_coverage_fout, "1009\n");
#line 32
  fflush(_coverage_fout);
#line 50
  if (tmp___5) {
#line 50
    fprintf(_coverage_fout, "981\n");
#line 50
    fflush(_coverage_fout);
#line 51
    tmp___4 = Obj(*(objs + 0));
#line 50
    fprintf(_coverage_fout, "982\n");
#line 50
    fflush(_coverage_fout);
#line 51
    sum = (double )((VyInt *)tmp___4)->val;
  } else {
#line 50
    fprintf(_coverage_fout, "983\n");
#line 50
    fflush(_coverage_fout);

  }
#line 32
  fprintf(_coverage_fout, "1010\n");
#line 32
  fflush(_coverage_fout);
#line 52
  tmp___7 = IsType(*(objs + 0), TypeFloat);
#line 32
  fprintf(_coverage_fout, "1011\n");
#line 32
  fflush(_coverage_fout);
#line 52
  if (tmp___7) {
#line 52
    fprintf(_coverage_fout, "984\n");
#line 52
    fflush(_coverage_fout);
#line 53
    tmp___6 = Obj(*(objs + 0));
#line 52
    fprintf(_coverage_fout, "985\n");
#line 52
    fflush(_coverage_fout);
#line 53
    sum = ((VyFloat *)tmp___6)->val;
  } else {
#line 52
    fprintf(_coverage_fout, "986\n");
#line 52
    fflush(_coverage_fout);

  }
#line 32
  fprintf(_coverage_fout, "1012\n");
#line 32
  fflush(_coverage_fout);
#line 55
  tmp___8 = IsType(*(objs + 0), TypeInt);
#line 32
  fprintf(_coverage_fout, "1013\n");
#line 32
  fflush(_coverage_fout);
#line 55
  integral = tmp___8;
#line 32
  fprintf(_coverage_fout, "1014\n");
#line 32
  fflush(_coverage_fout);
#line 57
  i = 1;
#line 32
  fprintf(_coverage_fout, "1015\n");
#line 32
  fflush(_coverage_fout);
#line 57
  while (1) {
#line 57
    fprintf(_coverage_fout, "995\n");
#line 57
    fflush(_coverage_fout);
#line 57
    if (i < num) {
#line 57
      fprintf(_coverage_fout, "987\n");
#line 57
      fflush(_coverage_fout);

    } else {
#line 57
      break;
    }
#line 57
    fprintf(_coverage_fout, "996\n");
#line 57
    fflush(_coverage_fout);
#line 58
    tmp___10 = IsType(*(objs + i), TypeInt);
#line 57
    fprintf(_coverage_fout, "997\n");
#line 57
    fflush(_coverage_fout);
#line 58
    if (tmp___10) {
#line 58
      fprintf(_coverage_fout, "988\n");
#line 58
      fflush(_coverage_fout);
#line 59
      tmp___9 = Obj(*(objs + i));
#line 58
      fprintf(_coverage_fout, "989\n");
#line 58
      fflush(_coverage_fout);
#line 59
      sum -= (double )((VyInt *)tmp___9)->val;
    } else {
#line 58
      fprintf(_coverage_fout, "990\n");
#line 58
      fflush(_coverage_fout);

    }
#line 57
    fprintf(_coverage_fout, "998\n");
#line 57
    fflush(_coverage_fout);
#line 60
    tmp___12 = IsType(*(objs + i), TypeFloat);
#line 57
    fprintf(_coverage_fout, "999\n");
#line 57
    fflush(_coverage_fout);
#line 60
    if (tmp___12) {
#line 60
      fprintf(_coverage_fout, "991\n");
#line 60
      fflush(_coverage_fout);
#line 61
      tmp___11 = Obj(*(objs + i));
#line 60
      fprintf(_coverage_fout, "992\n");
#line 60
      fflush(_coverage_fout);
#line 61
      sum -= ((VyFloat *)tmp___11)->val;
#line 60
      fprintf(_coverage_fout, "993\n");
#line 60
      fflush(_coverage_fout);
#line 62
      integral = 0;
    } else {
#line 60
      fprintf(_coverage_fout, "994\n");
#line 60
      fflush(_coverage_fout);

    }
#line 57
    fprintf(_coverage_fout, "1000\n");
#line 57
    fflush(_coverage_fout);
#line 57
    i ++;
  }
#line 32
  fprintf(_coverage_fout, "1016\n");
#line 32
  fflush(_coverage_fout);
#line 66
  if (integral) {
#line 66
    fprintf(_coverage_fout, "1001\n");
#line 66
    fflush(_coverage_fout);
#line 67
    tmp___13 = CreateInt((int )sum);
#line 66
    fprintf(_coverage_fout, "1002\n");
#line 66
    fflush(_coverage_fout);
#line 67
    return (tmp___13);
  } else {
#line 66
    fprintf(_coverage_fout, "1003\n");
#line 66
    fflush(_coverage_fout);
#line 69
    tmp___14 = CreateFloat(sum);
#line 66
    fprintf(_coverage_fout, "1004\n");
#line 66
    fflush(_coverage_fout);
#line 69
    return (tmp___14);
  }
}
}
#line 71 "external/Math.c"
VyObj MulFun(VyObj *objs , int num ) 
{ double product ;
  bool integral ;
  int i ;
  void *tmp ;
  bool tmp___0 ;
  void *tmp___1 ;
  bool tmp___2 ;
  VyObj tmp___3 ;
  VyObj tmp___4 ;

  {
#line 71
  fprintf(_coverage_fout, "1035\n");
#line 71
  fflush(_coverage_fout);
#line 72
  product = (double )1;
#line 71
  fprintf(_coverage_fout, "1036\n");
#line 71
  fflush(_coverage_fout);
#line 73
  integral = 1;
#line 71
  fprintf(_coverage_fout, "1037\n");
#line 71
  fflush(_coverage_fout);
#line 75
  i = 0;
#line 71
  fprintf(_coverage_fout, "1038\n");
#line 71
  fflush(_coverage_fout);
#line 75
  while (1) {
#line 75
    fprintf(_coverage_fout, "1025\n");
#line 75
    fflush(_coverage_fout);
#line 75
    if (i < num) {
#line 75
      fprintf(_coverage_fout, "1017\n");
#line 75
      fflush(_coverage_fout);

    } else {
#line 75
      break;
    }
#line 75
    fprintf(_coverage_fout, "1026\n");
#line 75
    fflush(_coverage_fout);
#line 76
    tmp___0 = IsType(*(objs + i), TypeInt);
#line 75
    fprintf(_coverage_fout, "1027\n");
#line 75
    fflush(_coverage_fout);
#line 76
    if (tmp___0) {
#line 76
      fprintf(_coverage_fout, "1018\n");
#line 76
      fflush(_coverage_fout);
#line 77
      tmp = Obj(*(objs + i));
#line 76
      fprintf(_coverage_fout, "1019\n");
#line 76
      fflush(_coverage_fout);
#line 77
      product *= (double )((VyInt *)tmp)->val;
    } else {
#line 76
      fprintf(_coverage_fout, "1020\n");
#line 76
      fflush(_coverage_fout);

    }
#line 75
    fprintf(_coverage_fout, "1028\n");
#line 75
    fflush(_coverage_fout);
#line 78
    tmp___2 = IsType(*(objs + i), TypeFloat);
#line 75
    fprintf(_coverage_fout, "1029\n");
#line 75
    fflush(_coverage_fout);
#line 78
    if (tmp___2) {
#line 78
      fprintf(_coverage_fout, "1021\n");
#line 78
      fflush(_coverage_fout);
#line 79
      tmp___1 = Obj(*(objs + i));
#line 78
      fprintf(_coverage_fout, "1022\n");
#line 78
      fflush(_coverage_fout);
#line 79
      product *= ((VyFloat *)tmp___1)->val;
#line 78
      fprintf(_coverage_fout, "1023\n");
#line 78
      fflush(_coverage_fout);
#line 80
      integral = 0;
    } else {
#line 78
      fprintf(_coverage_fout, "1024\n");
#line 78
      fflush(_coverage_fout);

    }
#line 75
    fprintf(_coverage_fout, "1030\n");
#line 75
    fflush(_coverage_fout);
#line 75
    i ++;
  }
#line 71
  fprintf(_coverage_fout, "1039\n");
#line 71
  fflush(_coverage_fout);
#line 84
  if (integral) {
#line 84
    fprintf(_coverage_fout, "1031\n");
#line 84
    fflush(_coverage_fout);
#line 85
    tmp___3 = CreateInt((int )product);
#line 84
    fprintf(_coverage_fout, "1032\n");
#line 84
    fflush(_coverage_fout);
#line 85
    return (tmp___3);
  } else {
#line 84
    fprintf(_coverage_fout, "1033\n");
#line 84
    fflush(_coverage_fout);
#line 87
    tmp___4 = CreateFloat(product);
#line 84
    fprintf(_coverage_fout, "1034\n");
#line 84
    fflush(_coverage_fout);
#line 87
    return (tmp___4);
  }
}
}
#line 89 "external/Math.c"
VyObj DivFun(VyObj *objs , int num ) 
{ float num_one ;
  void *tmp ;
  bool tmp___0 ;
  void *tmp___1 ;
  bool tmp___2 ;
  float num_two ;
  void *tmp___3 ;
  bool tmp___4 ;
  void *tmp___5 ;
  bool tmp___6 ;
  VyObj tmp___7 ;

  {
#line 89
  fprintf(_coverage_fout, "1052\n");
#line 89
  fflush(_coverage_fout);
#line 91
  tmp___0 = IsType(*(objs + 0), TypeInt);
#line 89
  fprintf(_coverage_fout, "1053\n");
#line 89
  fflush(_coverage_fout);
#line 91
  if (tmp___0) {
#line 91
    fprintf(_coverage_fout, "1040\n");
#line 91
    fflush(_coverage_fout);
#line 92
    tmp = Obj(*(objs + 0));
#line 91
    fprintf(_coverage_fout, "1041\n");
#line 91
    fflush(_coverage_fout);
#line 92
    num_one = (float )((VyInt *)tmp)->val;
  } else {
#line 91
    fprintf(_coverage_fout, "1042\n");
#line 91
    fflush(_coverage_fout);

  }
#line 89
  fprintf(_coverage_fout, "1054\n");
#line 89
  fflush(_coverage_fout);
#line 93
  tmp___2 = IsType(*(objs + 0), TypeFloat);
#line 89
  fprintf(_coverage_fout, "1055\n");
#line 89
  fflush(_coverage_fout);
#line 93
  if (tmp___2) {
#line 93
    fprintf(_coverage_fout, "1043\n");
#line 93
    fflush(_coverage_fout);
#line 94
    tmp___1 = Obj(*(objs + 0));
#line 93
    fprintf(_coverage_fout, "1044\n");
#line 93
    fflush(_coverage_fout);
#line 94
    num_one = (float )((VyFloat *)tmp___1)->val;
  } else {
#line 93
    fprintf(_coverage_fout, "1045\n");
#line 93
    fflush(_coverage_fout);

  }
#line 89
  fprintf(_coverage_fout, "1056\n");
#line 89
  fflush(_coverage_fout);
#line 97
  tmp___4 = IsType(*(objs + 1), TypeInt);
#line 89
  fprintf(_coverage_fout, "1057\n");
#line 89
  fflush(_coverage_fout);
#line 97
  if (tmp___4) {
#line 97
    fprintf(_coverage_fout, "1046\n");
#line 97
    fflush(_coverage_fout);
#line 98
    tmp___3 = Obj(*(objs + 1));
#line 97
    fprintf(_coverage_fout, "1047\n");
#line 97
    fflush(_coverage_fout);
#line 98
    num_two = (float )((VyInt *)tmp___3)->val;
  } else {
#line 97
    fprintf(_coverage_fout, "1048\n");
#line 97
    fflush(_coverage_fout);

  }
#line 89
  fprintf(_coverage_fout, "1058\n");
#line 89
  fflush(_coverage_fout);
#line 99
  tmp___6 = IsType(*(objs + 1), TypeFloat);
#line 89
  fprintf(_coverage_fout, "1059\n");
#line 89
  fflush(_coverage_fout);
#line 99
  if (tmp___6) {
#line 99
    fprintf(_coverage_fout, "1049\n");
#line 99
    fflush(_coverage_fout);
#line 100
    tmp___5 = Obj(*(objs + 1));
#line 99
    fprintf(_coverage_fout, "1050\n");
#line 99
    fflush(_coverage_fout);
#line 100
    num_two = (float )((VyFloat *)tmp___5)->val;
  } else {
#line 99
    fprintf(_coverage_fout, "1051\n");
#line 99
    fflush(_coverage_fout);

  }
#line 89
  fprintf(_coverage_fout, "1060\n");
#line 89
  fflush(_coverage_fout);
#line 102
  tmp___7 = CreateFloat((double )(num_one / num_two));
#line 89
  fprintf(_coverage_fout, "1061\n");
#line 89
  fflush(_coverage_fout);
#line 102
  return (tmp___7);
}
}
#line 105 "external/Math.c"
bool NumEq(VyObj one , VyObj two ) 
{ float num_one ;
  void *tmp ;
  bool tmp___0 ;
  void *tmp___1 ;
  bool tmp___2 ;
  float num_two ;
  void *tmp___3 ;
  bool tmp___4 ;
  void *tmp___5 ;
  bool tmp___6 ;

  {
#line 105
  fprintf(_coverage_fout, "1074\n");
#line 105
  fflush(_coverage_fout);
#line 107
  tmp___0 = IsType(one, TypeInt);
#line 105
  fprintf(_coverage_fout, "1075\n");
#line 105
  fflush(_coverage_fout);
#line 107
  if (tmp___0) {
#line 107
    fprintf(_coverage_fout, "1062\n");
#line 107
    fflush(_coverage_fout);
#line 108
    tmp = Obj(one);
#line 107
    fprintf(_coverage_fout, "1063\n");
#line 107
    fflush(_coverage_fout);
#line 108
    num_one = (float )((VyInt *)tmp)->val;
  } else {
#line 107
    fprintf(_coverage_fout, "1064\n");
#line 107
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "1076\n");
#line 105
  fflush(_coverage_fout);
#line 109
  tmp___2 = IsType(one, TypeFloat);
#line 105
  fprintf(_coverage_fout, "1077\n");
#line 105
  fflush(_coverage_fout);
#line 109
  if (tmp___2) {
#line 109
    fprintf(_coverage_fout, "1065\n");
#line 109
    fflush(_coverage_fout);
#line 110
    tmp___1 = Obj(one);
#line 109
    fprintf(_coverage_fout, "1066\n");
#line 109
    fflush(_coverage_fout);
#line 110
    num_one = (float )((VyFloat *)tmp___1)->val;
  } else {
#line 109
    fprintf(_coverage_fout, "1067\n");
#line 109
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "1078\n");
#line 105
  fflush(_coverage_fout);
#line 113
  tmp___4 = IsType(two, TypeInt);
#line 105
  fprintf(_coverage_fout, "1079\n");
#line 105
  fflush(_coverage_fout);
#line 113
  if (tmp___4) {
#line 113
    fprintf(_coverage_fout, "1068\n");
#line 113
    fflush(_coverage_fout);
#line 114
    tmp___3 = Obj(two);
#line 113
    fprintf(_coverage_fout, "1069\n");
#line 113
    fflush(_coverage_fout);
#line 114
    num_two = (float )((VyInt *)tmp___3)->val;
  } else {
#line 113
    fprintf(_coverage_fout, "1070\n");
#line 113
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "1080\n");
#line 105
  fflush(_coverage_fout);
#line 115
  tmp___6 = IsType(two, TypeFloat);
#line 105
  fprintf(_coverage_fout, "1081\n");
#line 105
  fflush(_coverage_fout);
#line 115
  if (tmp___6) {
#line 115
    fprintf(_coverage_fout, "1071\n");
#line 115
    fflush(_coverage_fout);
#line 116
    tmp___5 = Obj(two);
#line 115
    fprintf(_coverage_fout, "1072\n");
#line 115
    fflush(_coverage_fout);
#line 116
    num_two = (float )((VyFloat *)tmp___5)->val;
  } else {
#line 115
    fprintf(_coverage_fout, "1073\n");
#line 115
    fflush(_coverage_fout);

  }
#line 105
  fprintf(_coverage_fout, "1082\n");
#line 105
  fflush(_coverage_fout);
#line 118
  return (num_two == num_one);
}
}
#line 121 "external/Math.c"
VyObj NumEqFun(VyObj *objs , int num ) 
{ VyObj first ;
  int i ;
  VyObj tmp ;
  bool tmp___0 ;
  VyObj tmp___1 ;

  {
#line 121
  fprintf(_coverage_fout, "1091\n");
#line 121
  fflush(_coverage_fout);
#line 122
  first = *(objs + 0);
#line 121
  fprintf(_coverage_fout, "1092\n");
#line 121
  fflush(_coverage_fout);
#line 124
  i = 1;
#line 121
  fprintf(_coverage_fout, "1093\n");
#line 121
  fflush(_coverage_fout);
#line 124
  while (1) {
#line 124
    fprintf(_coverage_fout, "1087\n");
#line 124
    fflush(_coverage_fout);
#line 124
    if (i < num) {
#line 124
      fprintf(_coverage_fout, "1083\n");
#line 124
      fflush(_coverage_fout);

    } else {
#line 124
      break;
    }
#line 124
    fprintf(_coverage_fout, "1088\n");
#line 124
    fflush(_coverage_fout);
#line 125
    tmp___0 = NumEq(first, *(objs + i));
#line 124
    fprintf(_coverage_fout, "1089\n");
#line 124
    fflush(_coverage_fout);
#line 125
    if (! tmp___0) {
#line 125
      fprintf(_coverage_fout, "1084\n");
#line 125
      fflush(_coverage_fout);
#line 126
      tmp = FalseObj();
#line 125
      fprintf(_coverage_fout, "1085\n");
#line 125
      fflush(_coverage_fout);
#line 126
      return (tmp);
    } else {
#line 125
      fprintf(_coverage_fout, "1086\n");
#line 125
      fflush(_coverage_fout);

    }
#line 124
    fprintf(_coverage_fout, "1090\n");
#line 124
    fflush(_coverage_fout);
#line 124
    i ++;
  }
#line 121
  fprintf(_coverage_fout, "1094\n");
#line 121
  fflush(_coverage_fout);
#line 128
  tmp___1 = TrueObj();
#line 121
  fprintf(_coverage_fout, "1095\n");
#line 121
  fflush(_coverage_fout);
#line 128
  return (tmp___1);
}
}
#line 132 "external/Math.c"
void LoadMath(void) 
{ 

  {
#line 132
  fprintf(_coverage_fout, "1096\n");
#line 132
  fflush(_coverage_fout);
#line 134
  NewFunction((char *)"int?", (char *)"(x)", & IsInt);
#line 132
  fprintf(_coverage_fout, "1097\n");
#line 132
  fflush(_coverage_fout);
#line 135
  NewFunction((char *)"float?", (char *)"(x)", & IsFloat);
#line 132
  fprintf(_coverage_fout, "1098\n");
#line 132
  fflush(_coverage_fout);
#line 138
  NewFunction((char *)"+", (char *)"(... vals)", & AddFun);
#line 132
  fprintf(_coverage_fout, "1099\n");
#line 132
  fflush(_coverage_fout);
#line 139
  NewFunction((char *)"-", (char *)"(... vals)", & SubFun);
#line 132
  fprintf(_coverage_fout, "1100\n");
#line 132
  fflush(_coverage_fout);
#line 140
  NewFunction((char *)"*", (char *)"(... vals)", & MulFun);
#line 132
  fprintf(_coverage_fout, "1101\n");
#line 132
  fflush(_coverage_fout);
#line 141
  NewFunction((char *)"/", (char *)"(a b)", & DivFun);
#line 132
  fprintf(_coverage_fout, "1102\n");
#line 132
  fflush(_coverage_fout);
#line 142
  NewFunction((char *)"=", (char *)"(a b .. vals)", & NumEqFun);
#line 132
  fprintf(_coverage_fout, "1103\n");
#line 132
  fflush(_coverage_fout);
#line 143
  return;
}
}
#line 1 "List.o"
/* #pragma merger(0,"/tmp/cil-9mszvL0R.i","-Wall,-g") */
#line 3 "external/List.c"
VyObj MakeCons(VyObj *values , int num_args ) ;
#line 4
VyObj MakeList(VyObj *values , int num_args ) ;
#line 5
VyObj GetCar(VyObj *values , int num_args ) ;
#line 6
VyObj GetCdr(VyObj *values , int num_args ) ;
#line 7
VyObj GetListLen(VyObj *values , int num_args ) ;
#line 10 "external/List.c"
void LoadList(void) 
{ 

  {
#line 10
  fprintf(_coverage_fout, "1104\n");
#line 10
  fflush(_coverage_fout);
#line 11
  NewFunction((char *)"pair", (char *)"(x y)", & MakeCons);
#line 10
  fprintf(_coverage_fout, "1105\n");
#line 10
  fflush(_coverage_fout);
#line 12
  NewFunction((char *)"list", (char *)"(... all)", & MakeList);
#line 10
  fprintf(_coverage_fout, "1106\n");
#line 10
  fflush(_coverage_fout);
#line 14
  NewFunction((char *)"first", (char *)"(lst)", & GetCar);
#line 10
  fprintf(_coverage_fout, "1107\n");
#line 10
  fflush(_coverage_fout);
#line 15
  NewFunction((char *)"rest", (char *)"(lst)", & GetCdr);
#line 10
  fprintf(_coverage_fout, "1108\n");
#line 10
  fflush(_coverage_fout);
#line 17
  NewFunction((char *)"length", (char *)"(lst)", & GetListLen);
#line 10
  fprintf(_coverage_fout, "1109\n");
#line 10
  fflush(_coverage_fout);
#line 18
  return;
}
}
#line 20 "external/List.c"
VyObj MakeCons(VyObj *values , int num_args ) 
{ VyObj tmp ;

  {
#line 20
  fprintf(_coverage_fout, "1110\n");
#line 20
  fflush(_coverage_fout);
#line 21
  tmp = Cons(*(values + 0), *(values + 1));
#line 20
  fprintf(_coverage_fout, "1111\n");
#line 20
  fflush(_coverage_fout);
#line 21
  return (tmp);
}
}
#line 23 "external/List.c"
VyObj MakeList(VyObj *values , int num_args ) 
{ VyObj lst ;
  VyObj tmp ;
  int i ;

  {
#line 23
  fprintf(_coverage_fout, "1116\n");
#line 23
  fflush(_coverage_fout);
#line 24
  tmp = Nil();
#line 23
  fprintf(_coverage_fout, "1117\n");
#line 23
  fflush(_coverage_fout);
#line 24
  lst = tmp;
#line 23
  fprintf(_coverage_fout, "1118\n");
#line 23
  fflush(_coverage_fout);
#line 26
  i = num_args - 1;
#line 23
  fprintf(_coverage_fout, "1119\n");
#line 23
  fflush(_coverage_fout);
#line 26
  while (1) {
#line 26
    fprintf(_coverage_fout, "1113\n");
#line 26
    fflush(_coverage_fout);
#line 26
    if (i >= 0) {
#line 26
      fprintf(_coverage_fout, "1112\n");
#line 26
      fflush(_coverage_fout);

    } else {
#line 26
      break;
    }
#line 26
    fprintf(_coverage_fout, "1114\n");
#line 26
    fflush(_coverage_fout);
#line 27
    lst = Cons(*(values + i), lst);
#line 26
    fprintf(_coverage_fout, "1115\n");
#line 26
    fflush(_coverage_fout);
#line 26
    i --;
  }
#line 23
  fprintf(_coverage_fout, "1120\n");
#line 23
  fflush(_coverage_fout);
#line 28
  return (lst);
}
}
#line 30 "external/List.c"
VyObj GetCar(VyObj *values , int num_args ) 
{ VyObj tmp ;

  {
#line 30
  fprintf(_coverage_fout, "1121\n");
#line 30
  fflush(_coverage_fout);
#line 31
  tmp = Car(*(values + 0));
#line 30
  fprintf(_coverage_fout, "1122\n");
#line 30
  fflush(_coverage_fout);
#line 31
  return (tmp);
}
}
#line 33 "external/List.c"
VyObj GetCdr(VyObj *values , int num_args ) 
{ VyObj tmp ;

  {
#line 33
  fprintf(_coverage_fout, "1123\n");
#line 33
  fflush(_coverage_fout);
#line 34
  tmp = Cdr(*(values + 0));
#line 33
  fprintf(_coverage_fout, "1124\n");
#line 33
  fflush(_coverage_fout);
#line 34
  return (tmp);
}
}
#line 37 "external/List.c"
VyObj GetListLen(VyObj *values , int num_args ) 
{ int tmp ;
  VyObj tmp___0 ;

  {
#line 37
  fprintf(_coverage_fout, "1125\n");
#line 37
  fflush(_coverage_fout);
#line 38
  tmp = ListLen(*(values + 0));
#line 37
  fprintf(_coverage_fout, "1126\n");
#line 37
  fflush(_coverage_fout);
#line 38
  tmp___0 = CreateInt(tmp);
#line 37
  fprintf(_coverage_fout, "1127\n");
#line 37
  fflush(_coverage_fout);
#line 38
  return (tmp___0);
}
}
#line 1 "Bool.o"
/* #pragma merger(0,"/tmp/cil-K55vuWY7.i","-Wall,-g") */
#line 24 "include/External.h"
bool IsFalse(VyObj x ) ;
#line 4 "external/Bool.c"
VyObj TrueObj(void) 
{ VyObj tmp ;

  {
#line 4
  fprintf(_coverage_fout, "1128\n");
#line 4
  fflush(_coverage_fout);
#line 5
  tmp = CreateSymbol((char *)"true");
#line 4
  fprintf(_coverage_fout, "1129\n");
#line 4
  fflush(_coverage_fout);
#line 5
  return (tmp);
}
}
#line 7 "external/Bool.c"
VyObj FalseObj(void) 
{ VyObj tmp ;

  {
#line 7
  fprintf(_coverage_fout, "1130\n");
#line 7
  fflush(_coverage_fout);
#line 8
  tmp = CreateSymbol((char *)"false");
#line 7
  fprintf(_coverage_fout, "1131\n");
#line 7
  fflush(_coverage_fout);
#line 8
  return (tmp);
}
}
#line 12 "external/Bool.c"
bool IsTrue(VyObj x ) 
{ bool tmp ;
  int tmp___0 ;

  {
#line 12
  fprintf(_coverage_fout, "1134\n");
#line 12
  fflush(_coverage_fout);
#line 13
  tmp = IsFalse(x);
#line 12
  fprintf(_coverage_fout, "1135\n");
#line 12
  fflush(_coverage_fout);
#line 13
  if (tmp) {
#line 13
    fprintf(_coverage_fout, "1132\n");
#line 13
    fflush(_coverage_fout);
#line 13
    tmp___0 = 0;
  } else {
#line 13
    fprintf(_coverage_fout, "1133\n");
#line 13
    fflush(_coverage_fout);
#line 13
    tmp___0 = 1;
  }
#line 12
  fprintf(_coverage_fout, "1136\n");
#line 12
  fflush(_coverage_fout);
#line 13
  return (tmp___0);
}
}
#line 15 "external/Bool.c"
bool IsFalse(VyObj x ) 
{ bool tmp ;
  bool tmp___0 ;
  int tmp___1 ;

  {
#line 15
  fprintf(_coverage_fout, "1142\n");
#line 15
  fflush(_coverage_fout);
#line 16
  tmp = IsNil(x);
#line 15
  fprintf(_coverage_fout, "1143\n");
#line 15
  fflush(_coverage_fout);
#line 16
  if (tmp) {
#line 16
    fprintf(_coverage_fout, "1137\n");
#line 16
    fflush(_coverage_fout);
#line 16
    tmp___1 = 1;
  } else {
#line 16
    fprintf(_coverage_fout, "1140\n");
#line 16
    fflush(_coverage_fout);
#line 16
    tmp___0 = ObjEq(x, SymbolFalse);
#line 16
    fprintf(_coverage_fout, "1141\n");
#line 16
    fflush(_coverage_fout);
#line 16
    if (tmp___0) {
#line 16
      fprintf(_coverage_fout, "1138\n");
#line 16
      fflush(_coverage_fout);
#line 16
      tmp___1 = 1;
    } else {
#line 16
      fprintf(_coverage_fout, "1139\n");
#line 16
      fflush(_coverage_fout);
#line 16
      tmp___1 = 0;
    }
  }
#line 15
  fprintf(_coverage_fout, "1144\n");
#line 15
  fflush(_coverage_fout);
#line 16
  return (tmp___1);
}
}
#line 19 "external/Bool.c"
VyObj AndFun(VyObj *objs , int num ) 
{ int i ;
  VyObj tmp ;
  bool tmp___0 ;
  VyObj tmp___1 ;

  {
#line 19
  fprintf(_coverage_fout, "1153\n");
#line 19
  fflush(_coverage_fout);
#line 21
  i = 0;
#line 19
  fprintf(_coverage_fout, "1154\n");
#line 19
  fflush(_coverage_fout);
#line 21
  while (1) {
#line 21
    fprintf(_coverage_fout, "1149\n");
#line 21
    fflush(_coverage_fout);
#line 21
    if (i < num) {
#line 21
      fprintf(_coverage_fout, "1145\n");
#line 21
      fflush(_coverage_fout);

    } else {
#line 21
      break;
    }
#line 21
    fprintf(_coverage_fout, "1150\n");
#line 21
    fflush(_coverage_fout);
#line 22
    tmp___0 = IsTrue(*(objs + i));
#line 21
    fprintf(_coverage_fout, "1151\n");
#line 21
    fflush(_coverage_fout);
#line 22
    if (! tmp___0) {
#line 22
      fprintf(_coverage_fout, "1146\n");
#line 22
      fflush(_coverage_fout);
#line 23
      tmp = FalseObj();
#line 22
      fprintf(_coverage_fout, "1147\n");
#line 22
      fflush(_coverage_fout);
#line 23
      return (tmp);
    } else {
#line 22
      fprintf(_coverage_fout, "1148\n");
#line 22
      fflush(_coverage_fout);

    }
#line 21
    fprintf(_coverage_fout, "1152\n");
#line 21
    fflush(_coverage_fout);
#line 21
    i ++;
  }
#line 19
  fprintf(_coverage_fout, "1155\n");
#line 19
  fflush(_coverage_fout);
#line 24
  tmp___1 = TrueObj();
#line 19
  fprintf(_coverage_fout, "1156\n");
#line 19
  fflush(_coverage_fout);
#line 24
  return (tmp___1);
}
}
#line 26 "external/Bool.c"
VyObj OrFun(VyObj *objs , int num ) 
{ int i ;
  VyObj tmp ;
  bool tmp___0 ;
  VyObj tmp___1 ;

  {
#line 26
  fprintf(_coverage_fout, "1165\n");
#line 26
  fflush(_coverage_fout);
#line 28
  i = 0;
#line 26
  fprintf(_coverage_fout, "1166\n");
#line 26
  fflush(_coverage_fout);
#line 28
  while (1) {
#line 28
    fprintf(_coverage_fout, "1161\n");
#line 28
    fflush(_coverage_fout);
#line 28
    if (i < num) {
#line 28
      fprintf(_coverage_fout, "1157\n");
#line 28
      fflush(_coverage_fout);

    } else {
#line 28
      break;
    }
#line 28
    fprintf(_coverage_fout, "1162\n");
#line 28
    fflush(_coverage_fout);
#line 29
    tmp___0 = IsFalse(*(objs + i));
#line 28
    fprintf(_coverage_fout, "1163\n");
#line 28
    fflush(_coverage_fout);
#line 29
    if (! tmp___0) {
#line 29
      fprintf(_coverage_fout, "1158\n");
#line 29
      fflush(_coverage_fout);
#line 30
      tmp = TrueObj();
#line 29
      fprintf(_coverage_fout, "1159\n");
#line 29
      fflush(_coverage_fout);
#line 30
      return (tmp);
    } else {
#line 29
      fprintf(_coverage_fout, "1160\n");
#line 29
      fflush(_coverage_fout);

    }
#line 28
    fprintf(_coverage_fout, "1164\n");
#line 28
    fflush(_coverage_fout);
#line 28
    i ++;
  }
#line 26
  fprintf(_coverage_fout, "1167\n");
#line 26
  fflush(_coverage_fout);
#line 31
  tmp___1 = FalseObj();
#line 26
  fprintf(_coverage_fout, "1168\n");
#line 26
  fflush(_coverage_fout);
#line 31
  return (tmp___1);
}
}
#line 33 "external/Bool.c"
VyObj NotFun(VyObj *objs , int num ) 
{ VyObj tmp ;
  VyObj tmp___0 ;
  bool tmp___1 ;

  {
#line 33
  fprintf(_coverage_fout, "1173\n");
#line 33
  fflush(_coverage_fout);
#line 34
  tmp___1 = IsTrue(*(objs + 0));
#line 33
  fprintf(_coverage_fout, "1174\n");
#line 33
  fflush(_coverage_fout);
#line 34
  if (tmp___1) {
#line 34
    fprintf(_coverage_fout, "1169\n");
#line 34
    fflush(_coverage_fout);
#line 35
    tmp = FalseObj();
#line 34
    fprintf(_coverage_fout, "1170\n");
#line 34
    fflush(_coverage_fout);
#line 35
    return (tmp);
  } else {
#line 34
    fprintf(_coverage_fout, "1171\n");
#line 34
    fflush(_coverage_fout);
#line 37
    tmp___0 = TrueObj();
#line 34
    fprintf(_coverage_fout, "1172\n");
#line 34
    fflush(_coverage_fout);
#line 37
    return (tmp___0);
  }
}
}
#line 41 "external/Bool.c"
VyObj EqFun(VyObj *objs , int num ) 
{ VyObj first ;
  int i ;
  VyObj tmp ;
  bool tmp___0 ;
  VyObj tmp___1 ;

  {
#line 41
  fprintf(_coverage_fout, "1183\n");
#line 41
  fflush(_coverage_fout);
#line 42
  first = *(objs + 0);
#line 41
  fprintf(_coverage_fout, "1184\n");
#line 41
  fflush(_coverage_fout);
#line 44
  i = 1;
#line 41
  fprintf(_coverage_fout, "1185\n");
#line 41
  fflush(_coverage_fout);
#line 44
  while (1) {
#line 44
    fprintf(_coverage_fout, "1179\n");
#line 44
    fflush(_coverage_fout);
#line 44
    if (i < num) {
#line 44
      fprintf(_coverage_fout, "1175\n");
#line 44
      fflush(_coverage_fout);

    } else {
#line 44
      break;
    }
#line 44
    fprintf(_coverage_fout, "1180\n");
#line 44
    fflush(_coverage_fout);
#line 45
    tmp___0 = ObjEq(first, *(objs + i));
#line 44
    fprintf(_coverage_fout, "1181\n");
#line 44
    fflush(_coverage_fout);
#line 45
    if (! tmp___0) {
#line 45
      fprintf(_coverage_fout, "1176\n");
#line 45
      fflush(_coverage_fout);
#line 46
      tmp = FalseObj();
#line 45
      fprintf(_coverage_fout, "1177\n");
#line 45
      fflush(_coverage_fout);
#line 46
      return (tmp);
    } else {
#line 45
      fprintf(_coverage_fout, "1178\n");
#line 45
      fflush(_coverage_fout);

    }
#line 44
    fprintf(_coverage_fout, "1182\n");
#line 44
    fflush(_coverage_fout);
#line 44
    i ++;
  }
#line 41
  fprintf(_coverage_fout, "1186\n");
#line 41
  fflush(_coverage_fout);
#line 48
  tmp___1 = TrueObj();
#line 41
  fprintf(_coverage_fout, "1187\n");
#line 41
  fflush(_coverage_fout);
#line 48
  return (tmp___1);
}
}
#line 51 "external/Bool.c"
void LoadBool(void) 
{ 

  {
#line 51
  fprintf(_coverage_fout, "1188\n");
#line 51
  fflush(_coverage_fout);
#line 52
  NewFunction((char *)"and", (char *)"(... vals)", & AndFun);
#line 51
  fprintf(_coverage_fout, "1189\n");
#line 51
  fflush(_coverage_fout);
#line 53
  NewFunction((char *)"or", (char *)"(... vals)", & OrFun);
#line 51
  fprintf(_coverage_fout, "1190\n");
#line 51
  fflush(_coverage_fout);
#line 54
  NewFunction((char *)"not", (char *)"(val)", & NotFun);
#line 51
  fprintf(_coverage_fout, "1191\n");
#line 51
  fflush(_coverage_fout);
#line 55
  NewFunction((char *)"is", (char *)"(.. vals)", & EqFun);
#line 51
  fprintf(_coverage_fout, "1192\n");
#line 51
  fflush(_coverage_fout);
#line 56
  return;
}
}
void __globinit_vyquon_comb(void) 
{ 

  {
#line 51
  _coverage_fout = fopen("./coverage.path", "wb");
}
}
