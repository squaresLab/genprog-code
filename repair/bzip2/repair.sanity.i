#line 87 "bzip2.c"
struct __anonstruct_bz_stream_1 {
   char *next_in ;
   unsigned int avail_in ;
   unsigned int total_in_lo32 ;
   unsigned int total_in_hi32 ;
   char *next_out ;
   unsigned int avail_out ;
   unsigned int total_out_lo32 ;
   unsigned int total_out_hi32 ;
   void *state ;
   void *(*bzalloc)(void * , int  , int  ) ;
   void (*bzfree)(void * , void * ) ;
   void *opaque ;
};
#line 87 "bzip2.c"
typedef struct __anonstruct_bz_stream_1 bz_stream;
#line 214 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stddef.h"
typedef unsigned long size_t;
#line 34 "/usr/include/bits/types.h"
typedef unsigned char __u_char;
#line 35 "/usr/include/bits/types.h"
typedef unsigned short __u_short;
#line 36 "/usr/include/bits/types.h"
typedef unsigned int __u_int;
#line 37 "/usr/include/bits/types.h"
typedef unsigned long __u_long;
#line 40 "/usr/include/bits/types.h"
typedef signed char __int8_t;
#line 41 "/usr/include/bits/types.h"
typedef unsigned char __uint8_t;
#line 42 "/usr/include/bits/types.h"
typedef short __int16_t;
#line 43 "/usr/include/bits/types.h"
typedef unsigned short __uint16_t;
#line 44 "/usr/include/bits/types.h"
typedef int __int32_t;
#line 45 "/usr/include/bits/types.h"
typedef unsigned int __uint32_t;
#line 47 "/usr/include/bits/types.h"
typedef long __int64_t;
#line 48 "/usr/include/bits/types.h"
typedef unsigned long __uint64_t;
#line 56 "/usr/include/bits/types.h"
typedef long __quad_t;
#line 57 "/usr/include/bits/types.h"
typedef unsigned long __u_quad_t;
#line 137 "/usr/include/bits/types.h"
typedef unsigned long __dev_t;
#line 138 "/usr/include/bits/types.h"
typedef unsigned int __uid_t;
#line 139 "/usr/include/bits/types.h"
typedef unsigned int __gid_t;
#line 140 "/usr/include/bits/types.h"
typedef unsigned long __ino_t;
#line 141 "/usr/include/bits/types.h"
typedef unsigned long __ino64_t;
#line 142 "/usr/include/bits/types.h"
typedef unsigned int __mode_t;
#line 143 "/usr/include/bits/types.h"
typedef unsigned long __nlink_t;
#line 144 "/usr/include/bits/types.h"
typedef long __off_t;
#line 145 "/usr/include/bits/types.h"
typedef long __off64_t;
#line 146 "/usr/include/bits/types.h"
typedef int __pid_t;
#line 147 "/usr/include/bits/types.h"
struct __anonstruct___fsid_t_2 {
   int __val[2] ;
};
#line 147 "/usr/include/bits/types.h"
typedef struct __anonstruct___fsid_t_2 __fsid_t;
#line 148 "/usr/include/bits/types.h"
typedef long __clock_t;
#line 149 "/usr/include/bits/types.h"
typedef unsigned long __rlim_t;
#line 150 "/usr/include/bits/types.h"
typedef unsigned long __rlim64_t;
#line 151 "/usr/include/bits/types.h"
typedef unsigned int __id_t;
#line 152 "/usr/include/bits/types.h"
typedef long __time_t;
#line 153 "/usr/include/bits/types.h"
typedef unsigned int __useconds_t;
#line 154 "/usr/include/bits/types.h"
typedef long __suseconds_t;
#line 156 "/usr/include/bits/types.h"
typedef int __daddr_t;
#line 157 "/usr/include/bits/types.h"
typedef long __swblk_t;
#line 158 "/usr/include/bits/types.h"
typedef int __key_t;
#line 161 "/usr/include/bits/types.h"
typedef int __clockid_t;
#line 164 "/usr/include/bits/types.h"
typedef void *__timer_t;
#line 167 "/usr/include/bits/types.h"
typedef long __blksize_t;
#line 172 "/usr/include/bits/types.h"
typedef long __blkcnt_t;
#line 173 "/usr/include/bits/types.h"
typedef long __blkcnt64_t;
#line 176 "/usr/include/bits/types.h"
typedef unsigned long __fsblkcnt_t;
#line 177 "/usr/include/bits/types.h"
typedef unsigned long __fsblkcnt64_t;
#line 180 "/usr/include/bits/types.h"
typedef unsigned long __fsfilcnt_t;
#line 181 "/usr/include/bits/types.h"
typedef unsigned long __fsfilcnt64_t;
#line 183 "/usr/include/bits/types.h"
typedef long __ssize_t;
#line 187 "/usr/include/bits/types.h"
typedef __off64_t __loff_t;
#line 188 "/usr/include/bits/types.h"
typedef __quad_t *__qaddr_t;
#line 189 "/usr/include/bits/types.h"
typedef char *__caddr_t;
#line 192 "/usr/include/bits/types.h"
typedef long __intptr_t;
#line 195 "/usr/include/bits/types.h"
typedef unsigned int __socklen_t;
#line 46 "/usr/include/stdio.h"
struct _IO_FILE;
#line 46 "/usr/include/stdio.h"
typedef struct _IO_FILE FILE;
#line 62 "/usr/include/stdio.h"
typedef struct _IO_FILE __FILE;
#line 326 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stddef.h"
typedef int wchar_t;
#line 355 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stddef.h"
typedef unsigned int wint_t;
#line 76 "/usr/include/wchar.h"
union __anonunion___value_4 {
   wint_t __wch ;
   char __wchb[4] ;
};
#line 76 "/usr/include/wchar.h"
struct __anonstruct___mbstate_t_3 {
   int __count ;
   union __anonunion___value_4 __value ;
};
#line 76 "/usr/include/wchar.h"
typedef struct __anonstruct___mbstate_t_3 __mbstate_t;
#line 26 "/usr/include/_G_config.h"
struct __anonstruct__G_fpos_t_5 {
   __off_t __pos ;
   __mbstate_t __state ;
};
#line 26 "/usr/include/_G_config.h"
typedef struct __anonstruct__G_fpos_t_5 _G_fpos_t;
#line 31 "/usr/include/_G_config.h"
struct __anonstruct__G_fpos64_t_6 {
   __off64_t __pos ;
   __mbstate_t __state ;
};
#line 31 "/usr/include/_G_config.h"
typedef struct __anonstruct__G_fpos64_t_6 _G_fpos64_t;
#line 37 "/usr/include/gconv.h"
enum __anonenum_7 {
    __GCONV_OK = 0,
    __GCONV_NOCONV = 1,
    __GCONV_NODB = 2,
    __GCONV_NOMEM = 3,
    __GCONV_EMPTY_INPUT = 4,
    __GCONV_FULL_OUTPUT = 5,
    __GCONV_ILLEGAL_INPUT = 6,
    __GCONV_INCOMPLETE_INPUT = 7,
    __GCONV_ILLEGAL_DESCRIPTOR = 8,
    __GCONV_INTERNAL_ERROR = 9
} ;
#line 55
enum __anonenum_8 {
    __GCONV_IS_LAST = 1,
    __GCONV_IGNORE_ERRORS = 2
} ;
#line 63
struct __gconv_step;
#line 63
struct __gconv_step;
#line 64
struct __gconv_step_data;
#line 64
struct __gconv_step_data;
#line 65
struct __gconv_loaded_object;
#line 65
struct __gconv_loaded_object;
#line 66
struct __gconv_trans_data;
#line 66
struct __gconv_trans_data;
#line 70 "/usr/include/gconv.h"
typedef int (*__gconv_fct)(struct __gconv_step * , struct __gconv_step_data * ,
                           unsigned char const   ** , unsigned char const   * ,
                           unsigned char ** , size_t * , int  , int  );
#line 75 "/usr/include/gconv.h"
typedef wint_t (*__gconv_btowc_fct)(struct __gconv_step * , unsigned char  );
#line 78 "/usr/include/gconv.h"
typedef int (*__gconv_init_fct)(struct __gconv_step * );
#line 79 "/usr/include/gconv.h"
typedef void (*__gconv_end_fct)(struct __gconv_step * );
#line 83 "/usr/include/gconv.h"
typedef int (*__gconv_trans_fct)(struct __gconv_step * ,
                                 struct __gconv_step_data * , void * ,
                                 unsigned char const   * ,
                                 unsigned char const   ** ,
                                 unsigned char const   * , unsigned char ** ,
                                 size_t * );
#line 91 "/usr/include/gconv.h"
typedef int (*__gconv_trans_context_fct)(void * , unsigned char const   * ,
                                         unsigned char const   * ,
                                         unsigned char * , unsigned char * );
#line 96 "/usr/include/gconv.h"
typedef int (*__gconv_trans_query_fct)(char const   * , char const   *** ,
                                       size_t * );
#line 100 "/usr/include/gconv.h"
typedef int (*__gconv_trans_init_fct)(void ** , char const   * );
#line 101 "/usr/include/gconv.h"
typedef void (*__gconv_trans_end_fct)(void * );
#line 103 "/usr/include/gconv.h"
struct __gconv_trans_data {
   int (*__trans_fct)(struct __gconv_step * , struct __gconv_step_data * ,
                      void * , unsigned char const   * ,
                      unsigned char const   ** , unsigned char const   * ,
                      unsigned char ** , size_t * ) ;
   int (*__trans_context_fct)(void * , unsigned char const   * ,
                              unsigned char const   * , unsigned char * ,
                              unsigned char * ) ;
   void (*__trans_end_fct)(void * ) ;
   void *__data ;
   struct __gconv_trans_data *__next ;
};
#line 115 "/usr/include/gconv.h"
struct __gconv_step {
   struct __gconv_loaded_object *__shlib_handle ;
   char const   *__modname ;
   int __counter ;
   char *__from_name ;
   char *__to_name ;
   int (*__fct)(struct __gconv_step * , struct __gconv_step_data * ,
                unsigned char const   ** , unsigned char const   * ,
                unsigned char ** , size_t * , int  , int  ) ;
   wint_t (*__btowc_fct)(struct __gconv_step * , unsigned char  ) ;
   int (*__init_fct)(struct __gconv_step * ) ;
   void (*__end_fct)(struct __gconv_step * ) ;
   int __min_needed_from ;
   int __max_needed_from ;
   int __min_needed_to ;
   int __max_needed_to ;
   int __stateful ;
   void *__data ;
};
#line 145 "/usr/include/gconv.h"
struct __gconv_step_data {
   unsigned char *__outbuf ;
   unsigned char *__outbufend ;
   int __flags ;
   int __invocation_counter ;
   int __internal_use ;
   __mbstate_t *__statep ;
   __mbstate_t __state ;
   struct __gconv_trans_data *__trans ;
};
#line 172 "/usr/include/gconv.h"
struct __gconv_info {
   size_t __nsteps ;
   struct __gconv_step *__steps ;
   struct __gconv_step_data __data[] ;
};
#line 172 "/usr/include/gconv.h"
typedef struct __gconv_info *__gconv_t;
#line 45 "/usr/include/_G_config.h"
struct __anonstruct___combined_10 {
   struct __gconv_info __cd ;
   struct __gconv_step_data __data ;
};
#line 45 "/usr/include/_G_config.h"
union __anonunion__G_iconv_t_9 {
   struct __gconv_info __cd ;
   struct __anonstruct___combined_10 __combined ;
};
#line 45 "/usr/include/_G_config.h"
typedef union __anonunion__G_iconv_t_9 _G_iconv_t;
#line 55 "/usr/include/_G_config.h"
typedef short _G_int16_t;
#line 56 "/usr/include/_G_config.h"
typedef int _G_int32_t;
#line 57 "/usr/include/_G_config.h"
typedef unsigned short _G_uint16_t;
#line 58 "/usr/include/_G_config.h"
typedef unsigned int _G_uint32_t;
#line 43 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stdarg.h"
typedef __builtin_va_list __gnuc_va_list;
#line 167 "/usr/include/libio.h"
struct _IO_jump_t;
#line 167
struct _IO_jump_t;
#line 167
struct _IO_FILE;
#line 177 "/usr/include/libio.h"
typedef void _IO_lock_t;
#line 183 "/usr/include/libio.h"
struct _IO_marker {
   struct _IO_marker *_next ;
   struct _IO_FILE *_sbuf ;
   int _pos ;
};
#line 203
enum __codecvt_result {
    __codecvt_ok = 0,
    __codecvt_partial = 1,
    __codecvt_error = 2,
    __codecvt_noconv = 3
} ;
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
#line 338 "/usr/include/libio.h"
typedef struct _IO_FILE _IO_FILE;
#line 341
struct _IO_FILE_plus;
#line 341
struct _IO_FILE_plus;
#line 361 "/usr/include/libio.h"
typedef __ssize_t __io_read_fn(void *__cookie , char *__buf , size_t __nbytes );
#line 369 "/usr/include/libio.h"
typedef __ssize_t __io_write_fn(void *__cookie , char const   *__buf ,
                                size_t __n );
#line 378 "/usr/include/libio.h"
typedef int __io_seek_fn(void *__cookie , __off64_t *__pos , int __w );
#line 381 "/usr/include/libio.h"
typedef int __io_close_fn(void *__cookie );
#line 88 "/usr/include/stdio.h"
typedef _G_fpos_t fpos_t;
#line 174 "bzip2.c"
typedef void BZFILE;
#line 98 "/usr/include/stdlib.h"
struct __anonstruct_div_t_11 {
   int quot ;
   int rem ;
};
#line 98 "/usr/include/stdlib.h"
typedef struct __anonstruct_div_t_11 div_t;
#line 106 "/usr/include/stdlib.h"
struct __anonstruct_ldiv_t_12 {
   long quot ;
   long rem ;
};
#line 106 "/usr/include/stdlib.h"
typedef struct __anonstruct_ldiv_t_12 ldiv_t;
#line 35 "/usr/include/sys/types.h"
typedef __u_char u_char;
#line 36 "/usr/include/sys/types.h"
typedef __u_short u_short;
#line 37 "/usr/include/sys/types.h"
typedef __u_int u_int;
#line 38 "/usr/include/sys/types.h"
typedef __u_long u_long;
#line 39 "/usr/include/sys/types.h"
typedef __quad_t quad_t;
#line 40 "/usr/include/sys/types.h"
typedef __u_quad_t u_quad_t;
#line 41 "/usr/include/sys/types.h"
typedef __fsid_t fsid_t;
#line 46 "/usr/include/sys/types.h"
typedef __loff_t loff_t;
#line 50 "/usr/include/sys/types.h"
typedef __ino_t ino_t;
#line 62 "/usr/include/sys/types.h"
typedef __dev_t dev_t;
#line 67 "/usr/include/sys/types.h"
typedef __gid_t gid_t;
#line 72 "/usr/include/sys/types.h"
typedef __mode_t mode_t;
#line 77 "/usr/include/sys/types.h"
typedef __nlink_t nlink_t;
#line 82 "/usr/include/sys/types.h"
typedef __uid_t uid_t;
#line 88 "/usr/include/sys/types.h"
typedef __off_t off_t;
#line 100 "/usr/include/sys/types.h"
typedef __pid_t pid_t;
#line 105 "/usr/include/sys/types.h"
typedef __id_t id_t;
#line 110 "/usr/include/sys/types.h"
typedef __ssize_t ssize_t;
#line 116 "/usr/include/sys/types.h"
typedef __daddr_t daddr_t;
#line 117 "/usr/include/sys/types.h"
typedef __caddr_t caddr_t;
#line 123 "/usr/include/sys/types.h"
typedef __key_t key_t;
#line 77 "/usr/include/time.h"
typedef __time_t time_t;
#line 93 "/usr/include/time.h"
typedef __clockid_t clockid_t;
#line 105 "/usr/include/time.h"
typedef __timer_t timer_t;
#line 151 "/usr/include/sys/types.h"
typedef unsigned long ulong;
#line 152 "/usr/include/sys/types.h"
typedef unsigned short ushort;
#line 153 "/usr/include/sys/types.h"
typedef unsigned int uint;
#line 195 "/usr/include/sys/types.h"
typedef signed char int8_t;
#line 196 "/usr/include/sys/types.h"
typedef short int16_t;
#line 197 "/usr/include/sys/types.h"
typedef int int32_t;
#line 198 "/usr/include/sys/types.h"
typedef long int64_t;
#line 201 "/usr/include/sys/types.h"
typedef unsigned char u_int8_t;
#line 202 "/usr/include/sys/types.h"
typedef unsigned short u_int16_t;
#line 203 "/usr/include/sys/types.h"
typedef unsigned int u_int32_t;
#line 204 "/usr/include/sys/types.h"
typedef unsigned long u_int64_t;
#line 206 "/usr/include/sys/types.h"
typedef int register_t;
#line 23 "/usr/include/bits/sigset.h"
typedef int __sig_atomic_t;
#line 28 "/usr/include/bits/sigset.h"
struct __anonstruct___sigset_t_13 {
   unsigned long __val[1024UL / (8UL * sizeof(unsigned long ))] ;
};
#line 28 "/usr/include/bits/sigset.h"
typedef struct __anonstruct___sigset_t_13 __sigset_t;
#line 38 "/usr/include/sys/select.h"
typedef __sigset_t sigset_t;
#line 121 "/usr/include/time.h"
struct timespec {
   __time_t tv_sec ;
   long tv_nsec ;
};
#line 69 "/usr/include/bits/time.h"
struct timeval {
   __time_t tv_sec ;
   __suseconds_t tv_usec ;
};
#line 49 "/usr/include/sys/select.h"
typedef __suseconds_t suseconds_t;
#line 55 "/usr/include/sys/select.h"
typedef long __fd_mask;
#line 67 "/usr/include/sys/select.h"
struct __anonstruct_fd_set_14 {
   __fd_mask __fds_bits[1024UL / (8UL * sizeof(__fd_mask ))] ;
};
#line 67 "/usr/include/sys/select.h"
typedef struct __anonstruct_fd_set_14 fd_set;
#line 85 "/usr/include/sys/select.h"
typedef __fd_mask fd_mask;
#line 235 "/usr/include/sys/types.h"
typedef __blkcnt_t blkcnt_t;
#line 239 "/usr/include/sys/types.h"
typedef __fsblkcnt_t fsblkcnt_t;
#line 243 "/usr/include/sys/types.h"
typedef __fsfilcnt_t fsfilcnt_t;
#line 50 "/usr/include/bits/pthreadtypes.h"
typedef unsigned long pthread_t;
#line 53 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_attr_t_15 {
   char __size[56] ;
   long __align ;
};
#line 53 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_attr_t_15 pthread_attr_t;
#line 61 "/usr/include/bits/pthreadtypes.h"
struct __pthread_internal_list {
   struct __pthread_internal_list *__prev ;
   struct __pthread_internal_list *__next ;
};
#line 61 "/usr/include/bits/pthreadtypes.h"
typedef struct __pthread_internal_list __pthread_list_t;
#line 76 "/usr/include/bits/pthreadtypes.h"
struct __pthread_mutex_s {
   int __lock ;
   unsigned int __count ;
   int __owner ;
   unsigned int __nusers ;
   int __kind ;
   int __spins ;
   __pthread_list_t __list ;
};
#line 76 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_mutex_t_16 {
   struct __pthread_mutex_s __data ;
   char __size[40] ;
   long __align ;
};
#line 76 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_mutex_t_16 pthread_mutex_t;
#line 106 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_mutexattr_t_17 {
   char __size[4] ;
   int __align ;
};
#line 106 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_mutexattr_t_17 pthread_mutexattr_t;
#line 115 "/usr/include/bits/pthreadtypes.h"
struct __anonstruct___data_19 {
   int __lock ;
   unsigned int __futex ;
   unsigned long long __total_seq ;
   unsigned long long __wakeup_seq ;
   unsigned long long __woken_seq ;
   void *__mutex ;
   unsigned int __nwaiters ;
   unsigned int __broadcast_seq ;
};
#line 115 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_cond_t_18 {
   struct __anonstruct___data_19 __data ;
   char __size[48] ;
   long long __align ;
};
#line 115 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_cond_t_18 pthread_cond_t;
#line 132 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_condattr_t_20 {
   char __size[4] ;
   int __align ;
};
#line 132 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_condattr_t_20 pthread_condattr_t;
#line 140 "/usr/include/bits/pthreadtypes.h"
typedef unsigned int pthread_key_t;
#line 144 "/usr/include/bits/pthreadtypes.h"
typedef int pthread_once_t;
#line 150 "/usr/include/bits/pthreadtypes.h"
struct __anonstruct___data_22 {
   int __lock ;
   unsigned int __nr_readers ;
   unsigned int __readers_wakeup ;
   unsigned int __writer_wakeup ;
   unsigned int __nr_readers_queued ;
   unsigned int __nr_writers_queued ;
   int __writer ;
   int __pad1 ;
   unsigned long __pad2 ;
   unsigned long __pad3 ;
   unsigned int __flags ;
};
#line 150 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_rwlock_t_21 {
   struct __anonstruct___data_22 __data ;
   char __size[56] ;
   long __align ;
};
#line 150 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_rwlock_t_21 pthread_rwlock_t;
#line 188 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_rwlockattr_t_23 {
   char __size[8] ;
   long __align ;
};
#line 188 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_rwlockattr_t_23 pthread_rwlockattr_t;
#line 198 "/usr/include/bits/pthreadtypes.h"
typedef int volatile   pthread_spinlock_t;
#line 203 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_barrier_t_24 {
   char __size[32] ;
   long __align ;
};
#line 203 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_barrier_t_24 pthread_barrier_t;
#line 209 "/usr/include/bits/pthreadtypes.h"
union __anonunion_pthread_barrierattr_t_25 {
   char __size[4] ;
   int __align ;
};
#line 209 "/usr/include/bits/pthreadtypes.h"
typedef union __anonunion_pthread_barrierattr_t_25 pthread_barrierattr_t;
#line 467 "/usr/include/stdlib.h"
struct random_data {
   int32_t *fptr ;
   int32_t *rptr ;
   int32_t *state ;
   int rand_type ;
   int rand_deg ;
   int rand_sep ;
   int32_t *end_ptr ;
};
#line 536 "/usr/include/stdlib.h"
struct drand48_data {
   unsigned short __x[3] ;
   unsigned short __old_x[3] ;
   unsigned short __c ;
   unsigned short __init ;
   unsigned long long __a ;
};
#line 764 "/usr/include/stdlib.h"
typedef int (*__compar_fn_t)(void const   * , void const   * );
#line 48 "/usr/include/ctype.h"
enum __anonenum_26 {
    _ISupper = 256,
    _ISlower = 512,
    _ISalpha = 1024,
    _ISdigit = 2048,
    _ISxdigit = 4096,
    _ISspace = 8192,
    _ISprint = 16384,
    _ISgraph = 32768,
    _ISblank = 1,
    _IScntrl = 2,
    _ISpunct = 4,
    _ISalnum = 8
} ;
#line 342 "bzip2.c"
typedef char Char;
#line 343 "bzip2.c"
typedef unsigned char Bool;
#line 344 "bzip2.c"
typedef unsigned char UChar;
#line 345 "bzip2.c"
typedef int Int32;
#line 346 "bzip2.c"
typedef unsigned int UInt32;
#line 347 "bzip2.c"
typedef short Int16;
#line 348 "bzip2.c"
typedef unsigned short UInt16;
#line 491 "bzip2.c"
struct __anonstruct_EState_27 {
   bz_stream *strm ;
   Int32 mode ;
   Int32 state ;
   UInt32 avail_in_expect ;
   UInt32 *arr1 ;
   UInt32 *arr2 ;
   UInt32 *ftab ;
   Int32 origPtr ;
   UInt32 *ptr ;
   UChar *block ;
   UInt16 *mtfv ;
   UChar *zbits ;
   Int32 workFactor ;
   UInt32 state_in_ch ;
   Int32 state_in_len ;
   Int32 rNToGo ;
   Int32 rTPos ;
   Int32 nblock ;
   Int32 nblockMAX ;
   Int32 numZ ;
   Int32 state_out_pos ;
   Int32 nInUse ;
   Bool inUse[256] ;
   UChar unseqToSeq[256] ;
   UInt32 bsBuff ;
   Int32 bsLive ;
   UInt32 blockCRC ;
   UInt32 combinedCRC ;
   Int32 verbosity ;
   Int32 blockNo ;
   Int32 blockSize100k ;
   Int32 nMTF ;
   Int32 mtfFreq[258] ;
   UChar selector[18002] ;
   UChar selectorMtf[18002] ;
   UChar len[6][258] ;
   Int32 code[6][258] ;
   Int32 rfreq[6][258] ;
   UInt32 len_pack[258][4] ;
};
#line 491 "bzip2.c"
typedef struct __anonstruct_EState_27 EState;
#line 642 "bzip2.c"
struct __anonstruct_DState_28 {
   bz_stream *strm ;
   Int32 state ;
   UChar state_out_ch ;
   Int32 state_out_len ;
   Bool blockRandomised ;
   Int32 rNToGo ;
   Int32 rTPos ;
   UInt32 bsBuff ;
   Int32 bsLive ;
   Int32 blockSize100k ;
   Bool smallDecompress ;
   Int32 currBlockNo ;
   Int32 verbosity ;
   Int32 origPtr ;
   UInt32 tPos ;
   Int32 k0 ;
   Int32 unzftab[256] ;
   Int32 nblock_used ;
   Int32 cftab[257] ;
   Int32 cftabCopy[257] ;
   UInt32 *tt ;
   UInt16 *ll16 ;
   UChar *ll4 ;
   UInt32 storedBlockCRC ;
   UInt32 storedCombinedCRC ;
   UInt32 calculatedBlockCRC ;
   UInt32 calculatedCombinedCRC ;
   Int32 nInUse ;
   Bool inUse[256] ;
   Bool inUse16[16] ;
   UChar seqToUnseq[256] ;
   UChar mtfa[4096] ;
   Int32 mtfbase[16] ;
   UChar selector[18002] ;
   UChar selectorMtf[18002] ;
   UChar len[6][258] ;
   Int32 limit[6][258] ;
   Int32 base[6][258] ;
   Int32 perm[6][258] ;
   Int32 minLens[6] ;
   Int32 save_i ;
   Int32 save_j ;
   Int32 save_t ;
   Int32 save_alphaSize ;
   Int32 save_nGroups ;
   Int32 save_nSelectors ;
   Int32 save_EOB ;
   Int32 save_groupNo ;
   Int32 save_groupPos ;
   Int32 save_nextSym ;
   Int32 save_nblockMAX ;
   Int32 save_nblock ;
   Int32 save_es ;
   Int32 save_N ;
   Int32 save_curr ;
   Int32 save_zt ;
   Int32 save_zn ;
   Int32 save_zvec ;
   Int32 save_zj ;
   Int32 save_gSel ;
   Int32 save_gMinlen ;
   Int32 *save_gLimit ;
   Int32 *save_gBase ;
   Int32 *save_gPerm ;
};
#line 642 "bzip2.c"
typedef struct __anonstruct_DState_28 DState;
#line 4294 "bzip2.c"
struct __anonstruct_bzFile_29 {
   FILE *handle ;
   Char buf[5000] ;
   Int32 bufN ;
   Bool writing ;
   bz_stream strm ;
   Int32 lastErr ;
   Bool initialisedOk ;
};
#line 4294 "bzip2.c"
typedef struct __anonstruct_bzFile_29 bzFile;
#line 41 "/usr/include/signal.h"
typedef __sig_atomic_t sig_atomic_t;
#line 75 "/usr/include/signal.h"
typedef void (*__sighandler_t)(int  );
#line 201 "/usr/include/signal.h"
typedef void (*sig_t)(int  );
#line 33 "/usr/include/bits/siginfo.h"
union sigval {
   int sival_int ;
   void *sival_ptr ;
};
#line 33 "/usr/include/bits/siginfo.h"
typedef union sigval sigval_t;
#line 51 "/usr/include/bits/siginfo.h"
struct __anonstruct__kill_31 {
   __pid_t si_pid ;
   __uid_t si_uid ;
};
#line 51 "/usr/include/bits/siginfo.h"
struct __anonstruct__timer_32 {
   int si_tid ;
   int si_overrun ;
   sigval_t si_sigval ;
};
#line 51 "/usr/include/bits/siginfo.h"
struct __anonstruct__rt_33 {
   __pid_t si_pid ;
   __uid_t si_uid ;
   sigval_t si_sigval ;
};
#line 51 "/usr/include/bits/siginfo.h"
struct __anonstruct__sigchld_34 {
   __pid_t si_pid ;
   __uid_t si_uid ;
   int si_status ;
   __clock_t si_utime ;
   __clock_t si_stime ;
};
#line 51 "/usr/include/bits/siginfo.h"
struct __anonstruct__sigfault_35 {
   void *si_addr ;
};
#line 51 "/usr/include/bits/siginfo.h"
struct __anonstruct__sigpoll_36 {
   long si_band ;
   int si_fd ;
};
#line 51 "/usr/include/bits/siginfo.h"
union __anonunion__sifields_30 {
   int _pad[128UL / sizeof(int ) - 4UL] ;
   struct __anonstruct__kill_31 _kill ;
   struct __anonstruct__timer_32 _timer ;
   struct __anonstruct__rt_33 _rt ;
   struct __anonstruct__sigchld_34 _sigchld ;
   struct __anonstruct__sigfault_35 _sigfault ;
   struct __anonstruct__sigpoll_36 _sigpoll ;
};
#line 51 "/usr/include/bits/siginfo.h"
struct siginfo {
   int si_signo ;
   int si_errno ;
   int si_code ;
   union __anonunion__sifields_30 _sifields ;
};
#line 51 "/usr/include/bits/siginfo.h"
typedef struct siginfo siginfo_t;
#line 129
enum __anonenum_37 {
    SI_ASYNCNL = -60,
    SI_TKILL = -6,
    SI_SIGIO = -5,
    SI_ASYNCIO = -4,
    SI_MESGQ = -3,
    SI_TIMER = -2,
    SI_QUEUE = -1,
    SI_USER = 0,
    SI_KERNEL = 128
} ;
#line 153
enum __anonenum_38 {
    ILL_ILLOPC = 1,
    ILL_ILLOPN = 2,
    ILL_ILLADR = 3,
    ILL_ILLTRP = 4,
    ILL_PRVOPC = 5,
    ILL_PRVREG = 6,
    ILL_COPROC = 7,
    ILL_BADSTK = 8
} ;
#line 174
enum __anonenum_39 {
    FPE_INTDIV = 1,
    FPE_INTOVF = 2,
    FPE_FLTDIV = 3,
    FPE_FLTOVF = 4,
    FPE_FLTUND = 5,
    FPE_FLTRES = 6,
    FPE_FLTINV = 7,
    FPE_FLTSUB = 8
} ;
#line 195
enum __anonenum_40 {
    SEGV_MAPERR = 1,
    SEGV_ACCERR = 2
} ;
#line 204
enum __anonenum_41 {
    BUS_ADRALN = 1,
    BUS_ADRERR = 2,
    BUS_OBJERR = 3
} ;
#line 215
enum __anonenum_42 {
    TRAP_BRKPT = 1,
    TRAP_TRACE = 2
} ;
#line 224
enum __anonenum_43 {
    CLD_EXITED = 1,
    CLD_KILLED = 2,
    CLD_DUMPED = 3,
    CLD_TRAPPED = 4,
    CLD_STOPPED = 5,
    CLD_CONTINUED = 6
} ;
#line 241
enum __anonenum_44 {
    POLL_IN = 1,
    POLL_OUT = 2,
    POLL_MSG = 3,
    POLL_ERR = 4,
    POLL_PRI = 5,
    POLL_HUP = 6
} ;
#line 273 "/usr/include/bits/siginfo.h"
struct __anonstruct__sigev_thread_46 {
   void (*_function)(sigval_t  ) ;
   void *_attribute ;
};
#line 273 "/usr/include/bits/siginfo.h"
union __anonunion__sigev_un_45 {
   int _pad[64UL / sizeof(int ) - 4UL] ;
   __pid_t _tid ;
   struct __anonstruct__sigev_thread_46 _sigev_thread ;
};
#line 273 "/usr/include/bits/siginfo.h"
struct sigevent {
   sigval_t sigev_value ;
   int sigev_signo ;
   int sigev_notify ;
   union __anonunion__sigev_un_45 _sigev_un ;
};
#line 273 "/usr/include/bits/siginfo.h"
typedef struct sigevent sigevent_t;
#line 300
enum __anonenum_47 {
    SIGEV_SIGNAL = 0,
    SIGEV_NONE = 1,
    SIGEV_THREAD = 2,
    SIGEV_THREAD_ID = 4
} ;
#line 25 "/usr/include/bits/sigaction.h"
union __anonunion___sigaction_handler_48 {
   void (*sa_handler)(int  ) ;
   void (*sa_sigaction)(int  , siginfo_t * , void * ) ;
};
#line 25 "/usr/include/bits/sigaction.h"
struct sigaction {
   union __anonunion___sigaction_handler_48 __sigaction_handler ;
   __sigset_t sa_mask ;
   int sa_flags ;
   void (*sa_restorer)(void) ;
};
#line 308 "/usr/include/signal.h"
struct sigvec {
   void (*sv_handler)(int  ) ;
   int sv_mask ;
   int sv_flags ;
};
#line 28 "/usr/include/bits/sigcontext.h"
struct _fpreg {
   unsigned short significand[4] ;
   unsigned short exponent ;
};
#line 34 "/usr/include/bits/sigcontext.h"
struct _fpxreg {
   unsigned short significand[4] ;
   unsigned short exponent ;
   unsigned short padding[3] ;
};
#line 41 "/usr/include/bits/sigcontext.h"
struct _xmmreg {
   __uint32_t element[4] ;
};
#line 109 "/usr/include/bits/sigcontext.h"
struct _fpstate {
   __uint16_t cwd ;
   __uint16_t swd ;
   __uint16_t ftw ;
   __uint16_t fop ;
   __uint64_t rip ;
   __uint64_t rdp ;
   __uint32_t mxcsr ;
   __uint32_t mxcr_mask ;
   struct _fpxreg _st[8] ;
   struct _xmmreg _xmm[16] ;
   __uint32_t padding[24] ;
};
#line 125 "/usr/include/bits/sigcontext.h"
struct sigcontext {
   unsigned long r8 ;
   unsigned long r9 ;
   unsigned long r10 ;
   unsigned long r11 ;
   unsigned long r12 ;
   unsigned long r13 ;
   unsigned long r14 ;
   unsigned long r15 ;
   unsigned long rdi ;
   unsigned long rsi ;
   unsigned long rbp ;
   unsigned long rbx ;
   unsigned long rdx ;
   unsigned long rax ;
   unsigned long rcx ;
   unsigned long rsp ;
   unsigned long rip ;
   unsigned long eflags ;
   unsigned short cs ;
   unsigned short gs ;
   unsigned short fs ;
   unsigned short __pad0 ;
   unsigned long err ;
   unsigned long trapno ;
   unsigned long oldmask ;
   unsigned long cr2 ;
   struct _fpstate *fpstate ;
   unsigned long __reserved1[8] ;
};
#line 26 "/usr/include/bits/sigstack.h"
struct sigstack {
   void *ss_sp ;
   int ss_onstack ;
};
#line 34
enum __anonenum_49 {
    SS_ONSTACK = 1,
    SS_DISABLE = 2
} ;
#line 50 "/usr/include/bits/sigstack.h"
struct sigaltstack {
   void *ss_sp ;
   int ss_flags ;
   size_t ss_size ;
};
#line 50 "/usr/include/bits/sigstack.h"
typedef struct sigaltstack stack_t;
#line 284 "/usr/include/math.h"
enum __anonenum__LIB_VERSION_TYPE_50 {
    _IEEE_ = -1,
    _SVID_ = 0,
    _XOPEN_ = 1,
    _POSIX_ = 2,
    _ISOC_ = 3
} ;
#line 284 "/usr/include/math.h"
typedef enum __anonenum__LIB_VERSION_TYPE_50 _LIB_VERSION_TYPE;
#line 309 "/usr/include/math.h"
struct exception {
   int type ;
   char *name ;
   double arg1 ;
   double arg2 ;
   double retval ;
};
#line 155 "/usr/include/bits/fcntl.h"
struct flock {
   short l_type ;
   short l_whence ;
   __off_t l_start ;
   __off_t l_len ;
   __pid_t l_pid ;
};
#line 38 "/usr/include/utime.h"
struct utimbuf {
   __time_t actime ;
   __time_t modtime ;
};
#line 226 "/usr/include/unistd.h"
typedef __useconds_t useconds_t;
#line 238 "/usr/include/unistd.h"
typedef __intptr_t intptr_t;
#line 245 "/usr/include/unistd.h"
typedef __socklen_t socklen_t;
#line 26 "/usr/include/bits/confname.h"
enum __anonenum_51 {
    _PC_LINK_MAX = 0,
    _PC_MAX_CANON = 1,
    _PC_MAX_INPUT = 2,
    _PC_NAME_MAX = 3,
    _PC_PATH_MAX = 4,
    _PC_PIPE_BUF = 5,
    _PC_CHOWN_RESTRICTED = 6,
    _PC_NO_TRUNC = 7,
    _PC_VDISABLE = 8,
    _PC_SYNC_IO = 9,
    _PC_ASYNC_IO = 10,
    _PC_PRIO_IO = 11,
    _PC_SOCK_MAXBUF = 12,
    _PC_FILESIZEBITS = 13,
    _PC_REC_INCR_XFER_SIZE = 14,
    _PC_REC_MAX_XFER_SIZE = 15,
    _PC_REC_MIN_XFER_SIZE = 16,
    _PC_REC_XFER_ALIGN = 17,
    _PC_ALLOC_SIZE_MIN = 18,
    _PC_SYMLINK_MAX = 19,
    _PC_2_SYMLINKS = 20
} ;
#line 73
enum __anonenum_52 {
    _SC_ARG_MAX = 0,
    _SC_CHILD_MAX = 1,
    _SC_CLK_TCK = 2,
    _SC_NGROUPS_MAX = 3,
    _SC_OPEN_MAX = 4,
    _SC_STREAM_MAX = 5,
    _SC_TZNAME_MAX = 6,
    _SC_JOB_CONTROL = 7,
    _SC_SAVED_IDS = 8,
    _SC_REALTIME_SIGNALS = 9,
    _SC_PRIORITY_SCHEDULING = 10,
    _SC_TIMERS = 11,
    _SC_ASYNCHRONOUS_IO = 12,
    _SC_PRIORITIZED_IO = 13,
    _SC_SYNCHRONIZED_IO = 14,
    _SC_FSYNC = 15,
    _SC_MAPPED_FILES = 16,
    _SC_MEMLOCK = 17,
    _SC_MEMLOCK_RANGE = 18,
    _SC_MEMORY_PROTECTION = 19,
    _SC_MESSAGE_PASSING = 20,
    _SC_SEMAPHORES = 21,
    _SC_SHARED_MEMORY_OBJECTS = 22,
    _SC_AIO_LISTIO_MAX = 23,
    _SC_AIO_MAX = 24,
    _SC_AIO_PRIO_DELTA_MAX = 25,
    _SC_DELAYTIMER_MAX = 26,
    _SC_MQ_OPEN_MAX = 27,
    _SC_MQ_PRIO_MAX = 28,
    _SC_VERSION = 29,
    _SC_PAGESIZE = 30,
    _SC_RTSIG_MAX = 31,
    _SC_SEM_NSEMS_MAX = 32,
    _SC_SEM_VALUE_MAX = 33,
    _SC_SIGQUEUE_MAX = 34,
    _SC_TIMER_MAX = 35,
    _SC_BC_BASE_MAX = 36,
    _SC_BC_DIM_MAX = 37,
    _SC_BC_SCALE_MAX = 38,
    _SC_BC_STRING_MAX = 39,
    _SC_COLL_WEIGHTS_MAX = 40,
    _SC_EQUIV_CLASS_MAX = 41,
    _SC_EXPR_NEST_MAX = 42,
    _SC_LINE_MAX = 43,
    _SC_RE_DUP_MAX = 44,
    _SC_CHARCLASS_NAME_MAX = 45,
    _SC_2_VERSION = 46,
    _SC_2_C_BIND = 47,
    _SC_2_C_DEV = 48,
    _SC_2_FORT_DEV = 49,
    _SC_2_FORT_RUN = 50,
    _SC_2_SW_DEV = 51,
    _SC_2_LOCALEDEF = 52,
    _SC_PII = 53,
    _SC_PII_XTI = 54,
    _SC_PII_SOCKET = 55,
    _SC_PII_INTERNET = 56,
    _SC_PII_OSI = 57,
    _SC_POLL = 58,
    _SC_SELECT = 59,
    _SC_UIO_MAXIOV = 60,
    _SC_IOV_MAX = 60,
    _SC_PII_INTERNET_STREAM = 61,
    _SC_PII_INTERNET_DGRAM = 62,
    _SC_PII_OSI_COTS = 63,
    _SC_PII_OSI_CLTS = 64,
    _SC_PII_OSI_M = 65,
    _SC_T_IOV_MAX = 66,
    _SC_THREADS = 67,
    _SC_THREAD_SAFE_FUNCTIONS = 68,
    _SC_GETGR_R_SIZE_MAX = 69,
    _SC_GETPW_R_SIZE_MAX = 70,
    _SC_LOGIN_NAME_MAX = 71,
    _SC_TTY_NAME_MAX = 72,
    _SC_THREAD_DESTRUCTOR_ITERATIONS = 73,
    _SC_THREAD_KEYS_MAX = 74,
    _SC_THREAD_STACK_MIN = 75,
    _SC_THREAD_THREADS_MAX = 76,
    _SC_THREAD_ATTR_STACKADDR = 77,
    _SC_THREAD_ATTR_STACKSIZE = 78,
    _SC_THREAD_PRIORITY_SCHEDULING = 79,
    _SC_THREAD_PRIO_INHERIT = 80,
    _SC_THREAD_PRIO_PROTECT = 81,
    _SC_THREAD_PROCESS_SHARED = 82,
    _SC_NPROCESSORS_CONF = 83,
    _SC_NPROCESSORS_ONLN = 84,
    _SC_PHYS_PAGES = 85,
    _SC_AVPHYS_PAGES = 86,
    _SC_ATEXIT_MAX = 87,
    _SC_PASS_MAX = 88,
    _SC_XOPEN_VERSION = 89,
    _SC_XOPEN_XCU_VERSION = 90,
    _SC_XOPEN_UNIX = 91,
    _SC_XOPEN_CRYPT = 92,
    _SC_XOPEN_ENH_I18N = 93,
    _SC_XOPEN_SHM = 94,
    _SC_2_CHAR_TERM = 95,
    _SC_2_C_VERSION = 96,
    _SC_2_UPE = 97,
    _SC_XOPEN_XPG2 = 98,
    _SC_XOPEN_XPG3 = 99,
    _SC_XOPEN_XPG4 = 100,
    _SC_CHAR_BIT = 101,
    _SC_CHAR_MAX = 102,
    _SC_CHAR_MIN = 103,
    _SC_INT_MAX = 104,
    _SC_INT_MIN = 105,
    _SC_LONG_BIT = 106,
    _SC_WORD_BIT = 107,
    _SC_MB_LEN_MAX = 108,
    _SC_NZERO = 109,
    _SC_SSIZE_MAX = 110,
    _SC_SCHAR_MAX = 111,
    _SC_SCHAR_MIN = 112,
    _SC_SHRT_MAX = 113,
    _SC_SHRT_MIN = 114,
    _SC_UCHAR_MAX = 115,
    _SC_UINT_MAX = 116,
    _SC_ULONG_MAX = 117,
    _SC_USHRT_MAX = 118,
    _SC_NL_ARGMAX = 119,
    _SC_NL_LANGMAX = 120,
    _SC_NL_MSGMAX = 121,
    _SC_NL_NMAX = 122,
    _SC_NL_SETMAX = 123,
    _SC_NL_TEXTMAX = 124,
    _SC_XBS5_ILP32_OFF32 = 125,
    _SC_XBS5_ILP32_OFFBIG = 126,
    _SC_XBS5_LP64_OFF64 = 127,
    _SC_XBS5_LPBIG_OFFBIG = 128,
    _SC_XOPEN_LEGACY = 129,
    _SC_XOPEN_REALTIME = 130,
    _SC_XOPEN_REALTIME_THREADS = 131,
    _SC_ADVISORY_INFO = 132,
    _SC_BARRIERS = 133,
    _SC_BASE = 134,
    _SC_C_LANG_SUPPORT = 135,
    _SC_C_LANG_SUPPORT_R = 136,
    _SC_CLOCK_SELECTION = 137,
    _SC_CPUTIME = 138,
    _SC_THREAD_CPUTIME = 139,
    _SC_DEVICE_IO = 140,
    _SC_DEVICE_SPECIFIC = 141,
    _SC_DEVICE_SPECIFIC_R = 142,
    _SC_FD_MGMT = 143,
    _SC_FIFO = 144,
    _SC_PIPE = 145,
    _SC_FILE_ATTRIBUTES = 146,
    _SC_FILE_LOCKING = 147,
    _SC_FILE_SYSTEM = 148,
    _SC_MONOTONIC_CLOCK = 149,
    _SC_MULTI_PROCESS = 150,
    _SC_SINGLE_PROCESS = 151,
    _SC_NETWORKING = 152,
    _SC_READER_WRITER_LOCKS = 153,
    _SC_SPIN_LOCKS = 154,
    _SC_REGEXP = 155,
    _SC_REGEX_VERSION = 156,
    _SC_SHELL = 157,
    _SC_SIGNALS = 158,
    _SC_SPAWN = 159,
    _SC_SPORADIC_SERVER = 160,
    _SC_THREAD_SPORADIC_SERVER = 161,
    _SC_SYSTEM_DATABASE = 162,
    _SC_SYSTEM_DATABASE_R = 163,
    _SC_TIMEOUTS = 164,
    _SC_TYPED_MEMORY_OBJECTS = 165,
    _SC_USER_GROUPS = 166,
    _SC_USER_GROUPS_R = 167,
    _SC_2_PBS = 168,
    _SC_2_PBS_ACCOUNTING = 169,
    _SC_2_PBS_LOCATE = 170,
    _SC_2_PBS_MESSAGE = 171,
    _SC_2_PBS_TRACK = 172,
    _SC_SYMLOOP_MAX = 173,
    _SC_STREAMS = 174,
    _SC_2_PBS_CHECKPOINT = 175,
    _SC_V6_ILP32_OFF32 = 176,
    _SC_V6_ILP32_OFFBIG = 177,
    _SC_V6_LP64_OFF64 = 178,
    _SC_V6_LPBIG_OFFBIG = 179,
    _SC_HOST_NAME_MAX = 180,
    _SC_TRACE = 181,
    _SC_TRACE_EVENT_FILTER = 182,
    _SC_TRACE_INHERIT = 183,
    _SC_TRACE_LOG = 184,
    _SC_LEVEL1_ICACHE_SIZE = 185,
    _SC_LEVEL1_ICACHE_ASSOC = 186,
    _SC_LEVEL1_ICACHE_LINESIZE = 187,
    _SC_LEVEL1_DCACHE_SIZE = 188,
    _SC_LEVEL1_DCACHE_ASSOC = 189,
    _SC_LEVEL1_DCACHE_LINESIZE = 190,
    _SC_LEVEL2_CACHE_SIZE = 191,
    _SC_LEVEL2_CACHE_ASSOC = 192,
    _SC_LEVEL2_CACHE_LINESIZE = 193,
    _SC_LEVEL3_CACHE_SIZE = 194,
    _SC_LEVEL3_CACHE_ASSOC = 195,
    _SC_LEVEL3_CACHE_LINESIZE = 196,
    _SC_LEVEL4_CACHE_SIZE = 197,
    _SC_LEVEL4_CACHE_ASSOC = 198,
    _SC_LEVEL4_CACHE_LINESIZE = 199,
    _SC_IPV6 = 235,
    _SC_RAW_SOCKETS = 236
} ;
#line 506
enum __anonenum_53 {
    _CS_PATH = 0,
    _CS_V6_WIDTH_RESTRICTED_ENVS = 1,
    _CS_GNU_LIBC_VERSION = 2,
    _CS_GNU_LIBPTHREAD_VERSION = 3,
    _CS_LFS_CFLAGS = 1000,
    _CS_LFS_LDFLAGS = 1001,
    _CS_LFS_LIBS = 1002,
    _CS_LFS_LINTFLAGS = 1003,
    _CS_LFS64_CFLAGS = 1004,
    _CS_LFS64_LDFLAGS = 1005,
    _CS_LFS64_LIBS = 1006,
    _CS_LFS64_LINTFLAGS = 1007,
    _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,
    _CS_XBS5_ILP32_OFF32_LDFLAGS = 1101,
    _CS_XBS5_ILP32_OFF32_LIBS = 1102,
    _CS_XBS5_ILP32_OFF32_LINTFLAGS = 1103,
    _CS_XBS5_ILP32_OFFBIG_CFLAGS = 1104,
    _CS_XBS5_ILP32_OFFBIG_LDFLAGS = 1105,
    _CS_XBS5_ILP32_OFFBIG_LIBS = 1106,
    _CS_XBS5_ILP32_OFFBIG_LINTFLAGS = 1107,
    _CS_XBS5_LP64_OFF64_CFLAGS = 1108,
    _CS_XBS5_LP64_OFF64_LDFLAGS = 1109,
    _CS_XBS5_LP64_OFF64_LIBS = 1110,
    _CS_XBS5_LP64_OFF64_LINTFLAGS = 1111,
    _CS_XBS5_LPBIG_OFFBIG_CFLAGS = 1112,
    _CS_XBS5_LPBIG_OFFBIG_LDFLAGS = 1113,
    _CS_XBS5_LPBIG_OFFBIG_LIBS = 1114,
    _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS = 1115,
    _CS_POSIX_V6_ILP32_OFF32_CFLAGS = 1116,
    _CS_POSIX_V6_ILP32_OFF32_LDFLAGS = 1117,
    _CS_POSIX_V6_ILP32_OFF32_LIBS = 1118,
    _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS = 1119,
    _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS = 1120,
    _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS = 1121,
    _CS_POSIX_V6_ILP32_OFFBIG_LIBS = 1122,
    _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS = 1123,
    _CS_POSIX_V6_LP64_OFF64_CFLAGS = 1124,
    _CS_POSIX_V6_LP64_OFF64_LDFLAGS = 1125,
    _CS_POSIX_V6_LP64_OFF64_LIBS = 1126,
    _CS_POSIX_V6_LP64_OFF64_LINTFLAGS = 1127,
    _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS = 1128,
    _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS = 1129,
    _CS_POSIX_V6_LPBIG_OFFBIG_LIBS = 1130,
    _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS = 1131
} ;
#line 43 "/usr/include/bits/stat.h"
struct stat {
   __dev_t st_dev ;
   __ino_t st_ino ;
   __nlink_t st_nlink ;
   __mode_t st_mode ;
   __uid_t st_uid ;
   __gid_t st_gid ;
   int pad0 ;
   __dev_t st_rdev ;
   __off_t st_size ;
   __blksize_t st_blksize ;
   __blkcnt_t st_blocks ;
   struct timespec st_atim ;
   struct timespec st_mtim ;
   struct timespec st_ctim ;
   long __unused[3] ;
};
#line 61 "/usr/include/time.h"
typedef __clock_t clock_t;
#line 35 "/usr/include/sys/times.h"
struct tms {
   clock_t tms_utime ;
   clock_t tms_stime ;
   clock_t tms_cutime ;
   clock_t tms_cstime ;
};
#line 5172 "bzip2.c"
typedef int IntNative;
#line 5227 "bzip2.c"
struct __anonstruct_UInt64_54 {
   UChar b[8] ;
};
#line 5227 "bzip2.c"
typedef struct __anonstruct_UInt64_54 UInt64;
#line 6658 "bzip2.c"
struct zzzz {
   Char *name ;
   struct zzzz *link ;
};
#line 6658 "bzip2.c"
typedef struct zzzz Cell;
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
#line 343 "/usr/include/libio.h"
extern struct _IO_FILE_plus _IO_2_1_stdin_ ;
#line 344
extern struct _IO_FILE_plus _IO_2_1_stdout_ ;
#line 345
extern struct _IO_FILE_plus _IO_2_1_stderr_ ;
#line 413
extern int __underflow(_IO_FILE * ) ;
#line 414
extern int __uflow(_IO_FILE * ) ;
#line 415
extern int __overflow(_IO_FILE * , int  ) ;
#line 416
extern wint_t __wunderflow(_IO_FILE * ) ;
#line 417
extern wint_t __wuflow(_IO_FILE * ) ;
#line 418
extern wint_t __woverflow(_IO_FILE * , wint_t  ) ;
#line 451
extern int _IO_getc(_IO_FILE *__fp ) ;
#line 452
extern int _IO_putc(int __c , _IO_FILE *__fp ) ;
#line 453
extern  __attribute__((__nothrow__)) int _IO_feof(_IO_FILE *__fp ) ;
#line 454
extern  __attribute__((__nothrow__)) int _IO_ferror(_IO_FILE *__fp ) ;
#line 456
extern int _IO_peekc_locked(_IO_FILE *__fp ) ;
#line 462
extern  __attribute__((__nothrow__)) void _IO_flockfile(_IO_FILE * ) ;
#line 463
extern  __attribute__((__nothrow__)) void _IO_funlockfile(_IO_FILE * ) ;
#line 464
extern  __attribute__((__nothrow__)) int _IO_ftrylockfile(_IO_FILE * ) ;
#line 481
extern int _IO_vfscanf(_IO_FILE * __restrict   , char const   * __restrict   ,
                       __gnuc_va_list  , int * __restrict   ) ;
#line 483
extern int _IO_vfprintf(_IO_FILE * __restrict   , char const   * __restrict   ,
                        __gnuc_va_list  ) ;
#line 485
extern __ssize_t _IO_padn(_IO_FILE * , int  , __ssize_t  ) ;
#line 486
extern size_t _IO_sgetn(_IO_FILE * , void * , size_t  ) ;
#line 488
extern __off64_t _IO_seekoff(_IO_FILE * , __off64_t  , int  , int  ) ;
#line 489
extern __off64_t _IO_seekpos(_IO_FILE * , __off64_t  , int  ) ;
#line 491
extern  __attribute__((__nothrow__)) void _IO_free_backup_area(_IO_FILE * ) ;
#line 142 "/usr/include/stdio.h"
extern struct _IO_FILE *stdin ;
#line 143
extern struct _IO_FILE *stdout ;
#line 144
extern struct _IO_FILE *stderr ;
#line 154
extern  __attribute__((__nothrow__)) int remove(char const   *__filename ) ;
#line 156
extern  __attribute__((__nothrow__)) int rename(char const   *__old ,
                                                char const   *__new ) ;
#line 171
extern FILE *tmpfile(void) ;
#line 185
extern  __attribute__((__nothrow__)) char *tmpnam(char *__s ) ;
#line 191
extern  __attribute__((__nothrow__)) char *tmpnam_r(char *__s ) ;
#line 203
extern  __attribute__((__nothrow__)) char *tempnam(char const   *__dir ,
                                                   char const   *__pfx )  __attribute__((__malloc__)) ;
#line 213
extern int fclose(FILE *__stream ) ;
#line 218
extern int fflush(FILE *__stream ) ;
#line 228
extern int fflush_unlocked(FILE *__stream ) ;
#line 248
extern FILE *fopen(char const   * __restrict  __filename ,
                   char const   * __restrict  __modes ) ;
#line 254
extern FILE *freopen(char const   * __restrict  __filename ,
                     char const   * __restrict  __modes ,
                     FILE * __restrict  __stream ) ;
#line 280
extern  __attribute__((__nothrow__)) FILE *fdopen(int __fd ,
                                                  char const   *__modes ) ;
#line 303
extern  __attribute__((__nothrow__)) void setbuf(FILE * __restrict  __stream ,
                                                 char * __restrict  __buf ) ;
#line 307
extern  __attribute__((__nothrow__)) int setvbuf(FILE * __restrict  __stream ,
                                                 char * __restrict  __buf ,
                                                 int __modes , size_t __n ) ;
#line 314
extern  __attribute__((__nothrow__)) void setbuffer(FILE * __restrict  __stream ,
                                                    char * __restrict  __buf ,
                                                    size_t __size ) ;
#line 318
extern  __attribute__((__nothrow__)) void setlinebuf(FILE *__stream ) ;
#line 327
extern int fprintf(FILE * __restrict  __stream ,
                   char const   * __restrict  __format  , ...) ;
#line 333
extern int printf(char const   * __restrict  __format  , ...) ;
#line 335
extern  __attribute__((__nothrow__)) int sprintf(char * __restrict  __s ,
                                                 char const   * __restrict  __format 
                                                 , ...) ;
#line 342
extern int vfprintf(FILE * __restrict  __s ,
                    char const   * __restrict  __format , __gnuc_va_list __arg ) ;
#line 348
extern int vprintf(char const   * __restrict  __format , __gnuc_va_list __arg ) ;
#line 350
extern  __attribute__((__nothrow__)) int vsprintf(char * __restrict  __s ,
                                                  char const   * __restrict  __format ,
                                                  __gnuc_va_list __arg ) ;
#line 357
extern  __attribute__((__nothrow__)) int ( /* format attribute */  snprintf)(char * __restrict  __s ,
                                                                             size_t __maxlen ,
                                                                             char const   * __restrict  __format 
                                                                             , ...) ;
#line 361
extern  __attribute__((__nothrow__)) int ( /* format attribute */  vsnprintf)(char * __restrict  __s ,
                                                                              size_t __maxlen ,
                                                                              char const   * __restrict  __format ,
                                                                              __gnuc_va_list __arg ) ;
#line 399
extern int fscanf(FILE * __restrict  __stream ,
                  char const   * __restrict  __format  , ...) ;
#line 405
extern int scanf(char const   * __restrict  __format  , ...) ;
#line 407
extern  __attribute__((__nothrow__)) int sscanf(char const   * __restrict  __s ,
                                                char const   * __restrict  __format 
                                                , ...) ;
#line 441
extern int fgetc(FILE *__stream ) ;
#line 442
extern int getc(FILE *__stream ) ;
#line 448
extern int getchar(void) ;
#line 460
extern int getc_unlocked(FILE *__stream ) ;
#line 461
extern int getchar_unlocked(void) ;
#line 471
extern int fgetc_unlocked(FILE *__stream ) ;
#line 483
extern int fputc(int __c , FILE *__stream ) ;
#line 484
extern int putc(int __c , FILE *__stream ) ;
#line 490
extern int putchar(int __c ) ;
#line 504
extern int fputc_unlocked(int __c , FILE *__stream ) ;
#line 512
extern int putc_unlocked(int __c , FILE *__stream ) ;
#line 513
extern int putchar_unlocked(int __c ) ;
#line 520
extern int getw(FILE *__stream ) ;
#line 523
extern int putw(int __w , FILE *__stream ) ;
#line 532
extern char *fgets(char * __restrict  __s , int __n ,
                   FILE * __restrict  __stream ) ;
#line 540
extern char *gets(char *__s ) ;
#line 590
extern int fputs(char const   * __restrict  __s , FILE * __restrict  __stream ) ;
#line 596
extern int puts(char const   *__s ) ;
#line 603
extern int ungetc(int __c , FILE *__stream ) ;
#line 610
extern size_t fread(void * __restrict  __ptr , size_t __size , size_t __n ,
                    FILE * __restrict  __stream ) ;
#line 616
extern size_t fwrite(void const   * __restrict  __ptr , size_t __size ,
                     size_t __n , FILE * __restrict  __s ) ;
#line 638
extern size_t fread_unlocked(void * __restrict  __ptr , size_t __size ,
                             size_t __n , FILE * __restrict  __stream ) ;
#line 640
extern size_t fwrite_unlocked(void const   * __restrict  __ptr , size_t __size ,
                              size_t __n , FILE * __restrict  __stream ) ;
#line 650
extern int fseek(FILE *__stream , long __off , int __whence ) ;
#line 655
extern long ftell(FILE *__stream ) ;
#line 660
extern void rewind(FILE *__stream ) ;
#line 674
extern int fseeko(FILE *__stream , __off_t __off , int __whence ) ;
#line 679
extern __off_t ftello(FILE *__stream ) ;
#line 699
extern int fgetpos(FILE * __restrict  __stream , fpos_t * __restrict  __pos ) ;
#line 704
extern int fsetpos(FILE *__stream , fpos_t const   *__pos ) ;
#line 727
extern  __attribute__((__nothrow__)) void clearerr(FILE *__stream ) ;
#line 729
extern  __attribute__((__nothrow__)) int feof(FILE *__stream ) ;
#line 731
extern  __attribute__((__nothrow__)) int ferror(FILE *__stream ) ;
#line 736
extern  __attribute__((__nothrow__)) void clearerr_unlocked(FILE *__stream ) ;
#line 737
extern  __attribute__((__nothrow__)) int feof_unlocked(FILE *__stream ) ;
#line 738
extern  __attribute__((__nothrow__)) int ferror_unlocked(FILE *__stream ) ;
#line 747
extern void perror(char const   *__s ) ;
#line 27 "/usr/include/bits/sys_errlist.h"
extern int sys_nerr ;
#line 28
extern char const   * const  sys_errlist[] ;
#line 759 "/usr/include/stdio.h"
extern  __attribute__((__nothrow__)) int fileno(FILE *__stream ) ;
#line 764
extern  __attribute__((__nothrow__)) int fileno_unlocked(FILE *__stream ) ;
#line 774
extern FILE *popen(char const   *__command , char const   *__modes ) ;
#line 780
extern int pclose(FILE *__stream ) ;
#line 786
extern  __attribute__((__nothrow__)) char *ctermid(char *__s ) ;
#line 814
extern  __attribute__((__nothrow__)) void flockfile(FILE *__stream ) ;
#line 818
extern  __attribute__((__nothrow__)) int ftrylockfile(FILE *__stream ) ;
#line 821
extern  __attribute__((__nothrow__)) void funlockfile(FILE *__stream ) ;
#line 137 "bzip2.c"
int BZ2_bzCompressInit(bz_stream *strm , int blockSize100k , int verbosity ,
                       int workFactor ) ;
#line 144
int BZ2_bzCompress(bz_stream *strm , int action ) ;
#line 149
int BZ2_bzCompressEnd(bz_stream *strm ) ;
#line 153
int BZ2_bzDecompressInit(bz_stream *strm , int verbosity , int small ) ;
#line 159
int BZ2_bzDecompress(bz_stream *strm ) ;
#line 163
int BZ2_bzDecompressEnd(bz_stream *strm ) ;
#line 176
BZFILE *BZ2_bzReadOpen(int *bzerror , FILE *f , int verbosity , int small ,
                       void *unused , int nUnused ) ;
#line 185
void BZ2_bzReadClose(int *bzerror , BZFILE *b ) ;
#line 190
void BZ2_bzReadGetUnused(int *bzerror , BZFILE *b , void **unused ,
                         int *nUnused ) ;
#line 197
int BZ2_bzRead(int *bzerror , BZFILE *b , void *buf , int len ) ;
#line 204
BZFILE *BZ2_bzWriteOpen(int *bzerror , FILE *f , int blockSize100k ,
                        int verbosity , int workFactor ) ;
#line 212
void BZ2_bzWrite(int *bzerror , BZFILE *b , void *buf , int len ) ;
#line 219
void BZ2_bzWriteClose(int *bzerror , BZFILE *b , int abandon ,
                      unsigned int *nbytes_in , unsigned int *nbytes_out ) ;
#line 227
void BZ2_bzWriteClose64(int *bzerror , BZFILE *b , int abandon ,
                        unsigned int *nbytes_in_lo32 ,
                        unsigned int *nbytes_in_hi32 ,
                        unsigned int *nbytes_out_lo32 ,
                        unsigned int *nbytes_out_hi32 ) ;
#line 241
int BZ2_bzBuffToBuffCompress(char *dest , unsigned int *destLen , char *source ,
                             unsigned int sourceLen , int blockSize100k ,
                             int verbosity , int workFactor ) ;
#line 251
int BZ2_bzBuffToBuffDecompress(char *dest , unsigned int *destLen ,
                               char *source , unsigned int sourceLen ,
                               int small , int verbosity ) ;
#line 271
char const   *BZ2_bzlibVersion(void) ;
#line 276
BZFILE *BZ2_bzopen(char const   *path , char const   *mode ) ;
#line 281
BZFILE *BZ2_bzdopen(int fd , char const   *mode ) ;
#line 286
int BZ2_bzread(BZFILE *b , void *buf , int len ) ;
#line 292
int BZ2_bzwrite(BZFILE *b , void *buf , int len ) ;
#line 298
int BZ2_bzflush(BZFILE *b ) ;
#line 302
void BZ2_bzclose(BZFILE *b ) ;
#line 306
char const   *BZ2_bzerror(BZFILE *b , int *errnum ) ;
#line 140 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) size_t __ctype_get_mb_cur_max(void) ;
#line 145
extern  __attribute__((__nothrow__)) double atof(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 148
extern  __attribute__((__nothrow__)) int atoi(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 151
extern  __attribute__((__nothrow__)) long atol(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 158
extern  __attribute__((__nothrow__)) long long atoll(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 165
extern  __attribute__((__nothrow__)) double strtod(char const   * __restrict  __nptr ,
                                                   char ** __restrict  __endptr )  __attribute__((__nonnull__(1))) ;
#line 184
extern  __attribute__((__nothrow__)) long strtol(char const   * __restrict  __nptr ,
                                                 char ** __restrict  __endptr ,
                                                 int __base )  __attribute__((__nonnull__(1))) ;
#line 188
extern  __attribute__((__nothrow__)) unsigned long strtoul(char const   * __restrict  __nptr ,
                                                           char ** __restrict  __endptr ,
                                                           int __base )  __attribute__((__nonnull__(1))) ;
#line 196
extern  __attribute__((__nothrow__)) long long strtoq(char const   * __restrict  __nptr ,
                                                      char ** __restrict  __endptr ,
                                                      int __base )  __attribute__((__nonnull__(1))) ;
#line 201
extern  __attribute__((__nothrow__)) unsigned long long strtouq(char const   * __restrict  __nptr ,
                                                                char ** __restrict  __endptr ,
                                                                int __base )  __attribute__((__nonnull__(1))) ;
#line 210
extern  __attribute__((__nothrow__)) long long strtoll(char const   * __restrict  __nptr ,
                                                       char ** __restrict  __endptr ,
                                                       int __base )  __attribute__((__nonnull__(1))) ;
#line 215
extern  __attribute__((__nothrow__)) unsigned long long strtoull(char const   * __restrict  __nptr ,
                                                                 char ** __restrict  __endptr ,
                                                                 int __base )  __attribute__((__nonnull__(1))) ;
#line 279
extern  __attribute__((__nothrow__)) double __strtod_internal(char const   * __restrict  __nptr ,
                                                              char ** __restrict  __endptr ,
                                                              int __group )  __attribute__((__nonnull__(1))) ;
#line 282
extern  __attribute__((__nothrow__)) float __strtof_internal(char const   * __restrict  __nptr ,
                                                             char ** __restrict  __endptr ,
                                                             int __group )  __attribute__((__nonnull__(1))) ;
#line 285
extern  __attribute__((__nothrow__)) long double __strtold_internal(char const   * __restrict  __nptr ,
                                                                    char ** __restrict  __endptr ,
                                                                    int __group )  __attribute__((__nonnull__(1))) ;
#line 290
extern  __attribute__((__nothrow__)) long __strtol_internal(char const   * __restrict  __nptr ,
                                                            char ** __restrict  __endptr ,
                                                            int __base ,
                                                            int __group )  __attribute__((__nonnull__(1))) ;
#line 297
extern  __attribute__((__nothrow__)) unsigned long __strtoul_internal(char const   * __restrict  __nptr ,
                                                                      char ** __restrict  __endptr ,
                                                                      int __base ,
                                                                      int __group )  __attribute__((__nonnull__(1))) ;
#line 306
extern  __attribute__((__nothrow__)) long long __strtoll_internal(char const   * __restrict  __nptr ,
                                                                  char ** __restrict  __endptr ,
                                                                  int __base ,
                                                                  int __group )  __attribute__((__nonnull__(1))) ;
#line 314
extern  __attribute__((__nothrow__)) unsigned long long __strtoull_internal(char const   * __restrict  __nptr ,
                                                                            char ** __restrict  __endptr ,
                                                                            int __base ,
                                                                            int __group )  __attribute__((__nonnull__(1))) ;
#line 429
extern  __attribute__((__nothrow__)) char *l64a(long __n ) ;
#line 432
extern  __attribute__((__nothrow__)) long a64l(char const   *__s )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 109 "/usr/include/sys/select.h"
extern int select(int __nfds , fd_set * __restrict  __readfds ,
                  fd_set * __restrict  __writefds ,
                  fd_set * __restrict  __exceptfds ,
                  struct timeval * __restrict  __timeout ) ;
#line 121
extern int pselect(int __nfds , fd_set * __restrict  __readfds ,
                   fd_set * __restrict  __writefds ,
                   fd_set * __restrict  __exceptfds ,
                   struct timespec  const  * __restrict  __timeout ,
                   __sigset_t const   * __restrict  __sigmask ) ;
#line 30 "/usr/include/sys/sysmacros.h"
__inline extern  __attribute__((__nothrow__)) unsigned int gnu_dev_major(unsigned long long __dev ) ;
#line 33
__inline extern  __attribute__((__nothrow__)) unsigned int gnu_dev_minor(unsigned long long __dev ) ;
#line 36
__inline extern  __attribute__((__nothrow__)) unsigned long long gnu_dev_makedev(unsigned int __major ,
                                                                                 unsigned int __minor ) ;
#line 41
__inline extern  __attribute__((__nothrow__)) unsigned int gnu_dev_major(unsigned long long __dev ) ;
#line 41 "/usr/include/sys/sysmacros.h"
__inline extern unsigned int gnu_dev_major(unsigned long long __dev ) 
{ 

  {
#line 44
  return ((unsigned int )(((__dev >> 8) & 4095ULL) | (unsigned long long )((unsigned int )(__dev >> 32) & 4294963200U)));
}
}
#line 47
__inline extern  __attribute__((__nothrow__)) unsigned int gnu_dev_minor(unsigned long long __dev ) ;
#line 47 "/usr/include/sys/sysmacros.h"
__inline extern unsigned int gnu_dev_minor(unsigned long long __dev ) 
{ 

  {
#line 50
  return ((unsigned int )((__dev & 255ULL) | (unsigned long long )((unsigned int )(__dev >> 12) & 4294967040U)));
}
}
#line 53
__inline extern  __attribute__((__nothrow__)) unsigned long long gnu_dev_makedev(unsigned int __major ,
                                                                                 unsigned int __minor ) ;
#line 53 "/usr/include/sys/sysmacros.h"
__inline extern unsigned long long gnu_dev_makedev(unsigned int __major ,
                                                   unsigned int __minor ) 
{ 

  {
#line 56
  return (((unsigned long long )((__minor & 255U) | ((__major & 4095U) << 8)) | ((unsigned long long )(__minor & 4294967040U) << 12)) | ((unsigned long long )(__major & 4294963200U) << 32));
}
}
#line 445 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) long random(void) ;
#line 448
extern  __attribute__((__nothrow__)) void srandom(unsigned int __seed ) ;
#line 454
extern  __attribute__((__nothrow__)) char *initstate(unsigned int __seed ,
                                                     char *__statebuf ,
                                                     size_t __statelen )  __attribute__((__nonnull__(2))) ;
#line 459
extern  __attribute__((__nothrow__)) char *setstate(char *__statebuf )  __attribute__((__nonnull__(1))) ;
#line 478
extern  __attribute__((__nothrow__)) int random_r(struct random_data * __restrict  __buf ,
                                                  int32_t * __restrict  __result )  __attribute__((__nonnull__(1,2))) ;
#line 481
extern  __attribute__((__nothrow__)) int srandom_r(unsigned int __seed ,
                                                   struct random_data *__buf )  __attribute__((__nonnull__(2))) ;
#line 484
extern  __attribute__((__nothrow__)) int initstate_r(unsigned int __seed ,
                                                     char * __restrict  __statebuf ,
                                                     size_t __statelen ,
                                                     struct random_data * __restrict  __buf )  __attribute__((__nonnull__(2,4))) ;
#line 489
extern  __attribute__((__nothrow__)) int setstate_r(char * __restrict  __statebuf ,
                                                    struct random_data * __restrict  __buf )  __attribute__((__nonnull__(1,2))) ;
#line 498
extern  __attribute__((__nothrow__)) int rand(void) ;
#line 500
extern  __attribute__((__nothrow__)) void srand(unsigned int __seed ) ;
#line 505
extern  __attribute__((__nothrow__)) int rand_r(unsigned int *__seed ) ;
#line 513
extern  __attribute__((__nothrow__)) double drand48(void) ;
#line 514
extern  __attribute__((__nothrow__)) double erand48(unsigned short *__xsubi )  __attribute__((__nonnull__(1))) ;
#line 517
extern  __attribute__((__nothrow__)) long lrand48(void) ;
#line 518
extern  __attribute__((__nothrow__)) long nrand48(unsigned short *__xsubi )  __attribute__((__nonnull__(1))) ;
#line 522
extern  __attribute__((__nothrow__)) long mrand48(void) ;
#line 523
extern  __attribute__((__nothrow__)) long jrand48(unsigned short *__xsubi )  __attribute__((__nonnull__(1))) ;
#line 527
extern  __attribute__((__nothrow__)) void srand48(long __seedval ) ;
#line 528
extern  __attribute__((__nothrow__)) unsigned short *seed48(unsigned short *__seed16v )  __attribute__((__nonnull__(1))) ;
#line 530
extern  __attribute__((__nothrow__)) void lcong48(unsigned short *__param )  __attribute__((__nonnull__(1))) ;
#line 546
extern  __attribute__((__nothrow__)) int drand48_r(struct drand48_data * __restrict  __buffer ,
                                                   double * __restrict  __result )  __attribute__((__nonnull__(1,2))) ;
#line 548
extern  __attribute__((__nothrow__)) int erand48_r(unsigned short *__xsubi ,
                                                   struct drand48_data * __restrict  __buffer ,
                                                   double * __restrict  __result )  __attribute__((__nonnull__(1,2))) ;
#line 553
extern  __attribute__((__nothrow__)) int lrand48_r(struct drand48_data * __restrict  __buffer ,
                                                   long * __restrict  __result )  __attribute__((__nonnull__(1,2))) ;
#line 556
extern  __attribute__((__nothrow__)) int nrand48_r(unsigned short *__xsubi ,
                                                   struct drand48_data * __restrict  __buffer ,
                                                   long * __restrict  __result )  __attribute__((__nonnull__(1,2))) ;
#line 562
extern  __attribute__((__nothrow__)) int mrand48_r(struct drand48_data * __restrict  __buffer ,
                                                   long * __restrict  __result )  __attribute__((__nonnull__(1,2))) ;
#line 565
extern  __attribute__((__nothrow__)) int jrand48_r(unsigned short *__xsubi ,
                                                   struct drand48_data * __restrict  __buffer ,
                                                   long * __restrict  __result )  __attribute__((__nonnull__(1,2))) ;
#line 571
extern  __attribute__((__nothrow__)) int srand48_r(long __seedval ,
                                                   struct drand48_data *__buffer )  __attribute__((__nonnull__(2))) ;
#line 574
extern  __attribute__((__nothrow__)) int seed48_r(unsigned short *__seed16v ,
                                                  struct drand48_data *__buffer )  __attribute__((__nonnull__(1,2))) ;
#line 577
extern  __attribute__((__nothrow__)) int lcong48_r(unsigned short *__param ,
                                                   struct drand48_data *__buffer )  __attribute__((__nonnull__(1,2))) ;
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
#line 608
extern  __attribute__((__nothrow__)) void cfree(void *__ptr ) ;
#line 33 "/usr/include/alloca.h"
extern  __attribute__((__nothrow__)) void *alloca(size_t __size ) ;
#line 617 "/usr/include/stdlib.h"
extern  __attribute__((__nothrow__)) void *valloc(size_t __size )  __attribute__((__malloc__)) ;
#line 622
extern  __attribute__((__nothrow__)) int posix_memalign(void **__memptr ,
                                                        size_t __alignment ,
                                                        size_t __size )  __attribute__((__nonnull__(1))) ;
#line 628
extern  __attribute__((__nothrow__, __noreturn__)) void abort(void) ;
#line 632
extern  __attribute__((__nothrow__)) int atexit(void (*__func)(void) )  __attribute__((__nonnull__(1))) ;
#line 638
extern  __attribute__((__nothrow__)) int on_exit(void (*__func)(int __status ,
                                                                void *__arg ) ,
                                                 void *__arg )  __attribute__((__nonnull__(1))) ;
#line 646
extern  __attribute__((__nothrow__, __noreturn__)) void exit(int __status ) ;
#line 660
extern  __attribute__((__nothrow__)) char *getenv(char const   *__name )  __attribute__((__nonnull__(1))) ;
#line 665
extern  __attribute__((__nothrow__)) char *__secure_getenv(char const   *__name )  __attribute__((__nonnull__(1))) ;
#line 672
extern  __attribute__((__nothrow__)) int putenv(char *__string )  __attribute__((__nonnull__(1))) ;
#line 678
extern  __attribute__((__nothrow__)) int setenv(char const   *__name ,
                                                char const   *__value ,
                                                int __replace )  __attribute__((__nonnull__(2))) ;
#line 682
extern  __attribute__((__nothrow__)) int unsetenv(char const   *__name ) ;
#line 689
extern  __attribute__((__nothrow__)) int clearenv(void) ;
#line 698
extern  __attribute__((__nothrow__)) char *mktemp(char *__template )  __attribute__((__nonnull__(1))) ;
#line 709
extern int mkstemp(char *__template )  __attribute__((__nonnull__(1))) ;
#line 729
extern  __attribute__((__nothrow__)) char *mkdtemp(char *__template )  __attribute__((__nonnull__(1))) ;
#line 738
extern int system(char const   *__command ) ;
#line 756
extern  __attribute__((__nothrow__)) char *realpath(char const   * __restrict  __name ,
                                                    char * __restrict  __resolved ) ;
#line 774
extern void *bsearch(void const   *__key , void const   *__base ,
                     size_t __nmemb , size_t __size ,
                     int (*__compar)(void const   * , void const   * ) )  __attribute__((__nonnull__(1,2,5))) ;
#line 780
extern void qsort(void *__base , size_t __nmemb , size_t __size ,
                  int (*__compar)(void const   * , void const   * ) )  __attribute__((__nonnull__(1,4))) ;
#line 785
extern  __attribute__((__nothrow__)) int abs(int __x )  __attribute__((__const__)) ;
#line 786
extern  __attribute__((__nothrow__)) long labs(long __x )  __attribute__((__const__)) ;
#line 799
extern  __attribute__((__nothrow__)) div_t div(int __numer , int __denom )  __attribute__((__const__)) ;
#line 801
extern  __attribute__((__nothrow__)) ldiv_t ldiv(long __numer , long __denom )  __attribute__((__const__)) ;
#line 821
extern  __attribute__((__nothrow__)) char *ecvt(double __value , int __ndigit ,
                                                int * __restrict  __decpt ,
                                                int * __restrict  __sign )  __attribute__((__nonnull__(3,4))) ;
#line 827
extern  __attribute__((__nothrow__)) char *fcvt(double __value , int __ndigit ,
                                                int * __restrict  __decpt ,
                                                int * __restrict  __sign )  __attribute__((__nonnull__(3,4))) ;
#line 833
extern  __attribute__((__nothrow__)) char *gcvt(double __value , int __ndigit ,
                                                char *__buf )  __attribute__((__nonnull__(3))) ;
#line 839
extern  __attribute__((__nothrow__)) char *qecvt(long double __value ,
                                                 int __ndigit ,
                                                 int * __restrict  __decpt ,
                                                 int * __restrict  __sign )  __attribute__((__nonnull__(3,4))) ;
#line 842
extern  __attribute__((__nothrow__)) char *qfcvt(long double __value ,
                                                 int __ndigit ,
                                                 int * __restrict  __decpt ,
                                                 int * __restrict  __sign )  __attribute__((__nonnull__(3,4))) ;
#line 845
extern  __attribute__((__nothrow__)) char *qgcvt(long double __value ,
                                                 int __ndigit , char *__buf )  __attribute__((__nonnull__(3))) ;
#line 851
extern  __attribute__((__nothrow__)) int ecvt_r(double __value , int __ndigit ,
                                                int * __restrict  __decpt ,
                                                int * __restrict  __sign ,
                                                char * __restrict  __buf ,
                                                size_t __len )  __attribute__((__nonnull__(3,4,5))) ;
#line 854
extern  __attribute__((__nothrow__)) int fcvt_r(double __value , int __ndigit ,
                                                int * __restrict  __decpt ,
                                                int * __restrict  __sign ,
                                                char * __restrict  __buf ,
                                                size_t __len )  __attribute__((__nonnull__(3,4,5))) ;
#line 858
extern  __attribute__((__nothrow__)) int qecvt_r(long double __value ,
                                                 int __ndigit ,
                                                 int * __restrict  __decpt ,
                                                 int * __restrict  __sign ,
                                                 char * __restrict  __buf ,
                                                 size_t __len )  __attribute__((__nonnull__(3,4,5))) ;
#line 862
extern  __attribute__((__nothrow__)) int qfcvt_r(long double __value ,
                                                 int __ndigit ,
                                                 int * __restrict  __decpt ,
                                                 int * __restrict  __sign ,
                                                 char * __restrict  __buf ,
                                                 size_t __len )  __attribute__((__nonnull__(3,4,5))) ;
#line 873
extern  __attribute__((__nothrow__)) int mblen(char const   *__s , size_t __n ) ;
#line 876
extern  __attribute__((__nothrow__)) int mbtowc(wchar_t * __restrict  __pwc ,
                                                char const   * __restrict  __s ,
                                                size_t __n ) ;
#line 880
extern  __attribute__((__nothrow__)) int wctomb(char *__s , wchar_t __wchar ) ;
#line 884
extern  __attribute__((__nothrow__)) size_t mbstowcs(wchar_t * __restrict  __pwcs ,
                                                     char const   * __restrict  __s ,
                                                     size_t __n ) ;
#line 887
extern  __attribute__((__nothrow__)) size_t wcstombs(char * __restrict  __s ,
                                                     wchar_t const   * __restrict  __pwcs ,
                                                     size_t __n ) ;
#line 898
extern  __attribute__((__nothrow__)) int rpmatch(char const   *__response )  __attribute__((__nonnull__(1))) ;
#line 926
extern int posix_openpt(int __oflag ) ;
#line 961
extern  __attribute__((__nothrow__)) int getloadavg(double *__loadavg ,
                                                    int __nelem )  __attribute__((__nonnull__(1))) ;
#line 81 "/usr/include/ctype.h"
extern unsigned short const   **__ctype_b_loc(void)  __attribute__((__const__)) ;
#line 83
extern __int32_t const   **__ctype_tolower_loc(void)  __attribute__((__const__)) ;
#line 85
extern __int32_t const   **__ctype_toupper_loc(void)  __attribute__((__const__)) ;
#line 102
extern  __attribute__((__nothrow__)) int isalnum(int  ) ;
#line 103
extern  __attribute__((__nothrow__)) int isalpha(int  ) ;
#line 104
extern  __attribute__((__nothrow__)) int iscntrl(int  ) ;
#line 105
extern  __attribute__((__nothrow__)) int isdigit(int  ) ;
#line 106
extern  __attribute__((__nothrow__)) int islower(int  ) ;
#line 107
extern  __attribute__((__nothrow__)) int isgraph(int  ) ;
#line 108
extern  __attribute__((__nothrow__)) int isprint(int  ) ;
#line 109
extern  __attribute__((__nothrow__)) int ispunct(int  ) ;
#line 110
extern  __attribute__((__nothrow__)) int isspace(int  ) ;
#line 111
extern  __attribute__((__nothrow__)) int isupper(int  ) ;
#line 112
extern  __attribute__((__nothrow__)) int isxdigit(int  ) ;
#line 116
extern  __attribute__((__nothrow__)) int tolower(int __c ) ;
#line 119
extern  __attribute__((__nothrow__)) int toupper(int __c ) ;
#line 142
extern  __attribute__((__nothrow__)) int isascii(int __c ) ;
#line 146
extern  __attribute__((__nothrow__)) int toascii(int __c ) ;
#line 150
extern  __attribute__((__nothrow__)) int _toupper(int  ) ;
#line 151
extern  __attribute__((__nothrow__)) int _tolower(int  ) ;
#line 38 "/usr/include/string.h"
extern  __attribute__((__nothrow__)) void *memcpy(void * __restrict  __dest ,
                                                  void const   * __restrict  __src ,
                                                  size_t __n )  __attribute__((__nonnull__(1,2))) ;
#line 43
extern  __attribute__((__nothrow__)) void *memmove(void *__dest ,
                                                   void const   *__src ,
                                                   size_t __n )  __attribute__((__nonnull__(1,2))) ;
#line 51
extern  __attribute__((__nothrow__)) void *memccpy(void * __restrict  __dest ,
                                                   void const   * __restrict  __src ,
                                                   int __c , size_t __n )  __attribute__((__nonnull__(1,2))) ;
#line 59
extern  __attribute__((__nothrow__)) void *memset(void *__s , int __c ,
                                                  size_t __n )  __attribute__((__nonnull__(1))) ;
#line 62
extern  __attribute__((__nothrow__)) int memcmp(void const   *__s1 ,
                                                void const   *__s2 , size_t __n )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 66
extern  __attribute__((__nothrow__)) void *memchr(void const   *__s , int __c ,
                                                  size_t __n )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 84
extern  __attribute__((__nothrow__)) char *strcpy(char * __restrict  __dest ,
                                                  char const   * __restrict  __src )  __attribute__((__nonnull__(1,2))) ;
#line 87
extern  __attribute__((__nothrow__)) char *strncpy(char * __restrict  __dest ,
                                                   char const   * __restrict  __src ,
                                                   size_t __n )  __attribute__((__nonnull__(1,2))) ;
#line 92
extern  __attribute__((__nothrow__)) char *strcat(char * __restrict  __dest ,
                                                  char const   * __restrict  __src )  __attribute__((__nonnull__(1,2))) ;
#line 95
extern  __attribute__((__nothrow__)) char *strncat(char * __restrict  __dest ,
                                                   char const   * __restrict  __src ,
                                                   size_t __n )  __attribute__((__nonnull__(1,2))) ;
#line 99
extern  __attribute__((__nothrow__)) int strcmp(char const   *__s1 ,
                                                char const   *__s2 )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 102
extern  __attribute__((__nothrow__)) int strncmp(char const   *__s1 ,
                                                 char const   *__s2 ,
                                                 size_t __n )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 106
extern  __attribute__((__nothrow__)) int strcoll(char const   *__s1 ,
                                                 char const   *__s2 )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 109
extern  __attribute__((__nothrow__)) size_t strxfrm(char * __restrict  __dest ,
                                                    char const   * __restrict  __src ,
                                                    size_t __n )  __attribute__((__nonnull__(2))) ;
#line 130
extern  __attribute__((__nothrow__)) char *strdup(char const   *__s )  __attribute__((__nonnull__(1),
__malloc__)) ;
#line 167
extern  __attribute__((__nothrow__)) char *strchr(char const   *__s , int __c )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 170
extern  __attribute__((__nothrow__)) char *strrchr(char const   *__s , int __c )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 184
extern  __attribute__((__nothrow__)) size_t strcspn(char const   *__s ,
                                                    char const   *__reject )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 188
extern  __attribute__((__nothrow__)) size_t strspn(char const   *__s ,
                                                   char const   *__accept )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 191
extern  __attribute__((__nothrow__)) char *strpbrk(char const   *__s ,
                                                   char const   *__accept )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 194
extern  __attribute__((__nothrow__)) char *strstr(char const   *__haystack ,
                                                  char const   *__needle )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 199
extern  __attribute__((__nothrow__)) char *strtok(char * __restrict  __s ,
                                                  char const   * __restrict  __delim )  __attribute__((__nonnull__(2))) ;
#line 205
extern  __attribute__((__nothrow__)) char *__strtok_r(char * __restrict  __s ,
                                                      char const   * __restrict  __delim ,
                                                      char ** __restrict  __save_ptr )  __attribute__((__nonnull__(2,3))) ;
#line 210
extern  __attribute__((__nothrow__)) char *strtok_r(char * __restrict  __s ,
                                                    char const   * __restrict  __delim ,
                                                    char ** __restrict  __save_ptr )  __attribute__((__nonnull__(2,3))) ;
#line 242
extern  __attribute__((__nothrow__)) size_t strlen(char const   *__s )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 256
extern  __attribute__((__nothrow__)) char *strerror(int __errnum ) ;
#line 270
extern  __attribute__((__nothrow__)) int strerror_r(int __errnum , char *__buf ,
                                                    size_t __buflen )  __asm__("__xpg_strerror_r") __attribute__((__nonnull__(2))) ;
#line 288
extern  __attribute__((__nothrow__)) void __bzero(void *__s , size_t __n )  __attribute__((__nonnull__(1))) ;
#line 292
extern  __attribute__((__nothrow__)) void bcopy(void const   *__src ,
                                                void *__dest , size_t __n )  __attribute__((__nonnull__(1,2))) ;
#line 296
extern  __attribute__((__nothrow__)) void bzero(void *__s , size_t __n )  __attribute__((__nonnull__(1))) ;
#line 299
extern  __attribute__((__nothrow__)) int bcmp(void const   *__s1 ,
                                              void const   *__s2 , size_t __n )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 303
extern  __attribute__((__nothrow__)) char *index(char const   *__s , int __c )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 307
extern  __attribute__((__nothrow__)) char *rindex(char const   *__s , int __c )  __attribute__((__pure__,
__nonnull__(1))) ;
#line 312
extern  __attribute__((__nothrow__)) int ffs(int __i )  __attribute__((__const__)) ;
#line 325
extern  __attribute__((__nothrow__)) int strcasecmp(char const   *__s1 ,
                                                    char const   *__s2 )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 329
extern  __attribute__((__nothrow__)) int strncasecmp(char const   *__s1 ,
                                                     char const   *__s2 ,
                                                     size_t __n )  __attribute__((__pure__,
__nonnull__(1,2))) ;
#line 348
extern  __attribute__((__nothrow__)) char *strsep(char ** __restrict  __stringp ,
                                                  char const   * __restrict  __delim )  __attribute__((__nonnull__(1,2))) ;
#line 358 "bzip2.c"
void BZ2_bz__AssertH__fail(int errcode ) ;
#line 426
Int32 BZ2_rNums[512] ;
#line 450
UInt32 BZ2_crc32Table[256] ;
#line 567
void BZ2_blockSort(EState *s ) ;
#line 570
void BZ2_compressBlock(EState *s , Bool is_last_block ) ;
#line 573
void BZ2_bsInitWrite(EState *s ) ;
#line 576
void BZ2_hbAssignCodes(Int32 *code , UChar *length , Int32 minLen ,
                       Int32 maxLen , Int32 alphaSize ) ;
#line 579
void BZ2_hbMakeCodeLengths(UChar *len , Int32 *freq , Int32 alphaSize ,
                           Int32 maxLen ) ;
#line 773
Int32 BZ2_indexIntoF(Int32 indx , Int32 *cftab ) ;
#line 776
Int32 BZ2_decompress(DState *s ) ;
#line 779
void BZ2_hbCreateDecodeTables(Int32 *limit , Int32 *base , Int32 *perm ,
                              UChar *length , Int32 minLen , Int32 maxLen ,
                              Int32 alphaSize ) ;
#line 813 "bzip2.c"
__inline static void fallbackSimpleSort(UInt32 *fmap , UInt32 *eclass ,
                                        Int32 lo , Int32 hi ) 
{ Int32 i ;
  Int32 j ;
  Int32 tmp ;
  UInt32 ec_tmp ;

  {
#line 823
  if (lo == hi) {
#line 823
    return;
  } else {

  }
#line 825
  if (hi - lo > 3) {
#line 826
    i = hi - 4;
#line 826
    while (i >= lo) {
#line 827
      tmp = (int )*(fmap + i);
#line 828
      ec_tmp = *(eclass + tmp);
#line 829
      j = i + 4;
#line 829
      while (1) {
#line 829
        if (j <= hi) {
#line 829
          if (ec_tmp > *(eclass + *(fmap + j))) {

          } else {
#line 829
            break;
          }
        } else {
#line 829
          break;
        }
#line 830
        *(fmap + (j - 4)) = *(fmap + j);
#line 829
        j += 4;
      }
#line 831
      *(fmap + (j - 4)) = (unsigned int )tmp;
#line 826
      i --;
    }
  } else {

  }
#line 835
  i = hi - 1;
#line 835
  while (i >= lo) {
#line 836
    tmp = (int )*(fmap + i);
#line 837
    ec_tmp = *(eclass + tmp);
#line 838
    j = i + 1;
#line 838
    while (1) {
#line 838
      if (j <= hi) {
#line 838
        if (ec_tmp > *(eclass + *(fmap + j))) {

        } else {
#line 838
          break;
        }
      } else {
#line 838
        break;
      }
#line 839
      *(fmap + (j - 1)) = *(fmap + j);
#line 838
      j ++;
    }
#line 840
    *(fmap + (j - 1)) = (unsigned int )tmp;
#line 835
    i --;
  }
#line 842
  return;
}
}
#line 875 "bzip2.c"
static void fallbackQSort3(UInt32 *fmap , UInt32 *eclass , Int32 loSt ,
                           Int32 hiSt ) 
{ Int32 unLo ;
  Int32 unHi ;
  Int32 ltLo ;
  Int32 gtHi ;
  Int32 n ;
  Int32 m ;
  Int32 sp ;
  Int32 lo ;
  Int32 hi ;
  UInt32 med ;
  UInt32 r ;
  UInt32 r3 ;
  Int32 stackLo[100] ;
  Int32 stackHi[100] ;
  Int32 zztmp ;
  Int32 zztmp___0 ;
  Int32 zztmp___1 ;
  Int32 yyp1 ;
  Int32 yyp2 ;
  Int32 yyn ;
  Int32 zztmp___2 ;
  Int32 yyp1___0 ;
  Int32 yyp2___0 ;
  Int32 yyn___0 ;
  Int32 zztmp___3 ;

  {
#line 887
  r = 0U;
#line 889
  sp = 0;
#line 890
  stackLo[sp] = loSt;
#line 890
  stackHi[sp] = hiSt;
#line 890
  sp ++;
#line 892
  while (sp > 0) {
#line 894
    if (! (sp < 100)) {
#line 894
      BZ2_bz__AssertH__fail(1004);
    } else {

    }
#line 896
    sp --;
#line 896
    lo = stackLo[sp];
#line 896
    hi = stackHi[sp];
#line 897
    if (hi - lo < 10) {
#line 898
      fallbackSimpleSort(fmap, eclass, lo, hi);
#line 899
      continue;
    } else {

    }
#line 909
    r = (r * 7621U + 1U) % 32768U;
#line 910
    r3 = r % 3U;
#line 911
    if (r3 == 0U) {
#line 911
      med = *(eclass + *(fmap + lo));
    } else {
#line 912
      if (r3 == 1U) {
#line 912
        med = *(eclass + *(fmap + ((lo + hi) >> 1)));
      } else {
#line 913
        med = *(eclass + *(fmap + hi));
      }
    }
#line 915
    ltLo = lo;
#line 915
    unLo = ltLo;
#line 916
    gtHi = hi;
#line 916
    unHi = gtHi;
#line 918
    while (1) {
#line 919
      while (! (unLo > unHi)) {
#line 921
        n = (int )*(eclass + *(fmap + unLo)) - (int )med;
#line 922
        if (n == 0) {
#line 923
          zztmp = (Int32 )*(fmap + unLo);
#line 923
          *(fmap + unLo) = *(fmap + ltLo);
#line 923
          *(fmap + ltLo) = (unsigned int )zztmp;
#line 924
          ltLo ++;
#line 924
          unLo ++;
#line 925
          continue;
        } else {

        }
#line 927
        if (n > 0) {
#line 927
          break;
        } else {

        }
#line 928
        unLo ++;
      }
#line 930
      while (! (unLo > unHi)) {
#line 932
        n = (int )*(eclass + *(fmap + unHi)) - (int )med;
#line 933
        if (n == 0) {
#line 934
          zztmp___0 = (Int32 )*(fmap + unHi);
#line 934
          *(fmap + unHi) = *(fmap + gtHi);
#line 934
          *(fmap + gtHi) = (unsigned int )zztmp___0;
#line 935
          gtHi --;
#line 935
          unHi --;
#line 936
          continue;
        } else {

        }
#line 938
        if (n < 0) {
#line 938
          break;
        } else {

        }
#line 939
        unHi --;
      }
#line 941
      if (unLo > unHi) {
#line 941
        break;
      } else {

      }
#line 942
      zztmp___1 = (Int32 )*(fmap + unLo);
#line 942
      *(fmap + unLo) = *(fmap + unHi);
#line 942
      *(fmap + unHi) = (unsigned int )zztmp___1;
#line 942
      unLo ++;
#line 942
      unHi --;
    }
#line 947
    if (gtHi < ltLo) {
#line 947
      continue;
    } else {

    }
#line 949
    if (ltLo - lo < unLo - ltLo) {
#line 949
      n = ltLo - lo;
    } else {
#line 949
      n = unLo - ltLo;
    }
#line 949
    yyp1 = lo;
#line 949
    yyp2 = unLo - n;
#line 949
    yyn = n;
#line 949
    while (yyn > 0) {
#line 949
      zztmp___2 = (Int32 )*(fmap + yyp1);
#line 949
      *(fmap + yyp1) = *(fmap + yyp2);
#line 949
      *(fmap + yyp2) = (unsigned int )zztmp___2;
#line 949
      yyp1 ++;
#line 949
      yyp2 ++;
#line 949
      yyn --;
    }
#line 950
    if (hi - gtHi < gtHi - unHi) {
#line 950
      m = hi - gtHi;
    } else {
#line 950
      m = gtHi - unHi;
    }
#line 950
    yyp1___0 = unLo;
#line 950
    yyp2___0 = (hi - m) + 1;
#line 950
    yyn___0 = m;
#line 950
    while (yyn___0 > 0) {
#line 950
      zztmp___3 = (Int32 )*(fmap + yyp1___0);
#line 950
      *(fmap + yyp1___0) = *(fmap + yyp2___0);
#line 950
      *(fmap + yyp2___0) = (unsigned int )zztmp___3;
#line 950
      yyp1___0 ++;
#line 950
      yyp2___0 ++;
#line 950
      yyn___0 --;
    }
#line 952
    n = ((lo + unLo) - ltLo) - 1;
#line 953
    m = (hi - (gtHi - unHi)) + 1;
#line 955
    if (n - lo > hi - m) {
#line 956
      stackLo[sp] = lo;
#line 956
      stackHi[sp] = n;
#line 956
      sp ++;
#line 957
      stackLo[sp] = m;
#line 957
      stackHi[sp] = hi;
#line 957
      sp ++;
    } else {
#line 959
      stackLo[sp] = m;
#line 959
      stackHi[sp] = hi;
#line 959
      sp ++;
#line 960
      stackLo[sp] = lo;
#line 960
      stackHi[sp] = n;
#line 960
      sp ++;
    }
  }
#line 963
  return;
}
}
#line 994 "bzip2.c"
static void fallbackSort(UInt32 *fmap , UInt32 *eclass , UInt32 *bhtab ,
                         Int32 nblock , Int32 verb ) 
{ Int32 ftab[257] ;
  Int32 ftabCopy[256] ;
  Int32 H ;
  Int32 i ;
  Int32 j ;
  Int32 k ;
  Int32 l ;
  Int32 r ;
  Int32 cc ;
  Int32 cc1 ;
  Int32 nNotDone ;
  Int32 nBhtab ;
  UChar *eclass8 ;

  {
#line 1006
  eclass8 = (UChar *)eclass;
#line 1012
  if (verb >= 4) {
#line 1013
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"        bucket sorting ...\n");
  } else {

  }
#line 1014
  i = 0;
#line 1014
  while (i < 257) {
#line 1014
    ftab[i] = 0;
#line 1014
    i ++;
  }
#line 1015
  i = 0;
#line 1015
  while (i < nblock) {
#line 1015
    (ftab[*(eclass8 + i)]) ++;
#line 1015
    i ++;
  }
#line 1016
  i = 0;
#line 1016
  while (i < 256) {
#line 1016
    ftabCopy[i] = ftab[i];
#line 1016
    i ++;
  }
#line 1017
  i = 1;
#line 1017
  while (i < 257) {
#line 1017
    ftab[i] += ftab[i - 1];
#line 1017
    i ++;
  }
#line 1019
  i = 0;
#line 1019
  while (i < nblock) {
#line 1020
    j = (int )*(eclass8 + i);
#line 1021
    k = ftab[j] - 1;
#line 1022
    ftab[j] = k;
#line 1023
    *(fmap + k) = (unsigned int )i;
#line 1019
    i ++;
  }
#line 1026
  nBhtab = 2 + nblock / 32;
#line 1027
  i = 0;
#line 1027
  while (i < nBhtab) {
#line 1027
    *(bhtab + i) = 0U;
#line 1027
    i ++;
  }
#line 1028
  i = 0;
#line 1028
  while (i < 256) {
#line 1028
    *(bhtab + (ftab[i] >> 5)) |= (unsigned int )(1 << (ftab[i] & 31));
#line 1028
    i ++;
  }
#line 1037
  i = 0;
#line 1037
  while (i < 32) {
#line 1038
    *(bhtab + ((nblock + 2 * i) >> 5)) |= (unsigned int )(1 << ((nblock + 2 * i) & 31));
#line 1039
    *(bhtab + (((nblock + 2 * i) + 1) >> 5)) &= (unsigned int )(~ (1 << (((nblock + 2 * i) + 1) & 31)));
#line 1037
    i ++;
  }
#line 1043
  H = 1;
#line 1044
  while (1) {
#line 1046
    if (verb >= 4) {
#line 1047
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"        depth %6d has ", H);
    } else {

    }
#line 1049
    j = 0;
#line 1050
    i = 0;
#line 1050
    while (i < nblock) {
#line 1051
      if (*(bhtab + (i >> 5)) & (unsigned int )(1 << (i & 31))) {
#line 1051
        j = i;
      } else {

      }
#line 1052
      k = (int )(*(fmap + i) - (UInt32 )H);
#line 1052
      if (k < 0) {
#line 1052
        k += nblock;
      } else {

      }
#line 1053
      *(eclass + k) = (unsigned int )j;
#line 1050
      i ++;
    }
#line 1056
    nNotDone = 0;
#line 1057
    r = -1;
#line 1058
    while (1) {
#line 1061
      k = r + 1;
#line 1062
      while (1) {
#line 1062
        if (*(bhtab + (k >> 5)) & (unsigned int )(1 << (k & 31))) {
#line 1062
          if (k & 31) {

          } else {
#line 1062
            break;
          }
        } else {
#line 1062
          break;
        }
#line 1062
        k ++;
      }
#line 1063
      if (*(bhtab + (k >> 5)) & (unsigned int )(1 << (k & 31))) {
#line 1064
        while (*(bhtab + (k >> 5)) == 4294967295U) {
#line 1064
          k += 32;
        }
#line 1065
        while (*(bhtab + (k >> 5)) & (unsigned int )(1 << (k & 31))) {
#line 1065
          k ++;
        }
      } else {

      }
#line 1067
      l = k - 1;
#line 1068
      if (l >= nblock) {
#line 1068
        break;
      } else {

      }
#line 1069
      while (1) {
#line 1069
        if (! (*(bhtab + (k >> 5)) & (unsigned int )(1 << (k & 31)))) {
#line 1069
          if (k & 31) {

          } else {
#line 1069
            break;
          }
        } else {
#line 1069
          break;
        }
#line 1069
        k ++;
      }
#line 1070
      if (! (*(bhtab + (k >> 5)) & (unsigned int )(1 << (k & 31)))) {
#line 1071
        while (*(bhtab + (k >> 5)) == 0U) {
#line 1071
          k += 32;
        }
#line 1072
        while (! (*(bhtab + (k >> 5)) & (unsigned int )(1 << (k & 31)))) {
#line 1072
          k ++;
        }
      } else {

      }
#line 1074
      r = k - 1;
#line 1075
      if (r >= nblock) {
#line 1075
        break;
      } else {

      }
#line 1078
      if (r > l) {
#line 1079
        nNotDone += (r - l) + 1;
#line 1080
        fallbackQSort3(fmap, eclass, l, r);
#line 1083
        cc = -1;
#line 1084
        i = l;
#line 1084
        while (i <= r) {
#line 1085
          cc1 = (int )*(eclass + *(fmap + i));
#line 1086
          if (cc != cc1) {
#line 1086
            *(bhtab + (i >> 5)) |= (unsigned int )(1 << (i & 31));
#line 1086
            cc = cc1;
          } else {

          }
#line 1084
          i ++;
        }
      } else {

      }
    }
#line 1091
    if (verb >= 4) {
#line 1092
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%6d unresolved strings\n",
              nNotDone);
    } else {

    }
#line 1094
    H *= 2;
#line 1095
    if (H > nblock) {
#line 1095
      break;
    } else {
#line 1095
      if (nNotDone == 0) {
#line 1095
        break;
      } else {

      }
    }
  }
#line 1103
  if (verb >= 4) {
#line 1104
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"        reconstructing block ...\n");
  } else {

  }
#line 1105
  j = 0;
#line 1106
  i = 0;
#line 1106
  while (i < nblock) {
#line 1107
    while (ftabCopy[j] == 0) {
#line 1107
      j ++;
    }
#line 1108
    (ftabCopy[j]) --;
#line 1109
    *(eclass8 + *(fmap + i)) = (unsigned char )j;
#line 1106
    i ++;
  }
#line 1111
  if (! (j < 256)) {
#line 1111
    BZ2_bz__AssertH__fail(1005);
  } else {

  }
#line 1112
  return;
}
}
#line 1128 "bzip2.c"
__inline static Bool mainGtU(UInt32 i1 , UInt32 i2 , UChar *block ,
                             UInt16 *quadrant , UInt32 nblock , Int32 *budget ) 
{ Int32 k ;
  UChar c1 ;
  UChar c2 ;
  UInt16 s1 ;
  UInt16 s2 ;

  {
#line 1143
  c1 = *(block + i1);
#line 1143
  c2 = *(block + i2);
#line 1144
  if ((int )c1 != (int )c2) {
#line 1144
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1145
  i1 ++;
#line 1145
  i2 ++;
#line 1147
  c1 = *(block + i1);
#line 1147
  c2 = *(block + i2);
#line 1148
  if ((int )c1 != (int )c2) {
#line 1148
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1149
  i1 ++;
#line 1149
  i2 ++;
#line 1151
  c1 = *(block + i1);
#line 1151
  c2 = *(block + i2);
#line 1152
  if ((int )c1 != (int )c2) {
#line 1152
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1153
  i1 ++;
#line 1153
  i2 ++;
#line 1155
  c1 = *(block + i1);
#line 1155
  c2 = *(block + i2);
#line 1156
  if ((int )c1 != (int )c2) {
#line 1156
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1157
  i1 ++;
#line 1157
  i2 ++;
#line 1159
  c1 = *(block + i1);
#line 1159
  c2 = *(block + i2);
#line 1160
  if ((int )c1 != (int )c2) {
#line 1160
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1161
  i1 ++;
#line 1161
  i2 ++;
#line 1163
  c1 = *(block + i1);
#line 1163
  c2 = *(block + i2);
#line 1164
  if ((int )c1 != (int )c2) {
#line 1164
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1165
  i1 ++;
#line 1165
  i2 ++;
#line 1167
  c1 = *(block + i1);
#line 1167
  c2 = *(block + i2);
#line 1168
  if ((int )c1 != (int )c2) {
#line 1168
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1169
  i1 ++;
#line 1169
  i2 ++;
#line 1171
  c1 = *(block + i1);
#line 1171
  c2 = *(block + i2);
#line 1172
  if ((int )c1 != (int )c2) {
#line 1172
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1173
  i1 ++;
#line 1173
  i2 ++;
#line 1175
  c1 = *(block + i1);
#line 1175
  c2 = *(block + i2);
#line 1176
  if ((int )c1 != (int )c2) {
#line 1176
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1177
  i1 ++;
#line 1177
  i2 ++;
#line 1179
  c1 = *(block + i1);
#line 1179
  c2 = *(block + i2);
#line 1180
  if ((int )c1 != (int )c2) {
#line 1180
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1181
  i1 ++;
#line 1181
  i2 ++;
#line 1183
  c1 = *(block + i1);
#line 1183
  c2 = *(block + i2);
#line 1184
  if ((int )c1 != (int )c2) {
#line 1184
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1185
  i1 ++;
#line 1185
  i2 ++;
#line 1187
  c1 = *(block + i1);
#line 1187
  c2 = *(block + i2);
#line 1188
  if ((int )c1 != (int )c2) {
#line 1188
    return ((unsigned char )((int )c1 > (int )c2));
  } else {

  }
#line 1189
  i1 ++;
#line 1189
  i2 ++;
#line 1191
  k = (int )(nblock + 8U);
#line 1193
  while (1) {
#line 1195
    c1 = *(block + i1);
#line 1195
    c2 = *(block + i2);
#line 1196
    if ((int )c1 != (int )c2) {
#line 1196
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1197
    s1 = *(quadrant + i1);
#line 1197
    s2 = *(quadrant + i2);
#line 1198
    if ((int )s1 != (int )s2) {
#line 1198
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1199
    i1 ++;
#line 1199
    i2 ++;
#line 1201
    c1 = *(block + i1);
#line 1201
    c2 = *(block + i2);
#line 1202
    if ((int )c1 != (int )c2) {
#line 1202
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1203
    s1 = *(quadrant + i1);
#line 1203
    s2 = *(quadrant + i2);
#line 1204
    if ((int )s1 != (int )s2) {
#line 1204
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1205
    i1 ++;
#line 1205
    i2 ++;
#line 1207
    c1 = *(block + i1);
#line 1207
    c2 = *(block + i2);
#line 1208
    if ((int )c1 != (int )c2) {
#line 1208
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1209
    s1 = *(quadrant + i1);
#line 1209
    s2 = *(quadrant + i2);
#line 1210
    if ((int )s1 != (int )s2) {
#line 1210
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1211
    i1 ++;
#line 1211
    i2 ++;
#line 1213
    c1 = *(block + i1);
#line 1213
    c2 = *(block + i2);
#line 1214
    if ((int )c1 != (int )c2) {
#line 1214
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1215
    s1 = *(quadrant + i1);
#line 1215
    s2 = *(quadrant + i2);
#line 1216
    if ((int )s1 != (int )s2) {
#line 1216
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1217
    i1 ++;
#line 1217
    i2 ++;
#line 1219
    c1 = *(block + i1);
#line 1219
    c2 = *(block + i2);
#line 1220
    if ((int )c1 != (int )c2) {
#line 1220
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1221
    s1 = *(quadrant + i1);
#line 1221
    s2 = *(quadrant + i2);
#line 1222
    if ((int )s1 != (int )s2) {
#line 1222
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1223
    i1 ++;
#line 1223
    i2 ++;
#line 1225
    c1 = *(block + i1);
#line 1225
    c2 = *(block + i2);
#line 1226
    if ((int )c1 != (int )c2) {
#line 1226
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1227
    s1 = *(quadrant + i1);
#line 1227
    s2 = *(quadrant + i2);
#line 1228
    if ((int )s1 != (int )s2) {
#line 1228
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1229
    i1 ++;
#line 1229
    i2 ++;
#line 1231
    c1 = *(block + i1);
#line 1231
    c2 = *(block + i2);
#line 1232
    if ((int )c1 != (int )c2) {
#line 1232
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1233
    s1 = *(quadrant + i1);
#line 1233
    s2 = *(quadrant + i2);
#line 1234
    if ((int )s1 != (int )s2) {
#line 1234
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1235
    i1 ++;
#line 1235
    i2 ++;
#line 1237
    c1 = *(block + i1);
#line 1237
    c2 = *(block + i2);
#line 1238
    if ((int )c1 != (int )c2) {
#line 1238
      return ((unsigned char )((int )c1 > (int )c2));
    } else {

    }
#line 1239
    s1 = *(quadrant + i1);
#line 1239
    s2 = *(quadrant + i2);
#line 1240
    if ((int )s1 != (int )s2) {
#line 1240
      return ((unsigned char )((int )s1 > (int )s2));
    } else {

    }
#line 1241
    i1 ++;
#line 1241
    i2 ++;
#line 1243
    if (i1 >= nblock) {
#line 1243
      i1 -= nblock;
    } else {

    }
#line 1244
    if (i2 >= nblock) {
#line 1244
      i2 -= nblock;
    } else {

    }
#line 1246
    k -= 8;
#line 1247
    (*budget) --;
#line 1193
    if (k >= 0) {

    } else {
#line 1193
      break;
    }
  }
#line 1251
  return ((unsigned char)0);
}
}
#line 1262 "bzip2.c"
static Int32 incs[14]  = 
#line 1262
  {      1,      4,      13,      40, 
        121,      364,      1093,      3280, 
        9841,      29524,      88573,      265720, 
        797161,      2391484};
#line 1267 "bzip2.c"
static void mainSimpleSort(UInt32 *ptr , UChar *block , UInt16 *quadrant ,
                           Int32 nblock , Int32 lo , Int32 hi , Int32 d ,
                           Int32 *budget ) 
{ Int32 i ;
  Int32 j ;
  Int32 h ;
  Int32 bigN ;
  Int32 hp ;
  UInt32 v ;
  Bool tmp ;
  Bool tmp___0 ;
  Bool tmp___1 ;

  {
#line 1280
  bigN = (hi - lo) + 1;
#line 1281
  if (bigN < 2) {
#line 1281
    return;
  } else {

  }
#line 1283
  hp = 0;
#line 1284
  while (incs[hp] < bigN) {
#line 1284
    hp ++;
  }
#line 1285
  hp --;
#line 1287
  while (hp >= 0) {
#line 1288
    h = incs[hp];
#line 1290
    i = lo + h;
#line 1291
    while (! (i > hi)) {
#line 1295
      v = *(ptr + i);
#line 1296
      j = i;
#line 1297
      while (1) {
#line 1297
        tmp = mainGtU(*(ptr + (j - h)) + (UInt32 )d, v + (UInt32 )d, block,
                      quadrant, (unsigned int )nblock, budget);
#line 1297
        if (tmp) {

        } else {
#line 1297
          break;
        }
#line 1300
        *(ptr + j) = *(ptr + (j - h));
#line 1301
        j -= h;
#line 1302
        if (j <= (lo + h) - 1) {
#line 1302
          break;
        } else {

        }
      }
#line 1304
      *(ptr + j) = v;
#line 1305
      i ++;
#line 1308
      if (i > hi) {
#line 1308
        break;
      } else {

      }
#line 1309
      v = *(ptr + i);
#line 1310
      j = i;
#line 1311
      while (1) {
#line 1311
        tmp___0 = mainGtU(*(ptr + (j - h)) + (UInt32 )d, v + (UInt32 )d, block,
                          quadrant, (unsigned int )nblock, budget);
#line 1311
        if (tmp___0) {

        } else {
#line 1311
          break;
        }
#line 1314
        *(ptr + j) = *(ptr + (j - h));
#line 1315
        j -= h;
#line 1316
        if (j <= (lo + h) - 1) {
#line 1316
          break;
        } else {

        }
      }
#line 1318
      *(ptr + j) = v;
#line 1319
      i ++;
#line 1322
      if (i > hi) {
#line 1322
        break;
      } else {

      }
#line 1323
      v = *(ptr + i);
#line 1324
      j = i;
#line 1325
      while (1) {
#line 1325
        tmp___1 = mainGtU(*(ptr + (j - h)) + (UInt32 )d, v + (UInt32 )d, block,
                          quadrant, (unsigned int )nblock, budget);
#line 1325
        if (tmp___1) {

        } else {
#line 1325
          break;
        }
#line 1328
        *(ptr + j) = *(ptr + (j - h));
#line 1329
        j -= h;
#line 1330
        if (j <= (lo + h) - 1) {
#line 1330
          break;
        } else {

        }
      }
#line 1332
      *(ptr + j) = v;
#line 1333
      i ++;
#line 1335
      if (*budget < 0) {
#line 1335
        return;
      } else {

      }
    }
#line 1287
    hp --;
  }
#line 1338
  return;
}
}
#line 1364 "bzip2.c"
__inline static UChar mmed3(UChar a , UChar b , UChar c ) 
{ UChar t ;

  {
#line 1369
  if ((int )a > (int )b) {
#line 1369
    t = a;
#line 1369
    a = b;
#line 1369
    b = t;
  } else {

  }
#line 1370
  if ((int )b > (int )c) {
#line 1371
    b = c;
#line 1372
    if ((int )a > (int )b) {
#line 1372
      b = a;
    } else {

    }
  } else {

  }
#line 1374
  return (b);
}
}
#line 1403 "bzip2.c"
static void mainQSort3(UInt32 *ptr , UChar *block , UInt16 *quadrant ,
                       Int32 nblock , Int32 loSt , Int32 hiSt , Int32 dSt ,
                       Int32 *budget ) 
{ Int32 unLo ;
  Int32 unHi ;
  Int32 ltLo ;
  Int32 gtHi ;
  Int32 n ;
  Int32 m ;
  Int32 med ;
  Int32 sp ;
  Int32 lo ;
  Int32 hi ;
  Int32 d ;
  Int32 stackLo[100] ;
  Int32 stackHi[100] ;
  Int32 stackD[100] ;
  Int32 nextLo[3] ;
  Int32 nextHi[3] ;
  Int32 nextD[3] ;
  UChar tmp ;
  Int32 zztmp ;
  Int32 zztmp___0 ;
  Int32 zztmp___1 ;
  Int32 yyp1 ;
  Int32 yyp2 ;
  Int32 yyn ;
  Int32 zztmp___2 ;
  Int32 yyp1___0 ;
  Int32 yyp2___0 ;
  Int32 yyn___0 ;
  Int32 zztmp___3 ;
  Int32 tz ;
  Int32 tz___0 ;
  Int32 tz___1 ;

  {
#line 1424
  sp = 0;
#line 1425
  stackLo[sp] = loSt;
#line 1425
  stackHi[sp] = hiSt;
#line 1425
  stackD[sp] = dSt;
#line 1425
  sp ++;
#line 1427
  while (sp > 0) {
#line 1429
    if (! (sp < 100)) {
#line 1429
      BZ2_bz__AssertH__fail(1001);
    } else {

    }
#line 1431
    sp --;
#line 1431
    lo = stackLo[sp];
#line 1431
    hi = stackHi[sp];
#line 1431
    d = stackD[sp];
#line 1432
    if (hi - lo < 20) {
      goto _L;
    } else {
#line 1432
      if (d > 14) {
        _L: /* CIL Label */ 
#line 1434
        mainSimpleSort(ptr, block, quadrant, nblock, lo, hi, d, budget);
#line 1435
        if (*budget < 0) {
#line 1435
          return;
        } else {

        }
#line 1436
        continue;
      } else {

      }
    }
#line 1439
    tmp = mmed3(*(block + (*(ptr + lo) + (UInt32 )d)),
                *(block + (*(ptr + hi) + (UInt32 )d)),
                *(block + (*(ptr + ((lo + hi) >> 1)) + (UInt32 )d)));
#line 1439
    med = (int )tmp;
#line 1444
    ltLo = lo;
#line 1444
    unLo = ltLo;
#line 1445
    gtHi = hi;
#line 1445
    unHi = gtHi;
#line 1447
    while (1) {
#line 1448
      while (! (unLo > unHi)) {
#line 1450
        n = (int )*(block + (*(ptr + unLo) + (UInt32 )d)) - med;
#line 1451
        if (n == 0) {
#line 1452
          zztmp = (Int32 )*(ptr + unLo);
#line 1452
          *(ptr + unLo) = *(ptr + ltLo);
#line 1452
          *(ptr + ltLo) = (unsigned int )zztmp;
#line 1453
          ltLo ++;
#line 1453
          unLo ++;
#line 1453
          continue;
        } else {

        }
#line 1455
        if (n > 0) {
#line 1455
          break;
        } else {

        }
#line 1456
        unLo ++;
      }
#line 1458
      while (! (unLo > unHi)) {
#line 1460
        n = (int )*(block + (*(ptr + unHi) + (UInt32 )d)) - med;
#line 1461
        if (n == 0) {
#line 1462
          zztmp___0 = (Int32 )*(ptr + unHi);
#line 1462
          *(ptr + unHi) = *(ptr + gtHi);
#line 1462
          *(ptr + gtHi) = (unsigned int )zztmp___0;
#line 1463
          gtHi --;
#line 1463
          unHi --;
#line 1463
          continue;
        } else {

        }
#line 1465
        if (n < 0) {
#line 1465
          break;
        } else {

        }
#line 1466
        unHi --;
      }
#line 1468
      if (unLo > unHi) {
#line 1468
        break;
      } else {

      }
#line 1469
      zztmp___1 = (Int32 )*(ptr + unLo);
#line 1469
      *(ptr + unLo) = *(ptr + unHi);
#line 1469
      *(ptr + unHi) = (unsigned int )zztmp___1;
#line 1469
      unLo ++;
#line 1469
      unHi --;
    }
#line 1474
    if (gtHi < ltLo) {
#line 1475
      stackLo[sp] = lo;
#line 1475
      stackHi[sp] = hi;
#line 1475
      stackD[sp] = d + 1;
#line 1475
      sp ++;
#line 1476
      continue;
    } else {

    }
#line 1479
    if (ltLo - lo < unLo - ltLo) {
#line 1479
      n = ltLo - lo;
    } else {
#line 1479
      n = unLo - ltLo;
    }
#line 1479
    yyp1 = lo;
#line 1479
    yyp2 = unLo - n;
#line 1479
    yyn = n;
#line 1479
    while (yyn > 0) {
#line 1479
      zztmp___2 = (Int32 )*(ptr + yyp1);
#line 1479
      *(ptr + yyp1) = *(ptr + yyp2);
#line 1479
      *(ptr + yyp2) = (unsigned int )zztmp___2;
#line 1479
      yyp1 ++;
#line 1479
      yyp2 ++;
#line 1479
      yyn --;
    }
#line 1480
    if (hi - gtHi < gtHi - unHi) {
#line 1480
      m = hi - gtHi;
    } else {
#line 1480
      m = gtHi - unHi;
    }
#line 1480
    yyp1___0 = unLo;
#line 1480
    yyp2___0 = (hi - m) + 1;
#line 1480
    yyn___0 = m;
#line 1480
    while (yyn___0 > 0) {
#line 1480
      zztmp___3 = (Int32 )*(ptr + yyp1___0);
#line 1480
      *(ptr + yyp1___0) = *(ptr + yyp2___0);
#line 1480
      *(ptr + yyp2___0) = (unsigned int )zztmp___3;
#line 1480
      yyp1___0 ++;
#line 1480
      yyp2___0 ++;
#line 1480
      yyn___0 --;
    }
#line 1482
    n = ((lo + unLo) - ltLo) - 1;
#line 1483
    m = (hi - (gtHi - unHi)) + 1;
#line 1485
    nextLo[0] = lo;
#line 1485
    nextHi[0] = n;
#line 1485
    nextD[0] = d;
#line 1486
    nextLo[1] = m;
#line 1486
    nextHi[1] = hi;
#line 1486
    nextD[1] = d;
#line 1487
    nextLo[2] = n + 1;
#line 1487
    nextHi[2] = m - 1;
#line 1487
    nextD[2] = d + 1;
#line 1489
    if (nextHi[0] - nextLo[0] < nextHi[1] - nextLo[1]) {
#line 1489
      tz = nextLo[0];
#line 1489
      nextLo[0] = nextLo[1];
#line 1489
      nextLo[1] = tz;
#line 1489
      tz = nextHi[0];
#line 1489
      nextHi[0] = nextHi[1];
#line 1489
      nextHi[1] = tz;
#line 1489
      tz = nextD[0];
#line 1489
      nextD[0] = nextD[1];
#line 1489
      nextD[1] = tz;
    } else {

    }
#line 1490
    if (nextHi[1] - nextLo[1] < nextHi[2] - nextLo[2]) {
#line 1490
      tz___0 = nextLo[1];
#line 1490
      nextLo[1] = nextLo[2];
#line 1490
      nextLo[2] = tz___0;
#line 1490
      tz___0 = nextHi[1];
#line 1490
      nextHi[1] = nextHi[2];
#line 1490
      nextHi[2] = tz___0;
#line 1490
      tz___0 = nextD[1];
#line 1490
      nextD[1] = nextD[2];
#line 1490
      nextD[2] = tz___0;
    } else {

    }
#line 1491
    if (nextHi[0] - nextLo[0] < nextHi[1] - nextLo[1]) {
#line 1491
      tz___1 = nextLo[0];
#line 1491
      nextLo[0] = nextLo[1];
#line 1491
      nextLo[1] = tz___1;
#line 1491
      tz___1 = nextHi[0];
#line 1491
      nextHi[0] = nextHi[1];
#line 1491
      nextHi[1] = tz___1;
#line 1491
      tz___1 = nextD[0];
#line 1491
      nextD[0] = nextD[1];
#line 1491
      nextD[1] = tz___1;
    } else {

    }
#line 1496
    stackLo[sp] = nextLo[0];
#line 1496
    stackHi[sp] = nextHi[0];
#line 1496
    stackD[sp] = nextD[0];
#line 1496
    sp ++;
#line 1497
    stackLo[sp] = nextLo[1];
#line 1497
    stackHi[sp] = nextHi[1];
#line 1497
    stackD[sp] = nextD[1];
#line 1497
    sp ++;
#line 1498
    stackLo[sp] = nextLo[2];
#line 1498
    stackHi[sp] = nextHi[2];
#line 1498
    stackD[sp] = nextD[2];
#line 1498
    sp ++;
  }
#line 1500
  return;
}
}
#line 1533 "bzip2.c"
static void mainSort(UInt32 *ptr , UChar *block , UInt16 *quadrant ,
                     UInt32 *ftab , Int32 nblock , Int32 verb , Int32 *budget ) 
{ Int32 i ;
  Int32 j ;
  Int32 k ;
  Int32 ss ;
  Int32 sb ;
  Int32 runningOrder[256] ;
  Bool bigDone[256] ;
  Int32 copyStart[256] ;
  Int32 copyEnd[256] ;
  UChar c1 ;
  Int32 numQSorted ;
  UInt16 s ;
  Int32 vv ;
  Int32 h ;
  Int32 lo ;
  Int32 hi ;
  Int32 tmp ;
  Int32 tmp___0 ;
  Int32 bbStart ;
  Int32 bbSize ;
  Int32 shifts ;
  Int32 a2update ;
  UInt16 qVal ;

  {
#line 1550
  if (verb >= 4) {
#line 1550
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"        main sort initialise ...\n");
  } else {

  }
#line 1553
  i = 65536;
#line 1553
  while (i >= 0) {
#line 1553
    *(ftab + i) = 0U;
#line 1553
    i --;
  }
#line 1555
  j = (int )*(block + 0) << 8;
#line 1556
  i = nblock - 1;
#line 1557
  while (i >= 3) {
#line 1558
    *(quadrant + i) = (unsigned short)0;
#line 1559
    j = (j >> 8) | ((int )((unsigned short )*(block + i)) << 8);
#line 1560
    (*(ftab + j)) ++;
#line 1561
    *(quadrant + (i - 1)) = (unsigned short)0;
#line 1562
    j = (j >> 8) | ((int )((unsigned short )*(block + (i - 1))) << 8);
#line 1563
    (*(ftab + j)) ++;
#line 1564
    *(quadrant + (i - 2)) = (unsigned short)0;
#line 1565
    j = (j >> 8) | ((int )((unsigned short )*(block + (i - 2))) << 8);
#line 1566
    (*(ftab + j)) ++;
#line 1567
    *(quadrant + (i - 3)) = (unsigned short)0;
#line 1568
    j = (j >> 8) | ((int )((unsigned short )*(block + (i - 3))) << 8);
#line 1569
    (*(ftab + j)) ++;
#line 1557
    i -= 4;
  }
#line 1571
  while (i >= 0) {
#line 1572
    *(quadrant + i) = (unsigned short)0;
#line 1573
    j = (j >> 8) | ((int )((unsigned short )*(block + i)) << 8);
#line 1574
    (*(ftab + j)) ++;
#line 1571
    i --;
  }
#line 1578
  i = 0;
#line 1578
  while (i < 34) {
#line 1579
    *(block + (nblock + i)) = *(block + i);
#line 1580
    *(quadrant + (nblock + i)) = (unsigned short)0;
#line 1578
    i ++;
  }
#line 1583
  if (verb >= 4) {
#line 1583
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"        bucket sorting ...\n");
  } else {

  }
#line 1586
  i = 1;
#line 1586
  while (i <= 65536) {
#line 1586
    *(ftab + i) += *(ftab + (i - 1));
#line 1586
    i ++;
  }
#line 1588
  s = (unsigned short )((int )*(block + 0) << 8);
#line 1589
  i = nblock - 1;
#line 1590
  while (i >= 3) {
#line 1591
    s = (unsigned short )(((int )s >> 8) | ((int )*(block + i) << 8));
#line 1592
    j = (int )(*(ftab + s) - 1U);
#line 1593
    *(ftab + s) = (unsigned int )j;
#line 1594
    *(ptr + j) = (unsigned int )i;
#line 1595
    s = (unsigned short )(((int )s >> 8) | ((int )*(block + (i - 1)) << 8));
#line 1596
    j = (int )(*(ftab + s) - 1U);
#line 1597
    *(ftab + s) = (unsigned int )j;
#line 1598
    *(ptr + j) = (unsigned int )(i - 1);
#line 1599
    s = (unsigned short )(((int )s >> 8) | ((int )*(block + (i - 2)) << 8));
#line 1600
    j = (int )(*(ftab + s) - 1U);
#line 1601
    *(ftab + s) = (unsigned int )j;
#line 1602
    *(ptr + j) = (unsigned int )(i - 2);
#line 1603
    s = (unsigned short )(((int )s >> 8) | ((int )*(block + (i - 3)) << 8));
#line 1604
    j = (int )(*(ftab + s) - 1U);
#line 1605
    *(ftab + s) = (unsigned int )j;
#line 1606
    *(ptr + j) = (unsigned int )(i - 3);
#line 1590
    i -= 4;
  }
#line 1608
  while (i >= 0) {
#line 1609
    s = (unsigned short )(((int )s >> 8) | ((int )*(block + i) << 8));
#line 1610
    j = (int )(*(ftab + s) - 1U);
#line 1611
    *(ftab + s) = (unsigned int )j;
#line 1612
    *(ptr + j) = (unsigned int )i;
#line 1608
    i --;
  }
#line 1620
  i = 0;
#line 1620
  while (i <= 255) {
#line 1621
    bigDone[i] = (unsigned char)0;
#line 1622
    runningOrder[i] = i;
#line 1620
    i ++;
  }
#line 1627
  h = 1;
#line 1628
  while (1) {
#line 1628
    h = 3 * h + 1;
#line 1628
    if (h <= 256) {

    } else {
#line 1628
      break;
    }
  }
#line 1629
  while (1) {
#line 1630
    h /= 3;
#line 1631
    i = h;
#line 1631
    while (i <= 255) {
#line 1632
      vv = runningOrder[i];
#line 1633
      j = i;
#line 1634
      while (*(ftab + ((runningOrder[j - h] + 1) << 8)) - *(ftab + (runningOrder[j - h] << 8)) > *(ftab + ((vv + 1) << 8)) - *(ftab + (vv << 8))) {
#line 1635
        runningOrder[j] = runningOrder[j - h];
#line 1636
        j -= h;
#line 1637
        if (j <= h - 1) {
          goto zero;
        } else {

        }
      }
      zero: 
#line 1640
      runningOrder[j] = vv;
#line 1631
      i ++;
    }
#line 1629
    if (h != 1) {

    } else {
#line 1629
      break;
    }
  }
#line 1649
  numQSorted = 0;
#line 1651
  i = 0;
#line 1651
  while (i <= 255) {
#line 1659
    ss = runningOrder[i];
#line 1669
    j = 0;
#line 1669
    while (j <= 255) {
#line 1670
      if (j != ss) {
#line 1671
        sb = (ss << 8) + j;
#line 1672
        if (! (*(ftab + sb) & (unsigned int )(1 << 21))) {
#line 1673
          lo = (Int32 )(*(ftab + sb) & (unsigned int )(~ (1 << 21)));
#line 1674
          hi = (Int32 )((*(ftab + (sb + 1)) & (unsigned int )(~ (1 << 21))) - 1U);
#line 1675
          if (hi > lo) {
#line 1676
            if (verb >= 4) {
#line 1677
              fprintf((FILE */* __restrict  */)stderr,
                      (char const   */* __restrict  */)"        qsort [0x%x, 0x%x]   done %d   this %d\n",
                      ss, j, numQSorted, (hi - lo) + 1);
            } else {

            }
#line 1680
            mainQSort3(ptr, block, quadrant, nblock, lo, hi, 2, budget);
#line 1684
            numQSorted += (hi - lo) + 1;
#line 1685
            if (*budget < 0) {
#line 1685
              return;
            } else {

            }
          } else {

          }
        } else {

        }
#line 1688
        *(ftab + sb) |= (unsigned int )(1 << 21);
      } else {

      }
#line 1669
      j ++;
    }
#line 1692
    if (! (! bigDone[ss])) {
#line 1692
      BZ2_bz__AssertH__fail(1006);
    } else {

    }
#line 1702
    j = 0;
#line 1702
    while (j <= 255) {
#line 1703
      copyStart[j] = (int )(*(ftab + ((j << 8) + ss)) & (unsigned int )(~ (1 << 21)));
#line 1704
      copyEnd[j] = (int )((*(ftab + (((j << 8) + ss) + 1)) & (unsigned int )(~ (1 << 21))) - 1U);
#line 1702
      j ++;
    }
#line 1706
    j = (int )(*(ftab + (ss << 8)) & (unsigned int )(~ (1 << 21)));
#line 1706
    while (j < copyStart[ss]) {
#line 1707
      k = (int )(*(ptr + j) - 1U);
#line 1707
      if (k < 0) {
#line 1707
        k += nblock;
      } else {

      }
#line 1708
      c1 = *(block + k);
#line 1709
      if (! bigDone[c1]) {
#line 1710
        tmp = copyStart[c1];
#line 1710
        (copyStart[c1]) ++;
#line 1710
        *(ptr + tmp) = (unsigned int )k;
      } else {

      }
#line 1706
      j ++;
    }
#line 1712
    j = (int )((*(ftab + ((ss + 1) << 8)) & (unsigned int )(~ (1 << 21))) - 1U);
#line 1712
    while (j > copyEnd[ss]) {
#line 1713
      k = (int )(*(ptr + j) - 1U);
#line 1713
      if (k < 0) {
#line 1713
        k += nblock;
      } else {

      }
#line 1714
      c1 = *(block + k);
#line 1715
      if (! bigDone[c1]) {
#line 1716
        tmp___0 = copyEnd[c1];
#line 1716
        (copyEnd[c1]) --;
#line 1716
        *(ptr + tmp___0) = (unsigned int )k;
      } else {

      }
#line 1712
      j --;
    }
#line 1720
    if (copyStart[ss] - 1 == copyEnd[ss]) {

    } else {
#line 1720
      if (copyStart[ss] == 0) {
#line 1720
        if (copyEnd[ss] == nblock - 1) {

        } else {
#line 1720
          BZ2_bz__AssertH__fail(1007);
        }
      } else {
#line 1720
        BZ2_bz__AssertH__fail(1007);
      }
    }
#line 1729
    j = 0;
#line 1729
    while (j <= 255) {
#line 1729
      *(ftab + ((j << 8) + ss)) |= (unsigned int )(1 << 21);
#line 1729
      j ++;
    }
#line 1770
    bigDone[ss] = (unsigned char)1;
#line 1772
    if (i < 255) {
#line 1773
      bbStart = (Int32 )(*(ftab + (ss << 8)) & (unsigned int )(~ (1 << 21)));
#line 1774
      bbSize = (Int32 )((*(ftab + ((ss + 1) << 8)) & (unsigned int )(~ (1 << 21))) - (unsigned int )bbStart);
#line 1775
      shifts = 0;
#line 1777
      while (bbSize >> shifts > 65534) {
#line 1777
        shifts ++;
      }
#line 1779
      j = bbSize - 1;
#line 1779
      while (j >= 0) {
#line 1780
        a2update = (Int32 )*(ptr + (bbStart + j));
#line 1781
        qVal = (unsigned short )(j >> shifts);
#line 1782
        *(quadrant + a2update) = qVal;
#line 1783
        if (a2update < 34) {
#line 1784
          *(quadrant + (a2update + nblock)) = qVal;
        } else {

        }
#line 1779
        j --;
      }
#line 1786
      if (! ((bbSize - 1) >> shifts <= 65535)) {
#line 1786
        BZ2_bz__AssertH__fail(1002);
      } else {

      }
    } else {

    }
#line 1651
    i ++;
  }
#line 1791
  if (verb >= 4) {
#line 1792
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"        %d pointers, %d sorted, %d scanned\n",
            nblock, numQSorted, nblock - numQSorted);
  } else {

  }
#line 1794
  return;
}
}
#line 1814 "bzip2.c"
void BZ2_blockSort(EState *s ) 
{ UInt32 *ptr ;
  UChar *block ;
  UInt32 *ftab ;
  Int32 nblock ;
  Int32 verb ;
  Int32 wfact ;
  UInt16 *quadrant ;
  Int32 budget ;
  Int32 budgetInit ;
  Int32 i ;
  int tmp ;

  {
#line 1816
  ptr = s->ptr;
#line 1817
  block = s->block;
#line 1818
  ftab = s->ftab;
#line 1819
  nblock = s->nblock;
#line 1820
  verb = s->verbosity;
#line 1821
  wfact = s->workFactor;
#line 1827
  if (nblock < 10000) {
#line 1828
    fallbackSort(s->arr1, s->arr2, ftab, nblock, verb);
  } else {
#line 1835
    i = nblock + 34;
#line 1836
    if (i & 1) {
#line 1836
      i ++;
    } else {

    }
#line 1837
    quadrant = (UInt16 *)(block + i);
#line 1846
    if (wfact < 1) {
#line 1846
      wfact = 1;
    } else {

    }
#line 1847
    if (wfact > 100) {
#line 1847
      wfact = 100;
    } else {

    }
#line 1848
    budgetInit = nblock * ((wfact - 1) / 3);
#line 1849
    budget = budgetInit;
#line 1851
    mainSort(ptr, block, quadrant, ftab, nblock, verb, & budget);
#line 1852
    if (verb >= 3) {
#line 1853
      if (nblock == 0) {
#line 1853
        tmp = 1;
      } else {
#line 1853
        tmp = nblock;
      }
#line 1853
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"      %d work, %d block, ratio %5.2f\n",
              budgetInit - budget, nblock,
              (float )(budgetInit - budget) / (float )tmp);
    } else {

    }
#line 1858
    if (budget < 0) {
#line 1859
      if (verb >= 2) {
#line 1860
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"    too repetitive; using fallback sorting algorithm\n");
      } else {

      }
#line 1862
      fallbackSort(s->arr1, s->arr2, ftab, nblock, verb);
    } else {

    }
  }
#line 1866
  s->origPtr = -1;
#line 1867
  i = 0;
#line 1867
  while (i < s->nblock) {
#line 1868
    if (*(ptr + i) == 0U) {
#line 1869
      s->origPtr = i;
#line 1869
      break;
    } else {

    }
#line 1867
    i ++;
  }
#line 1871
  if (! (s->origPtr != -1)) {
#line 1871
    BZ2_bz__AssertH__fail(1003);
  } else {

  }
#line 1872
  return;
}
}
#line 1924 "bzip2.c"
void BZ2_hbMakeCodeLengths(UChar *len , Int32 *freq , Int32 alphaSize ,
                           Int32 maxLen ) 
{ Int32 nNodes ;
  Int32 nHeap ;
  Int32 n1 ;
  Int32 n2 ;
  Int32 i ;
  Int32 j ;
  Int32 k ;
  Bool tooLong ;
  Int32 heap[260] ;
  Int32 weight[516] ;
  Int32 parent[516] ;
  int tmp ;
  Int32 zz ;
  Int32 tmp___0 ;
  Int32 zz___0 ;
  Int32 yy ;
  Int32 tmp___1 ;
  Int32 zz___1 ;
  Int32 yy___0 ;
  Int32 tmp___2 ;
  Int32 tmp___3 ;
  int tmp___4 ;
  Int32 zz___2 ;
  Int32 tmp___5 ;

  {
#line 1940
  i = 0;
#line 1940
  while (i < alphaSize) {
#line 1941
    if (*(freq + i) == 0) {
#line 1941
      tmp = 1;
    } else {
#line 1941
      tmp = *(freq + i);
    }
#line 1941
    weight[i + 1] = tmp << 8;
#line 1940
    i ++;
  }
#line 1943
  while (1) {
#line 1945
    nNodes = alphaSize;
#line 1946
    nHeap = 0;
#line 1948
    heap[0] = 0;
#line 1949
    weight[0] = 0;
#line 1950
    parent[0] = -2;
#line 1952
    i = 1;
#line 1952
    while (i <= alphaSize) {
#line 1953
      parent[i] = -1;
#line 1954
      nHeap ++;
#line 1955
      heap[nHeap] = i;
#line 1956
      zz = nHeap;
#line 1956
      tmp___0 = heap[zz];
#line 1956
      while (weight[tmp___0] < weight[heap[zz >> 1]]) {
#line 1956
        heap[zz] = heap[zz >> 1];
#line 1956
        zz >>= 1;
      }
#line 1956
      heap[zz] = tmp___0;
#line 1952
      i ++;
    }
#line 1959
    if (! (nHeap < 260)) {
#line 1959
      BZ2_bz__AssertH__fail(2001);
    } else {

    }
#line 1961
    while (nHeap > 1) {
#line 1962
      n1 = heap[1];
#line 1962
      heap[1] = heap[nHeap];
#line 1962
      nHeap --;
#line 1962
      zz___0 = 1;
#line 1962
      tmp___1 = heap[zz___0];
#line 1962
      while (1) {
#line 1962
        yy = zz___0 << 1;
#line 1962
        if (yy > nHeap) {
#line 1962
          break;
        } else {

        }
#line 1962
        if (yy < nHeap) {
#line 1962
          if (weight[heap[yy + 1]] < weight[heap[yy]]) {
#line 1962
            yy ++;
          } else {

          }
        } else {

        }
#line 1962
        if (weight[tmp___1] < weight[heap[yy]]) {
#line 1962
          break;
        } else {

        }
#line 1962
        heap[zz___0] = heap[yy];
#line 1962
        zz___0 = yy;
      }
#line 1962
      heap[zz___0] = tmp___1;
#line 1963
      n2 = heap[1];
#line 1963
      heap[1] = heap[nHeap];
#line 1963
      nHeap --;
#line 1963
      zz___1 = 1;
#line 1963
      tmp___2 = heap[zz___1];
#line 1963
      while (1) {
#line 1963
        yy___0 = zz___1 << 1;
#line 1963
        if (yy___0 > nHeap) {
#line 1963
          break;
        } else {

        }
#line 1963
        if (yy___0 < nHeap) {
#line 1963
          if (weight[heap[yy___0 + 1]] < weight[heap[yy___0]]) {
#line 1963
            yy___0 ++;
          } else {

          }
        } else {

        }
#line 1963
        if (weight[tmp___2] < weight[heap[yy___0]]) {
#line 1963
          break;
        } else {

        }
#line 1963
        heap[zz___1] = heap[yy___0];
#line 1963
        zz___1 = yy___0;
      }
#line 1963
      heap[zz___1] = tmp___2;
#line 1964
      nNodes ++;
#line 1965
      tmp___3 = nNodes;
#line 1965
      parent[n2] = tmp___3;
#line 1965
      parent[n1] = tmp___3;
#line 1966
      if ((weight[n1] & 255) > (weight[n2] & 255)) {
#line 1966
        tmp___4 = weight[n1] & 255;
      } else {
#line 1966
        tmp___4 = weight[n2] & 255;
      }
#line 1966
      weight[nNodes] = (int )((((unsigned int )weight[n1] & 4294967040U) + ((unsigned int )weight[n2] & 4294967040U)) | (unsigned int )(1 + tmp___4));
#line 1967
      parent[nNodes] = -1;
#line 1968
      nHeap ++;
#line 1969
      heap[nHeap] = nNodes;
#line 1970
      zz___2 = nHeap;
#line 1970
      tmp___5 = heap[zz___2];
#line 1970
      while (weight[tmp___5] < weight[heap[zz___2 >> 1]]) {
#line 1970
        heap[zz___2] = heap[zz___2 >> 1];
#line 1970
        zz___2 >>= 1;
      }
#line 1970
      heap[zz___2] = tmp___5;
    }
#line 1973
    if (! (nNodes < 516)) {
#line 1973
      BZ2_bz__AssertH__fail(2002);
    } else {

    }
#line 1975
    tooLong = (unsigned char)0;
#line 1976
    i = 1;
#line 1976
    while (i <= alphaSize) {
#line 1977
      j = 0;
#line 1978
      k = i;
#line 1979
      while (parent[k] >= 0) {
#line 1979
        k = parent[k];
#line 1979
        j ++;
      }
#line 1980
      *(len + (i - 1)) = (unsigned char )j;
#line 1981
      if (j > maxLen) {
#line 1981
        tooLong = (unsigned char)1;
      } else {

      }
#line 1976
      i ++;
    }
#line 1984
    if (! tooLong) {
#line 1984
      break;
    } else {

    }
#line 1986
    i = 1;
#line 1986
    while (i < alphaSize) {
#line 1987
      j = weight[i] >> 8;
#line 1988
      j = 1 + j / 2;
#line 1989
      weight[i] = j << 8;
#line 1986
      i ++;
    }
  }
#line 1992
  return;
}
}
#line 1996 "bzip2.c"
void BZ2_hbAssignCodes(Int32 *code , UChar *length , Int32 minLen ,
                       Int32 maxLen , Int32 alphaSize ) 
{ Int32 n ;
  Int32 vec ;
  Int32 i ;

  {
#line 2004
  vec = 0;
#line 2005
  n = minLen;
#line 2005
  while (n <= maxLen) {
#line 2006
    i = 0;
#line 2006
    while (i < alphaSize) {
#line 2007
      if ((int )*(length + i) == n) {
#line 2007
        *(code + i) = vec;
#line 2007
        vec ++;
      } else {

      }
#line 2006
      i ++;
    }
#line 2008
    vec <<= 1;
#line 2005
    n ++;
  }
#line 2010
  return;
}
}
#line 2014 "bzip2.c"
void BZ2_hbCreateDecodeTables(Int32 *limit , Int32 *base , Int32 *perm ,
                              UChar *length , Int32 minLen , Int32 maxLen ,
                              Int32 alphaSize ) 
{ Int32 pp ;
  Int32 i ;
  Int32 j ;
  Int32 vec ;

  {
#line 2024
  pp = 0;
#line 2025
  i = minLen;
#line 2025
  while (i <= maxLen) {
#line 2026
    j = 0;
#line 2026
    while (j < alphaSize) {
#line 2027
      if ((int )*(length + j) == i) {
#line 2027
        *(perm + pp) = j;
#line 2027
        pp ++;
      } else {

      }
#line 2026
      j ++;
    }
#line 2025
    i ++;
  }
#line 2029
  i = 0;
#line 2029
  while (i < 23) {
#line 2029
    *(base + i) = 0;
#line 2029
    i ++;
  }
#line 2030
  i = 0;
#line 2030
  while (i < alphaSize) {
#line 2030
    (*(base + ((int )*(length + i) + 1))) ++;
#line 2030
    i ++;
  }
#line 2032
  i = 1;
#line 2032
  while (i < 23) {
#line 2032
    *(base + i) += *(base + (i - 1));
#line 2032
    i ++;
  }
#line 2034
  i = 0;
#line 2034
  while (i < 23) {
#line 2034
    *(limit + i) = 0;
#line 2034
    i ++;
  }
#line 2035
  vec = 0;
#line 2037
  i = minLen;
#line 2037
  while (i <= maxLen) {
#line 2038
    vec += *(base + (i + 1)) - *(base + i);
#line 2039
    *(limit + i) = vec - 1;
#line 2040
    vec <<= 1;
#line 2037
    i ++;
  }
#line 2042
  i = minLen + 1;
#line 2042
  while (i <= maxLen) {
#line 2043
    *(base + i) = ((*(limit + (i - 1)) + 1) << 1) - *(base + i);
#line 2042
    i ++;
  }
#line 2044
  return;
}
}
#line 2064 "bzip2.c"
UInt32 BZ2_crc32Table[256]  = 
#line 2064
  {      (UInt32 )0L,      (UInt32 )79764919L,      (UInt32 )159529838L,      (UInt32 )222504665L, 
        (UInt32 )319059676L,      (UInt32 )398814059L,      (UInt32 )445009330L,      (UInt32 )507990021L, 
        (UInt32 )638119352L,      (UInt32 )583659535L,      (UInt32 )797628118L,      (UInt32 )726387553L, 
        (UInt32 )890018660L,      (UInt32 )835552979L,      (UInt32 )1015980042L,      (UInt32 )944750013L, 
        (UInt32 )1276238704L,      (UInt32 )1221641927L,      (UInt32 )1167319070L,      (UInt32 )1095957929L, 
        (UInt32 )1595256236L,      (UInt32 )1540665371L,      (UInt32 )1452775106L,      (UInt32 )1381403509L, 
        (UInt32 )1780037320L,      (UInt32 )1859660671L,      (UInt32 )1671105958L,      (UInt32 )1733955601L, 
        (UInt32 )2031960084L,      (UInt32 )2111593891L,      (UInt32 )1889500026L,      (UInt32 )1952343757L, 
        (UInt32 )2552477408L,      (UInt32 )2632100695L,      (UInt32 )2443283854L,      (UInt32 )2506133561L, 
        (UInt32 )2334638140L,      (UInt32 )2414271883L,      (UInt32 )2191915858L,      (UInt32 )2254759653L, 
        (UInt32 )3190512472L,      (UInt32 )3135915759L,      (UInt32 )3081330742L,      (UInt32 )3009969537L, 
        (UInt32 )2905550212L,      (UInt32 )2850959411L,      (UInt32 )2762807018L,      (UInt32 )2691435357L, 
        (UInt32 )3560074640L,      (UInt32 )3505614887L,      (UInt32 )3719321342L,      (UInt32 )3648080713L, 
        (UInt32 )3342211916L,      (UInt32 )3287746299L,      (UInt32 )3467911202L,      (UInt32 )3396681109L, 
        (UInt32 )4063920168L,      (UInt32 )4143685023L,      (UInt32 )4223187782L,      (UInt32 )4286162673L, 
        (UInt32 )3779000052L,      (UInt32 )3858754371L,      (UInt32 )3904687514L,      (UInt32 )3967668269L, 
        (UInt32 )881225847L,      (UInt32 )809987520L,      (UInt32 )1023691545L,      (UInt32 )969234094L, 
        (UInt32 )662832811L,      (UInt32 )591600412L,      (UInt32 )771767749L,      (UInt32 )717299826L, 
        (UInt32 )311336399L,      (UInt32 )374308984L,      (UInt32 )453813921L,      (UInt32 )533576470L, 
        (UInt32 )25881363L,      (UInt32 )88864420L,      (UInt32 )134795389L,      (UInt32 )214552010L, 
        (UInt32 )2023205639L,      (UInt32 )2086057648L,      (UInt32 )1897238633L,      (UInt32 )1976864222L, 
        (UInt32 )1804852699L,      (UInt32 )1867694188L,      (UInt32 )1645340341L,      (UInt32 )1724971778L, 
        (UInt32 )1587496639L,      (UInt32 )1516133128L,      (UInt32 )1461550545L,      (UInt32 )1406951526L, 
        (UInt32 )1302016099L,      (UInt32 )1230646740L,      (UInt32 )1142491917L,      (UInt32 )1087903418L, 
        (UInt32 )2896545431L,      (UInt32 )2825181984L,      (UInt32 )2770861561L,      (UInt32 )2716262478L, 
        (UInt32 )3215044683L,      (UInt32 )3143675388L,      (UInt32 )3055782693L,      (UInt32 )3001194130L, 
        (UInt32 )2326604591L,      (UInt32 )2389456536L,      (UInt32 )2200899649L,      (UInt32 )2280525302L, 
        (UInt32 )2578013683L,      (UInt32 )2640855108L,      (UInt32 )2418763421L,      (UInt32 )2498394922L, 
        (UInt32 )3769900519L,      (UInt32 )3832873040L,      (UInt32 )3912640137L,      (UInt32 )3992402750L, 
        (UInt32 )4088425275L,      (UInt32 )4151408268L,      (UInt32 )4197601365L,      (UInt32 )4277358050L, 
        (UInt32 )3334271071L,      (UInt32 )3263032808L,      (UInt32 )3476998961L,      (UInt32 )3422541446L, 
        (UInt32 )3585640067L,      (UInt32 )3514407732L,      (UInt32 )3694837229L,      (UInt32 )3640369242L, 
        (UInt32 )1762451694L,      (UInt32 )1842216281L,      (UInt32 )1619975040L,      (UInt32 )1682949687L, 
        (UInt32 )2047383090L,      (UInt32 )2127137669L,      (UInt32 )1938468188L,      (UInt32 )2001449195L, 
        (UInt32 )1325665622L,      (UInt32 )1271206113L,      (UInt32 )1183200824L,      (UInt32 )1111960463L, 
        (UInt32 )1543535498L,      (UInt32 )1489069629L,      (UInt32 )1434599652L,      (UInt32 )1363369299L, 
        (UInt32 )622672798L,      (UInt32 )568075817L,      (UInt32 )748617968L,      (UInt32 )677256519L, 
        (UInt32 )907627842L,      (UInt32 )853037301L,      (UInt32 )1067152940L,      (UInt32 )995781531L, 
        (UInt32 )51762726L,      (UInt32 )131386257L,      (UInt32 )177728840L,      (UInt32 )240578815L, 
        (UInt32 )269590778L,      (UInt32 )349224269L,      (UInt32 )429104020L,      (UInt32 )491947555L, 
        (UInt32 )4046411278L,      (UInt32 )4126034873L,      (UInt32 )4172115296L,      (UInt32 )4234965207L, 
        (UInt32 )3794477266L,      (UInt32 )3874110821L,      (UInt32 )3953728444L,      (UInt32 )4016571915L, 
        (UInt32 )3609705398L,      (UInt32 )3555108353L,      (UInt32 )3735388376L,      (UInt32 )3664026991L, 
        (UInt32 )3290680682L,      (UInt32 )3236090077L,      (UInt32 )3449943556L,      (UInt32 )3378572211L, 
        (UInt32 )3174993278L,      (UInt32 )3120533705L,      (UInt32 )3032266256L,      (UInt32 )2961025959L, 
        (UInt32 )2923101090L,      (UInt32 )2868635157L,      (UInt32 )2813903052L,      (UInt32 )2742672763L, 
        (UInt32 )2604032198L,      (UInt32 )2683796849L,      (UInt32 )2461293480L,      (UInt32 )2524268063L, 
        (UInt32 )2284983834L,      (UInt32 )2364738477L,      (UInt32 )2175806836L,      (UInt32 )2238787779L, 
        (UInt32 )1569362073L,      (UInt32 )1498123566L,      (UInt32 )1409854455L,      (UInt32 )1355396672L, 
        (UInt32 )1317987909L,      (UInt32 )1246755826L,      (UInt32 )1192025387L,      (UInt32 )1137557660L, 
        (UInt32 )2072149281L,      (UInt32 )2135122070L,      (UInt32 )1912620623L,      (UInt32 )1992383480L, 
        (UInt32 )1753615357L,      (UInt32 )1816598090L,      (UInt32 )1627664531L,      (UInt32 )1707420964L, 
        (UInt32 )295390185L,      (UInt32 )358241886L,      (UInt32 )404320391L,      (UInt32 )483945776L, 
        (UInt32 )43990325L,      (UInt32 )106832002L,      (UInt32 )186451547L,      (UInt32 )266083308L, 
        (UInt32 )932423249L,      (UInt32 )861060070L,      (UInt32 )1041341759L,      (UInt32 )986742920L, 
        (UInt32 )613929101L,      (UInt32 )542559546L,      (UInt32 )756411363L,      (UInt32 )701822548L, 
        (UInt32 )3316196985L,      (UInt32 )3244833742L,      (UInt32 )3425377559L,      (UInt32 )3370778784L, 
        (UInt32 )3601682597L,      (UInt32 )3530312978L,      (UInt32 )3744426955L,      (UInt32 )3689838204L, 
        (UInt32 )3819031489L,      (UInt32 )3881883254L,      (UInt32 )3928223919L,      (UInt32 )4007849240L, 
        (UInt32 )4037393693L,      (UInt32 )4100235434L,      (UInt32 )4180117107L,      (UInt32 )4259748804L, 
        (UInt32 )2310601993L,      (UInt32 )2373574846L,      (UInt32 )2151335527L,      (UInt32 )2231098320L, 
        (UInt32 )2596047829L,      (UInt32 )2659030626L,      (UInt32 )2470359227L,      (UInt32 )2550115596L, 
        (UInt32 )2947551409L,      (UInt32 )2876312838L,      (UInt32 )2788305887L,      (UInt32 )2733848168L, 
        (UInt32 )3165939309L,      (UInt32 )3094707162L,      (UInt32 )3040238851L,      (UInt32 )2985771188L};
#line 2146 "bzip2.c"
Int32 BZ2_rNums[512]  = 
#line 2146
  {      619,      720,      127,      481, 
        931,      816,      813,      233, 
        566,      247,      985,      724, 
        205,      454,      863,      491, 
        741,      242,      949,      214, 
        733,      859,      335,      708, 
        621,      574,      73,      654, 
        730,      472,      419,      436, 
        278,      496,      867,      210, 
        399,      680,      480,      51, 
        878,      465,      811,      169, 
        869,      675,      611,      697, 
        867,      561,      862,      687, 
        507,      283,      482,      129, 
        807,      591,      733,      623, 
        150,      238,      59,      379, 
        684,      877,      625,      169, 
        643,      105,      170,      607, 
        520,      932,      727,      476, 
        693,      425,      174,      647, 
        73,      122,      335,      530, 
        442,      853,      695,      249, 
        445,      515,      909,      545, 
        703,      919,      874,      474, 
        882,      500,      594,      612, 
        641,      801,      220,      162, 
        819,      984,      589,      513, 
        495,      799,      161,      604, 
        958,      533,      221,      400, 
        386,      867,      600,      782, 
        382,      596,      414,      171, 
        516,      375,      682,      485, 
        911,      276,      98,      553, 
        163,      354,      666,      933, 
        424,      341,      533,      870, 
        227,      730,      475,      186, 
        263,      647,      537,      686, 
        600,      224,      469,      68, 
        770,      919,      190,      373, 
        294,      822,      808,      206, 
        184,      943,      795,      384, 
        383,      461,      404,      758, 
        839,      887,      715,      67, 
        618,      276,      204,      918, 
        873,      777,      604,      560, 
        951,      160,      578,      722, 
        79,      804,      96,      409, 
        713,      940,      652,      934, 
        970,      447,      318,      353, 
        859,      672,      112,      785, 
        645,      863,      803,      350, 
        139,      93,      354,      99, 
        820,      908,      609,      772, 
        154,      274,      580,      184, 
        79,      626,      630,      742, 
        653,      282,      762,      623, 
        680,      81,      927,      626, 
        789,      125,      411,      521, 
        938,      300,      821,      78, 
        343,      175,      128,      250, 
        170,      774,      972,      275, 
        999,      639,      495,      78, 
        352,      126,      857,      956, 
        358,      619,      580,      124, 
        737,      594,      701,      612, 
        669,      112,      134,      694, 
        363,      992,      809,      743, 
        168,      974,      944,      375, 
        748,      52,      600,      747, 
        642,      182,      862,      81, 
        344,      805,      988,      739, 
        511,      655,      814,      334, 
        249,      515,      897,      955, 
        664,      981,      649,      113, 
        974,      459,      893,      228, 
        433,      837,      553,      268, 
        926,      240,      102,      654, 
        459,      51,      686,      754, 
        806,      760,      493,      403, 
        415,      394,      687,      700, 
        946,      670,      656,      610, 
        738,      392,      760,      799, 
        887,      653,      978,      321, 
        576,      617,      626,      502, 
        894,      679,      243,      440, 
        680,      879,      194,      572, 
        640,      724,      926,      56, 
        204,      700,      707,      151, 
        457,      449,      797,      195, 
        791,      558,      945,      679, 
        297,      59,      87,      824, 
        713,      663,      412,      693, 
        342,      606,      134,      108, 
        571,      364,      631,      212, 
        174,      643,      304,      329, 
        343,      97,      430,      751, 
        497,      314,      983,      374, 
        822,      928,      140,      206, 
        73,      263,      980,      736, 
        876,      478,      430,      305, 
        170,      514,      364,      692, 
        829,      82,      855,      953, 
        676,      246,      369,      970, 
        294,      750,      807,      827, 
        150,      790,      288,      923, 
        804,      378,      215,      828, 
        592,      281,      565,      555, 
        710,      82,      896,      831, 
        547,      261,      524,      462, 
        293,      465,      502,      56, 
        661,      821,      976,      991, 
        658,      869,      905,      758, 
        745,      193,      768,      550, 
        608,      933,      378,      286, 
        215,      979,      792,      961, 
        61,      688,      793,      644, 
        986,      403,      106,      366, 
        905,      644,      372,      567, 
        466,      434,      645,      210, 
        389,      550,      919,      135, 
        780,      773,      635,      389, 
        707,      100,      626,      958, 
        165,      504,      920,      176, 
        193,      713,      857,      265, 
        203,      50,      668,      108, 
        645,      990,      626,      197, 
        510,      357,      358,      850, 
        858,      364,      936,      638};
#line 2217 "bzip2.c"
void BZ2_bsInitWrite(EState *s ) 
{ 

  {
#line 2219
  s->bsLive = 0;
#line 2220
  s->bsBuff = 0U;
#line 2221
  return;
}
}
#line 2225 "bzip2.c"
static void bsFinishWrite(EState *s ) 
{ 

  {
#line 2228
  while (s->bsLive > 0) {
#line 2229
    *(s->zbits + s->numZ) = (unsigned char )(s->bsBuff >> 24);
#line 2230
    (s->numZ) ++;
#line 2231
    s->bsBuff <<= 8;
#line 2232
    s->bsLive -= 8;
  }
#line 2234
  return;
}
}
#line 2251 "bzip2.c"
__inline static void bsW(EState *s , Int32 n , UInt32 v ) 
{ 

  {
#line 2255
  while (s->bsLive >= 8) {
#line 2255
    *(s->zbits + s->numZ) = (unsigned char )(s->bsBuff >> 24);
#line 2255
    (s->numZ) ++;
#line 2255
    s->bsBuff <<= 8;
#line 2255
    s->bsLive -= 8;
  }
#line 2256
  s->bsBuff |= v << ((32 - s->bsLive) - n);
#line 2257
  s->bsLive += n;
#line 2258
  return;
}
}
#line 2262 "bzip2.c"
static void bsPutUInt32(EState *s , UInt32 u ) 
{ 

  {
#line 2265
  bsW(s, 8, (unsigned int )((long )(u >> 24) & 255L));
#line 2266
  bsW(s, 8, (unsigned int )((long )(u >> 16) & 255L));
#line 2267
  bsW(s, 8, (unsigned int )((long )(u >> 8) & 255L));
#line 2268
  bsW(s, 8, (unsigned int )((long )u & 255L));
#line 2269
  return;
}
}
#line 2273 "bzip2.c"
static void bsPutUChar(EState *s , UChar c ) 
{ 

  {
#line 2276
  bsW(s, 8, (unsigned int )c);
#line 2277
  return;
}
}
#line 2285 "bzip2.c"
static void makeMaps_e(EState *s ) 
{ Int32 i ;

  {
#line 2289
  s->nInUse = 0;
#line 2290
  i = 0;
#line 2290
  while (i < 256) {
#line 2291
    if (s->inUse[i]) {
#line 2292
      s->unseqToSeq[i] = (unsigned char )s->nInUse;
#line 2293
      (s->nInUse) ++;
    } else {

    }
#line 2290
    i ++;
  }
#line 2295
  return;
}
}
#line 2299 "bzip2.c"
static void generateMTFValues(EState *s ) 
{ UChar yy[256] ;
  Int32 i ;
  Int32 j ;
  Int32 zPend ;
  Int32 wr ;
  Int32 EOB ;
  UInt32 *ptr ;
  UChar *block ;
  UInt16 *mtfv ;
  UChar ll_i ;
  register UChar rtmp ;
  register UChar *ryy_j ;
  register UChar rll_i ;
  register UChar rtmp2 ;

  {
#line 2330
  ptr = s->ptr;
#line 2331
  block = s->block;
#line 2332
  mtfv = s->mtfv;
#line 2334
  makeMaps_e(s);
#line 2335
  EOB = s->nInUse + 1;
#line 2337
  i = 0;
#line 2337
  while (i <= EOB) {
#line 2337
    s->mtfFreq[i] = 0;
#line 2337
    i ++;
  }
#line 2339
  wr = 0;
#line 2340
  zPend = 0;
#line 2341
  i = 0;
#line 2341
  while (i < s->nInUse) {
#line 2341
    yy[i] = (unsigned char )i;
#line 2341
    i ++;
  }
#line 2343
  i = 0;
#line 2343
  while (i < s->nblock) {
#line 2346
    j = (int )(*(ptr + i) - 1U);
#line 2346
    if (j < 0) {
#line 2346
      j += s->nblock;
    } else {

    }
#line 2347
    ll_i = s->unseqToSeq[*(block + j)];
#line 2350
    if ((int )yy[0] == (int )ll_i) {
#line 2351
      zPend ++;
    } else {
#line 2354
      if (zPend > 0) {
#line 2355
        zPend --;
#line 2356
        while (1) {
#line 2357
          if (zPend & 1) {
#line 2358
            *(mtfv + wr) = (unsigned short)1;
#line 2358
            wr ++;
#line 2359
            (s->mtfFreq[1]) ++;
          } else {
#line 2361
            *(mtfv + wr) = (unsigned short)0;
#line 2361
            wr ++;
#line 2362
            (s->mtfFreq[0]) ++;
          }
#line 2364
          if (zPend < 2) {
#line 2364
            break;
          } else {

          }
#line 2365
          zPend = (zPend - 2) / 2;
        }
#line 2367
        zPend = 0;
      } else {

      }
#line 2373
      rtmp = yy[1];
#line 2374
      yy[1] = yy[0];
#line 2375
      ryy_j = & yy[1];
#line 2376
      rll_i = ll_i;
#line 2377
      while ((int )rll_i != (int )rtmp) {
#line 2379
        ryy_j ++;
#line 2380
        rtmp2 = rtmp;
#line 2381
        rtmp = *ryy_j;
#line 2382
        *ryy_j = rtmp2;
      }
#line 2384
      yy[0] = rtmp;
#line 2385
      j = ryy_j - & yy[0];
#line 2386
      *(mtfv + wr) = (unsigned short )(j + 1);
#line 2386
      wr ++;
#line 2386
      (s->mtfFreq[j + 1]) ++;
    }
#line 2343
    i ++;
  }
#line 2392
  if (zPend > 0) {
#line 2393
    zPend --;
#line 2394
    while (1) {
#line 2395
      if (zPend & 1) {
#line 2396
        *(mtfv + wr) = (unsigned short)1;
#line 2396
        wr ++;
#line 2397
        (s->mtfFreq[1]) ++;
      } else {
#line 2399
        *(mtfv + wr) = (unsigned short)0;
#line 2399
        wr ++;
#line 2400
        (s->mtfFreq[0]) ++;
      }
#line 2402
      if (zPend < 2) {
#line 2402
        break;
      } else {

      }
#line 2403
      zPend = (zPend - 2) / 2;
    }
#line 2405
    zPend = 0;
  } else {

  }
#line 2408
  *(mtfv + wr) = (unsigned short )EOB;
#line 2408
  wr ++;
#line 2408
  (s->mtfFreq[EOB]) ++;
#line 2410
  s->nMTF = wr;
#line 2411
  return;
}
}
#line 2418 "bzip2.c"
static void sendMTFValues(EState *s ) 
{ Int32 v ;
  Int32 t ;
  Int32 i ;
  Int32 j ;
  Int32 gs ;
  Int32 ge ;
  Int32 totc ;
  Int32 bt ;
  Int32 bc ;
  Int32 iter ;
  Int32 nSelectors ;
  Int32 alphaSize ;
  Int32 minLen ;
  Int32 maxLen ;
  Int32 selCtr ;
  Int32 nGroups ;
  Int32 nBytes ;
  UInt16 cost[6] ;
  Int32 fave[6] ;
  UInt16 *mtfv ;
  Int32 nPart ;
  Int32 remF ;
  Int32 tFreq ;
  Int32 aFreq ;
  register UInt32 cost01 ;
  register UInt32 cost23 ;
  register UInt32 cost45 ;
  register UInt16 icv ;
  UInt16 icv___0 ;
  UChar pos[6] ;
  UChar ll_i ;
  UChar tmp2 ;
  UChar tmp ;
  Bool inUse16[16] ;
  Int32 curr ;
  UInt16 mtfv_i ;
  UChar *s_len_sel_selCtr ;
  Int32 *s_code_sel_selCtr ;

  {
#line 2439
  mtfv = s->mtfv;
#line 2441
  if (s->verbosity >= 3) {
#line 2442
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"      %d in block, %d after MTF & 1-2 coding, %d+2 syms in use\n",
            s->nblock, s->nMTF, s->nInUse);
  } else {

  }
#line 2446
  alphaSize = s->nInUse + 2;
#line 2447
  t = 0;
#line 2447
  while (t < 6) {
#line 2448
    v = 0;
#line 2448
    while (v < alphaSize) {
#line 2449
      s->len[t][v] = (unsigned char)15;
#line 2448
      v ++;
    }
#line 2447
    t ++;
  }
#line 2452
  if (! (s->nMTF > 0)) {
#line 2452
    BZ2_bz__AssertH__fail(3001);
  } else {

  }
#line 2453
  if (s->nMTF < 200) {
#line 2453
    nGroups = 2;
  } else {
#line 2454
    if (s->nMTF < 600) {
#line 2454
      nGroups = 3;
    } else {
#line 2455
      if (s->nMTF < 1200) {
#line 2455
        nGroups = 4;
      } else {
#line 2456
        if (s->nMTF < 2400) {
#line 2456
          nGroups = 5;
        } else {
#line 2457
          nGroups = 6;
        }
      }
    }
  }
#line 2463
  nPart = nGroups;
#line 2464
  remF = s->nMTF;
#line 2465
  gs = 0;
#line 2466
  while (nPart > 0) {
#line 2467
    tFreq = remF / nPart;
#line 2468
    ge = gs - 1;
#line 2469
    aFreq = 0;
#line 2470
    while (1) {
#line 2470
      if (aFreq < tFreq) {
#line 2470
        if (ge < alphaSize - 1) {

        } else {
#line 2470
          break;
        }
      } else {
#line 2470
        break;
      }
#line 2471
      ge ++;
#line 2472
      aFreq += s->mtfFreq[ge];
    }
#line 2475
    if (ge > gs) {
#line 2475
      if (nPart != nGroups) {
#line 2475
        if (nPart != 1) {
#line 2475
          if ((nGroups - nPart) % 2 == 1) {
#line 2478
            aFreq -= s->mtfFreq[ge];
#line 2479
            ge --;
          } else {

          }
        } else {

        }
      } else {

      }
    } else {

    }
#line 2482
    if (s->verbosity >= 3) {
#line 2483
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"      initial group %d, [%d .. %d], has %d syms (%4.1f%%)\n",
              nPart, gs, ge, aFreq,
              (100.0 * (double )((float )aFreq)) / (double )((float )s->nMTF));
    } else {

    }
#line 2488
    v = 0;
#line 2488
    while (v < alphaSize) {
#line 2489
      if (v >= gs) {
#line 2489
        if (v <= ge) {
#line 2490
          s->len[nPart - 1][v] = (unsigned char)0;
        } else {
#line 2491
          s->len[nPart - 1][v] = (unsigned char)15;
        }
      } else {
#line 2491
        s->len[nPart - 1][v] = (unsigned char)15;
      }
#line 2488
      v ++;
    }
#line 2493
    nPart --;
#line 2494
    gs = ge + 1;
#line 2495
    remF -= aFreq;
  }
#line 2502
  iter = 0;
#line 2502
  while (iter < 4) {
#line 2504
    t = 0;
#line 2504
    while (t < nGroups) {
#line 2504
      fave[t] = 0;
#line 2504
      t ++;
    }
#line 2506
    t = 0;
#line 2506
    while (t < nGroups) {
#line 2507
      v = 0;
#line 2507
      while (v < alphaSize) {
#line 2508
        s->rfreq[t][v] = 0;
#line 2507
        v ++;
      }
#line 2506
      t ++;
    }
#line 2514
    if (nGroups == 6) {
#line 2515
      v = 0;
#line 2515
      while (v < alphaSize) {
#line 2516
        s->len_pack[v][0] = (unsigned int )(((int )s->len[1][v] << 16) | (int )s->len[0][v]);
#line 2517
        s->len_pack[v][1] = (unsigned int )(((int )s->len[3][v] << 16) | (int )s->len[2][v]);
#line 2518
        s->len_pack[v][2] = (unsigned int )(((int )s->len[5][v] << 16) | (int )s->len[4][v]);
#line 2515
        v ++;
      }
    } else {

    }
#line 2522
    nSelectors = 0;
#line 2523
    totc = 0;
#line 2524
    gs = 0;
#line 2525
    while (! (gs >= s->nMTF)) {
#line 2529
      ge = (gs + 50) - 1;
#line 2530
      if (ge >= s->nMTF) {
#line 2530
        ge = s->nMTF - 1;
      } else {

      }
#line 2536
      t = 0;
#line 2536
      while (t < nGroups) {
#line 2536
        cost[t] = (unsigned short)0;
#line 2536
        t ++;
      }
#line 2538
      if (nGroups == 6) {
#line 2538
        if (50 == (ge - gs) + 1) {
#line 2542
          cost45 = 0U;
#line 2542
          cost23 = cost45;
#line 2542
          cost01 = cost23;
#line 2550
          icv = *(mtfv + gs);
#line 2550
          cost01 += s->len_pack[icv][0];
#line 2550
          cost23 += s->len_pack[icv][1];
#line 2550
          cost45 += s->len_pack[icv][2];
#line 2550
          icv = *(mtfv + (gs + 1));
#line 2550
          cost01 += s->len_pack[icv][0];
#line 2550
          cost23 += s->len_pack[icv][1];
#line 2550
          cost45 += s->len_pack[icv][2];
#line 2550
          icv = *(mtfv + (gs + 2));
#line 2550
          cost01 += s->len_pack[icv][0];
#line 2550
          cost23 += s->len_pack[icv][1];
#line 2550
          cost45 += s->len_pack[icv][2];
#line 2550
          icv = *(mtfv + (gs + 3));
#line 2550
          cost01 += s->len_pack[icv][0];
#line 2550
          cost23 += s->len_pack[icv][1];
#line 2550
          cost45 += s->len_pack[icv][2];
#line 2550
          icv = *(mtfv + (gs + 4));
#line 2550
          cost01 += s->len_pack[icv][0];
#line 2550
          cost23 += s->len_pack[icv][1];
#line 2550
          cost45 += s->len_pack[icv][2];
#line 2551
          icv = *(mtfv + (gs + 5));
#line 2551
          cost01 += s->len_pack[icv][0];
#line 2551
          cost23 += s->len_pack[icv][1];
#line 2551
          cost45 += s->len_pack[icv][2];
#line 2551
          icv = *(mtfv + (gs + 6));
#line 2551
          cost01 += s->len_pack[icv][0];
#line 2551
          cost23 += s->len_pack[icv][1];
#line 2551
          cost45 += s->len_pack[icv][2];
#line 2551
          icv = *(mtfv + (gs + 7));
#line 2551
          cost01 += s->len_pack[icv][0];
#line 2551
          cost23 += s->len_pack[icv][1];
#line 2551
          cost45 += s->len_pack[icv][2];
#line 2551
          icv = *(mtfv + (gs + 8));
#line 2551
          cost01 += s->len_pack[icv][0];
#line 2551
          cost23 += s->len_pack[icv][1];
#line 2551
          cost45 += s->len_pack[icv][2];
#line 2551
          icv = *(mtfv + (gs + 9));
#line 2551
          cost01 += s->len_pack[icv][0];
#line 2551
          cost23 += s->len_pack[icv][1];
#line 2551
          cost45 += s->len_pack[icv][2];
#line 2552
          icv = *(mtfv + (gs + 10));
#line 2552
          cost01 += s->len_pack[icv][0];
#line 2552
          cost23 += s->len_pack[icv][1];
#line 2552
          cost45 += s->len_pack[icv][2];
#line 2552
          icv = *(mtfv + (gs + 11));
#line 2552
          cost01 += s->len_pack[icv][0];
#line 2552
          cost23 += s->len_pack[icv][1];
#line 2552
          cost45 += s->len_pack[icv][2];
#line 2552
          icv = *(mtfv + (gs + 12));
#line 2552
          cost01 += s->len_pack[icv][0];
#line 2552
          cost23 += s->len_pack[icv][1];
#line 2552
          cost45 += s->len_pack[icv][2];
#line 2552
          icv = *(mtfv + (gs + 13));
#line 2552
          cost01 += s->len_pack[icv][0];
#line 2552
          cost23 += s->len_pack[icv][1];
#line 2552
          cost45 += s->len_pack[icv][2];
#line 2552
          icv = *(mtfv + (gs + 14));
#line 2552
          cost01 += s->len_pack[icv][0];
#line 2552
          cost23 += s->len_pack[icv][1];
#line 2552
          cost45 += s->len_pack[icv][2];
#line 2553
          icv = *(mtfv + (gs + 15));
#line 2553
          cost01 += s->len_pack[icv][0];
#line 2553
          cost23 += s->len_pack[icv][1];
#line 2553
          cost45 += s->len_pack[icv][2];
#line 2553
          icv = *(mtfv + (gs + 16));
#line 2553
          cost01 += s->len_pack[icv][0];
#line 2553
          cost23 += s->len_pack[icv][1];
#line 2553
          cost45 += s->len_pack[icv][2];
#line 2553
          icv = *(mtfv + (gs + 17));
#line 2553
          cost01 += s->len_pack[icv][0];
#line 2553
          cost23 += s->len_pack[icv][1];
#line 2553
          cost45 += s->len_pack[icv][2];
#line 2553
          icv = *(mtfv + (gs + 18));
#line 2553
          cost01 += s->len_pack[icv][0];
#line 2553
          cost23 += s->len_pack[icv][1];
#line 2553
          cost45 += s->len_pack[icv][2];
#line 2553
          icv = *(mtfv + (gs + 19));
#line 2553
          cost01 += s->len_pack[icv][0];
#line 2553
          cost23 += s->len_pack[icv][1];
#line 2553
          cost45 += s->len_pack[icv][2];
#line 2554
          icv = *(mtfv + (gs + 20));
#line 2554
          cost01 += s->len_pack[icv][0];
#line 2554
          cost23 += s->len_pack[icv][1];
#line 2554
          cost45 += s->len_pack[icv][2];
#line 2554
          icv = *(mtfv + (gs + 21));
#line 2554
          cost01 += s->len_pack[icv][0];
#line 2554
          cost23 += s->len_pack[icv][1];
#line 2554
          cost45 += s->len_pack[icv][2];
#line 2554
          icv = *(mtfv + (gs + 22));
#line 2554
          cost01 += s->len_pack[icv][0];
#line 2554
          cost23 += s->len_pack[icv][1];
#line 2554
          cost45 += s->len_pack[icv][2];
#line 2554
          icv = *(mtfv + (gs + 23));
#line 2554
          cost01 += s->len_pack[icv][0];
#line 2554
          cost23 += s->len_pack[icv][1];
#line 2554
          cost45 += s->len_pack[icv][2];
#line 2554
          icv = *(mtfv + (gs + 24));
#line 2554
          cost01 += s->len_pack[icv][0];
#line 2554
          cost23 += s->len_pack[icv][1];
#line 2554
          cost45 += s->len_pack[icv][2];
#line 2555
          icv = *(mtfv + (gs + 25));
#line 2555
          cost01 += s->len_pack[icv][0];
#line 2555
          cost23 += s->len_pack[icv][1];
#line 2555
          cost45 += s->len_pack[icv][2];
#line 2555
          icv = *(mtfv + (gs + 26));
#line 2555
          cost01 += s->len_pack[icv][0];
#line 2555
          cost23 += s->len_pack[icv][1];
#line 2555
          cost45 += s->len_pack[icv][2];
#line 2555
          icv = *(mtfv + (gs + 27));
#line 2555
          cost01 += s->len_pack[icv][0];
#line 2555
          cost23 += s->len_pack[icv][1];
#line 2555
          cost45 += s->len_pack[icv][2];
#line 2555
          icv = *(mtfv + (gs + 28));
#line 2555
          cost01 += s->len_pack[icv][0];
#line 2555
          cost23 += s->len_pack[icv][1];
#line 2555
          cost45 += s->len_pack[icv][2];
#line 2555
          icv = *(mtfv + (gs + 29));
#line 2555
          cost01 += s->len_pack[icv][0];
#line 2555
          cost23 += s->len_pack[icv][1];
#line 2555
          cost45 += s->len_pack[icv][2];
#line 2556
          icv = *(mtfv + (gs + 30));
#line 2556
          cost01 += s->len_pack[icv][0];
#line 2556
          cost23 += s->len_pack[icv][1];
#line 2556
          cost45 += s->len_pack[icv][2];
#line 2556
          icv = *(mtfv + (gs + 31));
#line 2556
          cost01 += s->len_pack[icv][0];
#line 2556
          cost23 += s->len_pack[icv][1];
#line 2556
          cost45 += s->len_pack[icv][2];
#line 2556
          icv = *(mtfv + (gs + 32));
#line 2556
          cost01 += s->len_pack[icv][0];
#line 2556
          cost23 += s->len_pack[icv][1];
#line 2556
          cost45 += s->len_pack[icv][2];
#line 2556
          icv = *(mtfv + (gs + 33));
#line 2556
          cost01 += s->len_pack[icv][0];
#line 2556
          cost23 += s->len_pack[icv][1];
#line 2556
          cost45 += s->len_pack[icv][2];
#line 2556
          icv = *(mtfv + (gs + 34));
#line 2556
          cost01 += s->len_pack[icv][0];
#line 2556
          cost23 += s->len_pack[icv][1];
#line 2556
          cost45 += s->len_pack[icv][2];
#line 2557
          icv = *(mtfv + (gs + 35));
#line 2557
          cost01 += s->len_pack[icv][0];
#line 2557
          cost23 += s->len_pack[icv][1];
#line 2557
          cost45 += s->len_pack[icv][2];
#line 2557
          icv = *(mtfv + (gs + 36));
#line 2557
          cost01 += s->len_pack[icv][0];
#line 2557
          cost23 += s->len_pack[icv][1];
#line 2557
          cost45 += s->len_pack[icv][2];
#line 2557
          icv = *(mtfv + (gs + 37));
#line 2557
          cost01 += s->len_pack[icv][0];
#line 2557
          cost23 += s->len_pack[icv][1];
#line 2557
          cost45 += s->len_pack[icv][2];
#line 2557
          icv = *(mtfv + (gs + 38));
#line 2557
          cost01 += s->len_pack[icv][0];
#line 2557
          cost23 += s->len_pack[icv][1];
#line 2557
          cost45 += s->len_pack[icv][2];
#line 2557
          icv = *(mtfv + (gs + 39));
#line 2557
          cost01 += s->len_pack[icv][0];
#line 2557
          cost23 += s->len_pack[icv][1];
#line 2557
          cost45 += s->len_pack[icv][2];
#line 2558
          icv = *(mtfv + (gs + 40));
#line 2558
          cost01 += s->len_pack[icv][0];
#line 2558
          cost23 += s->len_pack[icv][1];
#line 2558
          cost45 += s->len_pack[icv][2];
#line 2558
          icv = *(mtfv + (gs + 41));
#line 2558
          cost01 += s->len_pack[icv][0];
#line 2558
          cost23 += s->len_pack[icv][1];
#line 2558
          cost45 += s->len_pack[icv][2];
#line 2558
          icv = *(mtfv + (gs + 42));
#line 2558
          cost01 += s->len_pack[icv][0];
#line 2558
          cost23 += s->len_pack[icv][1];
#line 2558
          cost45 += s->len_pack[icv][2];
#line 2558
          icv = *(mtfv + (gs + 43));
#line 2558
          cost01 += s->len_pack[icv][0];
#line 2558
          cost23 += s->len_pack[icv][1];
#line 2558
          cost45 += s->len_pack[icv][2];
#line 2558
          icv = *(mtfv + (gs + 44));
#line 2558
          cost01 += s->len_pack[icv][0];
#line 2558
          cost23 += s->len_pack[icv][1];
#line 2558
          cost45 += s->len_pack[icv][2];
#line 2559
          icv = *(mtfv + (gs + 45));
#line 2559
          cost01 += s->len_pack[icv][0];
#line 2559
          cost23 += s->len_pack[icv][1];
#line 2559
          cost45 += s->len_pack[icv][2];
#line 2559
          icv = *(mtfv + (gs + 46));
#line 2559
          cost01 += s->len_pack[icv][0];
#line 2559
          cost23 += s->len_pack[icv][1];
#line 2559
          cost45 += s->len_pack[icv][2];
#line 2559
          icv = *(mtfv + (gs + 47));
#line 2559
          cost01 += s->len_pack[icv][0];
#line 2559
          cost23 += s->len_pack[icv][1];
#line 2559
          cost45 += s->len_pack[icv][2];
#line 2559
          icv = *(mtfv + (gs + 48));
#line 2559
          cost01 += s->len_pack[icv][0];
#line 2559
          cost23 += s->len_pack[icv][1];
#line 2559
          cost45 += s->len_pack[icv][2];
#line 2559
          icv = *(mtfv + (gs + 49));
#line 2559
          cost01 += s->len_pack[icv][0];
#line 2559
          cost23 += s->len_pack[icv][1];
#line 2559
          cost45 += s->len_pack[icv][2];
#line 2563
          cost[0] = (unsigned short )(cost01 & 65535U);
#line 2563
          cost[1] = (unsigned short )(cost01 >> 16);
#line 2564
          cost[2] = (unsigned short )(cost23 & 65535U);
#line 2564
          cost[3] = (unsigned short )(cost23 >> 16);
#line 2565
          cost[4] = (unsigned short )(cost45 & 65535U);
#line 2565
          cost[5] = (unsigned short )(cost45 >> 16);
        } else {
          goto _L;
        }
      } else {
        _L: /* CIL Label */ 
#line 2569
        i = gs;
#line 2569
        while (i <= ge) {
#line 2570
          icv___0 = *(mtfv + i);
#line 2571
          t = 0;
#line 2571
          while (t < nGroups) {
#line 2571
            cost[t] = (unsigned short )((int )cost[t] + (int )s->len[t][icv___0]);
#line 2571
            t ++;
          }
#line 2569
          i ++;
        }
      }
#line 2579
      bc = 999999999;
#line 2579
      bt = -1;
#line 2580
      t = 0;
#line 2580
      while (t < nGroups) {
#line 2581
        if ((int )cost[t] < bc) {
#line 2581
          bc = (int )cost[t];
#line 2581
          bt = t;
        } else {

        }
#line 2580
        t ++;
      }
#line 2582
      totc += bc;
#line 2583
      (fave[bt]) ++;
#line 2584
      s->selector[nSelectors] = (unsigned char )bt;
#line 2585
      nSelectors ++;
#line 2590
      if (nGroups == 6) {
#line 2590
        if (50 == (ge - gs) + 1) {
#line 2595
          (s->rfreq[bt][*(mtfv + gs)]) ++;
#line 2595
          (s->rfreq[bt][*(mtfv + (gs + 1))]) ++;
#line 2595
          (s->rfreq[bt][*(mtfv + (gs + 2))]) ++;
#line 2595
          (s->rfreq[bt][*(mtfv + (gs + 3))]) ++;
#line 2595
          (s->rfreq[bt][*(mtfv + (gs + 4))]) ++;
#line 2596
          (s->rfreq[bt][*(mtfv + (gs + 5))]) ++;
#line 2596
          (s->rfreq[bt][*(mtfv + (gs + 6))]) ++;
#line 2596
          (s->rfreq[bt][*(mtfv + (gs + 7))]) ++;
#line 2596
          (s->rfreq[bt][*(mtfv + (gs + 8))]) ++;
#line 2596
          (s->rfreq[bt][*(mtfv + (gs + 9))]) ++;
#line 2597
          (s->rfreq[bt][*(mtfv + (gs + 10))]) ++;
#line 2597
          (s->rfreq[bt][*(mtfv + (gs + 11))]) ++;
#line 2597
          (s->rfreq[bt][*(mtfv + (gs + 12))]) ++;
#line 2597
          (s->rfreq[bt][*(mtfv + (gs + 13))]) ++;
#line 2597
          (s->rfreq[bt][*(mtfv + (gs + 14))]) ++;
#line 2598
          (s->rfreq[bt][*(mtfv + (gs + 15))]) ++;
#line 2598
          (s->rfreq[bt][*(mtfv + (gs + 16))]) ++;
#line 2598
          (s->rfreq[bt][*(mtfv + (gs + 17))]) ++;
#line 2598
          (s->rfreq[bt][*(mtfv + (gs + 18))]) ++;
#line 2598
          (s->rfreq[bt][*(mtfv + (gs + 19))]) ++;
#line 2599
          (s->rfreq[bt][*(mtfv + (gs + 20))]) ++;
#line 2599
          (s->rfreq[bt][*(mtfv + (gs + 21))]) ++;
#line 2599
          (s->rfreq[bt][*(mtfv + (gs + 22))]) ++;
#line 2599
          (s->rfreq[bt][*(mtfv + (gs + 23))]) ++;
#line 2599
          (s->rfreq[bt][*(mtfv + (gs + 24))]) ++;
#line 2600
          (s->rfreq[bt][*(mtfv + (gs + 25))]) ++;
#line 2600
          (s->rfreq[bt][*(mtfv + (gs + 26))]) ++;
#line 2600
          (s->rfreq[bt][*(mtfv + (gs + 27))]) ++;
#line 2600
          (s->rfreq[bt][*(mtfv + (gs + 28))]) ++;
#line 2600
          (s->rfreq[bt][*(mtfv + (gs + 29))]) ++;
#line 2601
          (s->rfreq[bt][*(mtfv + (gs + 30))]) ++;
#line 2601
          (s->rfreq[bt][*(mtfv + (gs + 31))]) ++;
#line 2601
          (s->rfreq[bt][*(mtfv + (gs + 32))]) ++;
#line 2601
          (s->rfreq[bt][*(mtfv + (gs + 33))]) ++;
#line 2601
          (s->rfreq[bt][*(mtfv + (gs + 34))]) ++;
#line 2602
          (s->rfreq[bt][*(mtfv + (gs + 35))]) ++;
#line 2602
          (s->rfreq[bt][*(mtfv + (gs + 36))]) ++;
#line 2602
          (s->rfreq[bt][*(mtfv + (gs + 37))]) ++;
#line 2602
          (s->rfreq[bt][*(mtfv + (gs + 38))]) ++;
#line 2602
          (s->rfreq[bt][*(mtfv + (gs + 39))]) ++;
#line 2603
          (s->rfreq[bt][*(mtfv + (gs + 40))]) ++;
#line 2603
          (s->rfreq[bt][*(mtfv + (gs + 41))]) ++;
#line 2603
          (s->rfreq[bt][*(mtfv + (gs + 42))]) ++;
#line 2603
          (s->rfreq[bt][*(mtfv + (gs + 43))]) ++;
#line 2603
          (s->rfreq[bt][*(mtfv + (gs + 44))]) ++;
#line 2604
          (s->rfreq[bt][*(mtfv + (gs + 45))]) ++;
#line 2604
          (s->rfreq[bt][*(mtfv + (gs + 46))]) ++;
#line 2604
          (s->rfreq[bt][*(mtfv + (gs + 47))]) ++;
#line 2604
          (s->rfreq[bt][*(mtfv + (gs + 48))]) ++;
#line 2604
          (s->rfreq[bt][*(mtfv + (gs + 49))]) ++;
        } else {
          goto _L___0;
        }
      } else {
        _L___0: /* CIL Label */ 
#line 2610
        i = gs;
#line 2610
        while (i <= ge) {
#line 2611
          (s->rfreq[bt][*(mtfv + i)]) ++;
#line 2610
          i ++;
        }
      }
#line 2614
      gs = ge + 1;
    }
#line 2616
    if (s->verbosity >= 3) {
#line 2617
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"      pass %d: size is %d, grp uses are ",
              iter + 1, totc / 8);
#line 2619
      t = 0;
#line 2619
      while (t < nGroups) {
#line 2620
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%d ", fave[t]);
#line 2619
        t ++;
      }
#line 2621
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"\n");
    } else {

    }
#line 2627
    t = 0;
#line 2627
    while (t < nGroups) {
#line 2628
      BZ2_hbMakeCodeLengths(& s->len[t][0], & s->rfreq[t][0], alphaSize, 20);
#line 2627
      t ++;
    }
#line 2502
    iter ++;
  }
#line 2633
  if (! (nGroups < 8)) {
#line 2633
    BZ2_bz__AssertH__fail(3002);
  } else {

  }
#line 2634
  if (nSelectors < 32768) {
#line 2634
    if (nSelectors <= 18002) {

    } else {
#line 2634
      BZ2_bz__AssertH__fail(3003);
    }
  } else {
#line 2634
    BZ2_bz__AssertH__fail(3003);
  }
#line 2642
  i = 0;
#line 2642
  while (i < nGroups) {
#line 2642
    pos[i] = (unsigned char )i;
#line 2642
    i ++;
  }
#line 2643
  i = 0;
#line 2643
  while (i < nSelectors) {
#line 2644
    ll_i = s->selector[i];
#line 2645
    j = 0;
#line 2646
    tmp = pos[j];
#line 2647
    while ((int )ll_i != (int )tmp) {
#line 2648
      j ++;
#line 2649
      tmp2 = tmp;
#line 2650
      tmp = pos[j];
#line 2651
      pos[j] = tmp2;
    }
#line 2653
    pos[0] = tmp;
#line 2654
    s->selectorMtf[i] = (unsigned char )j;
#line 2643
    i ++;
  }
#line 2659
  t = 0;
#line 2659
  while (t < nGroups) {
#line 2660
    minLen = 32;
#line 2661
    maxLen = 0;
#line 2662
    i = 0;
#line 2662
    while (i < alphaSize) {
#line 2663
      if ((int )s->len[t][i] > maxLen) {
#line 2663
        maxLen = (int )s->len[t][i];
      } else {

      }
#line 2664
      if ((int )s->len[t][i] < minLen) {
#line 2664
        minLen = (int )s->len[t][i];
      } else {

      }
#line 2662
      i ++;
    }
#line 2666
    if (! (! (maxLen > 20))) {
#line 2666
      BZ2_bz__AssertH__fail(3004);
    } else {

    }
#line 2667
    if (! (! (minLen < 1))) {
#line 2667
      BZ2_bz__AssertH__fail(3005);
    } else {

    }
#line 2668
    BZ2_hbAssignCodes(& s->code[t][0], & s->len[t][0], minLen, maxLen, alphaSize);
#line 2659
    t ++;
  }
#line 2675
  i = 0;
#line 2675
  while (i < 16) {
#line 2676
    inUse16[i] = (unsigned char)0;
#line 2677
    j = 0;
#line 2677
    while (j < 16) {
#line 2678
      if (s->inUse[i * 16 + j]) {
#line 2678
        inUse16[i] = (unsigned char)1;
      } else {

      }
#line 2677
      j ++;
    }
#line 2675
    i ++;
  }
#line 2681
  nBytes = s->numZ;
#line 2682
  i = 0;
#line 2682
  while (i < 16) {
#line 2683
    if (inUse16[i]) {
#line 2683
      bsW(s, 1, 1U);
    } else {
#line 2683
      bsW(s, 1, 0U);
    }
#line 2682
    i ++;
  }
#line 2685
  i = 0;
#line 2685
  while (i < 16) {
#line 2686
    if (inUse16[i]) {
#line 2687
      j = 0;
#line 2687
      while (j < 16) {
#line 2688
        if (s->inUse[i * 16 + j]) {
#line 2688
          bsW(s, 1, 1U);
        } else {
#line 2688
          bsW(s, 1, 0U);
        }
#line 2687
        j ++;
      }
    } else {

    }
#line 2685
    i ++;
  }
#line 2691
  if (s->verbosity >= 3) {
#line 2692
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"      bytes: mapping %d, ",
            s->numZ - nBytes);
  } else {

  }
#line 2696
  nBytes = s->numZ;
#line 2697
  bsW(s, 3, (unsigned int )nGroups);
#line 2698
  bsW(s, 15, (unsigned int )nSelectors);
#line 2699
  i = 0;
#line 2699
  while (i < nSelectors) {
#line 2700
    j = 0;
#line 2700
    while (j < (Int32 )s->selectorMtf[i]) {
#line 2700
      bsW(s, 1, 1U);
#line 2700
      j ++;
    }
#line 2701
    bsW(s, 1, 0U);
#line 2699
    i ++;
  }
#line 2703
  if (s->verbosity >= 3) {
#line 2704
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"selectors %d, ", s->numZ - nBytes);
  } else {

  }
#line 2707
  nBytes = s->numZ;
#line 2709
  t = 0;
#line 2709
  while (t < nGroups) {
#line 2710
    curr = (Int32 )s->len[t][0];
#line 2711
    bsW(s, 5, (unsigned int )curr);
#line 2712
    i = 0;
#line 2712
    while (i < alphaSize) {
#line 2713
      while (curr < (Int32 )s->len[t][i]) {
#line 2713
        bsW(s, 2, 2U);
#line 2713
        curr ++;
      }
#line 2714
      while (curr > (Int32 )s->len[t][i]) {
#line 2714
        bsW(s, 2, 3U);
#line 2714
        curr --;
      }
#line 2715
      bsW(s, 1, 0U);
#line 2712
      i ++;
    }
#line 2709
    t ++;
  }
#line 2719
  if (s->verbosity >= 3) {
#line 2720
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"code lengths %d, ",
            s->numZ - nBytes);
  } else {

  }
#line 2723
  nBytes = s->numZ;
#line 2724
  selCtr = 0;
#line 2725
  gs = 0;
#line 2726
  while (! (gs >= s->nMTF)) {
#line 2728
    ge = (gs + 50) - 1;
#line 2729
    if (ge >= s->nMTF) {
#line 2729
      ge = s->nMTF - 1;
    } else {

    }
#line 2730
    if (! ((int )s->selector[selCtr] < nGroups)) {
#line 2730
      BZ2_bz__AssertH__fail(3006);
    } else {

    }
#line 2732
    if (nGroups == 6) {
#line 2732
      if (50 == (ge - gs) + 1) {
#line 2735
        s_len_sel_selCtr = & s->len[s->selector[selCtr]][0];
#line 2737
        s_code_sel_selCtr = & s->code[s->selector[selCtr]][0];
#line 2746
        mtfv_i = *(mtfv + gs);
#line 2746
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2746
        mtfv_i = *(mtfv + (gs + 1));
#line 2746
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2746
        mtfv_i = *(mtfv + (gs + 2));
#line 2746
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2746
        mtfv_i = *(mtfv + (gs + 3));
#line 2746
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2746
        mtfv_i = *(mtfv + (gs + 4));
#line 2746
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2747
        mtfv_i = *(mtfv + (gs + 5));
#line 2747
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2747
        mtfv_i = *(mtfv + (gs + 6));
#line 2747
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2747
        mtfv_i = *(mtfv + (gs + 7));
#line 2747
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2747
        mtfv_i = *(mtfv + (gs + 8));
#line 2747
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2747
        mtfv_i = *(mtfv + (gs + 9));
#line 2747
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2748
        mtfv_i = *(mtfv + (gs + 10));
#line 2748
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2748
        mtfv_i = *(mtfv + (gs + 11));
#line 2748
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2748
        mtfv_i = *(mtfv + (gs + 12));
#line 2748
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2748
        mtfv_i = *(mtfv + (gs + 13));
#line 2748
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2748
        mtfv_i = *(mtfv + (gs + 14));
#line 2748
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2749
        mtfv_i = *(mtfv + (gs + 15));
#line 2749
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2749
        mtfv_i = *(mtfv + (gs + 16));
#line 2749
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2749
        mtfv_i = *(mtfv + (gs + 17));
#line 2749
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2749
        mtfv_i = *(mtfv + (gs + 18));
#line 2749
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2749
        mtfv_i = *(mtfv + (gs + 19));
#line 2749
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2750
        mtfv_i = *(mtfv + (gs + 20));
#line 2750
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2750
        mtfv_i = *(mtfv + (gs + 21));
#line 2750
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2750
        mtfv_i = *(mtfv + (gs + 22));
#line 2750
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2750
        mtfv_i = *(mtfv + (gs + 23));
#line 2750
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2750
        mtfv_i = *(mtfv + (gs + 24));
#line 2750
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2751
        mtfv_i = *(mtfv + (gs + 25));
#line 2751
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2751
        mtfv_i = *(mtfv + (gs + 26));
#line 2751
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2751
        mtfv_i = *(mtfv + (gs + 27));
#line 2751
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2751
        mtfv_i = *(mtfv + (gs + 28));
#line 2751
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2751
        mtfv_i = *(mtfv + (gs + 29));
#line 2751
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2752
        mtfv_i = *(mtfv + (gs + 30));
#line 2752
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2752
        mtfv_i = *(mtfv + (gs + 31));
#line 2752
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2752
        mtfv_i = *(mtfv + (gs + 32));
#line 2752
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2752
        mtfv_i = *(mtfv + (gs + 33));
#line 2752
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2752
        mtfv_i = *(mtfv + (gs + 34));
#line 2752
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2753
        mtfv_i = *(mtfv + (gs + 35));
#line 2753
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2753
        mtfv_i = *(mtfv + (gs + 36));
#line 2753
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2753
        mtfv_i = *(mtfv + (gs + 37));
#line 2753
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2753
        mtfv_i = *(mtfv + (gs + 38));
#line 2753
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2753
        mtfv_i = *(mtfv + (gs + 39));
#line 2753
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2754
        mtfv_i = *(mtfv + (gs + 40));
#line 2754
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2754
        mtfv_i = *(mtfv + (gs + 41));
#line 2754
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2754
        mtfv_i = *(mtfv + (gs + 42));
#line 2754
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2754
        mtfv_i = *(mtfv + (gs + 43));
#line 2754
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2754
        mtfv_i = *(mtfv + (gs + 44));
#line 2754
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2755
        mtfv_i = *(mtfv + (gs + 45));
#line 2755
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2755
        mtfv_i = *(mtfv + (gs + 46));
#line 2755
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2755
        mtfv_i = *(mtfv + (gs + 47));
#line 2755
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2755
        mtfv_i = *(mtfv + (gs + 48));
#line 2755
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
#line 2755
        mtfv_i = *(mtfv + (gs + 49));
#line 2755
        bsW(s, (int )*(s_len_sel_selCtr + mtfv_i),
            (unsigned int )*(s_code_sel_selCtr + mtfv_i));
      } else {
        goto _L___1;
      }
    } else {
      _L___1: /* CIL Label */ 
#line 2761
      i = gs;
#line 2761
      while (i <= ge) {
#line 2762
        bsW(s, (int )s->len[s->selector[selCtr]][*(mtfv + i)],
            (unsigned int )s->code[s->selector[selCtr]][*(mtfv + i)]);
#line 2761
        i ++;
      }
    }
#line 2769
    gs = ge + 1;
#line 2770
    selCtr ++;
  }
#line 2772
  if (! (selCtr == nSelectors)) {
#line 2772
    BZ2_bz__AssertH__fail(3007);
  } else {

  }
#line 2774
  if (s->verbosity >= 3) {
#line 2775
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"codes %d\n", s->numZ - nBytes);
  } else {

  }
#line 2776
  return;
}
}
#line 2780 "bzip2.c"
void BZ2_compressBlock(EState *s , Bool is_last_block ) 
{ 

  {
#line 2782
  if (s->nblock > 0) {
#line 2784
    s->blockCRC = ~ s->blockCRC;
#line 2785
    s->combinedCRC = (s->combinedCRC << 1) | (s->combinedCRC >> 31);
#line 2786
    s->combinedCRC ^= s->blockCRC;
#line 2787
    if (s->blockNo > 1) {
#line 2787
      s->numZ = 0;
    } else {

    }
#line 2789
    if (s->verbosity >= 2) {
#line 2790
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"    block %d: crc = 0x%8x, combined CRC = 0x%8x, size = %d\n",
              s->blockNo, s->blockCRC, s->combinedCRC, s->nblock);
    } else {

    }
#line 2794
    BZ2_blockSort(s);
  } else {

  }
#line 2797
  s->zbits = (UChar *)s->arr2 + s->nblock;
#line 2800
  if (s->blockNo == 1) {
#line 2801
    BZ2_bsInitWrite(s);
#line 2802
    bsPutUChar(s, (unsigned char)66);
#line 2803
    bsPutUChar(s, (unsigned char)90);
#line 2804
    bsPutUChar(s, (unsigned char)104);
#line 2805
    bsPutUChar(s, (unsigned char )(48 + s->blockSize100k));
  } else {

  }
#line 2808
  if (s->nblock > 0) {
#line 2810
    bsPutUChar(s, (unsigned char)49);
#line 2810
    bsPutUChar(s, (unsigned char)65);
#line 2811
    bsPutUChar(s, (unsigned char)89);
#line 2811
    bsPutUChar(s, (unsigned char)38);
#line 2812
    bsPutUChar(s, (unsigned char)83);
#line 2812
    bsPutUChar(s, (unsigned char)89);
#line 2815
    bsPutUInt32(s, s->blockCRC);
#line 2826
    bsW(s, 1, 0U);
#line 2828
    bsW(s, 24, (unsigned int )s->origPtr);
#line 2829
    generateMTFValues(s);
#line 2830
    sendMTFValues(s);
  } else {

  }
#line 2835
  if (is_last_block) {
#line 2837
    bsPutUChar(s, (unsigned char)23);
#line 2837
    bsPutUChar(s, (unsigned char)114);
#line 2838
    bsPutUChar(s, (unsigned char)69);
#line 2838
    bsPutUChar(s, (unsigned char)56);
#line 2839
    bsPutUChar(s, (unsigned char)80);
#line 2839
    bsPutUChar(s, (unsigned char)144);
#line 2840
    bsPutUInt32(s, s->combinedCRC);
#line 2841
    if (s->verbosity >= 2) {
#line 2842
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"    final combined CRC = 0x%x\n   ",
              s->combinedCRC);
    } else {

    }
#line 2843
    bsFinishWrite(s);
  } else {

  }
#line 2845
  return;
}
}
#line 2859 "bzip2.c"
static void makeMaps_d(DState *s ) 
{ Int32 i ;

  {
#line 2863
  s->nInUse = 0;
#line 2864
  i = 0;
#line 2864
  while (i < 256) {
#line 2865
    if (s->inUse[i]) {
#line 2866
      s->seqToUnseq[s->nInUse] = (unsigned char )i;
#line 2867
      (s->nInUse) ++;
    } else {

    }
#line 2864
    i ++;
  }
#line 2869
  return;
}
}
#line 2939 "bzip2.c"
Int32 BZ2_decompress(DState *s ) 
{ UChar uc ;
  Int32 retVal ;
  Int32 minLen ;
  Int32 maxLen ;
  bz_stream *strm ;
  Int32 i ;
  Int32 j ;
  Int32 t ;
  Int32 alphaSize ;
  Int32 nGroups ;
  Int32 nSelectors ;
  Int32 EOB ;
  Int32 groupNo ;
  Int32 groupPos ;
  Int32 nextSym ;
  Int32 nblockMAX ;
  Int32 nblock ;
  Int32 es ;
  Int32 N ;
  Int32 curr ;
  Int32 zt ;
  Int32 zn ;
  Int32 zvec ;
  Int32 zj ;
  Int32 gSel ;
  Int32 gMinlen ;
  Int32 *gLimit ;
  Int32 *gBase ;
  Int32 *gPerm ;
  UInt32 v ;
  UInt32 v___0 ;
  UInt32 v___1 ;
  UInt32 v___2 ;
  void *tmp ;
  void *tmp___0 ;
  void *tmp___1 ;
  UInt32 v___3 ;
  UInt32 v___4 ;
  UInt32 v___5 ;
  UInt32 v___6 ;
  UInt32 v___7 ;
  UInt32 v___8 ;
  UInt32 v___9 ;
  UInt32 v___10 ;
  UInt32 v___11 ;
  UInt32 v___12 ;
  UInt32 v___13 ;
  UInt32 v___14 ;
  UInt32 v___15 ;
  UInt32 v___16 ;
  UInt32 v___17 ;
  UInt32 v___18 ;
  UInt32 v___19 ;
  UInt32 v___20 ;
  UInt32 v___21 ;
  UChar pos[6] ;
  UChar tmp___2 ;
  UChar v___22 ;
  UInt32 v___23 ;
  UInt32 v___24 ;
  UInt32 v___25 ;
  Int32 ii ;
  Int32 jj ;
  Int32 kk ;
  UInt32 v___26 ;
  UInt32 v___27 ;
  UInt32 v___28 ;
  UInt32 v___29 ;
  Int32 ii___0 ;
  Int32 jj___0 ;
  Int32 kk___0 ;
  Int32 pp ;
  Int32 lno ;
  Int32 off ;
  UInt32 nn ;
  Int32 z ;
  UInt32 v___30 ;
  UInt32 v___31 ;
  Int32 tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  UInt32 v___32 ;
  UInt32 v___33 ;
  UInt32 v___34 ;
  UInt32 v___35 ;
  UInt32 v___36 ;
  UInt32 v___37 ;
  UInt32 v___38 ;
  UInt32 v___39 ;
  UInt32 v___40 ;

  {
#line 2944
  strm = s->strm;
#line 2972
  if (s->state == 10) {
#line 2974
    s->save_i = 0;
#line 2975
    s->save_j = 0;
#line 2976
    s->save_t = 0;
#line 2977
    s->save_alphaSize = 0;
#line 2978
    s->save_nGroups = 0;
#line 2979
    s->save_nSelectors = 0;
#line 2980
    s->save_EOB = 0;
#line 2981
    s->save_groupNo = 0;
#line 2982
    s->save_groupPos = 0;
#line 2983
    s->save_nextSym = 0;
#line 2984
    s->save_nblockMAX = 0;
#line 2985
    s->save_nblock = 0;
#line 2986
    s->save_es = 0;
#line 2987
    s->save_N = 0;
#line 2988
    s->save_curr = 0;
#line 2989
    s->save_zt = 0;
#line 2990
    s->save_zn = 0;
#line 2991
    s->save_zvec = 0;
#line 2992
    s->save_zj = 0;
#line 2993
    s->save_gSel = 0;
#line 2994
    s->save_gMinlen = 0;
#line 2995
    s->save_gLimit = (Int32 *)((void *)0);
#line 2996
    s->save_gBase = (Int32 *)((void *)0);
#line 2997
    s->save_gPerm = (Int32 *)((void *)0);
  } else {

  }
#line 3001
  i = s->save_i;
#line 3002
  j = s->save_j;
#line 3003
  t = s->save_t;
#line 3004
  alphaSize = s->save_alphaSize;
#line 3005
  nGroups = s->save_nGroups;
#line 3006
  nSelectors = s->save_nSelectors;
#line 3007
  EOB = s->save_EOB;
#line 3008
  groupNo = s->save_groupNo;
#line 3009
  groupPos = s->save_groupPos;
#line 3010
  nextSym = s->save_nextSym;
#line 3011
  nblockMAX = s->save_nblockMAX;
#line 3012
  nblock = s->save_nblock;
#line 3013
  es = s->save_es;
#line 3014
  N = s->save_N;
#line 3015
  curr = s->save_curr;
#line 3016
  zt = s->save_zt;
#line 3017
  zn = s->save_zn;
#line 3018
  zvec = s->save_zvec;
#line 3019
  zj = s->save_zj;
#line 3020
  gSel = s->save_gSel;
#line 3021
  gMinlen = s->save_gMinlen;
#line 3022
  gLimit = s->save_gLimit;
#line 3023
  gBase = s->save_gBase;
#line 3024
  gPerm = s->save_gPerm;
#line 3026
  retVal = 0;
#line 3028
  switch (s->state) {
  case 10: 
#line 3030
  s->state = 10;
#line 3030
  while (1) {
#line 3030
    if (s->bsLive >= 8) {
#line 3030
      v = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3030
      s->bsLive -= 8;
#line 3030
      uc = (unsigned char )v;
#line 3030
      break;
    } else {

    }
#line 3030
    if ((s->strm)->avail_in == 0U) {
#line 3030
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3030
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3030
    s->bsLive += 8;
#line 3030
    ((s->strm)->next_in) ++;
#line 3030
    ((s->strm)->avail_in) --;
#line 3030
    ((s->strm)->total_in_lo32) ++;
#line 3030
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3030
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3031
  if ((int )uc != 66) {
#line 3031
    retVal = -5;
    goto save_state_and_return;
  } else {

  }
  case 11: 
#line 3033
  s->state = 11;
#line 3033
  while (1) {
#line 3033
    if (s->bsLive >= 8) {
#line 3033
      v___0 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3033
      s->bsLive -= 8;
#line 3033
      uc = (unsigned char )v___0;
#line 3033
      break;
    } else {

    }
#line 3033
    if ((s->strm)->avail_in == 0U) {
#line 3033
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3033
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3033
    s->bsLive += 8;
#line 3033
    ((s->strm)->next_in) ++;
#line 3033
    ((s->strm)->avail_in) --;
#line 3033
    ((s->strm)->total_in_lo32) ++;
#line 3033
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3033
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3034
  if ((int )uc != 90) {
#line 3034
    retVal = -5;
    goto save_state_and_return;
  } else {

  }
  case 12: 
#line 3036
  s->state = 12;
#line 3036
  while (1) {
#line 3036
    if (s->bsLive >= 8) {
#line 3036
      v___1 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3036
      s->bsLive -= 8;
#line 3036
      uc = (unsigned char )v___1;
#line 3036
      break;
    } else {

    }
#line 3036
    if ((s->strm)->avail_in == 0U) {
#line 3036
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3036
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3036
    s->bsLive += 8;
#line 3036
    ((s->strm)->next_in) ++;
#line 3036
    ((s->strm)->avail_in) --;
#line 3036
    ((s->strm)->total_in_lo32) ++;
#line 3036
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3036
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3037
  if ((int )uc != 104) {
#line 3037
    retVal = -5;
    goto save_state_and_return;
  } else {

  }
  case 13: 
#line 3039
  s->state = 13;
#line 3039
  while (1) {
#line 3039
    if (s->bsLive >= 8) {
#line 3039
      v___2 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3039
      s->bsLive -= 8;
#line 3039
      s->blockSize100k = (int )v___2;
#line 3039
      break;
    } else {

    }
#line 3039
    if ((s->strm)->avail_in == 0U) {
#line 3039
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3039
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3039
    s->bsLive += 8;
#line 3039
    ((s->strm)->next_in) ++;
#line 3039
    ((s->strm)->avail_in) --;
#line 3039
    ((s->strm)->total_in_lo32) ++;
#line 3039
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3039
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3040
  if (s->blockSize100k < 49) {
#line 3041
    retVal = -5;
    goto save_state_and_return;
  } else {
#line 3040
    if (s->blockSize100k > 57) {
#line 3041
      retVal = -5;
      goto save_state_and_return;
    } else {

    }
  }
#line 3042
  s->blockSize100k -= 48;
#line 3044
  if (s->smallDecompress) {
#line 3045
    tmp = (*(strm->bzalloc))(strm->opaque,
                             (int )((unsigned long )(s->blockSize100k * 100000) * sizeof(UInt16 )),
                             1);
#line 3045
    s->ll16 = (UInt16 *)tmp;
#line 3046
    tmp___0 = (*(strm->bzalloc))(strm->opaque,
                                 (int )((unsigned long )((1 + s->blockSize100k * 100000) >> 1) * sizeof(UChar )),
                                 1);
#line 3046
    s->ll4 = (UChar *)tmp___0;
#line 3049
    if ((unsigned long )s->ll16 == (unsigned long )((void *)0)) {
#line 3049
      retVal = -3;
      goto save_state_and_return;
    } else {
#line 3049
      if ((unsigned long )s->ll4 == (unsigned long )((void *)0)) {
#line 3049
        retVal = -3;
        goto save_state_and_return;
      } else {

      }
    }
  } else {
#line 3051
    tmp___1 = (*(strm->bzalloc))(strm->opaque,
                                 (int )((unsigned long )(s->blockSize100k * 100000) * sizeof(Int32 )),
                                 1);
#line 3051
    s->tt = (UInt32 *)tmp___1;
#line 3052
    if ((unsigned long )s->tt == (unsigned long )((void *)0)) {
#line 3052
      retVal = -3;
      goto save_state_and_return;
    } else {

    }
  }
  case 14: 
#line 3055
  s->state = 14;
#line 3055
  while (1) {
#line 3055
    if (s->bsLive >= 8) {
#line 3055
      v___3 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3055
      s->bsLive -= 8;
#line 3055
      uc = (unsigned char )v___3;
#line 3055
      break;
    } else {

    }
#line 3055
    if ((s->strm)->avail_in == 0U) {
#line 3055
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3055
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3055
    s->bsLive += 8;
#line 3055
    ((s->strm)->next_in) ++;
#line 3055
    ((s->strm)->avail_in) --;
#line 3055
    ((s->strm)->total_in_lo32) ++;
#line 3055
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3055
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3057
  if ((int )uc == 23) {
    goto endhdr_2;
  } else {

  }
#line 3058
  if ((int )uc != 49) {
#line 3058
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 15: 
#line 3059
  s->state = 15;
#line 3059
  while (1) {
#line 3059
    if (s->bsLive >= 8) {
#line 3059
      v___4 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3059
      s->bsLive -= 8;
#line 3059
      uc = (unsigned char )v___4;
#line 3059
      break;
    } else {

    }
#line 3059
    if ((s->strm)->avail_in == 0U) {
#line 3059
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3059
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3059
    s->bsLive += 8;
#line 3059
    ((s->strm)->next_in) ++;
#line 3059
    ((s->strm)->avail_in) --;
#line 3059
    ((s->strm)->total_in_lo32) ++;
#line 3059
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3059
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3060
  if ((int )uc != 65) {
#line 3060
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 16: 
#line 3061
  s->state = 16;
#line 3061
  while (1) {
#line 3061
    if (s->bsLive >= 8) {
#line 3061
      v___5 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3061
      s->bsLive -= 8;
#line 3061
      uc = (unsigned char )v___5;
#line 3061
      break;
    } else {

    }
#line 3061
    if ((s->strm)->avail_in == 0U) {
#line 3061
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3061
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3061
    s->bsLive += 8;
#line 3061
    ((s->strm)->next_in) ++;
#line 3061
    ((s->strm)->avail_in) --;
#line 3061
    ((s->strm)->total_in_lo32) ++;
#line 3061
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3061
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3062
  if ((int )uc != 89) {
#line 3062
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 17: 
#line 3063
  s->state = 17;
#line 3063
  while (1) {
#line 3063
    if (s->bsLive >= 8) {
#line 3063
      v___6 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3063
      s->bsLive -= 8;
#line 3063
      uc = (unsigned char )v___6;
#line 3063
      break;
    } else {

    }
#line 3063
    if ((s->strm)->avail_in == 0U) {
#line 3063
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3063
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3063
    s->bsLive += 8;
#line 3063
    ((s->strm)->next_in) ++;
#line 3063
    ((s->strm)->avail_in) --;
#line 3063
    ((s->strm)->total_in_lo32) ++;
#line 3063
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3063
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3064
  if ((int )uc != 38) {
#line 3064
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 18: 
#line 3065
  s->state = 18;
#line 3065
  while (1) {
#line 3065
    if (s->bsLive >= 8) {
#line 3065
      v___7 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3065
      s->bsLive -= 8;
#line 3065
      uc = (unsigned char )v___7;
#line 3065
      break;
    } else {

    }
#line 3065
    if ((s->strm)->avail_in == 0U) {
#line 3065
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3065
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3065
    s->bsLive += 8;
#line 3065
    ((s->strm)->next_in) ++;
#line 3065
    ((s->strm)->avail_in) --;
#line 3065
    ((s->strm)->total_in_lo32) ++;
#line 3065
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3065
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3066
  if ((int )uc != 83) {
#line 3066
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 19: 
#line 3067
  s->state = 19;
#line 3067
  while (1) {
#line 3067
    if (s->bsLive >= 8) {
#line 3067
      v___8 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3067
      s->bsLive -= 8;
#line 3067
      uc = (unsigned char )v___8;
#line 3067
      break;
    } else {

    }
#line 3067
    if ((s->strm)->avail_in == 0U) {
#line 3067
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3067
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3067
    s->bsLive += 8;
#line 3067
    ((s->strm)->next_in) ++;
#line 3067
    ((s->strm)->avail_in) --;
#line 3067
    ((s->strm)->total_in_lo32) ++;
#line 3067
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3067
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3068
  if ((int )uc != 89) {
#line 3068
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
#line 3070
  (s->currBlockNo) ++;
#line 3071
  if (s->verbosity >= 2) {
#line 3072
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n    [%d: huff+mtf ",
            s->currBlockNo);
  } else {

  }
#line 3074
  s->storedBlockCRC = 0U;
  case 20: 
#line 3075
  s->state = 20;
#line 3075
  while (1) {
#line 3075
    if (s->bsLive >= 8) {
#line 3075
      v___9 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3075
      s->bsLive -= 8;
#line 3075
      uc = (unsigned char )v___9;
#line 3075
      break;
    } else {

    }
#line 3075
    if ((s->strm)->avail_in == 0U) {
#line 3075
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3075
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3075
    s->bsLive += 8;
#line 3075
    ((s->strm)->next_in) ++;
#line 3075
    ((s->strm)->avail_in) --;
#line 3075
    ((s->strm)->total_in_lo32) ++;
#line 3075
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3075
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3076
  s->storedBlockCRC = (s->storedBlockCRC << 8) | (unsigned int )uc;
  case 21: 
#line 3077
  s->state = 21;
#line 3077
  while (1) {
#line 3077
    if (s->bsLive >= 8) {
#line 3077
      v___10 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3077
      s->bsLive -= 8;
#line 3077
      uc = (unsigned char )v___10;
#line 3077
      break;
    } else {

    }
#line 3077
    if ((s->strm)->avail_in == 0U) {
#line 3077
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3077
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3077
    s->bsLive += 8;
#line 3077
    ((s->strm)->next_in) ++;
#line 3077
    ((s->strm)->avail_in) --;
#line 3077
    ((s->strm)->total_in_lo32) ++;
#line 3077
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3077
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3078
  s->storedBlockCRC = (s->storedBlockCRC << 8) | (unsigned int )uc;
  case 22: 
#line 3079
  s->state = 22;
#line 3079
  while (1) {
#line 3079
    if (s->bsLive >= 8) {
#line 3079
      v___11 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3079
      s->bsLive -= 8;
#line 3079
      uc = (unsigned char )v___11;
#line 3079
      break;
    } else {

    }
#line 3079
    if ((s->strm)->avail_in == 0U) {
#line 3079
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3079
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3079
    s->bsLive += 8;
#line 3079
    ((s->strm)->next_in) ++;
#line 3079
    ((s->strm)->avail_in) --;
#line 3079
    ((s->strm)->total_in_lo32) ++;
#line 3079
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3079
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3080
  s->storedBlockCRC = (s->storedBlockCRC << 8) | (unsigned int )uc;
  case 23: 
#line 3081
  s->state = 23;
#line 3081
  while (1) {
#line 3081
    if (s->bsLive >= 8) {
#line 3081
      v___12 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3081
      s->bsLive -= 8;
#line 3081
      uc = (unsigned char )v___12;
#line 3081
      break;
    } else {

    }
#line 3081
    if ((s->strm)->avail_in == 0U) {
#line 3081
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3081
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3081
    s->bsLive += 8;
#line 3081
    ((s->strm)->next_in) ++;
#line 3081
    ((s->strm)->avail_in) --;
#line 3081
    ((s->strm)->total_in_lo32) ++;
#line 3081
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3081
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3082
  s->storedBlockCRC = (s->storedBlockCRC << 8) | (unsigned int )uc;
  case 24: 
#line 3084
  s->state = 24;
#line 3084
  while (1) {
#line 3084
    if (s->bsLive >= 1) {
#line 3084
      v___13 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3084
      (s->bsLive) --;
#line 3084
      s->blockRandomised = (unsigned char )v___13;
#line 3084
      break;
    } else {

    }
#line 3084
    if ((s->strm)->avail_in == 0U) {
#line 3084
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3084
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3084
    s->bsLive += 8;
#line 3084
    ((s->strm)->next_in) ++;
#line 3084
    ((s->strm)->avail_in) --;
#line 3084
    ((s->strm)->total_in_lo32) ++;
#line 3084
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3084
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3086
  s->origPtr = 0;
  case 25: 
#line 3087
  s->state = 25;
#line 3087
  while (1) {
#line 3087
    if (s->bsLive >= 8) {
#line 3087
      v___14 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3087
      s->bsLive -= 8;
#line 3087
      uc = (unsigned char )v___14;
#line 3087
      break;
    } else {

    }
#line 3087
    if ((s->strm)->avail_in == 0U) {
#line 3087
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3087
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3087
    s->bsLive += 8;
#line 3087
    ((s->strm)->next_in) ++;
#line 3087
    ((s->strm)->avail_in) --;
#line 3087
    ((s->strm)->total_in_lo32) ++;
#line 3087
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3087
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3088
  s->origPtr = (s->origPtr << 8) | (int )uc;
  case 26: 
#line 3089
  s->state = 26;
#line 3089
  while (1) {
#line 3089
    if (s->bsLive >= 8) {
#line 3089
      v___15 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3089
      s->bsLive -= 8;
#line 3089
      uc = (unsigned char )v___15;
#line 3089
      break;
    } else {

    }
#line 3089
    if ((s->strm)->avail_in == 0U) {
#line 3089
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3089
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3089
    s->bsLive += 8;
#line 3089
    ((s->strm)->next_in) ++;
#line 3089
    ((s->strm)->avail_in) --;
#line 3089
    ((s->strm)->total_in_lo32) ++;
#line 3089
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3089
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3090
  s->origPtr = (s->origPtr << 8) | (int )uc;
  case 27: 
#line 3091
  s->state = 27;
#line 3091
  while (1) {
#line 3091
    if (s->bsLive >= 8) {
#line 3091
      v___16 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3091
      s->bsLive -= 8;
#line 3091
      uc = (unsigned char )v___16;
#line 3091
      break;
    } else {

    }
#line 3091
    if ((s->strm)->avail_in == 0U) {
#line 3091
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3091
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3091
    s->bsLive += 8;
#line 3091
    ((s->strm)->next_in) ++;
#line 3091
    ((s->strm)->avail_in) --;
#line 3091
    ((s->strm)->total_in_lo32) ++;
#line 3091
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3091
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3092
  s->origPtr = (s->origPtr << 8) | (int )uc;
#line 3094
  if (s->origPtr < 0) {
#line 3095
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
#line 3096
  if (s->origPtr > 10 + 100000 * s->blockSize100k) {
#line 3097
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
#line 3100
  i = 0;
#line 3100
  while (i < 16) {
    case 28: 
#line 3101
    s->state = 28;
#line 3101
    while (1) {
#line 3101
      if (s->bsLive >= 1) {
#line 3101
        v___17 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3101
        (s->bsLive) --;
#line 3101
        uc = (unsigned char )v___17;
#line 3101
        break;
      } else {

      }
#line 3101
      if ((s->strm)->avail_in == 0U) {
#line 3101
        retVal = 0;
        goto save_state_and_return;
      } else {

      }
#line 3101
      s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3101
      s->bsLive += 8;
#line 3101
      ((s->strm)->next_in) ++;
#line 3101
      ((s->strm)->avail_in) --;
#line 3101
      ((s->strm)->total_in_lo32) ++;
#line 3101
      if ((s->strm)->total_in_lo32 == 0U) {
#line 3101
        ((s->strm)->total_in_hi32) ++;
      } else {

      }
    }
#line 3102
    if ((int )uc == 1) {
#line 3103
      s->inUse16[i] = (unsigned char)1;
    } else {
#line 3104
      s->inUse16[i] = (unsigned char)0;
    }
#line 3100
    i ++;
  }
#line 3107
  i = 0;
#line 3107
  while (i < 256) {
#line 3107
    s->inUse[i] = (unsigned char)0;
#line 3107
    i ++;
  }
#line 3109
  i = 0;
#line 3109
  while (i < 16) {
#line 3110
    if (s->inUse16[i]) {
#line 3111
      j = 0;
#line 3111
      while (j < 16) {
        case 29: 
#line 3112
        s->state = 29;
#line 3112
        while (1) {
#line 3112
          if (s->bsLive >= 1) {
#line 3112
            v___18 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3112
            (s->bsLive) --;
#line 3112
            uc = (unsigned char )v___18;
#line 3112
            break;
          } else {

          }
#line 3112
          if ((s->strm)->avail_in == 0U) {
#line 3112
            retVal = 0;
            goto save_state_and_return;
          } else {

          }
#line 3112
          s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3112
          s->bsLive += 8;
#line 3112
          ((s->strm)->next_in) ++;
#line 3112
          ((s->strm)->avail_in) --;
#line 3112
          ((s->strm)->total_in_lo32) ++;
#line 3112
          if ((s->strm)->total_in_lo32 == 0U) {
#line 3112
            ((s->strm)->total_in_hi32) ++;
          } else {

          }
        }
#line 3113
        if ((int )uc == 1) {
#line 3113
          s->inUse[i * 16 + j] = (unsigned char)1;
        } else {

        }
#line 3111
        j ++;
      }
    } else {

    }
#line 3109
    i ++;
  }
#line 3115
  makeMaps_d(s);
#line 3116
  if (s->nInUse == 0) {
#line 3116
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
#line 3117
  alphaSize = s->nInUse + 2;
  case 30: 
#line 3120
  s->state = 30;
#line 3120
  while (1) {
#line 3120
    if (s->bsLive >= 3) {
#line 3120
      v___19 = (s->bsBuff >> (s->bsLive - 3)) & (unsigned int )((1 << 3) - 1);
#line 3120
      s->bsLive -= 3;
#line 3120
      nGroups = (int )v___19;
#line 3120
      break;
    } else {

    }
#line 3120
    if ((s->strm)->avail_in == 0U) {
#line 3120
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3120
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3120
    s->bsLive += 8;
#line 3120
    ((s->strm)->next_in) ++;
#line 3120
    ((s->strm)->avail_in) --;
#line 3120
    ((s->strm)->total_in_lo32) ++;
#line 3120
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3120
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3121
  if (nGroups < 2) {
#line 3121
    retVal = -4;
    goto save_state_and_return;
  } else {
#line 3121
    if (nGroups > 6) {
#line 3121
      retVal = -4;
      goto save_state_and_return;
    } else {

    }
  }
  case 31: 
#line 3122
  s->state = 31;
#line 3122
  while (1) {
#line 3122
    if (s->bsLive >= 15) {
#line 3122
      v___20 = (s->bsBuff >> (s->bsLive - 15)) & (unsigned int )((1 << 15) - 1);
#line 3122
      s->bsLive -= 15;
#line 3122
      nSelectors = (int )v___20;
#line 3122
      break;
    } else {

    }
#line 3122
    if ((s->strm)->avail_in == 0U) {
#line 3122
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3122
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3122
    s->bsLive += 8;
#line 3122
    ((s->strm)->next_in) ++;
#line 3122
    ((s->strm)->avail_in) --;
#line 3122
    ((s->strm)->total_in_lo32) ++;
#line 3122
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3122
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3123
  if (nSelectors < 1) {
#line 3123
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
#line 3124
  i = 0;
#line 3124
  while (i < nSelectors) {
#line 3125
    j = 0;
#line 3126
    while (1) {
      case 32: 
#line 3127
      s->state = 32;
#line 3127
      while (1) {
#line 3127
        if (s->bsLive >= 1) {
#line 3127
          v___21 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3127
          (s->bsLive) --;
#line 3127
          uc = (unsigned char )v___21;
#line 3127
          break;
        } else {

        }
#line 3127
        if ((s->strm)->avail_in == 0U) {
#line 3127
          retVal = 0;
          goto save_state_and_return;
        } else {

        }
#line 3127
        s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3127
        s->bsLive += 8;
#line 3127
        ((s->strm)->next_in) ++;
#line 3127
        ((s->strm)->avail_in) --;
#line 3127
        ((s->strm)->total_in_lo32) ++;
#line 3127
        if ((s->strm)->total_in_lo32 == 0U) {
#line 3127
          ((s->strm)->total_in_hi32) ++;
        } else {

        }
      }
#line 3128
      if ((int )uc == 0) {
#line 3128
        break;
      } else {

      }
#line 3129
      j ++;
#line 3130
      if (j >= nGroups) {
#line 3130
        retVal = -4;
        goto save_state_and_return;
      } else {

      }
    }
#line 3132
    s->selectorMtf[i] = (unsigned char )j;
#line 3124
    i ++;
  }
#line 3138
  v___22 = (unsigned char)0;
#line 3138
  while ((int )v___22 < nGroups) {
#line 3138
    pos[v___22] = v___22;
#line 3138
    v___22 = (UChar )((int )v___22 + 1);
  }
#line 3140
  i = 0;
#line 3140
  while (i < nSelectors) {
#line 3141
    v___22 = s->selectorMtf[i];
#line 3142
    tmp___2 = pos[v___22];
#line 3143
    while ((int )v___22 > 0) {
#line 3143
      pos[v___22] = pos[(int )v___22 - 1];
#line 3143
      v___22 = (UChar )((int )v___22 - 1);
    }
#line 3144
    pos[0] = tmp___2;
#line 3145
    s->selector[i] = tmp___2;
#line 3140
    i ++;
  }
#line 3150
  t = 0;
#line 3150
  while (t < nGroups) {
    case 33: 
#line 3151
    s->state = 33;
#line 3151
    while (1) {
#line 3151
      if (s->bsLive >= 5) {
#line 3151
        v___23 = (s->bsBuff >> (s->bsLive - 5)) & (unsigned int )((1 << 5) - 1);
#line 3151
        s->bsLive -= 5;
#line 3151
        curr = (int )v___23;
#line 3151
        break;
      } else {

      }
#line 3151
      if ((s->strm)->avail_in == 0U) {
#line 3151
        retVal = 0;
        goto save_state_and_return;
      } else {

      }
#line 3151
      s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3151
      s->bsLive += 8;
#line 3151
      ((s->strm)->next_in) ++;
#line 3151
      ((s->strm)->avail_in) --;
#line 3151
      ((s->strm)->total_in_lo32) ++;
#line 3151
      if ((s->strm)->total_in_lo32 == 0U) {
#line 3151
        ((s->strm)->total_in_hi32) ++;
      } else {

      }
    }
#line 3152
    i = 0;
#line 3152
    while (i < alphaSize) {
#line 3153
      while (1) {
#line 3154
        if (curr < 1) {
#line 3154
          retVal = -4;
          goto save_state_and_return;
        } else {
#line 3154
          if (curr > 20) {
#line 3154
            retVal = -4;
            goto save_state_and_return;
          } else {

          }
        }
        case 34: 
#line 3155
        s->state = 34;
#line 3155
        while (1) {
#line 3155
          if (s->bsLive >= 1) {
#line 3155
            v___24 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3155
            (s->bsLive) --;
#line 3155
            uc = (unsigned char )v___24;
#line 3155
            break;
          } else {

          }
#line 3155
          if ((s->strm)->avail_in == 0U) {
#line 3155
            retVal = 0;
            goto save_state_and_return;
          } else {

          }
#line 3155
          s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3155
          s->bsLive += 8;
#line 3155
          ((s->strm)->next_in) ++;
#line 3155
          ((s->strm)->avail_in) --;
#line 3155
          ((s->strm)->total_in_lo32) ++;
#line 3155
          if ((s->strm)->total_in_lo32 == 0U) {
#line 3155
            ((s->strm)->total_in_hi32) ++;
          } else {

          }
        }
#line 3156
        if ((int )uc == 0) {
#line 3156
          break;
        } else {

        }
        case 35: 
#line 3157
        s->state = 35;
#line 3157
        while (1) {
#line 3157
          if (s->bsLive >= 1) {
#line 3157
            v___25 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3157
            (s->bsLive) --;
#line 3157
            uc = (unsigned char )v___25;
#line 3157
            break;
          } else {

          }
#line 3157
          if ((s->strm)->avail_in == 0U) {
#line 3157
            retVal = 0;
            goto save_state_and_return;
          } else {

          }
#line 3157
          s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3157
          s->bsLive += 8;
#line 3157
          ((s->strm)->next_in) ++;
#line 3157
          ((s->strm)->avail_in) --;
#line 3157
          ((s->strm)->total_in_lo32) ++;
#line 3157
          if ((s->strm)->total_in_lo32 == 0U) {
#line 3157
            ((s->strm)->total_in_hi32) ++;
          } else {

          }
        }
#line 3158
        if ((int )uc == 0) {
#line 3158
          curr ++;
        } else {
#line 3158
          curr --;
        }
      }
#line 3160
      s->len[t][i] = (unsigned char )curr;
#line 3152
      i ++;
    }
#line 3150
    t ++;
  }
#line 3165
  t = 0;
#line 3165
  while (t < nGroups) {
#line 3166
    minLen = 32;
#line 3167
    maxLen = 0;
#line 3168
    i = 0;
#line 3168
    while (i < alphaSize) {
#line 3169
      if ((int )s->len[t][i] > maxLen) {
#line 3169
        maxLen = (int )s->len[t][i];
      } else {

      }
#line 3170
      if ((int )s->len[t][i] < minLen) {
#line 3170
        minLen = (int )s->len[t][i];
      } else {

      }
#line 3168
      i ++;
    }
#line 3172
    BZ2_hbCreateDecodeTables(& s->limit[t][0], & s->base[t][0], & s->perm[t][0],
                             & s->len[t][0], minLen, maxLen, alphaSize);
#line 3179
    s->minLens[t] = minLen;
#line 3165
    t ++;
  }
#line 3184
  EOB = s->nInUse + 1;
#line 3185
  nblockMAX = 100000 * s->blockSize100k;
#line 3186
  groupNo = -1;
#line 3187
  groupPos = 0;
#line 3189
  i = 0;
#line 3189
  while (i <= 255) {
#line 3189
    s->unzftab[i] = 0;
#line 3189
    i ++;
  }
#line 3194
  kk = 4095;
#line 3195
  ii = 15;
#line 3195
  while (ii >= 0) {
#line 3196
    jj = 15;
#line 3196
    while (jj >= 0) {
#line 3197
      s->mtfa[kk] = (unsigned char )(ii * 16 + jj);
#line 3198
      kk --;
#line 3196
      jj --;
    }
#line 3200
    s->mtfbase[ii] = kk + 1;
#line 3195
    ii --;
  }
#line 3205
  nblock = 0;
#line 3206
  if (groupPos == 0) {
#line 3206
    groupNo ++;
#line 3206
    if (groupNo >= nSelectors) {
#line 3206
      retVal = -4;
      goto save_state_and_return;
    } else {

    }
#line 3206
    groupPos = 50;
#line 3206
    gSel = (int )s->selector[groupNo];
#line 3206
    gMinlen = s->minLens[gSel];
#line 3206
    gLimit = & s->limit[gSel][0];
#line 3206
    gPerm = & s->perm[gSel][0];
#line 3206
    gBase = & s->base[gSel][0];
  } else {

  }
#line 3206
  groupPos --;
#line 3206
  zn = gMinlen;
  case 36: 
#line 3206
  s->state = 36;
#line 3206
  while (1) {
#line 3206
    if (s->bsLive >= zn) {
#line 3206
      v___26 = (s->bsBuff >> (s->bsLive - zn)) & (unsigned int )((1 << zn) - 1);
#line 3206
      s->bsLive -= zn;
#line 3206
      zvec = (int )v___26;
#line 3206
      break;
    } else {

    }
#line 3206
    if ((s->strm)->avail_in == 0U) {
#line 3206
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3206
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3206
    s->bsLive += 8;
#line 3206
    ((s->strm)->next_in) ++;
#line 3206
    ((s->strm)->avail_in) --;
#line 3206
    ((s->strm)->total_in_lo32) ++;
#line 3206
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3206
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3206
  while (1) {
#line 3206
    if (zn > 20) {
#line 3206
      retVal = -4;
      goto save_state_and_return;
    } else {

    }
#line 3206
    if (zvec <= *(gLimit + zn)) {
#line 3206
      break;
    } else {

    }
#line 3206
    zn ++;
    case 37: 
#line 3206
    s->state = 37;
#line 3206
    while (1) {
#line 3206
      if (s->bsLive >= 1) {
#line 3206
        v___27 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3206
        (s->bsLive) --;
#line 3206
        zj = (int )v___27;
#line 3206
        break;
      } else {

      }
#line 3206
      if ((s->strm)->avail_in == 0U) {
#line 3206
        retVal = 0;
        goto save_state_and_return;
      } else {

      }
#line 3206
      s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3206
      s->bsLive += 8;
#line 3206
      ((s->strm)->next_in) ++;
#line 3206
      ((s->strm)->avail_in) --;
#line 3206
      ((s->strm)->total_in_lo32) ++;
#line 3206
      if ((s->strm)->total_in_lo32 == 0U) {
#line 3206
        ((s->strm)->total_in_hi32) ++;
      } else {

      }
    }
#line 3206
    zvec = (zvec << 1) | zj;
  }
#line 3206
  if (zvec - *(gBase + zn) < 0) {
#line 3206
    retVal = -4;
    goto save_state_and_return;
  } else {
#line 3206
    if (zvec - *(gBase + zn) >= 258) {
#line 3206
      retVal = -4;
      goto save_state_and_return;
    } else {

    }
  }
#line 3206
  nextSym = *(gPerm + (zvec - *(gBase + zn)));
#line 3208
  while (! (nextSym == EOB)) {
#line 3212
    if (nextSym == 0) {
      goto _L;
    } else {
#line 3212
      if (nextSym == 1) {
        _L: /* CIL Label */ 
#line 3214
        es = -1;
#line 3215
        N = 1;
#line 3216
        while (1) {
#line 3217
          if (nextSym == 0) {
#line 3217
            es += N;
          } else {
#line 3218
            if (nextSym == 1) {
#line 3218
              es += 2 * N;
            } else {

            }
          }
#line 3219
          N *= 2;
#line 3220
          if (groupPos == 0) {
#line 3220
            groupNo ++;
#line 3220
            if (groupNo >= nSelectors) {
#line 3220
              retVal = -4;
              goto save_state_and_return;
            } else {

            }
#line 3220
            groupPos = 50;
#line 3220
            gSel = (int )s->selector[groupNo];
#line 3220
            gMinlen = s->minLens[gSel];
#line 3220
            gLimit = & s->limit[gSel][0];
#line 3220
            gPerm = & s->perm[gSel][0];
#line 3220
            gBase = & s->base[gSel][0];
          } else {

          }
#line 3220
          groupPos --;
#line 3220
          zn = gMinlen;
          case 38: 
#line 3220
          s->state = 38;
#line 3220
          while (1) {
#line 3220
            if (s->bsLive >= zn) {
#line 3220
              v___28 = (s->bsBuff >> (s->bsLive - zn)) & (unsigned int )((1 << zn) - 1);
#line 3220
              s->bsLive -= zn;
#line 3220
              zvec = (int )v___28;
#line 3220
              break;
            } else {

            }
#line 3220
            if ((s->strm)->avail_in == 0U) {
#line 3220
              retVal = 0;
              goto save_state_and_return;
            } else {

            }
#line 3220
            s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3220
            s->bsLive += 8;
#line 3220
            ((s->strm)->next_in) ++;
#line 3220
            ((s->strm)->avail_in) --;
#line 3220
            ((s->strm)->total_in_lo32) ++;
#line 3220
            if ((s->strm)->total_in_lo32 == 0U) {
#line 3220
              ((s->strm)->total_in_hi32) ++;
            } else {

            }
          }
#line 3220
          while (1) {
#line 3220
            if (zn > 20) {
#line 3220
              retVal = -4;
              goto save_state_and_return;
            } else {

            }
#line 3220
            if (zvec <= *(gLimit + zn)) {
#line 3220
              break;
            } else {

            }
#line 3220
            zn ++;
            case 39: 
#line 3220
            s->state = 39;
#line 3220
            while (1) {
#line 3220
              if (s->bsLive >= 1) {
#line 3220
                v___29 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3220
                (s->bsLive) --;
#line 3220
                zj = (int )v___29;
#line 3220
                break;
              } else {

              }
#line 3220
              if ((s->strm)->avail_in == 0U) {
#line 3220
                retVal = 0;
                goto save_state_and_return;
              } else {

              }
#line 3220
              s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3220
              s->bsLive += 8;
#line 3220
              ((s->strm)->next_in) ++;
#line 3220
              ((s->strm)->avail_in) --;
#line 3220
              ((s->strm)->total_in_lo32) ++;
#line 3220
              if ((s->strm)->total_in_lo32 == 0U) {
#line 3220
                ((s->strm)->total_in_hi32) ++;
              } else {

              }
            }
#line 3220
            zvec = (zvec << 1) | zj;
          }
#line 3220
          if (zvec - *(gBase + zn) < 0) {
#line 3220
            retVal = -4;
            goto save_state_and_return;
          } else {
#line 3220
            if (zvec - *(gBase + zn) >= 258) {
#line 3220
              retVal = -4;
              goto save_state_and_return;
            } else {

            }
          }
#line 3220
          nextSym = *(gPerm + (zvec - *(gBase + zn)));
#line 3216
          if (nextSym == 0) {

          } else {
#line 3216
            if (nextSym == 1) {

            } else {
#line 3216
              break;
            }
          }
        }
#line 3224
        es ++;
#line 3225
        uc = s->seqToUnseq[s->mtfa[s->mtfbase[0]]];
#line 3226
        s->unzftab[uc] += es;
#line 3228
        if (s->smallDecompress) {
#line 3229
          while (es > 0) {
#line 3230
            if (nblock >= nblockMAX) {
#line 3230
              retVal = -4;
              goto save_state_and_return;
            } else {

            }
#line 3231
            *(s->ll16 + nblock) = (unsigned short )uc;
#line 3232
            nblock ++;
#line 3233
            es --;
          }
        } else {
#line 3236
          while (es > 0) {
#line 3237
            if (nblock >= nblockMAX) {
#line 3237
              retVal = -4;
              goto save_state_and_return;
            } else {

            }
#line 3238
            *(s->tt + nblock) = (unsigned int )uc;
#line 3239
            nblock ++;
#line 3240
            es --;
          }
        }
#line 3243
        continue;
      } else {
#line 3247
        if (nblock >= nblockMAX) {
#line 3247
          retVal = -4;
          goto save_state_and_return;
        } else {

        }
#line 3253
        nn = (unsigned int )(nextSym - 1);
#line 3255
        if (nn < 16U) {
#line 3257
          pp = s->mtfbase[0];
#line 3258
          uc = s->mtfa[(UInt32 )pp + nn];
#line 3259
          while (nn > 3U) {
#line 3260
            z = (Int32 )((UInt32 )pp + nn);
#line 3261
            s->mtfa[z] = s->mtfa[z - 1];
#line 3262
            s->mtfa[z - 1] = s->mtfa[z - 2];
#line 3263
            s->mtfa[z - 2] = s->mtfa[z - 3];
#line 3264
            s->mtfa[z - 3] = s->mtfa[z - 4];
#line 3265
            nn -= 4U;
          }
#line 3267
          while (nn > 0U) {
#line 3268
            s->mtfa[(UInt32 )pp + nn] = s->mtfa[((UInt32 )pp + nn) - 1U];
#line 3268
            nn --;
          }
#line 3270
          s->mtfa[pp] = uc;
        } else {
#line 3273
          lno = (int )(nn / 16U);
#line 3274
          off = (int )(nn % 16U);
#line 3275
          pp = s->mtfbase[lno] + off;
#line 3276
          uc = s->mtfa[pp];
#line 3277
          while (pp > s->mtfbase[lno]) {
#line 3278
            s->mtfa[pp] = s->mtfa[pp - 1];
#line 3278
            pp --;
          }
#line 3280
          (s->mtfbase[lno]) ++;
#line 3281
          while (lno > 0) {
#line 3282
            (s->mtfbase[lno]) --;
#line 3283
            s->mtfa[s->mtfbase[lno]] = s->mtfa[(s->mtfbase[lno - 1] + 16) - 1];
#line 3285
            lno --;
          }
#line 3287
          (s->mtfbase[0]) --;
#line 3288
          s->mtfa[s->mtfbase[0]] = uc;
#line 3289
          if (s->mtfbase[0] == 0) {
#line 3290
            kk___0 = 4095;
#line 3291
            ii___0 = 15;
#line 3291
            while (ii___0 >= 0) {
#line 3292
              jj___0 = 15;
#line 3292
              while (jj___0 >= 0) {
#line 3293
                s->mtfa[kk___0] = s->mtfa[s->mtfbase[ii___0] + jj___0];
#line 3294
                kk___0 --;
#line 3292
                jj___0 --;
              }
#line 3296
              s->mtfbase[ii___0] = kk___0 + 1;
#line 3291
              ii___0 --;
            }
          } else {

          }
        }
#line 3303
        (s->unzftab[s->seqToUnseq[uc]]) ++;
#line 3304
        if (s->smallDecompress) {
#line 3305
          *(s->ll16 + nblock) = (unsigned short )s->seqToUnseq[uc];
        } else {
#line 3306
          *(s->tt + nblock) = (unsigned int )s->seqToUnseq[uc];
        }
#line 3307
        nblock ++;
#line 3309
        if (groupPos == 0) {
#line 3309
          groupNo ++;
#line 3309
          if (groupNo >= nSelectors) {
#line 3309
            retVal = -4;
            goto save_state_and_return;
          } else {

          }
#line 3309
          groupPos = 50;
#line 3309
          gSel = (int )s->selector[groupNo];
#line 3309
          gMinlen = s->minLens[gSel];
#line 3309
          gLimit = & s->limit[gSel][0];
#line 3309
          gPerm = & s->perm[gSel][0];
#line 3309
          gBase = & s->base[gSel][0];
        } else {

        }
#line 3309
        groupPos --;
#line 3309
        zn = gMinlen;
        case 40: 
#line 3309
        s->state = 40;
#line 3309
        while (1) {
#line 3309
          if (s->bsLive >= zn) {
#line 3309
            v___30 = (s->bsBuff >> (s->bsLive - zn)) & (unsigned int )((1 << zn) - 1);
#line 3309
            s->bsLive -= zn;
#line 3309
            zvec = (int )v___30;
#line 3309
            break;
          } else {

          }
#line 3309
          if ((s->strm)->avail_in == 0U) {
#line 3309
            retVal = 0;
            goto save_state_and_return;
          } else {

          }
#line 3309
          s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3309
          s->bsLive += 8;
#line 3309
          ((s->strm)->next_in) ++;
#line 3309
          ((s->strm)->avail_in) --;
#line 3309
          ((s->strm)->total_in_lo32) ++;
#line 3309
          if ((s->strm)->total_in_lo32 == 0U) {
#line 3309
            ((s->strm)->total_in_hi32) ++;
          } else {

          }
        }
#line 3309
        while (1) {
#line 3309
          if (zn > 20) {
#line 3309
            retVal = -4;
            goto save_state_and_return;
          } else {

          }
#line 3309
          if (zvec <= *(gLimit + zn)) {
#line 3309
            break;
          } else {

          }
#line 3309
          zn ++;
          case 41: 
#line 3309
          s->state = 41;
#line 3309
          while (1) {
#line 3309
            if (s->bsLive >= 1) {
#line 3309
              v___31 = (s->bsBuff >> (s->bsLive - 1)) & (unsigned int )((1 << 1) - 1);
#line 3309
              (s->bsLive) --;
#line 3309
              zj = (int )v___31;
#line 3309
              break;
            } else {

            }
#line 3309
            if ((s->strm)->avail_in == 0U) {
#line 3309
              retVal = 0;
              goto save_state_and_return;
            } else {

            }
#line 3309
            s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3309
            s->bsLive += 8;
#line 3309
            ((s->strm)->next_in) ++;
#line 3309
            ((s->strm)->avail_in) --;
#line 3309
            ((s->strm)->total_in_lo32) ++;
#line 3309
            if ((s->strm)->total_in_lo32 == 0U) {
#line 3309
              ((s->strm)->total_in_hi32) ++;
            } else {

            }
          }
#line 3309
          zvec = (zvec << 1) | zj;
        }
#line 3309
        if (zvec - *(gBase + zn) < 0) {
#line 3309
          retVal = -4;
          goto save_state_and_return;
        } else {
#line 3309
          if (zvec - *(gBase + zn) >= 258) {
#line 3309
            retVal = -4;
            goto save_state_and_return;
          } else {

          }
        }
#line 3309
        nextSym = *(gPerm + (zvec - *(gBase + zn)));
#line 3310
        continue;
      }
    }
  }
#line 3317
  if (s->origPtr < 0) {
#line 3318
    retVal = -4;
    goto save_state_and_return;
  } else {
#line 3317
    if (s->origPtr >= nblock) {
#line 3318
      retVal = -4;
      goto save_state_and_return;
    } else {

    }
  }
#line 3320
  s->state_out_len = 0;
#line 3321
  s->state_out_ch = (unsigned char)0;
#line 3322
  s->calculatedBlockCRC = 4294967295U;
#line 3323
  s->state = 2;
#line 3324
  if (s->verbosity >= 2) {
#line 3324
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"rt+rld");
  } else {

  }
#line 3327
  s->cftab[0] = 0;
#line 3328
  i = 1;
#line 3328
  while (i <= 256) {
#line 3328
    s->cftab[i] = s->unzftab[i - 1];
#line 3328
    i ++;
  }
#line 3329
  i = 1;
#line 3329
  while (i <= 256) {
#line 3329
    s->cftab[i] += s->cftab[i - 1];
#line 3329
    i ++;
  }
#line 3331
  if (s->smallDecompress) {
#line 3334
    i = 0;
#line 3334
    while (i <= 256) {
#line 3334
      s->cftabCopy[i] = s->cftab[i];
#line 3334
      i ++;
    }
#line 3337
    i = 0;
#line 3337
    while (i < nblock) {
#line 3338
      uc = (unsigned char )*(s->ll16 + i);
#line 3339
      *(s->ll16 + i) = (unsigned short )(s->cftabCopy[uc] & 65535);
#line 3339
      if ((i & 1) == 0) {
#line 3339
        *(s->ll4 + (i >> 1)) = (unsigned char )(((int )*(s->ll4 + (i >> 1)) & 240) | (s->cftabCopy[uc] >> 16));
      } else {
#line 3339
        *(s->ll4 + (i >> 1)) = (unsigned char )(((int )*(s->ll4 + (i >> 1)) & 15) | ((s->cftabCopy[uc] >> 16) << 4));
      }
#line 3340
      (s->cftabCopy[uc]) ++;
#line 3337
      i ++;
    }
#line 3344
    i = s->origPtr;
#line 3345
    j = (int )((unsigned int )*(s->ll16 + i) | ((((unsigned int )*(s->ll4 + (i >> 1)) >> ((i << 2) & 4)) & 15U) << 16));
#line 3346
    while (1) {
#line 3347
      tmp___3 = (Int32 )((unsigned int )*(s->ll16 + j) | ((((unsigned int )*(s->ll4 + (j >> 1)) >> ((j << 2) & 4)) & 15U) << 16));
#line 3348
      *(s->ll16 + j) = (unsigned short )(i & 65535);
#line 3348
      if ((j & 1) == 0) {
#line 3348
        *(s->ll4 + (j >> 1)) = (unsigned char )(((int )*(s->ll4 + (j >> 1)) & 240) | (i >> 16));
      } else {
#line 3348
        *(s->ll4 + (j >> 1)) = (unsigned char )(((int )*(s->ll4 + (j >> 1)) & 15) | ((i >> 16) << 4));
      }
#line 3349
      i = j;
#line 3350
      j = tmp___3;
#line 3346
      if (i != s->origPtr) {

      } else {
#line 3346
        break;
      }
    }
#line 3354
    s->tPos = (unsigned int )s->origPtr;
#line 3355
    s->nblock_used = 0;
#line 3356
    if (s->blockRandomised) {
#line 3357
      s->rNToGo = 0;
#line 3357
      s->rTPos = 0;
#line 3358
      s->k0 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 3358
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 3358
      (s->nblock_used) ++;
#line 3359
      if (s->rNToGo == 0) {
#line 3359
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 3359
        (s->rTPos) ++;
#line 3359
        if (s->rTPos == 512) {
#line 3359
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 3359
      (s->rNToGo) --;
#line 3359
      if (s->rNToGo == 1) {
#line 3359
        tmp___4 = 1;
      } else {
#line 3359
        tmp___4 = 0;
      }
#line 3359
      s->k0 ^= tmp___4;
    } else {
#line 3361
      s->k0 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 3361
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 3361
      (s->nblock_used) ++;
    }
  } else {
#line 3367
    i = 0;
#line 3367
    while (i < nblock) {
#line 3368
      uc = (unsigned char )(*(s->tt + i) & 255U);
#line 3369
      *(s->tt + s->cftab[uc]) |= (unsigned int )(i << 8);
#line 3370
      (s->cftab[uc]) ++;
#line 3367
      i ++;
    }
#line 3373
    s->tPos = *(s->tt + s->origPtr) >> 8;
#line 3374
    s->nblock_used = 0;
#line 3375
    if (s->blockRandomised) {
#line 3376
      s->rNToGo = 0;
#line 3376
      s->rTPos = 0;
#line 3377
      s->tPos = *(s->tt + s->tPos);
#line 3377
      s->k0 = (int )((unsigned char )(s->tPos & 255U));
#line 3377
      s->tPos >>= 8;
#line 3377
      (s->nblock_used) ++;
#line 3378
      if (s->rNToGo == 0) {
#line 3378
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 3378
        (s->rTPos) ++;
#line 3378
        if (s->rTPos == 512) {
#line 3378
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 3378
      (s->rNToGo) --;
#line 3378
      if (s->rNToGo == 1) {
#line 3378
        tmp___5 = 1;
      } else {
#line 3378
        tmp___5 = 0;
      }
#line 3378
      s->k0 ^= tmp___5;
    } else {
#line 3380
      s->tPos = *(s->tt + s->tPos);
#line 3380
      s->k0 = (int )((unsigned char )(s->tPos & 255U));
#line 3380
      s->tPos >>= 8;
#line 3380
      (s->nblock_used) ++;
    }
  }
#line 3385
  retVal = 0;
  goto save_state_and_return;
  endhdr_2: 
  case 42: 
#line 3391
  s->state = 42;
#line 3391
  while (1) {
#line 3391
    if (s->bsLive >= 8) {
#line 3391
      v___32 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3391
      s->bsLive -= 8;
#line 3391
      uc = (unsigned char )v___32;
#line 3391
      break;
    } else {

    }
#line 3391
    if ((s->strm)->avail_in == 0U) {
#line 3391
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3391
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3391
    s->bsLive += 8;
#line 3391
    ((s->strm)->next_in) ++;
#line 3391
    ((s->strm)->avail_in) --;
#line 3391
    ((s->strm)->total_in_lo32) ++;
#line 3391
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3391
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3392
  if ((int )uc != 114) {
#line 3392
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 43: 
#line 3393
  s->state = 43;
#line 3393
  while (1) {
#line 3393
    if (s->bsLive >= 8) {
#line 3393
      v___33 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3393
      s->bsLive -= 8;
#line 3393
      uc = (unsigned char )v___33;
#line 3393
      break;
    } else {

    }
#line 3393
    if ((s->strm)->avail_in == 0U) {
#line 3393
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3393
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3393
    s->bsLive += 8;
#line 3393
    ((s->strm)->next_in) ++;
#line 3393
    ((s->strm)->avail_in) --;
#line 3393
    ((s->strm)->total_in_lo32) ++;
#line 3393
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3393
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3394
  if ((int )uc != 69) {
#line 3394
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 44: 
#line 3395
  s->state = 44;
#line 3395
  while (1) {
#line 3395
    if (s->bsLive >= 8) {
#line 3395
      v___34 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3395
      s->bsLive -= 8;
#line 3395
      uc = (unsigned char )v___34;
#line 3395
      break;
    } else {

    }
#line 3395
    if ((s->strm)->avail_in == 0U) {
#line 3395
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3395
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3395
    s->bsLive += 8;
#line 3395
    ((s->strm)->next_in) ++;
#line 3395
    ((s->strm)->avail_in) --;
#line 3395
    ((s->strm)->total_in_lo32) ++;
#line 3395
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3395
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3396
  if ((int )uc != 56) {
#line 3396
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 45: 
#line 3397
  s->state = 45;
#line 3397
  while (1) {
#line 3397
    if (s->bsLive >= 8) {
#line 3397
      v___35 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3397
      s->bsLive -= 8;
#line 3397
      uc = (unsigned char )v___35;
#line 3397
      break;
    } else {

    }
#line 3397
    if ((s->strm)->avail_in == 0U) {
#line 3397
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3397
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3397
    s->bsLive += 8;
#line 3397
    ((s->strm)->next_in) ++;
#line 3397
    ((s->strm)->avail_in) --;
#line 3397
    ((s->strm)->total_in_lo32) ++;
#line 3397
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3397
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3398
  if ((int )uc != 80) {
#line 3398
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
  case 46: 
#line 3399
  s->state = 46;
#line 3399
  while (1) {
#line 3399
    if (s->bsLive >= 8) {
#line 3399
      v___36 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3399
      s->bsLive -= 8;
#line 3399
      uc = (unsigned char )v___36;
#line 3399
      break;
    } else {

    }
#line 3399
    if ((s->strm)->avail_in == 0U) {
#line 3399
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3399
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3399
    s->bsLive += 8;
#line 3399
    ((s->strm)->next_in) ++;
#line 3399
    ((s->strm)->avail_in) --;
#line 3399
    ((s->strm)->total_in_lo32) ++;
#line 3399
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3399
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3400
  if ((int )uc != 144) {
#line 3400
    retVal = -4;
    goto save_state_and_return;
  } else {

  }
#line 3402
  s->storedCombinedCRC = 0U;
  case 47: 
#line 3403
  s->state = 47;
#line 3403
  while (1) {
#line 3403
    if (s->bsLive >= 8) {
#line 3403
      v___37 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3403
      s->bsLive -= 8;
#line 3403
      uc = (unsigned char )v___37;
#line 3403
      break;
    } else {

    }
#line 3403
    if ((s->strm)->avail_in == 0U) {
#line 3403
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3403
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3403
    s->bsLive += 8;
#line 3403
    ((s->strm)->next_in) ++;
#line 3403
    ((s->strm)->avail_in) --;
#line 3403
    ((s->strm)->total_in_lo32) ++;
#line 3403
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3403
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3404
  s->storedCombinedCRC = (s->storedCombinedCRC << 8) | (unsigned int )uc;
  case 48: 
#line 3405
  s->state = 48;
#line 3405
  while (1) {
#line 3405
    if (s->bsLive >= 8) {
#line 3405
      v___38 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3405
      s->bsLive -= 8;
#line 3405
      uc = (unsigned char )v___38;
#line 3405
      break;
    } else {

    }
#line 3405
    if ((s->strm)->avail_in == 0U) {
#line 3405
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3405
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3405
    s->bsLive += 8;
#line 3405
    ((s->strm)->next_in) ++;
#line 3405
    ((s->strm)->avail_in) --;
#line 3405
    ((s->strm)->total_in_lo32) ++;
#line 3405
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3405
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3406
  s->storedCombinedCRC = (s->storedCombinedCRC << 8) | (unsigned int )uc;
  case 49: 
#line 3407
  s->state = 49;
#line 3407
  while (1) {
#line 3407
    if (s->bsLive >= 8) {
#line 3407
      v___39 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3407
      s->bsLive -= 8;
#line 3407
      uc = (unsigned char )v___39;
#line 3407
      break;
    } else {

    }
#line 3407
    if ((s->strm)->avail_in == 0U) {
#line 3407
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3407
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3407
    s->bsLive += 8;
#line 3407
    ((s->strm)->next_in) ++;
#line 3407
    ((s->strm)->avail_in) --;
#line 3407
    ((s->strm)->total_in_lo32) ++;
#line 3407
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3407
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3408
  s->storedCombinedCRC = (s->storedCombinedCRC << 8) | (unsigned int )uc;
  case 50: 
#line 3409
  s->state = 50;
#line 3409
  while (1) {
#line 3409
    if (s->bsLive >= 8) {
#line 3409
      v___40 = (s->bsBuff >> (s->bsLive - 8)) & (unsigned int )((1 << 8) - 1);
#line 3409
      s->bsLive -= 8;
#line 3409
      uc = (unsigned char )v___40;
#line 3409
      break;
    } else {

    }
#line 3409
    if ((s->strm)->avail_in == 0U) {
#line 3409
      retVal = 0;
      goto save_state_and_return;
    } else {

    }
#line 3409
    s->bsBuff = (s->bsBuff << 8) | (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3409
    s->bsLive += 8;
#line 3409
    ((s->strm)->next_in) ++;
#line 3409
    ((s->strm)->avail_in) --;
#line 3409
    ((s->strm)->total_in_lo32) ++;
#line 3409
    if ((s->strm)->total_in_lo32 == 0U) {
#line 3409
      ((s->strm)->total_in_hi32) ++;
    } else {

    }
  }
#line 3410
  s->storedCombinedCRC = (s->storedCombinedCRC << 8) | (unsigned int )uc;
#line 3412
  s->state = 1;
#line 3413
  retVal = 4;
  goto save_state_and_return;
  default: 
#line 3415
  BZ2_bz__AssertH__fail(4001);
  }
#line 3418
  BZ2_bz__AssertH__fail(4002);
  save_state_and_return: 
#line 3422
  s->save_i = i;
#line 3423
  s->save_j = j;
#line 3424
  s->save_t = t;
#line 3425
  s->save_alphaSize = alphaSize;
#line 3426
  s->save_nGroups = nGroups;
#line 3427
  s->save_nSelectors = nSelectors;
#line 3428
  s->save_EOB = EOB;
#line 3429
  s->save_groupNo = groupNo;
#line 3430
  s->save_groupPos = groupPos;
#line 3431
  s->save_nextSym = nextSym;
#line 3432
  s->save_nblockMAX = nblockMAX;
#line 3433
  s->save_nblock = nblock;
#line 3434
  s->save_es = es;
#line 3435
  s->save_N = N;
#line 3436
  s->save_curr = curr;
#line 3437
  s->save_zt = zt;
#line 3438
  s->save_zn = zn;
#line 3439
  s->save_zvec = zvec;
#line 3440
  s->save_zj = zj;
#line 3441
  s->save_gSel = gSel;
#line 3442
  s->save_gMinlen = gMinlen;
#line 3443
  s->save_gLimit = gLimit;
#line 3444
  s->save_gBase = gBase;
#line 3445
  s->save_gPerm = gPerm;
#line 3447
  return (retVal);
}
}
#line 3467 "bzip2.c"
void BZ2_bz__AssertH__fail(int errcode ) 
{ char const   *tmp ;

  {
#line 3469
  tmp = BZ2_bzlibVersion();
#line 3469
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"\n\nbzip2/libbzip2: internal error number %d.\nThis is a bug in bzip2/libbzip2, %s.\nPlease report it to me at: jseward@acm.org.  If this happened\nwhen you were using some program which uses libbzip2 as a\ncomponent, you should also report this bug to the author(s)\nof that program.  Please make an effort to report this bug;\ntimely and accurate bug reports eventually lead to higher\nquality software.  Thanks.  Julian Seward, 30 December 2001.\n\n",
          errcode, tmp);
#line 3482
  if (errcode == 1007) {
#line 3483
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n*** A special note about internal error number 1007 ***\n\nExperience suggests that a common cause of i.e. 1007\nis unreliable memory or other hardware.  The 1007 assertion\njust happens to cross-check the results of huge numbers of\nmemory reads/writes, and so acts (unintendedly) as a stress\ntest of your memory system.\n\nI suggest the following: try compressing the file again,\npossibly monitoring progress in detail with the -vv flag.\n\n* If the error cannot be reproduced, and/or happens at different\n  points in compression, you may have a flaky memory system.\n  Try a memory-test program.  I have used Memtest86\n  (www.memtest86.com).  At the time of writing it is free (GPLd).\n  Memtest86 tests memory much more thorougly than your BIOSs\n  power-on test, and may find failures that the BIOS doesn\'t.\n\n* If the error can be repeatably reproduced, this is a bug in\n  bzip2, and I would very much like to hear about it.  Please\n  let me know, and, ideally, save a copy of the file causing the\n  problem -- without which I will be unable to investigate it.\n\n");
  } else {

  }
#line 3510
  exit(3);
}
}
#line 3516 "bzip2.c"
static int bz_config_ok(void) 
{ 

  {
#line 3519
  if (sizeof(int ) != 4UL) {
#line 3519
    return (0);
  } else {

  }
#line 3520
  if (sizeof(short ) != 2UL) {
#line 3520
    return (0);
  } else {

  }
#line 3521
  if (sizeof(char ) != 1UL) {
#line 3521
    return (0);
  } else {

  }
#line 3522
  return (1);
}
}
#line 3527 "bzip2.c"
static void *default_bzalloc(void *opaque , Int32 items , Int32 size ) 
{ void *v ;
  void *tmp ;

  {
#line 3530
  tmp = malloc((unsigned long )(items * size));
#line 3530
  v = tmp;
#line 3531
  return (v);
}
}
#line 3534 "bzip2.c"
static void default_bzfree(void *opaque , void *addr ) 
{ 

  {
#line 3537
  if ((unsigned long )addr != (unsigned long )((void *)0)) {
#line 3537
    free(addr);
  } else {

  }
#line 3538
  return;
}
}
#line 3542 "bzip2.c"
static void prepare_new_block(EState *s ) 
{ Int32 i ;

  {
#line 3546
  s->nblock = 0;
#line 3547
  s->numZ = 0;
#line 3548
  s->state_out_pos = 0;
#line 3549
  s->blockCRC = 4294967295U;
#line 3550
  i = 0;
#line 3550
  while (i < 256) {
#line 3550
    s->inUse[i] = (unsigned char)0;
#line 3550
    i ++;
  }
#line 3551
  (s->blockNo) ++;
#line 3552
  return;
}
}
#line 3556 "bzip2.c"
static void init_RL(EState *s ) 
{ 

  {
#line 3559
  s->state_in_ch = 256U;
#line 3560
  s->state_in_len = 0;
#line 3561
  return;
}
}
#line 3564 "bzip2.c"
static Bool isempty_RL(EState *s ) 
{ 

  {
#line 3567
  if (s->state_in_ch < 256U) {
#line 3567
    if (s->state_in_len > 0) {
#line 3568
      return ((unsigned char)0);
    } else {
#line 3569
      return ((unsigned char)1);
    }
  } else {
#line 3569
    return ((unsigned char)1);
  }
}
}
#line 3574 "bzip2.c"
int BZ2_bzCompressInit(bz_stream *strm , int blockSize100k , int verbosity ,
                       int workFactor ) 
{ Int32 n ;
  EState *s ;
  int tmp ;
  void *tmp___0 ;
  void *tmp___1 ;
  void *tmp___2 ;
  void *tmp___3 ;

  {
#line 3583
  tmp = bz_config_ok();
#line 3583
  if (tmp) {

  } else {
#line 3583
    return (-9);
  }
#line 3585
  if ((unsigned long )strm == (unsigned long )((void *)0)) {
#line 3588
    return (-2);
  } else {
#line 3585
    if (blockSize100k < 1) {
#line 3588
      return (-2);
    } else {
#line 3585
      if (blockSize100k > 9) {
#line 3588
        return (-2);
      } else {
#line 3585
        if (workFactor < 0) {
#line 3588
          return (-2);
        } else {
#line 3585
          if (workFactor > 250) {
#line 3588
            return (-2);
          } else {

          }
        }
      }
    }
  }
#line 3590
  if (workFactor == 0) {
#line 3590
    workFactor = 30;
  } else {

  }
#line 3591
  if ((unsigned long )strm->bzalloc == (unsigned long )((void *)0)) {
#line 3591
    strm->bzalloc = & default_bzalloc;
  } else {

  }
#line 3592
  if ((unsigned long )strm->bzfree == (unsigned long )((void *)0)) {
#line 3592
    strm->bzfree = & default_bzfree;
  } else {

  }
#line 3594
  tmp___0 = (*(strm->bzalloc))(strm->opaque, (int )sizeof(EState ), 1);
#line 3594
  s = (EState *)tmp___0;
#line 3595
  if ((unsigned long )s == (unsigned long )((void *)0)) {
#line 3595
    return (-3);
  } else {

  }
#line 3596
  s->strm = strm;
#line 3598
  s->arr1 = (UInt32 *)((void *)0);
#line 3599
  s->arr2 = (UInt32 *)((void *)0);
#line 3600
  s->ftab = (UInt32 *)((void *)0);
#line 3602
  n = 100000 * blockSize100k;
#line 3603
  tmp___1 = (*(strm->bzalloc))(strm->opaque,
                               (int )((unsigned long )n * sizeof(UInt32 )), 1);
#line 3603
  s->arr1 = (UInt32 *)tmp___1;
#line 3604
  tmp___2 = (*(strm->bzalloc))(strm->opaque,
                               (int )((unsigned long )(n + 34) * sizeof(UInt32 )),
                               1);
#line 3604
  s->arr2 = (UInt32 *)tmp___2;
#line 3605
  tmp___3 = (*(strm->bzalloc))(strm->opaque, (int )(65537UL * sizeof(UInt32 )),
                               1);
#line 3605
  s->ftab = (UInt32 *)tmp___3;
#line 3607
  if ((unsigned long )s->arr1 == (unsigned long )((void *)0)) {
    goto _L;
  } else {
#line 3607
    if ((unsigned long )s->arr2 == (unsigned long )((void *)0)) {
      goto _L;
    } else {
#line 3607
      if ((unsigned long )s->ftab == (unsigned long )((void *)0)) {
        _L: /* CIL Label */ 
#line 3608
        if ((unsigned long )s->arr1 != (unsigned long )((void *)0)) {
#line 3608
          (*(strm->bzfree))(strm->opaque, (void *)s->arr1);
        } else {

        }
#line 3609
        if ((unsigned long )s->arr2 != (unsigned long )((void *)0)) {
#line 3609
          (*(strm->bzfree))(strm->opaque, (void *)s->arr2);
        } else {

        }
#line 3610
        if ((unsigned long )s->ftab != (unsigned long )((void *)0)) {
#line 3610
          (*(strm->bzfree))(strm->opaque, (void *)s->ftab);
        } else {

        }
#line 3611
        if ((unsigned long )s != (unsigned long )((void *)0)) {
#line 3611
          (*(strm->bzfree))(strm->opaque, (void *)s);
        } else {

        }
#line 3612
        return (-3);
      } else {

      }
    }
  }
#line 3615
  s->blockNo = 0;
#line 3616
  s->state = 2;
#line 3617
  s->mode = 2;
#line 3618
  s->combinedCRC = 0U;
#line 3619
  s->blockSize100k = blockSize100k;
#line 3620
  s->nblockMAX = 100000 * blockSize100k - 19;
#line 3621
  s->verbosity = verbosity;
#line 3622
  s->workFactor = workFactor;
#line 3624
  s->block = (UChar *)s->arr2;
#line 3625
  s->mtfv = (UInt16 *)s->arr1;
#line 3626
  s->zbits = (UChar *)((void *)0);
#line 3627
  s->ptr = s->arr1;
#line 3629
  strm->state = (void *)s;
#line 3630
  strm->total_in_lo32 = 0U;
#line 3631
  strm->total_in_hi32 = 0U;
#line 3632
  strm->total_out_lo32 = 0U;
#line 3633
  strm->total_out_hi32 = 0U;
#line 3634
  init_RL(s);
#line 3635
  prepare_new_block(s);
#line 3636
  return (0);
}
}
#line 3641 "bzip2.c"
static void add_pair_to_block(EState *s ) 
{ Int32 i ;
  UChar ch ;

  {
#line 3645
  ch = (unsigned char )s->state_in_ch;
#line 3646
  i = 0;
#line 3646
  while (i < s->state_in_len) {
#line 3647
    s->blockCRC = (s->blockCRC << 8) ^ BZ2_crc32Table[(s->blockCRC >> 24) ^ (unsigned int )ch];
#line 3646
    i ++;
  }
#line 3649
  s->inUse[s->state_in_ch] = (unsigned char)1;
#line 3650
  switch (s->state_in_len) {
  case 1: 
#line 3652
  *(s->block + s->nblock) = ch;
#line 3652
  (s->nblock) ++;
#line 3653
  break;
  case 2: 
#line 3655
  *(s->block + s->nblock) = ch;
#line 3655
  (s->nblock) ++;
#line 3656
  *(s->block + s->nblock) = ch;
#line 3656
  (s->nblock) ++;
#line 3657
  break;
  case 3: 
#line 3659
  *(s->block + s->nblock) = ch;
#line 3659
  (s->nblock) ++;
#line 3660
  *(s->block + s->nblock) = ch;
#line 3660
  (s->nblock) ++;
#line 3661
  *(s->block + s->nblock) = ch;
#line 3661
  (s->nblock) ++;
#line 3662
  break;
  default: 
#line 3664
  s->inUse[s->state_in_len - 4] = (unsigned char)1;
#line 3665
  *(s->block + s->nblock) = ch;
#line 3665
  (s->nblock) ++;
#line 3666
  *(s->block + s->nblock) = ch;
#line 3666
  (s->nblock) ++;
#line 3667
  *(s->block + s->nblock) = ch;
#line 3667
  (s->nblock) ++;
#line 3668
  *(s->block + s->nblock) = ch;
#line 3668
  (s->nblock) ++;
#line 3669
  *(s->block + s->nblock) = (unsigned char )(s->state_in_len - 4);
#line 3670
  (s->nblock) ++;
#line 3671
  break;
  }
#line 3673
  return;
}
}
#line 3677 "bzip2.c"
static void flush_RL(EState *s ) 
{ 

  {
#line 3680
  if (s->state_in_ch < 256U) {
#line 3680
    add_pair_to_block(s);
  } else {

  }
#line 3681
  init_RL(s);
#line 3682
  return;
}
}
#line 3714 "bzip2.c"
static Bool copy_input_until_stop(EState *s ) 
{ Bool progress_in ;
  UInt32 zchh ;
  UChar ch ;
  UInt32 zchh___0 ;
  UChar ch___0 ;

  {
#line 3717
  progress_in = (unsigned char)0;
#line 3719
  if (s->mode == 2) {
#line 3722
    while (! (s->nblock >= s->nblockMAX)) {
#line 3726
      if ((s->strm)->avail_in == 0U) {
#line 3726
        break;
      } else {

      }
#line 3727
      progress_in = (unsigned char)1;
#line 3728
      zchh = (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3728
      if (zchh != s->state_in_ch) {
#line 3728
        if (s->state_in_len == 1) {
#line 3728
          ch = (unsigned char )s->state_in_ch;
#line 3728
          s->blockCRC = (s->blockCRC << 8) ^ BZ2_crc32Table[(s->blockCRC >> 24) ^ (unsigned int )ch];
#line 3728
          s->inUse[s->state_in_ch] = (unsigned char)1;
#line 3728
          *(s->block + s->nblock) = ch;
#line 3728
          (s->nblock) ++;
#line 3728
          s->state_in_ch = zchh;
        } else {
          goto _L___0;
        }
      } else {
        _L___0: /* CIL Label */ 
#line 3728
        if (zchh != s->state_in_ch) {
          goto _L;
        } else {
#line 3728
          if (s->state_in_len == 255) {
            _L: /* CIL Label */ 
#line 3728
            if (s->state_in_ch < 256U) {
#line 3728
              add_pair_to_block(s);
            } else {

            }
#line 3728
            s->state_in_ch = zchh;
#line 3728
            s->state_in_len = 1;
          } else {
#line 3728
            (s->state_in_len) ++;
          }
        }
      }
#line 3729
      ((s->strm)->next_in) ++;
#line 3730
      ((s->strm)->avail_in) --;
#line 3731
      ((s->strm)->total_in_lo32) ++;
#line 3732
      if ((s->strm)->total_in_lo32 == 0U) {
#line 3732
        ((s->strm)->total_in_hi32) ++;
      } else {

      }
    }
  } else {
#line 3738
    while (! (s->nblock >= s->nblockMAX)) {
#line 3742
      if ((s->strm)->avail_in == 0U) {
#line 3742
        break;
      } else {

      }
#line 3744
      if (s->avail_in_expect == 0U) {
#line 3744
        break;
      } else {

      }
#line 3745
      progress_in = (unsigned char)1;
#line 3746
      zchh___0 = (unsigned int )*((UChar *)(s->strm)->next_in);
#line 3746
      if (zchh___0 != s->state_in_ch) {
#line 3746
        if (s->state_in_len == 1) {
#line 3746
          ch___0 = (unsigned char )s->state_in_ch;
#line 3746
          s->blockCRC = (s->blockCRC << 8) ^ BZ2_crc32Table[(s->blockCRC >> 24) ^ (unsigned int )ch___0];
#line 3746
          s->inUse[s->state_in_ch] = (unsigned char)1;
#line 3746
          *(s->block + s->nblock) = ch___0;
#line 3746
          (s->nblock) ++;
#line 3746
          s->state_in_ch = zchh___0;
        } else {
          goto _L___2;
        }
      } else {
        _L___2: /* CIL Label */ 
#line 3746
        if (zchh___0 != s->state_in_ch) {
          goto _L___1;
        } else {
#line 3746
          if (s->state_in_len == 255) {
            _L___1: /* CIL Label */ 
#line 3746
            if (s->state_in_ch < 256U) {
#line 3746
              add_pair_to_block(s);
            } else {

            }
#line 3746
            s->state_in_ch = zchh___0;
#line 3746
            s->state_in_len = 1;
          } else {
#line 3746
            (s->state_in_len) ++;
          }
        }
      }
#line 3747
      ((s->strm)->next_in) ++;
#line 3748
      ((s->strm)->avail_in) --;
#line 3749
      ((s->strm)->total_in_lo32) ++;
#line 3750
      if ((s->strm)->total_in_lo32 == 0U) {
#line 3750
        ((s->strm)->total_in_hi32) ++;
      } else {

      }
#line 3751
      (s->avail_in_expect) --;
    }
  }
#line 3754
  return (progress_in);
}
}
#line 3759 "bzip2.c"
static Bool copy_output_until_stop(EState *s ) 
{ Bool progress_out ;

  {
#line 3762
  progress_out = (unsigned char)0;
#line 3764
  while (! ((s->strm)->avail_out == 0U)) {
#line 3770
    if (s->state_out_pos >= s->numZ) {
#line 3770
      break;
    } else {

    }
#line 3772
    progress_out = (unsigned char)1;
#line 3773
    *((s->strm)->next_out) = (char )*(s->zbits + s->state_out_pos);
#line 3774
    (s->state_out_pos) ++;
#line 3775
    ((s->strm)->avail_out) --;
#line 3776
    ((s->strm)->next_out) ++;
#line 3777
    ((s->strm)->total_out_lo32) ++;
#line 3778
    if ((s->strm)->total_out_lo32 == 0U) {
#line 3778
      ((s->strm)->total_out_hi32) ++;
    } else {

    }
  }
#line 3781
  return (progress_out);
}
}
#line 3786 "bzip2.c"
static Bool handle_compress(bz_stream *strm ) 
{ Bool progress_in ;
  Bool progress_out ;
  EState *s ;
  Bool tmp ;
  Bool tmp___0 ;
  Bool tmp___1 ;
  Bool tmp___2 ;
  int tmp___3 ;

  {
#line 3789
  progress_in = (unsigned char)0;
#line 3790
  progress_out = (unsigned char)0;
#line 3791
  s = (EState *)strm->state;
#line 3793
  while (1) {
#line 3795
    if (s->state == 1) {
#line 3796
      tmp = copy_output_until_stop(s);
#line 3796
      progress_out = (unsigned char )((int )progress_out | (int )tmp);
#line 3797
      if (s->state_out_pos < s->numZ) {
#line 3797
        break;
      } else {

      }
#line 3798
      if (s->mode == 4) {
#line 3798
        if (s->avail_in_expect == 0U) {
#line 3798
          tmp___0 = isempty_RL(s);
#line 3798
          if (tmp___0) {
#line 3800
            break;
          } else {

          }
        } else {

        }
      } else {

      }
#line 3801
      prepare_new_block(s);
#line 3802
      s->state = 2;
#line 3803
      if (s->mode == 3) {
#line 3803
        if (s->avail_in_expect == 0U) {
#line 3803
          tmp___1 = isempty_RL(s);
#line 3803
          if (tmp___1) {
#line 3805
            break;
          } else {

          }
        } else {

        }
      } else {

      }
    } else {

    }
#line 3808
    if (s->state == 2) {
#line 3809
      tmp___2 = copy_input_until_stop(s);
#line 3809
      progress_in = (unsigned char )((int )progress_in | (int )tmp___2);
#line 3810
      if (s->mode != 2) {
#line 3810
        if (s->avail_in_expect == 0U) {
#line 3811
          flush_RL(s);
#line 3812
          BZ2_compressBlock(s, (unsigned char )(s->mode == 4));
#line 3813
          s->state = 1;
        } else {
          goto _L;
        }
      } else {
        _L: /* CIL Label */ 
#line 3816
        if (s->nblock >= s->nblockMAX) {
#line 3817
          BZ2_compressBlock(s, (unsigned char)0);
#line 3818
          s->state = 1;
        } else {
#line 3821
          if ((s->strm)->avail_in == 0U) {
#line 3822
            break;
          } else {

          }
        }
      }
    } else {

    }
  }
#line 3828
  if (progress_in) {
#line 3828
    tmp___3 = 1;
  } else {
#line 3828
    if (progress_out) {
#line 3828
      tmp___3 = 1;
    } else {
#line 3828
      tmp___3 = 0;
    }
  }
#line 3828
  return ((unsigned char )tmp___3);
}
}
#line 3833 "bzip2.c"
int BZ2_bzCompress(bz_stream *strm , int action ) 
{ Bool progress ;
  EState *s ;
  int tmp ;
  Bool tmp___0 ;
  Bool tmp___1 ;

  {
#line 3837
  if ((unsigned long )strm == (unsigned long )((void *)0)) {
#line 3837
    return (-2);
  } else {

  }
#line 3838
  s = (EState *)strm->state;
#line 3839
  if ((unsigned long )s == (unsigned long )((void *)0)) {
#line 3839
    return (-2);
  } else {

  }
#line 3840
  if ((unsigned long )s->strm != (unsigned long )strm) {
#line 3840
    return (-2);
  } else {

  }
  preswitch: 
#line 3843
  switch (s->mode) {
  case 1: 
#line 3846
  return (-1);
  case 2: 
#line 3849
  if (action == 0) {
#line 3850
    progress = handle_compress(strm);
#line 3851
    if (progress) {
#line 3851
      tmp = 1;
    } else {
#line 3851
      tmp = -2;
    }
#line 3851
    return (tmp);
  } else {
#line 3854
    if (action == 1) {
#line 3855
      s->avail_in_expect = strm->avail_in;
#line 3856
      s->mode = 3;
      goto preswitch;
    } else {
#line 3860
      if (action == 2) {
#line 3861
        s->avail_in_expect = strm->avail_in;
#line 3862
        s->mode = 4;
        goto preswitch;
      } else {
#line 3866
        return (-2);
      }
    }
  }
  case 3: 
#line 3869
  if (action != 1) {
#line 3869
    return (-1);
  } else {

  }
#line 3870
  if (s->avail_in_expect != (s->strm)->avail_in) {
#line 3871
    return (-1);
  } else {

  }
#line 3872
  progress = handle_compress(strm);
#line 3873
  if (s->avail_in_expect > 0U) {
#line 3874
    return (2);
  } else {
#line 3873
    tmp___0 = isempty_RL(s);
#line 3873
    if (tmp___0) {
#line 3873
      if (s->state_out_pos < s->numZ) {
#line 3874
        return (2);
      } else {

      }
    } else {
#line 3874
      return (2);
    }
  }
#line 3875
  s->mode = 2;
#line 3876
  return (1);
  case 4: 
#line 3879
  if (action != 2) {
#line 3879
    return (-1);
  } else {

  }
#line 3880
  if (s->avail_in_expect != (s->strm)->avail_in) {
#line 3881
    return (-1);
  } else {

  }
#line 3882
  progress = handle_compress(strm);
#line 3883
  if (! progress) {
#line 3883
    return (-1);
  } else {

  }
#line 3884
  if (s->avail_in_expect > 0U) {
#line 3885
    return (3);
  } else {
#line 3884
    tmp___1 = isempty_RL(s);
#line 3884
    if (tmp___1) {
#line 3884
      if (s->state_out_pos < s->numZ) {
#line 3885
        return (3);
      } else {

      }
    } else {
#line 3885
      return (3);
    }
  }
#line 3886
  s->mode = 1;
#line 3887
  return (4);
  }
#line 3889
  return (0);
}
}
#line 3894 "bzip2.c"
int BZ2_bzCompressEnd(bz_stream *strm ) 
{ EState *s ;

  {
#line 3897
  if ((unsigned long )strm == (unsigned long )((void *)0)) {
#line 3897
    return (-2);
  } else {

  }
#line 3898
  s = (EState *)strm->state;
#line 3899
  if ((unsigned long )s == (unsigned long )((void *)0)) {
#line 3899
    return (-2);
  } else {

  }
#line 3900
  if ((unsigned long )s->strm != (unsigned long )strm) {
#line 3900
    return (-2);
  } else {

  }
#line 3902
  if ((unsigned long )s->arr1 != (unsigned long )((void *)0)) {
#line 3902
    (*(strm->bzfree))(strm->opaque, (void *)s->arr1);
  } else {

  }
#line 3903
  if ((unsigned long )s->arr2 != (unsigned long )((void *)0)) {
#line 3903
    (*(strm->bzfree))(strm->opaque, (void *)s->arr2);
  } else {

  }
#line 3904
  if ((unsigned long )s->ftab != (unsigned long )((void *)0)) {
#line 3904
    (*(strm->bzfree))(strm->opaque, (void *)s->ftab);
  } else {

  }
#line 3905
  (*(strm->bzfree))(strm->opaque, strm->state);
#line 3907
  strm->state = (void *)0;
#line 3909
  return (0);
}
}
#line 3918 "bzip2.c"
int BZ2_bzDecompressInit(bz_stream *strm , int verbosity , int small ) 
{ DState *s ;
  int tmp ;
  void *tmp___0 ;

  {
#line 3925
  tmp = bz_config_ok();
#line 3925
  if (tmp) {

  } else {
#line 3925
    return (-9);
  }
#line 3927
  if ((unsigned long )strm == (unsigned long )((void *)0)) {
#line 3927
    return (-2);
  } else {

  }
#line 3928
  if (small != 0) {
#line 3928
    if (small != 1) {
#line 3928
      return (-2);
    } else {

    }
  } else {

  }
#line 3929
  if (verbosity < 0) {
#line 3929
    return (-2);
  } else {
#line 3929
    if (verbosity > 4) {
#line 3929
      return (-2);
    } else {

    }
  }
#line 3931
  if ((unsigned long )strm->bzalloc == (unsigned long )((void *)0)) {
#line 3931
    strm->bzalloc = & default_bzalloc;
  } else {

  }
#line 3932
  if ((unsigned long )strm->bzfree == (unsigned long )((void *)0)) {
#line 3932
    strm->bzfree = & default_bzfree;
  } else {

  }
#line 3934
  tmp___0 = (*(strm->bzalloc))(strm->opaque, (int )sizeof(DState ), 1);
#line 3934
  s = (DState *)tmp___0;
#line 3935
  if ((unsigned long )s == (unsigned long )((void *)0)) {
#line 3935
    return (-3);
  } else {

  }
#line 3936
  s->strm = strm;
#line 3937
  strm->state = (void *)s;
#line 3938
  s->state = 10;
#line 3939
  s->bsLive = 0;
#line 3940
  s->bsBuff = 0U;
#line 3941
  s->calculatedCombinedCRC = 0U;
#line 3942
  strm->total_in_lo32 = 0U;
#line 3943
  strm->total_in_hi32 = 0U;
#line 3944
  strm->total_out_lo32 = 0U;
#line 3945
  strm->total_out_hi32 = 0U;
#line 3946
  s->smallDecompress = (unsigned char )small;
#line 3947
  s->ll4 = (UChar *)((void *)0);
#line 3948
  s->ll16 = (UInt16 *)((void *)0);
#line 3949
  s->tt = (UInt32 *)((void *)0);
#line 3950
  s->currBlockNo = 0;
#line 3951
  s->verbosity = verbosity;
#line 3953
  return (0);
}
}
#line 3958 "bzip2.c"
static void unRLE_obuf_to_output_FAST(DState *s ) 
{ UChar k1 ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  UInt32 c_calculatedBlockCRC ;
  UChar c_state_out_ch ;
  Int32 c_state_out_len ;
  Int32 c_nblock_used ;
  Int32 c_k0 ;
  UInt32 *c_tt ;
  UInt32 c_tPos ;
  char *cs_next_out ;
  unsigned int cs_avail_out ;
  UInt32 avail_out_INIT ;
  Int32 s_save_nblockPP ;
  unsigned int total_out_lo32_old ;

  {
#line 3963
  if (s->blockRandomised) {
#line 3965
    while (1) {
#line 3967
      while (1) {
#line 3968
        if ((s->strm)->avail_out == 0U) {
#line 3968
          return;
        } else {

        }
#line 3969
        if (s->state_out_len == 0) {
#line 3969
          break;
        } else {

        }
#line 3970
        *((UChar *)(s->strm)->next_out) = s->state_out_ch;
#line 3971
        s->calculatedBlockCRC = (s->calculatedBlockCRC << 8) ^ BZ2_crc32Table[(s->calculatedBlockCRC >> 24) ^ (unsigned int )s->state_out_ch];
#line 3972
        (s->state_out_len) --;
#line 3973
        ((s->strm)->next_out) ++;
#line 3974
        ((s->strm)->avail_out) --;
#line 3975
        ((s->strm)->total_out_lo32) ++;
#line 3976
        if ((s->strm)->total_out_lo32 == 0U) {
#line 3976
          ((s->strm)->total_out_hi32) ++;
        } else {

        }
      }
#line 3980
      if (s->nblock_used == s->save_nblock + 1) {
#line 3980
        return;
      } else {

      }
#line 3983
      s->state_out_len = 1;
#line 3984
      s->state_out_ch = (unsigned char )s->k0;
#line 3985
      s->tPos = *(s->tt + s->tPos);
#line 3985
      k1 = (unsigned char )(s->tPos & 255U);
#line 3985
      s->tPos >>= 8;
#line 3985
      if (s->rNToGo == 0) {
#line 3985
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 3985
        (s->rTPos) ++;
#line 3985
        if (s->rTPos == 512) {
#line 3985
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 3985
      (s->rNToGo) --;
#line 3986
      if (s->rNToGo == 1) {
#line 3986
        tmp = 1;
      } else {
#line 3986
        tmp = 0;
      }
#line 3986
      k1 = (unsigned char )((int )k1 ^ tmp);
#line 3986
      (s->nblock_used) ++;
#line 3987
      if (s->nblock_used == s->save_nblock + 1) {
#line 3987
        continue;
      } else {

      }
#line 3988
      if ((int )k1 != s->k0) {
#line 3988
        s->k0 = (int )k1;
#line 3988
        continue;
      } else {

      }
#line 3990
      s->state_out_len = 2;
#line 3991
      s->tPos = *(s->tt + s->tPos);
#line 3991
      k1 = (unsigned char )(s->tPos & 255U);
#line 3991
      s->tPos >>= 8;
#line 3991
      if (s->rNToGo == 0) {
#line 3991
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 3991
        (s->rTPos) ++;
#line 3991
        if (s->rTPos == 512) {
#line 3991
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 3991
      (s->rNToGo) --;
#line 3992
      if (s->rNToGo == 1) {
#line 3992
        tmp___0 = 1;
      } else {
#line 3992
        tmp___0 = 0;
      }
#line 3992
      k1 = (unsigned char )((int )k1 ^ tmp___0);
#line 3992
      (s->nblock_used) ++;
#line 3993
      if (s->nblock_used == s->save_nblock + 1) {
#line 3993
        continue;
      } else {

      }
#line 3994
      if ((int )k1 != s->k0) {
#line 3994
        s->k0 = (int )k1;
#line 3994
        continue;
      } else {

      }
#line 3996
      s->state_out_len = 3;
#line 3997
      s->tPos = *(s->tt + s->tPos);
#line 3997
      k1 = (unsigned char )(s->tPos & 255U);
#line 3997
      s->tPos >>= 8;
#line 3997
      if (s->rNToGo == 0) {
#line 3997
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 3997
        (s->rTPos) ++;
#line 3997
        if (s->rTPos == 512) {
#line 3997
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 3997
      (s->rNToGo) --;
#line 3998
      if (s->rNToGo == 1) {
#line 3998
        tmp___1 = 1;
      } else {
#line 3998
        tmp___1 = 0;
      }
#line 3998
      k1 = (unsigned char )((int )k1 ^ tmp___1);
#line 3998
      (s->nblock_used) ++;
#line 3999
      if (s->nblock_used == s->save_nblock + 1) {
#line 3999
        continue;
      } else {

      }
#line 4000
      if ((int )k1 != s->k0) {
#line 4000
        s->k0 = (int )k1;
#line 4000
        continue;
      } else {

      }
#line 4002
      s->tPos = *(s->tt + s->tPos);
#line 4002
      k1 = (unsigned char )(s->tPos & 255U);
#line 4002
      s->tPos >>= 8;
#line 4002
      if (s->rNToGo == 0) {
#line 4002
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 4002
        (s->rTPos) ++;
#line 4002
        if (s->rTPos == 512) {
#line 4002
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 4002
      (s->rNToGo) --;
#line 4003
      if (s->rNToGo == 1) {
#line 4003
        tmp___2 = 1;
      } else {
#line 4003
        tmp___2 = 0;
      }
#line 4003
      k1 = (unsigned char )((int )k1 ^ tmp___2);
#line 4003
      (s->nblock_used) ++;
#line 4004
      s->state_out_len = (int )k1 + 4;
#line 4005
      s->tPos = *(s->tt + s->tPos);
#line 4005
      s->k0 = (int )((unsigned char )(s->tPos & 255U));
#line 4005
      s->tPos >>= 8;
#line 4005
      if (s->rNToGo == 0) {
#line 4005
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 4005
        (s->rTPos) ++;
#line 4005
        if (s->rTPos == 512) {
#line 4005
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 4005
      (s->rNToGo) --;
#line 4006
      if (s->rNToGo == 1) {
#line 4006
        tmp___3 = 1;
      } else {
#line 4006
        tmp___3 = 0;
      }
#line 4006
      s->k0 ^= tmp___3;
#line 4006
      (s->nblock_used) ++;
    }
  } else {
#line 4012
    c_calculatedBlockCRC = s->calculatedBlockCRC;
#line 4013
    c_state_out_ch = s->state_out_ch;
#line 4014
    c_state_out_len = s->state_out_len;
#line 4015
    c_nblock_used = s->nblock_used;
#line 4016
    c_k0 = s->k0;
#line 4017
    c_tt = s->tt;
#line 4018
    c_tPos = s->tPos;
#line 4019
    cs_next_out = (s->strm)->next_out;
#line 4020
    cs_avail_out = (s->strm)->avail_out;
#line 4023
    avail_out_INIT = cs_avail_out;
#line 4024
    s_save_nblockPP = s->save_nblock + 1;
#line 4027
    while (1) {
#line 4030
      if (c_state_out_len > 0) {
#line 4031
        while (1) {
#line 4032
          if (cs_avail_out == 0U) {
            goto return_notr;
          } else {

          }
#line 4033
          if (c_state_out_len == 1) {
#line 4033
            break;
          } else {

          }
#line 4034
          *((UChar *)cs_next_out) = c_state_out_ch;
#line 4035
          c_calculatedBlockCRC = (c_calculatedBlockCRC << 8) ^ BZ2_crc32Table[(c_calculatedBlockCRC >> 24) ^ (unsigned int )c_state_out_ch];
#line 4036
          c_state_out_len --;
#line 4037
          cs_next_out ++;
#line 4038
          cs_avail_out --;
        }
        s_state_out_len_eq_one: 
#line 4042
        if (cs_avail_out == 0U) {
#line 4043
          c_state_out_len = 1;
          goto return_notr;
        } else {

        }
#line 4045
        *((UChar *)cs_next_out) = c_state_out_ch;
#line 4046
        c_calculatedBlockCRC = (c_calculatedBlockCRC << 8) ^ BZ2_crc32Table[(c_calculatedBlockCRC >> 24) ^ (unsigned int )c_state_out_ch];
#line 4047
        cs_next_out ++;
#line 4048
        cs_avail_out --;
      } else {

      }
#line 4052
      if (c_nblock_used == s_save_nblockPP) {
#line 4053
        c_state_out_len = 0;
        goto return_notr;
      } else {

      }
#line 4055
      c_state_out_ch = (unsigned char )c_k0;
#line 4056
      c_tPos = *(c_tt + c_tPos);
#line 4056
      k1 = (unsigned char )(c_tPos & 255U);
#line 4056
      c_tPos >>= 8;
#line 4056
      c_nblock_used ++;
#line 4057
      if ((int )k1 != c_k0) {
#line 4058
        c_k0 = (int )k1;
        goto s_state_out_len_eq_one;
      } else {

      }
#line 4060
      if (c_nblock_used == s_save_nblockPP) {
        goto s_state_out_len_eq_one;
      } else {

      }
#line 4063
      c_state_out_len = 2;
#line 4064
      c_tPos = *(c_tt + c_tPos);
#line 4064
      k1 = (unsigned char )(c_tPos & 255U);
#line 4064
      c_tPos >>= 8;
#line 4064
      c_nblock_used ++;
#line 4065
      if (c_nblock_used == s_save_nblockPP) {
#line 4065
        continue;
      } else {

      }
#line 4066
      if ((int )k1 != c_k0) {
#line 4066
        c_k0 = (int )k1;
#line 4066
        continue;
      } else {

      }
#line 4068
      c_state_out_len = 3;
#line 4069
      c_tPos = *(c_tt + c_tPos);
#line 4069
      k1 = (unsigned char )(c_tPos & 255U);
#line 4069
      c_tPos >>= 8;
#line 4069
      c_nblock_used ++;
#line 4070
      if (c_nblock_used == s_save_nblockPP) {
#line 4070
        continue;
      } else {

      }
#line 4071
      if ((int )k1 != c_k0) {
#line 4071
        c_k0 = (int )k1;
#line 4071
        continue;
      } else {

      }
#line 4073
      c_tPos = *(c_tt + c_tPos);
#line 4073
      k1 = (unsigned char )(c_tPos & 255U);
#line 4073
      c_tPos >>= 8;
#line 4073
      c_nblock_used ++;
#line 4074
      c_state_out_len = (int )k1 + 4;
#line 4075
      c_tPos = *(c_tt + c_tPos);
#line 4075
      c_k0 = (int )((unsigned char )(c_tPos & 255U));
#line 4075
      c_tPos >>= 8;
#line 4075
      c_nblock_used ++;
    }
    return_notr: 
#line 4079
    total_out_lo32_old = (s->strm)->total_out_lo32;
#line 4080
    (s->strm)->total_out_lo32 += avail_out_INIT - cs_avail_out;
#line 4081
    if ((s->strm)->total_out_lo32 < total_out_lo32_old) {
#line 4082
      ((s->strm)->total_out_hi32) ++;
    } else {

    }
#line 4085
    s->calculatedBlockCRC = c_calculatedBlockCRC;
#line 4086
    s->state_out_ch = c_state_out_ch;
#line 4087
    s->state_out_len = c_state_out_len;
#line 4088
    s->nblock_used = c_nblock_used;
#line 4089
    s->k0 = c_k0;
#line 4090
    s->tt = c_tt;
#line 4091
    s->tPos = c_tPos;
#line 4092
    (s->strm)->next_out = cs_next_out;
#line 4093
    (s->strm)->avail_out = cs_avail_out;
  }
#line 4096
  return;
}
}
#line 4101 "bzip2.c"
Int32 BZ2_indexIntoF(Int32 indx , Int32 *cftab ) 
{ Int32 nb ;
  Int32 na ;
  Int32 mid ;

  {
#line 4104
  nb = 0;
#line 4105
  na = 256;
#line 4106
  while (1) {
#line 4107
    mid = (nb + na) >> 1;
#line 4108
    if (indx >= *(cftab + mid)) {
#line 4108
      nb = mid;
    } else {
#line 4108
      na = mid;
    }
#line 4106
    if (na - nb != 1) {

    } else {
#line 4106
      break;
    }
  }
#line 4111
  return (nb);
}
}
#line 4116 "bzip2.c"
static void unRLE_obuf_to_output_SMALL(DState *s ) 
{ UChar k1 ;
  Int32 tmp ;
  int tmp___0 ;
  Int32 tmp___1 ;
  int tmp___2 ;
  Int32 tmp___3 ;
  int tmp___4 ;
  Int32 tmp___5 ;
  int tmp___6 ;
  int tmp___7 ;
  Int32 tmp___8 ;
  Int32 tmp___9 ;
  Int32 tmp___10 ;
  Int32 tmp___11 ;

  {
#line 4121
  if (s->blockRandomised) {
#line 4123
    while (1) {
#line 4125
      while (1) {
#line 4126
        if ((s->strm)->avail_out == 0U) {
#line 4126
          return;
        } else {

        }
#line 4127
        if (s->state_out_len == 0) {
#line 4127
          break;
        } else {

        }
#line 4128
        *((UChar *)(s->strm)->next_out) = s->state_out_ch;
#line 4129
        s->calculatedBlockCRC = (s->calculatedBlockCRC << 8) ^ BZ2_crc32Table[(s->calculatedBlockCRC >> 24) ^ (unsigned int )s->state_out_ch];
#line 4130
        (s->state_out_len) --;
#line 4131
        ((s->strm)->next_out) ++;
#line 4132
        ((s->strm)->avail_out) --;
#line 4133
        ((s->strm)->total_out_lo32) ++;
#line 4134
        if ((s->strm)->total_out_lo32 == 0U) {
#line 4134
          ((s->strm)->total_out_hi32) ++;
        } else {

        }
      }
#line 4138
      if (s->nblock_used == s->save_nblock + 1) {
#line 4138
        return;
      } else {

      }
#line 4141
      s->state_out_len = 1;
#line 4142
      s->state_out_ch = (unsigned char )s->k0;
#line 4143
      tmp = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4143
      k1 = (unsigned char )tmp;
#line 4143
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4143
      if (s->rNToGo == 0) {
#line 4143
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 4143
        (s->rTPos) ++;
#line 4143
        if (s->rTPos == 512) {
#line 4143
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 4143
      (s->rNToGo) --;
#line 4144
      if (s->rNToGo == 1) {
#line 4144
        tmp___0 = 1;
      } else {
#line 4144
        tmp___0 = 0;
      }
#line 4144
      k1 = (unsigned char )((int )k1 ^ tmp___0);
#line 4144
      (s->nblock_used) ++;
#line 4145
      if (s->nblock_used == s->save_nblock + 1) {
#line 4145
        continue;
      } else {

      }
#line 4146
      if ((int )k1 != s->k0) {
#line 4146
        s->k0 = (int )k1;
#line 4146
        continue;
      } else {

      }
#line 4148
      s->state_out_len = 2;
#line 4149
      tmp___1 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4149
      k1 = (unsigned char )tmp___1;
#line 4149
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4149
      if (s->rNToGo == 0) {
#line 4149
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 4149
        (s->rTPos) ++;
#line 4149
        if (s->rTPos == 512) {
#line 4149
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 4149
      (s->rNToGo) --;
#line 4150
      if (s->rNToGo == 1) {
#line 4150
        tmp___2 = 1;
      } else {
#line 4150
        tmp___2 = 0;
      }
#line 4150
      k1 = (unsigned char )((int )k1 ^ tmp___2);
#line 4150
      (s->nblock_used) ++;
#line 4151
      if (s->nblock_used == s->save_nblock + 1) {
#line 4151
        continue;
      } else {

      }
#line 4152
      if ((int )k1 != s->k0) {
#line 4152
        s->k0 = (int )k1;
#line 4152
        continue;
      } else {

      }
#line 4154
      s->state_out_len = 3;
#line 4155
      tmp___3 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4155
      k1 = (unsigned char )tmp___3;
#line 4155
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4155
      if (s->rNToGo == 0) {
#line 4155
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 4155
        (s->rTPos) ++;
#line 4155
        if (s->rTPos == 512) {
#line 4155
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 4155
      (s->rNToGo) --;
#line 4156
      if (s->rNToGo == 1) {
#line 4156
        tmp___4 = 1;
      } else {
#line 4156
        tmp___4 = 0;
      }
#line 4156
      k1 = (unsigned char )((int )k1 ^ tmp___4);
#line 4156
      (s->nblock_used) ++;
#line 4157
      if (s->nblock_used == s->save_nblock + 1) {
#line 4157
        continue;
      } else {

      }
#line 4158
      if ((int )k1 != s->k0) {
#line 4158
        s->k0 = (int )k1;
#line 4158
        continue;
      } else {

      }
#line 4160
      tmp___5 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4160
      k1 = (unsigned char )tmp___5;
#line 4160
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4160
      if (s->rNToGo == 0) {
#line 4160
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 4160
        (s->rTPos) ++;
#line 4160
        if (s->rTPos == 512) {
#line 4160
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 4160
      (s->rNToGo) --;
#line 4161
      if (s->rNToGo == 1) {
#line 4161
        tmp___6 = 1;
      } else {
#line 4161
        tmp___6 = 0;
      }
#line 4161
      k1 = (unsigned char )((int )k1 ^ tmp___6);
#line 4161
      (s->nblock_used) ++;
#line 4162
      s->state_out_len = (int )k1 + 4;
#line 4163
      s->k0 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4163
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4163
      if (s->rNToGo == 0) {
#line 4163
        s->rNToGo = BZ2_rNums[s->rTPos];
#line 4163
        (s->rTPos) ++;
#line 4163
        if (s->rTPos == 512) {
#line 4163
          s->rTPos = 0;
        } else {

        }
      } else {

      }
#line 4163
      (s->rNToGo) --;
#line 4164
      if (s->rNToGo == 1) {
#line 4164
        tmp___7 = 1;
      } else {
#line 4164
        tmp___7 = 0;
      }
#line 4164
      s->k0 ^= tmp___7;
#line 4164
      (s->nblock_used) ++;
    }
  } else {
#line 4169
    while (1) {
#line 4171
      while (1) {
#line 4172
        if ((s->strm)->avail_out == 0U) {
#line 4172
          return;
        } else {

        }
#line 4173
        if (s->state_out_len == 0) {
#line 4173
          break;
        } else {

        }
#line 4174
        *((UChar *)(s->strm)->next_out) = s->state_out_ch;
#line 4175
        s->calculatedBlockCRC = (s->calculatedBlockCRC << 8) ^ BZ2_crc32Table[(s->calculatedBlockCRC >> 24) ^ (unsigned int )s->state_out_ch];
#line 4176
        (s->state_out_len) --;
#line 4177
        ((s->strm)->next_out) ++;
#line 4178
        ((s->strm)->avail_out) --;
#line 4179
        ((s->strm)->total_out_lo32) ++;
#line 4180
        if ((s->strm)->total_out_lo32 == 0U) {
#line 4180
          ((s->strm)->total_out_hi32) ++;
        } else {

        }
      }
#line 4184
      if (s->nblock_used == s->save_nblock + 1) {
#line 4184
        return;
      } else {

      }
#line 4186
      s->state_out_len = 1;
#line 4187
      s->state_out_ch = (unsigned char )s->k0;
#line 4188
      tmp___8 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4188
      k1 = (unsigned char )tmp___8;
#line 4188
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4188
      (s->nblock_used) ++;
#line 4189
      if (s->nblock_used == s->save_nblock + 1) {
#line 4189
        continue;
      } else {

      }
#line 4190
      if ((int )k1 != s->k0) {
#line 4190
        s->k0 = (int )k1;
#line 4190
        continue;
      } else {

      }
#line 4192
      s->state_out_len = 2;
#line 4193
      tmp___9 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4193
      k1 = (unsigned char )tmp___9;
#line 4193
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4193
      (s->nblock_used) ++;
#line 4194
      if (s->nblock_used == s->save_nblock + 1) {
#line 4194
        continue;
      } else {

      }
#line 4195
      if ((int )k1 != s->k0) {
#line 4195
        s->k0 = (int )k1;
#line 4195
        continue;
      } else {

      }
#line 4197
      s->state_out_len = 3;
#line 4198
      tmp___10 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4198
      k1 = (unsigned char )tmp___10;
#line 4198
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4198
      (s->nblock_used) ++;
#line 4199
      if (s->nblock_used == s->save_nblock + 1) {
#line 4199
        continue;
      } else {

      }
#line 4200
      if ((int )k1 != s->k0) {
#line 4200
        s->k0 = (int )k1;
#line 4200
        continue;
      } else {

      }
#line 4202
      tmp___11 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4202
      k1 = (unsigned char )tmp___11;
#line 4202
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4202
      (s->nblock_used) ++;
#line 4203
      s->state_out_len = (int )k1 + 4;
#line 4204
      s->k0 = BZ2_indexIntoF((int )s->tPos, s->cftab);
#line 4204
      s->tPos = (unsigned int )*(s->ll16 + s->tPos) | ((((unsigned int )*(s->ll4 + (s->tPos >> 1)) >> ((s->tPos << 2) & 4U)) & 15U) << 16);
#line 4204
      (s->nblock_used) ++;
    }
  }
}
}
#line 4212 "bzip2.c"
int BZ2_bzDecompress(bz_stream *strm ) 
{ DState *s ;
  Int32 r ;
  Int32 tmp ;

  {
#line 4215
  if ((unsigned long )strm == (unsigned long )((void *)0)) {
#line 4215
    return (-2);
  } else {

  }
#line 4216
  s = (DState *)strm->state;
#line 4217
  if ((unsigned long )s == (unsigned long )((void *)0)) {
#line 4217
    return (-2);
  } else {

  }
#line 4218
  if ((unsigned long )s->strm != (unsigned long )strm) {
#line 4218
    return (-2);
  } else {

  }
#line 4220
  while (1) {
#line 4221
    if (s->state == 1) {
#line 4221
      return (-1);
    } else {

    }
#line 4222
    if (s->state == 2) {
#line 4223
      if (s->smallDecompress) {
#line 4224
        unRLE_obuf_to_output_SMALL(s);
      } else {
#line 4225
        unRLE_obuf_to_output_FAST(s);
      }
#line 4226
      if (s->nblock_used == s->save_nblock + 1) {
#line 4226
        if (s->state_out_len == 0) {
#line 4227
          s->calculatedBlockCRC = ~ s->calculatedBlockCRC;
#line 4228
          if (s->verbosity >= 3) {
#line 4229
            fprintf((FILE */* __restrict  */)stderr,
                    (char const   */* __restrict  */)" {0x%x, 0x%x}",
                    s->storedBlockCRC, s->calculatedBlockCRC);
          } else {

          }
#line 4231
          if (s->verbosity >= 2) {
#line 4231
            fprintf((FILE */* __restrict  */)stderr,
                    (char const   */* __restrict  */)"]");
          } else {

          }
#line 4232
          if (s->calculatedBlockCRC != s->storedBlockCRC) {
#line 4233
            return (-4);
          } else {

          }
#line 4234
          s->calculatedCombinedCRC = (s->calculatedCombinedCRC << 1) | (s->calculatedCombinedCRC >> 31);
#line 4237
          s->calculatedCombinedCRC ^= s->calculatedBlockCRC;
#line 4238
          s->state = 14;
        } else {
#line 4240
          return (0);
        }
      } else {
#line 4240
        return (0);
      }
    } else {

    }
#line 4243
    if (s->state >= 10) {
#line 4244
      tmp = BZ2_decompress(s);
#line 4244
      r = tmp;
#line 4245
      if (r == 4) {
#line 4246
        if (s->verbosity >= 3) {
#line 4247
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"\n    combined CRCs: stored = 0x%x, computed = 0x%x",
                  s->storedCombinedCRC, s->calculatedCombinedCRC);
        } else {

        }
#line 4249
        if (s->calculatedCombinedCRC != s->storedCombinedCRC) {
#line 4250
          return (-4);
        } else {

        }
#line 4251
        return (r);
      } else {

      }
#line 4253
      if (s->state != 2) {
#line 4253
        return (r);
      } else {

      }
    } else {

    }
  }
#line 4257
  BZ2_bz__AssertH__fail(6001);
#line 4259
  return (0);
}
}
#line 4264 "bzip2.c"
int BZ2_bzDecompressEnd(bz_stream *strm ) 
{ DState *s ;

  {
#line 4267
  if ((unsigned long )strm == (unsigned long )((void *)0)) {
#line 4267
    return (-2);
  } else {

  }
#line 4268
  s = (DState *)strm->state;
#line 4269
  if ((unsigned long )s == (unsigned long )((void *)0)) {
#line 4269
    return (-2);
  } else {

  }
#line 4270
  if ((unsigned long )s->strm != (unsigned long )strm) {
#line 4270
    return (-2);
  } else {

  }
#line 4272
  if ((unsigned long )s->tt != (unsigned long )((void *)0)) {
#line 4272
    (*(strm->bzfree))(strm->opaque, (void *)s->tt);
  } else {

  }
#line 4273
  if ((unsigned long )s->ll16 != (unsigned long )((void *)0)) {
#line 4273
    (*(strm->bzfree))(strm->opaque, (void *)s->ll16);
  } else {

  }
#line 4274
  if ((unsigned long )s->ll4 != (unsigned long )((void *)0)) {
#line 4274
    (*(strm->bzfree))(strm->opaque, (void *)s->ll4);
  } else {

  }
#line 4276
  (*(strm->bzfree))(strm->opaque, strm->state);
#line 4277
  strm->state = (void *)0;
#line 4279
  return (0);
}
}
#line 4308 "bzip2.c"
static Bool myfeof(FILE *f ) 
{ Int32 c ;
  int tmp ;

  {
#line 4310
  tmp = fgetc(f);
#line 4310
  c = tmp;
#line 4311
  if (c == -1) {
#line 4311
    return ((unsigned char)1);
  } else {

  }
#line 4312
  ungetc(c, f);
#line 4313
  return ((unsigned char)0);
}
}
#line 4318 "bzip2.c"
BZFILE *BZ2_bzWriteOpen(int *bzerror , FILE *f , int blockSize100k ,
                        int verbosity , int workFactor ) 
{ Int32 ret ;
  bzFile *bzf ;
  int tmp ;
  void *tmp___0 ;

  {
#line 4326
  bzf = (bzFile *)((void *)0);
#line 4328
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4328
    *bzerror = 0;
  } else {

  }
#line 4328
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4328
    bzf->lastErr = 0;
  } else {

  }
#line 4330
  if ((unsigned long )f == (unsigned long )((void *)0)) {
    goto _L___0;
  } else {
#line 4330
    if (blockSize100k < 1) {
      goto _L___0;
    } else {
#line 4330
      if (blockSize100k > 9) {
        goto _L___0;
      } else {
#line 4330
        if (workFactor < 0) {
          goto _L___0;
        } else {
#line 4330
          if (workFactor > 250) {
            goto _L___0;
          } else {
#line 4330
            if (verbosity < 0) {
              goto _L___0;
            } else {
#line 4330
              if (verbosity > 4) {
                _L___0: /* CIL Label */ 
                _L: /* CIL Label */ 
#line 4334
                if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4334
                  *bzerror = -2;
                } else {

                }
#line 4334
                if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4334
                  bzf->lastErr = -2;
                } else {

                }
#line 4334
                return ((void *)0);
              } else {

              }
            }
          }
        }
      }
    }
  }
#line 4336
  tmp = ferror(f);
#line 4336
  if (tmp) {
#line 4337
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4337
      *bzerror = -6;
    } else {

    }
#line 4337
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4337
      bzf->lastErr = -6;
    } else {

    }
#line 4337
    return ((void *)0);
  } else {

  }
#line 4339
  tmp___0 = malloc(sizeof(bzFile ));
#line 4339
  bzf = (bzFile *)tmp___0;
#line 4340
  if ((unsigned long )bzf == (unsigned long )((void *)0)) {
#line 4341
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4341
      *bzerror = -3;
    } else {

    }
#line 4341
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4341
      bzf->lastErr = -3;
    } else {

    }
#line 4341
    return ((void *)0);
  } else {

  }
#line 4343
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4343
    *bzerror = 0;
  } else {

  }
#line 4343
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4343
    bzf->lastErr = 0;
  } else {

  }
#line 4344
  bzf->initialisedOk = (unsigned char)0;
#line 4345
  bzf->bufN = 0;
#line 4346
  bzf->handle = f;
#line 4347
  bzf->writing = (unsigned char)1;
#line 4348
  bzf->strm.bzalloc = (void *(*)(void * , int  , int  ))((void *)0);
#line 4349
  bzf->strm.bzfree = (void (*)(void * , void * ))((void *)0);
#line 4350
  bzf->strm.opaque = (void *)0;
#line 4352
  if (workFactor == 0) {
#line 4352
    workFactor = 30;
  } else {

  }
#line 4353
  ret = BZ2_bzCompressInit(& bzf->strm, blockSize100k, verbosity, workFactor);
#line 4355
  if (ret != 0) {
#line 4356
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4356
      *bzerror = ret;
    } else {

    }
#line 4356
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4356
      bzf->lastErr = ret;
    } else {

    }
#line 4356
    free((void *)bzf);
#line 4356
    return ((void *)0);
  } else {

  }
#line 4358
  bzf->strm.avail_in = 0U;
#line 4359
  bzf->initialisedOk = (unsigned char)1;
#line 4360
  return ((BZFILE *)bzf);
}
}
#line 4366 "bzip2.c"
void BZ2_bzWrite(int *bzerror , BZFILE *b , void *buf , int len ) 
{ Int32 n ;
  Int32 n2 ;
  Int32 ret ;
  bzFile *bzf ;
  int tmp ;
  size_t tmp___0 ;
  int tmp___1 ;

  {
#line 4373
  bzf = (bzFile *)b;
#line 4375
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4375
    *bzerror = 0;
  } else {

  }
#line 4375
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4375
    bzf->lastErr = 0;
  } else {

  }
#line 4376
  if ((unsigned long )bzf == (unsigned long )((void *)0)) {
    goto _L;
  } else {
#line 4376
    if ((unsigned long )buf == (unsigned long )((void *)0)) {
      goto _L;
    } else {
#line 4376
      if (len < 0) {
        _L: /* CIL Label */ 
#line 4377
        if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4377
          *bzerror = -2;
        } else {

        }
#line 4377
        if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4377
          bzf->lastErr = -2;
        } else {

        }
#line 4377
        return;
      } else {

      }
    }
  }
#line 4378
  if (! bzf->writing) {
#line 4379
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4379
      *bzerror = -1;
    } else {

    }
#line 4379
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4379
      bzf->lastErr = -1;
    } else {

    }
#line 4379
    return;
  } else {

  }
#line 4380
  tmp = ferror(bzf->handle);
#line 4380
  if (tmp) {
#line 4381
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4381
      *bzerror = -6;
    } else {

    }
#line 4381
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4381
      bzf->lastErr = -6;
    } else {

    }
#line 4381
    return;
  } else {

  }
#line 4383
  if (len == 0) {
#line 4384
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4384
      *bzerror = 0;
    } else {

    }
#line 4384
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4384
      bzf->lastErr = 0;
    } else {

    }
#line 4384
    return;
  } else {

  }
#line 4386
  bzf->strm.avail_in = (unsigned int )len;
#line 4387
  bzf->strm.next_in = (char *)buf;
#line 4389
  while (1) {
#line 4390
    bzf->strm.avail_out = 5000U;
#line 4391
    bzf->strm.next_out = bzf->buf;
#line 4392
    ret = BZ2_bzCompress(& bzf->strm, 0);
#line 4393
    if (ret != 1) {
#line 4394
      if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4394
        *bzerror = ret;
      } else {

      }
#line 4394
      if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4394
        bzf->lastErr = ret;
      } else {

      }
#line 4394
      return;
    } else {

    }
#line 4396
    if (bzf->strm.avail_out < 5000U) {
#line 4397
      n = (int )(5000U - bzf->strm.avail_out);
#line 4398
      tmp___0 = fwrite((void const   */* __restrict  */)((void *)(bzf->buf)),
                       sizeof(UChar ), (unsigned long )n,
                       (FILE */* __restrict  */)bzf->handle);
#line 4398
      n2 = (int )tmp___0;
#line 4400
      if (n != n2) {
        goto _L___0;
      } else {
#line 4400
        tmp___1 = ferror(bzf->handle);
#line 4400
        if (tmp___1) {
          _L___0: /* CIL Label */ 
#line 4401
          if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4401
            *bzerror = -6;
          } else {

          }
#line 4401
          if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4401
            bzf->lastErr = -6;
          } else {

          }
#line 4401
          return;
        } else {

        }
      }
    } else {

    }
#line 4404
    if (bzf->strm.avail_in == 0U) {
#line 4405
      if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4405
        *bzerror = 0;
      } else {

      }
#line 4405
      if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4405
        bzf->lastErr = 0;
      } else {

      }
#line 4405
      return;
    } else {

    }
  }
}
}
#line 4411 "bzip2.c"
void BZ2_bzWriteClose(int *bzerror , BZFILE *b , int abandon ,
                      unsigned int *nbytes_in , unsigned int *nbytes_out ) 
{ 

  {
#line 4418
  BZ2_bzWriteClose64(bzerror, b, abandon, nbytes_in,
                     (unsigned int *)((void *)0), nbytes_out,
                     (unsigned int *)((void *)0));
#line 4420
  return;
}
}
#line 4423 "bzip2.c"
void BZ2_bzWriteClose64(int *bzerror , BZFILE *b , int abandon ,
                        unsigned int *nbytes_in_lo32 ,
                        unsigned int *nbytes_in_hi32 ,
                        unsigned int *nbytes_out_lo32 ,
                        unsigned int *nbytes_out_hi32 ) 
{ Int32 n ;
  Int32 n2 ;
  Int32 ret ;
  bzFile *bzf ;
  int tmp ;
  size_t tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;

  {
#line 4433
  bzf = (bzFile *)b;
#line 4435
  if ((unsigned long )bzf == (unsigned long )((void *)0)) {
#line 4436
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4436
      *bzerror = 0;
    } else {

    }
#line 4436
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4436
      bzf->lastErr = 0;
    } else {

    }
#line 4436
    return;
  } else {

  }
#line 4437
  if (! bzf->writing) {
#line 4438
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4438
      *bzerror = -1;
    } else {

    }
#line 4438
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4438
      bzf->lastErr = -1;
    } else {

    }
#line 4438
    return;
  } else {

  }
#line 4439
  tmp = ferror(bzf->handle);
#line 4439
  if (tmp) {
#line 4440
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4440
      *bzerror = -6;
    } else {

    }
#line 4440
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4440
      bzf->lastErr = -6;
    } else {

    }
#line 4440
    return;
  } else {

  }
#line 4442
  if ((unsigned long )nbytes_in_lo32 != (unsigned long )((void *)0)) {
#line 4442
    *nbytes_in_lo32 = 0U;
  } else {

  }
#line 4443
  if ((unsigned long )nbytes_in_hi32 != (unsigned long )((void *)0)) {
#line 4443
    *nbytes_in_hi32 = 0U;
  } else {

  }
#line 4444
  if ((unsigned long )nbytes_out_lo32 != (unsigned long )((void *)0)) {
#line 4444
    *nbytes_out_lo32 = 0U;
  } else {

  }
#line 4445
  if ((unsigned long )nbytes_out_hi32 != (unsigned long )((void *)0)) {
#line 4445
    *nbytes_out_hi32 = 0U;
  } else {

  }
#line 4447
  if (! abandon) {
#line 4447
    if (bzf->lastErr == 0) {
#line 4448
      while (1) {
#line 4449
        bzf->strm.avail_out = 5000U;
#line 4450
        bzf->strm.next_out = bzf->buf;
#line 4451
        ret = BZ2_bzCompress(& bzf->strm, 2);
#line 4452
        if (ret != 3) {
#line 4452
          if (ret != 4) {
#line 4453
            if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4453
              *bzerror = ret;
            } else {

            }
#line 4453
            if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4453
              bzf->lastErr = ret;
            } else {

            }
#line 4453
            return;
          } else {

          }
        } else {

        }
#line 4455
        if (bzf->strm.avail_out < 5000U) {
#line 4456
          n = (int )(5000U - bzf->strm.avail_out);
#line 4457
          tmp___0 = fwrite((void const   */* __restrict  */)((void *)(bzf->buf)),
                           sizeof(UChar ), (unsigned long )n,
                           (FILE */* __restrict  */)bzf->handle);
#line 4457
          n2 = (int )tmp___0;
#line 4459
          if (n != n2) {
            goto _L;
          } else {
#line 4459
            tmp___1 = ferror(bzf->handle);
#line 4459
            if (tmp___1) {
              _L: /* CIL Label */ 
#line 4460
              if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4460
                *bzerror = -6;
              } else {

              }
#line 4460
              if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4460
                bzf->lastErr = -6;
              } else {

              }
#line 4460
              return;
            } else {

            }
          }
        } else {

        }
#line 4463
        if (ret == 4) {
#line 4463
          break;
        } else {

        }
      }
    } else {

    }
  } else {

  }
#line 4467
  if (! abandon) {
#line 4467
    tmp___3 = ferror(bzf->handle);
#line 4467
    if (tmp___3) {

    } else {
#line 4468
      fflush(bzf->handle);
#line 4469
      tmp___2 = ferror(bzf->handle);
#line 4469
      if (tmp___2) {
#line 4470
        if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4470
          *bzerror = -6;
        } else {

        }
#line 4470
        if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4470
          bzf->lastErr = -6;
        } else {

        }
#line 4470
        return;
      } else {

      }
    }
  } else {

  }
#line 4473
  if ((unsigned long )nbytes_in_lo32 != (unsigned long )((void *)0)) {
#line 4474
    *nbytes_in_lo32 = bzf->strm.total_in_lo32;
  } else {

  }
#line 4475
  if ((unsigned long )nbytes_in_hi32 != (unsigned long )((void *)0)) {
#line 4476
    *nbytes_in_hi32 = bzf->strm.total_in_hi32;
  } else {

  }
#line 4477
  if ((unsigned long )nbytes_out_lo32 != (unsigned long )((void *)0)) {
#line 4478
    *nbytes_out_lo32 = bzf->strm.total_out_lo32;
  } else {

  }
#line 4479
  if ((unsigned long )nbytes_out_hi32 != (unsigned long )((void *)0)) {
#line 4480
    *nbytes_out_hi32 = bzf->strm.total_out_hi32;
  } else {

  }
#line 4482
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4482
    *bzerror = 0;
  } else {

  }
#line 4482
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4482
    bzf->lastErr = 0;
  } else {

  }
#line 4483
  BZ2_bzCompressEnd(& bzf->strm);
#line 4484
  free((void *)bzf);
#line 4485
  return;
}
}
#line 4489 "bzip2.c"
BZFILE *BZ2_bzReadOpen(int *bzerror , FILE *f , int verbosity , int small ,
                       void *unused , int nUnused ) 
{ bzFile *bzf ;
  int ret ;
  int tmp ;
  void *tmp___0 ;

  {
#line 4497
  bzf = (bzFile *)((void *)0);
#line 4500
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4500
    *bzerror = 0;
  } else {

  }
#line 4500
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4500
    bzf->lastErr = 0;
  } else {

  }
#line 4502
  if ((unsigned long )f == (unsigned long )((void *)0)) {
    goto _L___0;
  } else {
#line 4502
    if (small != 0) {
#line 4502
      if (small != 1) {
        goto _L___0;
      } else {
        goto _L___2;
      }
    } else {
      _L___2: /* CIL Label */ 
#line 4502
      if (verbosity < 0) {
        goto _L___0;
      } else {
#line 4502
        if (verbosity > 4) {
          goto _L___0;
        } else {
#line 4502
          if ((unsigned long )unused == (unsigned long )((void *)0)) {
#line 4502
            if (nUnused != 0) {
              goto _L___0;
            } else {
              goto _L___1;
            }
          } else {
            _L___1: /* CIL Label */ 
#line 4502
            if ((unsigned long )unused != (unsigned long )((void *)0)) {
#line 4502
              if (nUnused < 0) {
                goto _L___0;
              } else {
#line 4502
                if (nUnused > 5000) {
                  _L___0: /* CIL Label */ 
                  _L: /* CIL Label */ 
#line 4507
                  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4507
                    *bzerror = -2;
                  } else {

                  }
#line 4507
                  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4507
                    bzf->lastErr = -2;
                  } else {

                  }
#line 4507
                  return ((void *)0);
                } else {

                }
              }
            } else {

            }
          }
        }
      }
    }
  }
#line 4509
  tmp = ferror(f);
#line 4509
  if (tmp) {
#line 4510
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4510
      *bzerror = -6;
    } else {

    }
#line 4510
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4510
      bzf->lastErr = -6;
    } else {

    }
#line 4510
    return ((void *)0);
  } else {

  }
#line 4512
  tmp___0 = malloc(sizeof(bzFile ));
#line 4512
  bzf = (bzFile *)tmp___0;
#line 4513
  if ((unsigned long )bzf == (unsigned long )((void *)0)) {
#line 4514
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4514
      *bzerror = -3;
    } else {

    }
#line 4514
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4514
      bzf->lastErr = -3;
    } else {

    }
#line 4514
    return ((void *)0);
  } else {

  }
#line 4516
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4516
    *bzerror = 0;
  } else {

  }
#line 4516
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4516
    bzf->lastErr = 0;
  } else {

  }
#line 4518
  bzf->initialisedOk = (unsigned char)0;
#line 4519
  bzf->handle = f;
#line 4520
  bzf->bufN = 0;
#line 4521
  bzf->writing = (unsigned char)0;
#line 4522
  bzf->strm.bzalloc = (void *(*)(void * , int  , int  ))((void *)0);
#line 4523
  bzf->strm.bzfree = (void (*)(void * , void * ))((void *)0);
#line 4524
  bzf->strm.opaque = (void *)0;
#line 4526
  while (nUnused > 0) {
#line 4527
    bzf->buf[bzf->bufN] = (char )*((UChar *)unused);
#line 4527
    (bzf->bufN) ++;
#line 4528
    unused = (void *)((UChar *)unused + 1);
#line 4529
    nUnused --;
  }
#line 4532
  ret = BZ2_bzDecompressInit(& bzf->strm, verbosity, small);
#line 4533
  if (ret != 0) {
#line 4534
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4534
      *bzerror = ret;
    } else {

    }
#line 4534
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4534
      bzf->lastErr = ret;
    } else {

    }
#line 4534
    free((void *)bzf);
#line 4534
    return ((void *)0);
  } else {

  }
#line 4536
  bzf->strm.avail_in = (unsigned int )bzf->bufN;
#line 4537
  bzf->strm.next_in = bzf->buf;
#line 4539
  bzf->initialisedOk = (unsigned char)1;
#line 4540
  return ((BZFILE *)bzf);
}
}
#line 4545 "bzip2.c"
void BZ2_bzReadClose(int *bzerror , BZFILE *b ) 
{ bzFile *bzf ;

  {
#line 4547
  bzf = (bzFile *)b;
#line 4549
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4549
    *bzerror = 0;
  } else {

  }
#line 4549
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4549
    bzf->lastErr = 0;
  } else {

  }
#line 4550
  if ((unsigned long )bzf == (unsigned long )((void *)0)) {
#line 4551
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4551
      *bzerror = 0;
    } else {

    }
#line 4551
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4551
      bzf->lastErr = 0;
    } else {

    }
#line 4551
    return;
  } else {

  }
#line 4553
  if (bzf->writing) {
#line 4554
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4554
      *bzerror = -1;
    } else {

    }
#line 4554
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4554
      bzf->lastErr = -1;
    } else {

    }
#line 4554
    return;
  } else {

  }
#line 4556
  if (bzf->initialisedOk) {
#line 4557
    BZ2_bzDecompressEnd(& bzf->strm);
  } else {

  }
#line 4558
  free((void *)bzf);
#line 4559
  return;
}
}
#line 4563 "bzip2.c"
int BZ2_bzRead(int *bzerror , BZFILE *b , void *buf , int len ) 
{ Int32 n ;
  Int32 ret ;
  bzFile *bzf ;
  int tmp ;
  size_t tmp___0 ;
  int tmp___1 ;
  Bool tmp___2 ;
  Bool tmp___3 ;

  {
#line 4570
  bzf = (bzFile *)b;
#line 4572
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4572
    *bzerror = 0;
  } else {

  }
#line 4572
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4572
    bzf->lastErr = 0;
  } else {

  }
#line 4574
  if ((unsigned long )bzf == (unsigned long )((void *)0)) {
    goto _L;
  } else {
#line 4574
    if ((unsigned long )buf == (unsigned long )((void *)0)) {
      goto _L;
    } else {
#line 4574
      if (len < 0) {
        _L: /* CIL Label */ 
#line 4575
        if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4575
          *bzerror = -2;
        } else {

        }
#line 4575
        if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4575
          bzf->lastErr = -2;
        } else {

        }
#line 4575
        return (0);
      } else {

      }
    }
  }
#line 4577
  if (bzf->writing) {
#line 4578
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4578
      *bzerror = -1;
    } else {

    }
#line 4578
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4578
      bzf->lastErr = -1;
    } else {

    }
#line 4578
    return (0);
  } else {

  }
#line 4580
  if (len == 0) {
#line 4581
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4581
      *bzerror = 0;
    } else {

    }
#line 4581
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4581
      bzf->lastErr = 0;
    } else {

    }
#line 4581
    return (0);
  } else {

  }
#line 4583
  bzf->strm.avail_out = (unsigned int )len;
#line 4584
  bzf->strm.next_out = (char *)buf;
#line 4586
  while (1) {
#line 4588
    tmp = ferror(bzf->handle);
#line 4588
    if (tmp) {
#line 4589
      if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4589
        *bzerror = -6;
      } else {

      }
#line 4589
      if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4589
        bzf->lastErr = -6;
      } else {

      }
#line 4589
      return (0);
    } else {

    }
#line 4591
    if (bzf->strm.avail_in == 0U) {
#line 4591
      tmp___2 = myfeof(bzf->handle);
#line 4591
      if (tmp___2) {

      } else {
#line 4592
        tmp___0 = fread((void */* __restrict  */)(bzf->buf), sizeof(UChar ),
                        5000UL, (FILE */* __restrict  */)bzf->handle);
#line 4592
        n = (int )tmp___0;
#line 4594
        tmp___1 = ferror(bzf->handle);
#line 4594
        if (tmp___1) {
#line 4595
          if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4595
            *bzerror = -6;
          } else {

          }
#line 4595
          if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4595
            bzf->lastErr = -6;
          } else {

          }
#line 4595
          return (0);
        } else {

        }
#line 4596
        bzf->bufN = n;
#line 4597
        bzf->strm.avail_in = (unsigned int )bzf->bufN;
#line 4598
        bzf->strm.next_in = bzf->buf;
      }
    } else {

    }
#line 4601
    ret = BZ2_bzDecompress(& bzf->strm);
#line 4603
    if (ret != 0) {
#line 4603
      if (ret != 4) {
#line 4604
        if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4604
          *bzerror = ret;
        } else {

        }
#line 4604
        if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4604
          bzf->lastErr = ret;
        } else {

        }
#line 4604
        return (0);
      } else {

      }
    } else {

    }
#line 4606
    if (ret == 0) {
#line 4606
      tmp___3 = myfeof(bzf->handle);
#line 4606
      if (tmp___3) {
#line 4606
        if (bzf->strm.avail_in == 0U) {
#line 4606
          if (bzf->strm.avail_out > 0U) {
#line 4608
            if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4608
              *bzerror = -7;
            } else {

            }
#line 4608
            if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4608
              bzf->lastErr = -7;
            } else {

            }
#line 4608
            return (0);
          } else {

          }
        } else {

        }
      } else {

      }
    } else {

    }
#line 4610
    if (ret == 4) {
#line 4611
      if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4611
        *bzerror = 4;
      } else {

      }
#line 4611
      if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4611
        bzf->lastErr = 4;
      } else {

      }
#line 4612
      return ((int )((unsigned int )len - bzf->strm.avail_out));
    } else {

    }
#line 4613
    if (bzf->strm.avail_out == 0U) {
#line 4614
      if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4614
        *bzerror = 0;
      } else {

      }
#line 4614
      if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4614
        bzf->lastErr = 0;
      } else {

      }
#line 4614
      return (len);
    } else {

    }
  }
#line 4618
  return (0);
}
}
#line 4623 "bzip2.c"
void BZ2_bzReadGetUnused(int *bzerror , BZFILE *b , void **unused ,
                         int *nUnused ) 
{ bzFile *bzf ;

  {
#line 4629
  bzf = (bzFile *)b;
#line 4630
  if ((unsigned long )bzf == (unsigned long )((void *)0)) {
#line 4631
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4631
      *bzerror = -2;
    } else {

    }
#line 4631
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4631
      bzf->lastErr = -2;
    } else {

    }
#line 4631
    return;
  } else {

  }
#line 4632
  if (bzf->lastErr != 4) {
#line 4633
    if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4633
      *bzerror = -1;
    } else {

    }
#line 4633
    if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4633
      bzf->lastErr = -1;
    } else {

    }
#line 4633
    return;
  } else {

  }
#line 4634
  if ((unsigned long )unused == (unsigned long )((void *)0)) {
    goto _L;
  } else {
#line 4634
    if ((unsigned long )nUnused == (unsigned long )((void *)0)) {
      _L: /* CIL Label */ 
#line 4635
      if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4635
        *bzerror = -2;
      } else {

      }
#line 4635
      if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4635
        bzf->lastErr = -2;
      } else {

      }
#line 4635
      return;
    } else {

    }
  }
#line 4637
  if ((unsigned long )bzerror != (unsigned long )((void *)0)) {
#line 4637
    *bzerror = 0;
  } else {

  }
#line 4637
  if ((unsigned long )bzf != (unsigned long )((void *)0)) {
#line 4637
    bzf->lastErr = 0;
  } else {

  }
#line 4638
  *nUnused = (int )bzf->strm.avail_in;
#line 4639
  *unused = (void *)bzf->strm.next_in;
#line 4640
  return;
}
}
#line 4649 "bzip2.c"
int BZ2_bzBuffToBuffCompress(char *dest , unsigned int *destLen , char *source ,
                             unsigned int sourceLen , int blockSize100k ,
                             int verbosity , int workFactor ) 
{ bz_stream strm ;
  int ret ;

  {
#line 4661
  if ((unsigned long )dest == (unsigned long )((void *)0)) {
#line 4666
    return (-2);
  } else {
#line 4661
    if ((unsigned long )destLen == (unsigned long )((void *)0)) {
#line 4666
      return (-2);
    } else {
#line 4661
      if ((unsigned long )source == (unsigned long )((void *)0)) {
#line 4666
        return (-2);
      } else {
#line 4661
        if (blockSize100k < 1) {
#line 4666
          return (-2);
        } else {
#line 4661
          if (blockSize100k > 9) {
#line 4666
            return (-2);
          } else {
#line 4661
            if (verbosity < 0) {
#line 4666
              return (-2);
            } else {
#line 4661
              if (verbosity > 4) {
#line 4666
                return (-2);
              } else {
#line 4661
                if (workFactor < 0) {
#line 4666
                  return (-2);
                } else {
#line 4661
                  if (workFactor > 250) {
#line 4666
                    return (-2);
                  } else {

                  }
                }
              }
            }
          }
        }
      }
    }
  }
#line 4668
  if (workFactor == 0) {
#line 4668
    workFactor = 30;
  } else {

  }
#line 4669
  strm.bzalloc = (void *(*)(void * , int  , int  ))((void *)0);
#line 4670
  strm.bzfree = (void (*)(void * , void * ))((void *)0);
#line 4671
  strm.opaque = (void *)0;
#line 4672
  ret = BZ2_bzCompressInit(& strm, blockSize100k, verbosity, workFactor);
#line 4674
  if (ret != 0) {
#line 4674
    return (ret);
  } else {

  }
#line 4676
  strm.next_in = source;
#line 4677
  strm.next_out = dest;
#line 4678
  strm.avail_in = sourceLen;
#line 4679
  strm.avail_out = *destLen;
#line 4681
  ret = BZ2_bzCompress(& strm, 2);
#line 4682
  if (ret == 3) {
    goto output_overflow;
  } else {

  }
#line 4683
  if (ret != 4) {
    goto errhandler;
  } else {

  }
#line 4686
  *destLen -= strm.avail_out;
#line 4687
  BZ2_bzCompressEnd(& strm);
#line 4688
  return (0);
  output_overflow: 
#line 4691
  BZ2_bzCompressEnd(& strm);
#line 4692
  return (-8);
  errhandler: 
#line 4695
  BZ2_bzCompressEnd(& strm);
#line 4696
  return (ret);
}
}
#line 4701 "bzip2.c"
int BZ2_bzBuffToBuffDecompress(char *dest , unsigned int *destLen ,
                               char *source , unsigned int sourceLen ,
                               int small , int verbosity ) 
{ bz_stream strm ;
  int ret ;

  {
#line 4712
  if ((unsigned long )dest == (unsigned long )((void *)0)) {
#line 4716
    return (-2);
  } else {
#line 4712
    if ((unsigned long )destLen == (unsigned long )((void *)0)) {
#line 4716
      return (-2);
    } else {
#line 4712
      if ((unsigned long )source == (unsigned long )((void *)0)) {
#line 4716
        return (-2);
      } else {
#line 4712
        if (small != 0) {
#line 4712
          if (small != 1) {
#line 4716
            return (-2);
          } else {
            goto _L;
          }
        } else {
          _L: /* CIL Label */ 
#line 4712
          if (verbosity < 0) {
#line 4716
            return (-2);
          } else {
#line 4712
            if (verbosity > 4) {
#line 4716
              return (-2);
            } else {

            }
          }
        }
      }
    }
  }
#line 4718
  strm.bzalloc = (void *(*)(void * , int  , int  ))((void *)0);
#line 4719
  strm.bzfree = (void (*)(void * , void * ))((void *)0);
#line 4720
  strm.opaque = (void *)0;
#line 4721
  ret = BZ2_bzDecompressInit(& strm, verbosity, small);
#line 4722
  if (ret != 0) {
#line 4722
    return (ret);
  } else {

  }
#line 4724
  strm.next_in = source;
#line 4725
  strm.next_out = dest;
#line 4726
  strm.avail_in = sourceLen;
#line 4727
  strm.avail_out = *destLen;
#line 4729
  ret = BZ2_bzDecompress(& strm);
#line 4730
  if (ret == 0) {
    goto output_overflow_or_eof;
  } else {

  }
#line 4731
  if (ret != 4) {
    goto errhandler;
  } else {

  }
#line 4734
  *destLen -= strm.avail_out;
#line 4735
  BZ2_bzDecompressEnd(& strm);
#line 4736
  return (0);
  output_overflow_or_eof: 
#line 4739
  if (strm.avail_out > 0U) {
#line 4740
    BZ2_bzDecompressEnd(& strm);
#line 4741
    return (-7);
  } else {
#line 4743
    BZ2_bzDecompressEnd(& strm);
#line 4744
    return (-8);
  }
  errhandler: 
#line 4748
  BZ2_bzDecompressEnd(& strm);
#line 4749
  return (ret);
}
}
#line 4769 "bzip2.c"
char const   *BZ2_bzlibVersion(void) 
{ 

  {
#line 4771
  return ("1.0.2, 30-Dec-2001");
}
}
#line 4778 "bzip2.c"
static BZFILE *bzopen_or_bzdopen(char const   *path , int fd ,
                                 char const   *mode , int open_mode ) 
{ int bzerr ;
  char unused[5000] ;
  int blockSize100k ;
  int writing ;
  char mode2[10] ;
  FILE *fp ;
  BZFILE *bzfp ;
  int verbosity ;
  int workFactor ;
  int smallMode ;
  int nUnused ;
  unsigned short const   **tmp ;
  char const   *tmp___0 ;
  int tmp___1 ;

  {
#line 4787
  blockSize100k = 9;
#line 4788
  writing = 0;
#line 4789
  mode2[0] = (char )'\000';
#line 4789
  mode2[1] = (char)0;
#line 4789
  mode2[2] = (char)0;
#line 4789
  mode2[3] = (char)0;
#line 4789
  mode2[4] = (char)0;
#line 4789
  mode2[5] = (char)0;
#line 4789
  mode2[6] = (char)0;
#line 4789
  mode2[7] = (char)0;
#line 4789
  mode2[8] = (char)0;
#line 4789
  mode2[9] = (char)0;
#line 4790
  fp = (FILE *)((void *)0);
#line 4791
  bzfp = (void *)0;
#line 4792
  verbosity = 0;
#line 4793
  workFactor = 30;
#line 4794
  smallMode = 0;
#line 4795
  nUnused = 0;
#line 4797
  if ((unsigned long )mode == (unsigned long )((void *)0)) {
#line 4797
    return ((void *)0);
  } else {

  }
#line 4798
  while (*mode) {
#line 4799
    switch ((int )*mode) {
    case 114: 
#line 4801
    writing = 0;
#line 4801
    break;
    case 119: 
#line 4803
    writing = 1;
#line 4803
    break;
    case 115: 
#line 4805
    smallMode = 1;
#line 4805
    break;
    default: 
#line 4807
    tmp = __ctype_b_loc();
#line 4807
    if ((int const   )*(*tmp + (int )*mode) & 2048) {
#line 4808
      blockSize100k = (int )((int const   )*mode - 48);
    } else {

    }
    }
#line 4811
    mode ++;
  }
#line 4813
  if (writing) {
#line 4813
    tmp___0 = "w";
  } else {
#line 4813
    tmp___0 = "r";
  }
#line 4813
  strcat((char */* __restrict  */)(mode2),
         (char const   */* __restrict  */)tmp___0);
#line 4814
  strcat((char */* __restrict  */)(mode2), (char const   */* __restrict  */)"b");
#line 4816
  if (open_mode == 0) {
#line 4817
    if ((unsigned long )path == (unsigned long )((void *)0)) {
      goto _L;
    } else {
#line 4817
      tmp___1 = strcmp(path, "");
#line 4817
      if (tmp___1 == 0) {
        _L: /* CIL Label */ 
#line 4818
        if (writing) {
#line 4818
          fp = stdout;
        } else {
#line 4818
          fp = stdin;
        }
      } else {
#line 4820
        fp = fopen((char const   */* __restrict  */)path,
                   (char const   */* __restrict  */)(mode2));
      }
    }
  } else {
#line 4826
    fp = fdopen(fd, (char const   *)(mode2));
  }
#line 4829
  if ((unsigned long )fp == (unsigned long )((void *)0)) {
#line 4829
    return ((void *)0);
  } else {

  }
#line 4831
  if (writing) {
#line 4833
    if (blockSize100k < 1) {
#line 4833
      blockSize100k = 1;
    } else {

    }
#line 4834
    if (blockSize100k > 9) {
#line 4834
      blockSize100k = 9;
    } else {

    }
#line 4835
    bzfp = BZ2_bzWriteOpen(& bzerr, fp, blockSize100k, verbosity, workFactor);
  } else {
#line 4838
    bzfp = BZ2_bzReadOpen(& bzerr, fp, verbosity, smallMode, (void *)(unused),
                          nUnused);
  }
#line 4841
  if ((unsigned long )bzfp == (unsigned long )((void *)0)) {
#line 4842
    if ((unsigned long )fp != (unsigned long )stdin) {
#line 4842
      if ((unsigned long )fp != (unsigned long )stdout) {
#line 4842
        fclose(fp);
      } else {

      }
    } else {

    }
#line 4843
    return ((void *)0);
  } else {

  }
#line 4845
  return (bzfp);
}
}
#line 4855 "bzip2.c"
BZFILE *BZ2_bzopen(char const   *path , char const   *mode ) 
{ BZFILE *tmp ;

  {
#line 4859
  tmp = bzopen_or_bzdopen(path, -1, mode, 0);
#line 4859
  return (tmp);
}
}
#line 4864 "bzip2.c"
BZFILE *BZ2_bzdopen(int fd , char const   *mode ) 
{ BZFILE *tmp ;

  {
#line 4868
  tmp = bzopen_or_bzdopen((char const   *)((void *)0), fd, mode, 1);
#line 4868
  return (tmp);
}
}
#line 4873 "bzip2.c"
int BZ2_bzread(BZFILE *b , void *buf , int len ) 
{ int bzerr ;
  int nread ;

  {
#line 4876
  if (((bzFile *)b)->lastErr == 4) {
#line 4876
    return (0);
  } else {

  }
#line 4877
  nread = BZ2_bzRead(& bzerr, b, buf, len);
#line 4878
  if (bzerr == 0) {
#line 4879
    return (nread);
  } else {
#line 4878
    if (bzerr == 4) {
#line 4879
      return (nread);
    } else {
#line 4881
      return (-1);
    }
  }
}
}
#line 4887 "bzip2.c"
int BZ2_bzwrite(BZFILE *b , void *buf , int len ) 
{ int bzerr ;

  {
#line 4891
  BZ2_bzWrite(& bzerr, b, buf, len);
#line 4892
  if (bzerr == 0) {
#line 4893
    return (len);
  } else {
#line 4895
    return (-1);
  }
}
}
#line 4901 "bzip2.c"
int BZ2_bzflush(BZFILE *b ) 
{ 

  {
#line 4904
  return (0);
}
}
#line 4909 "bzip2.c"
void BZ2_bzclose(BZFILE *b ) 
{ int bzerr ;
  FILE *fp ;

  {
#line 4912
  fp = ((bzFile *)b)->handle;
#line 4914
  if ((unsigned long )b == (unsigned long )((void *)0)) {
#line 4914
    return;
  } else {

  }
#line 4915
  if (((bzFile *)b)->writing) {
#line 4916
    BZ2_bzWriteClose(& bzerr, b, 0, (unsigned int *)((void *)0),
                     (unsigned int *)((void *)0));
#line 4917
    if (bzerr != 0) {
#line 4918
      BZ2_bzWriteClose((int *)((void *)0), b, 1, (unsigned int *)((void *)0),
                       (unsigned int *)((void *)0));
    } else {

    }
  } else {
#line 4921
    BZ2_bzReadClose(& bzerr, b);
  }
#line 4923
  if ((unsigned long )fp != (unsigned long )stdin) {
#line 4923
    if ((unsigned long )fp != (unsigned long )stdout) {
#line 4924
      fclose(fp);
    } else {

    }
  } else {

  }
#line 4926
  return;
}
}
#line 4933 "bzip2.c"
static char *bzerrorstrings[16]  = 
#line 4933
  {      (char *)"OK",      (char *)"SEQUENCE_ERROR",      (char *)"PARAM_ERROR",      (char *)"MEM_ERROR", 
        (char *)"DATA_ERROR",      (char *)"DATA_ERROR_MAGIC",      (char *)"IO_ERROR",      (char *)"UNEXPECTED_EOF", 
        (char *)"OUTBUFF_FULL",      (char *)"CONFIG_ERROR",      (char *)"???",      (char *)"???", 
        (char *)"???",      (char *)"???",      (char *)"???",      (char *)"???"};
#line 4953 "bzip2.c"
char const   *BZ2_bzerror(BZFILE *b , int *errnum ) 
{ int err ;

  {
#line 4955
  err = ((bzFile *)b)->lastErr;
#line 4957
  if (err > 0) {
#line 4957
    err = 0;
  } else {

  }
#line 4958
  *errnum = err;
#line 4959
  return ((char const   *)bzerrorstrings[err * -1]);
}
}
#line 103 "/usr/include/bits/sigset.h"
extern int __sigismember(__sigset_t const   * , int  ) ;
#line 104
extern int __sigaddset(__sigset_t * , int  ) ;
#line 105
extern int __sigdelset(__sigset_t * , int  ) ;
#line 80 "/usr/include/signal.h"
extern  __attribute__((__nothrow__)) __sighandler_t __sysv_signal(int __sig ,
                                                                  void (*__handler)(int  ) ) ;
#line 92
extern  __attribute__((__nothrow__)) __sighandler_t signal(int __sig ,
                                                           void (*__handler)(int  ) ) ;
#line 117
extern  __attribute__((__nothrow__)) int kill(__pid_t __pid , int __sig ) ;
#line 124
extern  __attribute__((__nothrow__)) int killpg(__pid_t __pgrp , int __sig ) ;
#line 129
extern  __attribute__((__nothrow__)) int raise(int __sig ) ;
#line 134
extern  __attribute__((__nothrow__)) __sighandler_t ssignal(int __sig ,
                                                            void (*__handler)(int  ) ) ;
#line 136
extern  __attribute__((__nothrow__)) int gsignal(int __sig ) ;
#line 141
extern void psignal(int __sig , char const   *__s ) ;
#line 153
extern int __sigpause(int __sig_or_mask , int __is_sig ) ;
#line 181
extern  __attribute__((__nothrow__)) int sigblock(int __mask )  __attribute__((__deprecated__)) ;
#line 184
extern  __attribute__((__nothrow__)) int sigsetmask(int __mask )  __attribute__((__deprecated__)) ;
#line 187
extern  __attribute__((__nothrow__)) int siggetmask(void)  __attribute__((__deprecated__)) ;
#line 216
extern  __attribute__((__nothrow__)) int sigemptyset(sigset_t *__set )  __attribute__((__nonnull__(1))) ;
#line 219
extern  __attribute__((__nothrow__)) int sigfillset(sigset_t *__set )  __attribute__((__nonnull__(1))) ;
#line 222
extern  __attribute__((__nothrow__)) int sigaddset(sigset_t *__set ,
                                                   int __signo )  __attribute__((__nonnull__(1))) ;
#line 225
extern  __attribute__((__nothrow__)) int sigdelset(sigset_t *__set ,
                                                   int __signo )  __attribute__((__nonnull__(1))) ;
#line 228
extern  __attribute__((__nothrow__)) int sigismember(sigset_t const   *__set ,
                                                     int __signo )  __attribute__((__nonnull__(1))) ;
#line 249
extern  __attribute__((__nothrow__)) int sigprocmask(int __how ,
                                                     sigset_t const   * __restrict  __set ,
                                                     sigset_t * __restrict  __oset ) ;
#line 257
extern int sigsuspend(sigset_t const   *__set )  __attribute__((__nonnull__(1))) ;
#line 260
extern  __attribute__((__nothrow__)) int sigaction(int __sig ,
                                                   struct sigaction  const  * __restrict  __act ,
                                                   struct sigaction * __restrict  __oact ) ;
#line 264
extern  __attribute__((__nothrow__)) int sigpending(sigset_t *__set )  __attribute__((__nonnull__(1))) ;
#line 271
extern int sigwait(sigset_t const   * __restrict  __set ,
                   int * __restrict  __sig )  __attribute__((__nonnull__(1,2))) ;
#line 279
extern int sigwaitinfo(sigset_t const   * __restrict  __set ,
                       siginfo_t * __restrict  __info )  __attribute__((__nonnull__(1))) ;
#line 287
extern int sigtimedwait(sigset_t const   * __restrict  __set ,
                        siginfo_t * __restrict  __info ,
                        struct timespec  const  * __restrict  __timeout )  __attribute__((__nonnull__(1))) ;
#line 294
extern  __attribute__((__nothrow__)) int sigqueue(__pid_t __pid , int __sig ,
                                                  union sigval __val ) ;
#line 304
extern char const   * const  _sys_siglist[65] ;
#line 305
extern char const   * const  sys_siglist[65] ;
#line 328
extern  __attribute__((__nothrow__)) int sigvec(int __sig ,
                                                struct sigvec  const  *__vec ,
                                                struct sigvec *__ovec ) ;
#line 336
extern  __attribute__((__nothrow__)) int sigreturn(struct sigcontext *__scp ) ;
#line 346
extern  __attribute__((__nothrow__)) int siginterrupt(int __sig ,
                                                      int __interrupt ) ;
#line 357
extern  __attribute__((__nothrow__)) int sigstack(struct sigstack *__ss ,
                                                  struct sigstack *__oss )  __attribute__((__deprecated__)) ;
#line 362
extern  __attribute__((__nothrow__)) int sigaltstack(struct sigaltstack  const  * __restrict  __ss ,
                                                     struct sigaltstack * __restrict  __oss ) ;
#line 31 "/usr/include/bits/sigthread.h"
extern  __attribute__((__nothrow__)) int pthread_sigmask(int __how ,
                                                         __sigset_t const   * __restrict  __newmask ,
                                                         __sigset_t * __restrict  __oldmask ) ;
#line 36
extern  __attribute__((__nothrow__)) int pthread_kill(pthread_t __threadid ,
                                                      int __signo ) ;
#line 394 "/usr/include/signal.h"
extern  __attribute__((__nothrow__)) int __libc_current_sigrtmin(void) ;
#line 396
extern  __attribute__((__nothrow__)) int __libc_current_sigrtmax(void) ;
#line 55 "/usr/include/bits/mathcalls.h"
extern  __attribute__((__nothrow__)) double acos(double __x ) ;
#line 55
extern  __attribute__((__nothrow__)) double __acos(double __x ) ;
#line 57
extern  __attribute__((__nothrow__)) double asin(double __x ) ;
#line 57
extern  __attribute__((__nothrow__)) double __asin(double __x ) ;
#line 59
extern  __attribute__((__nothrow__)) double atan(double __x ) ;
#line 59
extern  __attribute__((__nothrow__)) double __atan(double __x ) ;
#line 61
extern  __attribute__((__nothrow__)) double atan2(double __y , double __x ) ;
#line 61
extern  __attribute__((__nothrow__)) double __atan2(double __y , double __x ) ;
#line 64
extern  __attribute__((__nothrow__)) double cos(double __x ) ;
#line 64
extern  __attribute__((__nothrow__)) double __cos(double __x ) ;
#line 66
extern  __attribute__((__nothrow__)) double sin(double __x ) ;
#line 66
extern  __attribute__((__nothrow__)) double __sin(double __x ) ;
#line 68
extern  __attribute__((__nothrow__)) double tan(double __x ) ;
#line 68
extern  __attribute__((__nothrow__)) double __tan(double __x ) ;
#line 73
extern  __attribute__((__nothrow__)) double cosh(double __x ) ;
#line 73
extern  __attribute__((__nothrow__)) double __cosh(double __x ) ;
#line 75
extern  __attribute__((__nothrow__)) double sinh(double __x ) ;
#line 75
extern  __attribute__((__nothrow__)) double __sinh(double __x ) ;
#line 77
extern  __attribute__((__nothrow__)) double tanh(double __x ) ;
#line 77
extern  __attribute__((__nothrow__)) double __tanh(double __x ) ;
#line 89
extern  __attribute__((__nothrow__)) double acosh(double __x ) ;
#line 89
extern  __attribute__((__nothrow__)) double __acosh(double __x ) ;
#line 91
extern  __attribute__((__nothrow__)) double asinh(double __x ) ;
#line 91
extern  __attribute__((__nothrow__)) double __asinh(double __x ) ;
#line 93
extern  __attribute__((__nothrow__)) double atanh(double __x ) ;
#line 93
extern  __attribute__((__nothrow__)) double __atanh(double __x ) ;
#line 101
extern  __attribute__((__nothrow__)) double exp(double __x ) ;
#line 101
extern  __attribute__((__nothrow__)) double __exp(double __x ) ;
#line 104
extern  __attribute__((__nothrow__)) double frexp(double __x , int *__exponent ) ;
#line 104
extern  __attribute__((__nothrow__)) double __frexp(double __x ,
                                                    int *__exponent ) ;
#line 107
extern  __attribute__((__nothrow__)) double ldexp(double __x , int __exponent ) ;
#line 107
extern  __attribute__((__nothrow__)) double __ldexp(double __x , int __exponent ) ;
#line 110
extern  __attribute__((__nothrow__)) double log(double __x ) ;
#line 110
extern  __attribute__((__nothrow__)) double __log(double __x ) ;
#line 113
extern  __attribute__((__nothrow__)) double log10(double __x ) ;
#line 113
extern  __attribute__((__nothrow__)) double __log10(double __x ) ;
#line 116
extern  __attribute__((__nothrow__)) double modf(double __x , double *__iptr ) ;
#line 116
extern  __attribute__((__nothrow__)) double __modf(double __x , double *__iptr ) ;
#line 129
extern  __attribute__((__nothrow__)) double expm1(double __x ) ;
#line 129
extern  __attribute__((__nothrow__)) double __expm1(double __x ) ;
#line 132
extern  __attribute__((__nothrow__)) double log1p(double __x ) ;
#line 132
extern  __attribute__((__nothrow__)) double __log1p(double __x ) ;
#line 135
extern  __attribute__((__nothrow__)) double logb(double __x ) ;
#line 135
extern  __attribute__((__nothrow__)) double __logb(double __x ) ;
#line 154
extern  __attribute__((__nothrow__)) double pow(double __x , double __y ) ;
#line 154
extern  __attribute__((__nothrow__)) double __pow(double __x , double __y ) ;
#line 157
extern  __attribute__((__nothrow__)) double sqrt(double __x ) ;
#line 157
extern  __attribute__((__nothrow__)) double __sqrt(double __x ) ;
#line 163
extern  __attribute__((__nothrow__)) double hypot(double __x , double __y ) ;
#line 163
extern  __attribute__((__nothrow__)) double __hypot(double __x , double __y ) ;
#line 170
extern  __attribute__((__nothrow__)) double cbrt(double __x ) ;
#line 170
extern  __attribute__((__nothrow__)) double __cbrt(double __x ) ;
#line 179
extern  __attribute__((__nothrow__)) double ceil(double __x )  __attribute__((__const__)) ;
#line 179
extern  __attribute__((__nothrow__)) double __ceil(double __x )  __attribute__((__const__)) ;
#line 182
extern  __attribute__((__nothrow__)) double fabs(double __x )  __attribute__((__const__)) ;
#line 182
extern  __attribute__((__nothrow__)) double __fabs(double __x )  __attribute__((__const__)) ;
#line 185
extern  __attribute__((__nothrow__)) double floor(double __x )  __attribute__((__const__)) ;
#line 185
extern  __attribute__((__nothrow__)) double __floor(double __x )  __attribute__((__const__)) ;
#line 188
extern  __attribute__((__nothrow__)) double fmod(double __x , double __y ) ;
#line 188
extern  __attribute__((__nothrow__)) double __fmod(double __x , double __y ) ;
#line 193
extern  __attribute__((__nothrow__)) int __isinf(double __value )  __attribute__((__const__)) ;
#line 196
extern  __attribute__((__nothrow__)) int __finite(double __value )  __attribute__((__const__)) ;
#line 202
extern  __attribute__((__nothrow__)) int isinf(double __value )  __attribute__((__const__)) ;
#line 205
extern  __attribute__((__nothrow__)) int finite(double __value )  __attribute__((__const__)) ;
#line 208
extern  __attribute__((__nothrow__)) double drem(double __x , double __y ) ;
#line 208
extern  __attribute__((__nothrow__)) double __drem(double __x , double __y ) ;
#line 212
extern  __attribute__((__nothrow__)) double significand(double __x ) ;
#line 212
extern  __attribute__((__nothrow__)) double __significand(double __x ) ;
#line 218
extern  __attribute__((__nothrow__)) double copysign(double __x , double __y )  __attribute__((__const__)) ;
#line 218
extern  __attribute__((__nothrow__)) double __copysign(double __x , double __y )  __attribute__((__const__)) ;
#line 231
extern  __attribute__((__nothrow__)) int __isnan(double __value )  __attribute__((__const__)) ;
#line 235
extern  __attribute__((__nothrow__)) int isnan(double __value )  __attribute__((__const__)) ;
#line 238
extern  __attribute__((__nothrow__)) double j0(double  ) ;
#line 238
extern  __attribute__((__nothrow__)) double __j0(double  ) ;
#line 239
extern  __attribute__((__nothrow__)) double j1(double  ) ;
#line 239
extern  __attribute__((__nothrow__)) double __j1(double  ) ;
#line 240
extern  __attribute__((__nothrow__)) double jn(int  , double  ) ;
#line 240
extern  __attribute__((__nothrow__)) double __jn(int  , double  ) ;
#line 241
extern  __attribute__((__nothrow__)) double y0(double  ) ;
#line 241
extern  __attribute__((__nothrow__)) double __y0(double  ) ;
#line 242
extern  __attribute__((__nothrow__)) double y1(double  ) ;
#line 242
extern  __attribute__((__nothrow__)) double __y1(double  ) ;
#line 243
extern  __attribute__((__nothrow__)) double yn(int  , double  ) ;
#line 243
extern  __attribute__((__nothrow__)) double __yn(int  , double  ) ;
#line 250
extern  __attribute__((__nothrow__)) double erf(double  ) ;
#line 250
extern  __attribute__((__nothrow__)) double __erf(double  ) ;
#line 251
extern  __attribute__((__nothrow__)) double erfc(double  ) ;
#line 251
extern  __attribute__((__nothrow__)) double __erfc(double  ) ;
#line 252
extern  __attribute__((__nothrow__)) double lgamma(double  ) ;
#line 252
extern  __attribute__((__nothrow__)) double __lgamma(double  ) ;
#line 265
extern  __attribute__((__nothrow__)) double gamma(double  ) ;
#line 265
extern  __attribute__((__nothrow__)) double __gamma(double  ) ;
#line 272
extern  __attribute__((__nothrow__)) double lgamma_r(double  , int *__signgamp ) ;
#line 272
extern  __attribute__((__nothrow__)) double __lgamma_r(double  ,
                                                       int *__signgamp ) ;
#line 280
extern  __attribute__((__nothrow__)) double rint(double __x ) ;
#line 280
extern  __attribute__((__nothrow__)) double __rint(double __x ) ;
#line 283
extern  __attribute__((__nothrow__)) double nextafter(double __x , double __y )  __attribute__((__const__)) ;
#line 283
extern  __attribute__((__nothrow__)) double __nextafter(double __x , double __y )  __attribute__((__const__)) ;
#line 289
extern  __attribute__((__nothrow__)) double remainder(double __x , double __y ) ;
#line 289
extern  __attribute__((__nothrow__)) double __remainder(double __x , double __y ) ;
#line 293
extern  __attribute__((__nothrow__)) double scalbn(double __x , int __n ) ;
#line 293
extern  __attribute__((__nothrow__)) double __scalbn(double __x , int __n ) ;
#line 297
extern  __attribute__((__nothrow__)) int ilogb(double __x ) ;
#line 297
extern  __attribute__((__nothrow__)) int __ilogb(double __x ) ;
#line 364
extern  __attribute__((__nothrow__)) double scalb(double __x , double __n ) ;
#line 364
extern  __attribute__((__nothrow__)) double __scalb(double __x , double __n ) ;
#line 55
extern  __attribute__((__nothrow__)) float acosf(float __x ) ;
#line 55
extern  __attribute__((__nothrow__)) float __acosf(float __x ) ;
#line 57
extern  __attribute__((__nothrow__)) float asinf(float __x ) ;
#line 57
extern  __attribute__((__nothrow__)) float __asinf(float __x ) ;
#line 59
extern  __attribute__((__nothrow__)) float atanf(float __x ) ;
#line 59
extern  __attribute__((__nothrow__)) float __atanf(float __x ) ;
#line 61
extern  __attribute__((__nothrow__)) float atan2f(float __y , float __x ) ;
#line 61
extern  __attribute__((__nothrow__)) float __atan2f(float __y , float __x ) ;
#line 64
extern  __attribute__((__nothrow__)) float cosf(float __x ) ;
#line 64
extern  __attribute__((__nothrow__)) float __cosf(float __x ) ;
#line 66
extern  __attribute__((__nothrow__)) float sinf(float __x ) ;
#line 66
extern  __attribute__((__nothrow__)) float __sinf(float __x ) ;
#line 68
extern  __attribute__((__nothrow__)) float tanf(float __x ) ;
#line 68
extern  __attribute__((__nothrow__)) float __tanf(float __x ) ;
#line 73
extern  __attribute__((__nothrow__)) float coshf(float __x ) ;
#line 73
extern  __attribute__((__nothrow__)) float __coshf(float __x ) ;
#line 75
extern  __attribute__((__nothrow__)) float sinhf(float __x ) ;
#line 75
extern  __attribute__((__nothrow__)) float __sinhf(float __x ) ;
#line 77
extern  __attribute__((__nothrow__)) float tanhf(float __x ) ;
#line 77
extern  __attribute__((__nothrow__)) float __tanhf(float __x ) ;
#line 89
extern  __attribute__((__nothrow__)) float acoshf(float __x ) ;
#line 89
extern  __attribute__((__nothrow__)) float __acoshf(float __x ) ;
#line 91
extern  __attribute__((__nothrow__)) float asinhf(float __x ) ;
#line 91
extern  __attribute__((__nothrow__)) float __asinhf(float __x ) ;
#line 93
extern  __attribute__((__nothrow__)) float atanhf(float __x ) ;
#line 93
extern  __attribute__((__nothrow__)) float __atanhf(float __x ) ;
#line 101
extern  __attribute__((__nothrow__)) float expf(float __x ) ;
#line 101
extern  __attribute__((__nothrow__)) float __expf(float __x ) ;
#line 104
extern  __attribute__((__nothrow__)) float frexpf(float __x , int *__exponent ) ;
#line 104
extern  __attribute__((__nothrow__)) float __frexpf(float __x , int *__exponent ) ;
#line 107
extern  __attribute__((__nothrow__)) float ldexpf(float __x , int __exponent ) ;
#line 107
extern  __attribute__((__nothrow__)) float __ldexpf(float __x , int __exponent ) ;
#line 110
extern  __attribute__((__nothrow__)) float logf(float __x ) ;
#line 110
extern  __attribute__((__nothrow__)) float __logf(float __x ) ;
#line 113
extern  __attribute__((__nothrow__)) float log10f(float __x ) ;
#line 113
extern  __attribute__((__nothrow__)) float __log10f(float __x ) ;
#line 116
extern  __attribute__((__nothrow__)) float modff(float __x , float *__iptr ) ;
#line 116
extern  __attribute__((__nothrow__)) float __modff(float __x , float *__iptr ) ;
#line 129
extern  __attribute__((__nothrow__)) float expm1f(float __x ) ;
#line 129
extern  __attribute__((__nothrow__)) float __expm1f(float __x ) ;
#line 132
extern  __attribute__((__nothrow__)) float log1pf(float __x ) ;
#line 132
extern  __attribute__((__nothrow__)) float __log1pf(float __x ) ;
#line 135
extern  __attribute__((__nothrow__)) float logbf(float __x ) ;
#line 135
extern  __attribute__((__nothrow__)) float __logbf(float __x ) ;
#line 154
extern  __attribute__((__nothrow__)) float powf(float __x , float __y ) ;
#line 154
extern  __attribute__((__nothrow__)) float __powf(float __x , float __y ) ;
#line 157
extern  __attribute__((__nothrow__)) float sqrtf(float __x ) ;
#line 157
extern  __attribute__((__nothrow__)) float __sqrtf(float __x ) ;
#line 163
extern  __attribute__((__nothrow__)) float hypotf(float __x , float __y ) ;
#line 163
extern  __attribute__((__nothrow__)) float __hypotf(float __x , float __y ) ;
#line 170
extern  __attribute__((__nothrow__)) float cbrtf(float __x ) ;
#line 170
extern  __attribute__((__nothrow__)) float __cbrtf(float __x ) ;
#line 179
extern  __attribute__((__nothrow__)) float ceilf(float __x )  __attribute__((__const__)) ;
#line 179
extern  __attribute__((__nothrow__)) float __ceilf(float __x )  __attribute__((__const__)) ;
#line 182
extern  __attribute__((__nothrow__)) float fabsf(float __x )  __attribute__((__const__)) ;
#line 182
extern  __attribute__((__nothrow__)) float __fabsf(float __x )  __attribute__((__const__)) ;
#line 185
extern  __attribute__((__nothrow__)) float floorf(float __x )  __attribute__((__const__)) ;
#line 185
extern  __attribute__((__nothrow__)) float __floorf(float __x )  __attribute__((__const__)) ;
#line 188
extern  __attribute__((__nothrow__)) float fmodf(float __x , float __y ) ;
#line 188
extern  __attribute__((__nothrow__)) float __fmodf(float __x , float __y ) ;
#line 193
extern  __attribute__((__nothrow__)) int __isinff(float __value )  __attribute__((__const__)) ;
#line 196
extern  __attribute__((__nothrow__)) int __finitef(float __value )  __attribute__((__const__)) ;
#line 202
extern  __attribute__((__nothrow__)) int isinff(float __value )  __attribute__((__const__)) ;
#line 205
extern  __attribute__((__nothrow__)) int finitef(float __value )  __attribute__((__const__)) ;
#line 208
extern  __attribute__((__nothrow__)) float dremf(float __x , float __y ) ;
#line 208
extern  __attribute__((__nothrow__)) float __dremf(float __x , float __y ) ;
#line 212
extern  __attribute__((__nothrow__)) float significandf(float __x ) ;
#line 212
extern  __attribute__((__nothrow__)) float __significandf(float __x ) ;
#line 218
extern  __attribute__((__nothrow__)) float copysignf(float __x , float __y )  __attribute__((__const__)) ;
#line 218
extern  __attribute__((__nothrow__)) float __copysignf(float __x , float __y )  __attribute__((__const__)) ;
#line 231
extern  __attribute__((__nothrow__)) int __isnanf(float __value )  __attribute__((__const__)) ;
#line 235
extern  __attribute__((__nothrow__)) int isnanf(float __value )  __attribute__((__const__)) ;
#line 238
extern  __attribute__((__nothrow__)) float j0f(float  ) ;
#line 238
extern  __attribute__((__nothrow__)) float __j0f(float  ) ;
#line 239
extern  __attribute__((__nothrow__)) float j1f(float  ) ;
#line 239
extern  __attribute__((__nothrow__)) float __j1f(float  ) ;
#line 240
extern  __attribute__((__nothrow__)) float jnf(int  , float  ) ;
#line 240
extern  __attribute__((__nothrow__)) float __jnf(int  , float  ) ;
#line 241
extern  __attribute__((__nothrow__)) float y0f(float  ) ;
#line 241
extern  __attribute__((__nothrow__)) float __y0f(float  ) ;
#line 242
extern  __attribute__((__nothrow__)) float y1f(float  ) ;
#line 242
extern  __attribute__((__nothrow__)) float __y1f(float  ) ;
#line 243
extern  __attribute__((__nothrow__)) float ynf(int  , float  ) ;
#line 243
extern  __attribute__((__nothrow__)) float __ynf(int  , float  ) ;
#line 250
extern  __attribute__((__nothrow__)) float erff(float  ) ;
#line 250
extern  __attribute__((__nothrow__)) float __erff(float  ) ;
#line 251
extern  __attribute__((__nothrow__)) float erfcf(float  ) ;
#line 251
extern  __attribute__((__nothrow__)) float __erfcf(float  ) ;
#line 252
extern  __attribute__((__nothrow__)) float lgammaf(float  ) ;
#line 252
extern  __attribute__((__nothrow__)) float __lgammaf(float  ) ;
#line 265
extern  __attribute__((__nothrow__)) float gammaf(float  ) ;
#line 265
extern  __attribute__((__nothrow__)) float __gammaf(float  ) ;
#line 272
extern  __attribute__((__nothrow__)) float lgammaf_r(float  , int *__signgamp ) ;
#line 272
extern  __attribute__((__nothrow__)) float __lgammaf_r(float  , int *__signgamp ) ;
#line 280
extern  __attribute__((__nothrow__)) float rintf(float __x ) ;
#line 280
extern  __attribute__((__nothrow__)) float __rintf(float __x ) ;
#line 283
extern  __attribute__((__nothrow__)) float nextafterf(float __x , float __y )  __attribute__((__const__)) ;
#line 283
extern  __attribute__((__nothrow__)) float __nextafterf(float __x , float __y )  __attribute__((__const__)) ;
#line 289
extern  __attribute__((__nothrow__)) float remainderf(float __x , float __y ) ;
#line 289
extern  __attribute__((__nothrow__)) float __remainderf(float __x , float __y ) ;
#line 293
extern  __attribute__((__nothrow__)) float scalbnf(float __x , int __n ) ;
#line 293
extern  __attribute__((__nothrow__)) float __scalbnf(float __x , int __n ) ;
#line 297
extern  __attribute__((__nothrow__)) int ilogbf(float __x ) ;
#line 297
extern  __attribute__((__nothrow__)) int __ilogbf(float __x ) ;
#line 364
extern  __attribute__((__nothrow__)) float scalbf(float __x , float __n ) ;
#line 364
extern  __attribute__((__nothrow__)) float __scalbf(float __x , float __n ) ;
#line 55
extern  __attribute__((__nothrow__)) long double acosl(long double __x ) ;
#line 55
extern  __attribute__((__nothrow__)) long double __acosl(long double __x ) ;
#line 57
extern  __attribute__((__nothrow__)) long double asinl(long double __x ) ;
#line 57
extern  __attribute__((__nothrow__)) long double __asinl(long double __x ) ;
#line 59
extern  __attribute__((__nothrow__)) long double atanl(long double __x ) ;
#line 59
extern  __attribute__((__nothrow__)) long double __atanl(long double __x ) ;
#line 61
extern  __attribute__((__nothrow__)) long double atan2l(long double __y ,
                                                        long double __x ) ;
#line 61
extern  __attribute__((__nothrow__)) long double __atan2l(long double __y ,
                                                          long double __x ) ;
#line 64
extern  __attribute__((__nothrow__)) long double cosl(long double __x ) ;
#line 64
extern  __attribute__((__nothrow__)) long double __cosl(long double __x ) ;
#line 66
extern  __attribute__((__nothrow__)) long double sinl(long double __x ) ;
#line 66
extern  __attribute__((__nothrow__)) long double __sinl(long double __x ) ;
#line 68
extern  __attribute__((__nothrow__)) long double tanl(long double __x ) ;
#line 68
extern  __attribute__((__nothrow__)) long double __tanl(long double __x ) ;
#line 73
extern  __attribute__((__nothrow__)) long double coshl(long double __x ) ;
#line 73
extern  __attribute__((__nothrow__)) long double __coshl(long double __x ) ;
#line 75
extern  __attribute__((__nothrow__)) long double sinhl(long double __x ) ;
#line 75
extern  __attribute__((__nothrow__)) long double __sinhl(long double __x ) ;
#line 77
extern  __attribute__((__nothrow__)) long double tanhl(long double __x ) ;
#line 77
extern  __attribute__((__nothrow__)) long double __tanhl(long double __x ) ;
#line 89
extern  __attribute__((__nothrow__)) long double acoshl(long double __x ) ;
#line 89
extern  __attribute__((__nothrow__)) long double __acoshl(long double __x ) ;
#line 91
extern  __attribute__((__nothrow__)) long double asinhl(long double __x ) ;
#line 91
extern  __attribute__((__nothrow__)) long double __asinhl(long double __x ) ;
#line 93
extern  __attribute__((__nothrow__)) long double atanhl(long double __x ) ;
#line 93
extern  __attribute__((__nothrow__)) long double __atanhl(long double __x ) ;
#line 101
extern  __attribute__((__nothrow__)) long double expl(long double __x ) ;
#line 101
extern  __attribute__((__nothrow__)) long double __expl(long double __x ) ;
#line 104
extern  __attribute__((__nothrow__)) long double frexpl(long double __x ,
                                                        int *__exponent ) ;
#line 104
extern  __attribute__((__nothrow__)) long double __frexpl(long double __x ,
                                                          int *__exponent ) ;
#line 107
extern  __attribute__((__nothrow__)) long double ldexpl(long double __x ,
                                                        int __exponent ) ;
#line 107
extern  __attribute__((__nothrow__)) long double __ldexpl(long double __x ,
                                                          int __exponent ) ;
#line 110
extern  __attribute__((__nothrow__)) long double logl(long double __x ) ;
#line 110
extern  __attribute__((__nothrow__)) long double __logl(long double __x ) ;
#line 113
extern  __attribute__((__nothrow__)) long double log10l(long double __x ) ;
#line 113
extern  __attribute__((__nothrow__)) long double __log10l(long double __x ) ;
#line 116
extern  __attribute__((__nothrow__)) long double modfl(long double __x ,
                                                       long double *__iptr ) ;
#line 116
extern  __attribute__((__nothrow__)) long double __modfl(long double __x ,
                                                         long double *__iptr ) ;
#line 129
extern  __attribute__((__nothrow__)) long double expm1l(long double __x ) ;
#line 129
extern  __attribute__((__nothrow__)) long double __expm1l(long double __x ) ;
#line 132
extern  __attribute__((__nothrow__)) long double log1pl(long double __x ) ;
#line 132
extern  __attribute__((__nothrow__)) long double __log1pl(long double __x ) ;
#line 135
extern  __attribute__((__nothrow__)) long double logbl(long double __x ) ;
#line 135
extern  __attribute__((__nothrow__)) long double __logbl(long double __x ) ;
#line 154
extern  __attribute__((__nothrow__)) long double powl(long double __x ,
                                                      long double __y ) ;
#line 154
extern  __attribute__((__nothrow__)) long double __powl(long double __x ,
                                                        long double __y ) ;
#line 157
extern  __attribute__((__nothrow__)) long double sqrtl(long double __x ) ;
#line 157
extern  __attribute__((__nothrow__)) long double __sqrtl(long double __x ) ;
#line 163
extern  __attribute__((__nothrow__)) long double hypotl(long double __x ,
                                                        long double __y ) ;
#line 163
extern  __attribute__((__nothrow__)) long double __hypotl(long double __x ,
                                                          long double __y ) ;
#line 170
extern  __attribute__((__nothrow__)) long double cbrtl(long double __x ) ;
#line 170
extern  __attribute__((__nothrow__)) long double __cbrtl(long double __x ) ;
#line 179
extern  __attribute__((__nothrow__)) long double ceill(long double __x )  __attribute__((__const__)) ;
#line 179
extern  __attribute__((__nothrow__)) long double __ceill(long double __x )  __attribute__((__const__)) ;
#line 182
extern  __attribute__((__nothrow__)) long double fabsl(long double __x )  __attribute__((__const__)) ;
#line 182
extern  __attribute__((__nothrow__)) long double __fabsl(long double __x )  __attribute__((__const__)) ;
#line 185
extern  __attribute__((__nothrow__)) long double floorl(long double __x )  __attribute__((__const__)) ;
#line 185
extern  __attribute__((__nothrow__)) long double __floorl(long double __x )  __attribute__((__const__)) ;
#line 188
extern  __attribute__((__nothrow__)) long double fmodl(long double __x ,
                                                       long double __y ) ;
#line 188
extern  __attribute__((__nothrow__)) long double __fmodl(long double __x ,
                                                         long double __y ) ;
#line 193
extern  __attribute__((__nothrow__)) int __isinfl(long double __value )  __attribute__((__const__)) ;
#line 196
extern  __attribute__((__nothrow__)) int __finitel(long double __value )  __attribute__((__const__)) ;
#line 202
extern  __attribute__((__nothrow__)) int isinfl(long double __value )  __attribute__((__const__)) ;
#line 205
extern  __attribute__((__nothrow__)) int finitel(long double __value )  __attribute__((__const__)) ;
#line 208
extern  __attribute__((__nothrow__)) long double dreml(long double __x ,
                                                       long double __y ) ;
#line 208
extern  __attribute__((__nothrow__)) long double __dreml(long double __x ,
                                                         long double __y ) ;
#line 212
extern  __attribute__((__nothrow__)) long double significandl(long double __x ) ;
#line 212
extern  __attribute__((__nothrow__)) long double __significandl(long double __x ) ;
#line 218
extern  __attribute__((__nothrow__)) long double copysignl(long double __x ,
                                                           long double __y )  __attribute__((__const__)) ;
#line 218
extern  __attribute__((__nothrow__)) long double __copysignl(long double __x ,
                                                             long double __y )  __attribute__((__const__)) ;
#line 231
extern  __attribute__((__nothrow__)) int __isnanl(long double __value )  __attribute__((__const__)) ;
#line 235
extern  __attribute__((__nothrow__)) int isnanl(long double __value )  __attribute__((__const__)) ;
#line 238
extern  __attribute__((__nothrow__)) long double j0l(long double  ) ;
#line 238
extern  __attribute__((__nothrow__)) long double __j0l(long double  ) ;
#line 239
extern  __attribute__((__nothrow__)) long double j1l(long double  ) ;
#line 239
extern  __attribute__((__nothrow__)) long double __j1l(long double  ) ;
#line 240
extern  __attribute__((__nothrow__)) long double jnl(int  , long double  ) ;
#line 240
extern  __attribute__((__nothrow__)) long double __jnl(int  , long double  ) ;
#line 241
extern  __attribute__((__nothrow__)) long double y0l(long double  ) ;
#line 241
extern  __attribute__((__nothrow__)) long double __y0l(long double  ) ;
#line 242
extern  __attribute__((__nothrow__)) long double y1l(long double  ) ;
#line 242
extern  __attribute__((__nothrow__)) long double __y1l(long double  ) ;
#line 243
extern  __attribute__((__nothrow__)) long double ynl(int  , long double  ) ;
#line 243
extern  __attribute__((__nothrow__)) long double __ynl(int  , long double  ) ;
#line 250
extern  __attribute__((__nothrow__)) long double erfl(long double  ) ;
#line 250
extern  __attribute__((__nothrow__)) long double __erfl(long double  ) ;
#line 251
extern  __attribute__((__nothrow__)) long double erfcl(long double  ) ;
#line 251
extern  __attribute__((__nothrow__)) long double __erfcl(long double  ) ;
#line 252
extern  __attribute__((__nothrow__)) long double lgammal(long double  ) ;
#line 252
extern  __attribute__((__nothrow__)) long double __lgammal(long double  ) ;
#line 265
extern  __attribute__((__nothrow__)) long double gammal(long double  ) ;
#line 265
extern  __attribute__((__nothrow__)) long double __gammal(long double  ) ;
#line 272
extern  __attribute__((__nothrow__)) long double lgammal_r(long double  ,
                                                           int *__signgamp ) ;
#line 272
extern  __attribute__((__nothrow__)) long double __lgammal_r(long double  ,
                                                             int *__signgamp ) ;
#line 280
extern  __attribute__((__nothrow__)) long double rintl(long double __x ) ;
#line 280
extern  __attribute__((__nothrow__)) long double __rintl(long double __x ) ;
#line 283
extern  __attribute__((__nothrow__)) long double nextafterl(long double __x ,
                                                            long double __y )  __attribute__((__const__)) ;
#line 283
extern  __attribute__((__nothrow__)) long double __nextafterl(long double __x ,
                                                              long double __y )  __attribute__((__const__)) ;
#line 289
extern  __attribute__((__nothrow__)) long double remainderl(long double __x ,
                                                            long double __y ) ;
#line 289
extern  __attribute__((__nothrow__)) long double __remainderl(long double __x ,
                                                              long double __y ) ;
#line 293
extern  __attribute__((__nothrow__)) long double scalbnl(long double __x ,
                                                         int __n ) ;
#line 293
extern  __attribute__((__nothrow__)) long double __scalbnl(long double __x ,
                                                           int __n ) ;
#line 297
extern  __attribute__((__nothrow__)) int ilogbl(long double __x ) ;
#line 297
extern  __attribute__((__nothrow__)) int __ilogbl(long double __x ) ;
#line 364
extern  __attribute__((__nothrow__)) long double scalbl(long double __x ,
                                                        long double __n ) ;
#line 364
extern  __attribute__((__nothrow__)) long double __scalbl(long double __x ,
                                                          long double __n ) ;
#line 157 "/usr/include/math.h"
extern int signgam ;
#line 296
extern _LIB_VERSION_TYPE _LIB_VERSION ;
#line 322
extern int matherr(struct exception *__exc ) ;
#line 43 "/usr/include/bits/errno.h"
extern  __attribute__((__nothrow__)) int *__errno_location(void)  __attribute__((__const__)) ;
#line 76 "/usr/include/fcntl.h"
extern int fcntl(int __fd , int __cmd  , ...) ;
#line 85
extern int open(char const   *__file , int __oflag  , ...)  __attribute__((__nonnull__(1))) ;
#line 130
extern int creat(char const   *__file , __mode_t __mode )  __attribute__((__nonnull__(1))) ;
#line 159
extern int lockf(int __fd , int __cmd , __off_t __len ) ;
#line 176
extern  __attribute__((__nothrow__)) int posix_fadvise(int __fd ,
                                                       __off_t __offset ,
                                                       __off_t __len ,
                                                       int __advise ) ;
#line 198
extern int posix_fallocate(int __fd , __off_t __offset , __off_t __len ) ;
#line 46 "/usr/include/utime.h"
extern  __attribute__((__nothrow__)) int utime(char const   *__file ,
                                               struct utimbuf  const  *__file_times )  __attribute__((__nonnull__(1))) ;
#line 258 "/usr/include/unistd.h"
extern  __attribute__((__nothrow__)) int access(char const   *__name ,
                                                int __type )  __attribute__((__nonnull__(1))) ;
#line 301
extern  __attribute__((__nothrow__)) __off_t lseek(int __fd , __off_t __offset ,
                                                   int __whence ) ;
#line 320
extern int close(int __fd ) ;
#line 327
extern ssize_t read(int __fd , void *__buf , size_t __nbytes ) ;
#line 333
extern ssize_t write(int __fd , void const   *__buf , size_t __n ) ;
#line 384
extern  __attribute__((__nothrow__)) int pipe(int *__pipedes ) ;
#line 393
extern  __attribute__((__nothrow__)) unsigned int alarm(unsigned int __seconds ) ;
#line 405
extern unsigned int sleep(unsigned int __seconds ) ;
#line 412
extern  __attribute__((__nothrow__)) __useconds_t ualarm(__useconds_t __value ,
                                                         __useconds_t __interval ) ;
#line 420
extern int usleep(__useconds_t __useconds ) ;
#line 429
extern int pause(void) ;
#line 433
extern  __attribute__((__nothrow__)) int chown(char const   *__file ,
                                               __uid_t __owner ,
                                               __gid_t __group )  __attribute__((__nonnull__(1))) ;
#line 438
extern  __attribute__((__nothrow__)) int fchown(int __fd , __uid_t __owner ,
                                                __gid_t __group ) ;
#line 443
extern  __attribute__((__nothrow__)) int lchown(char const   *__file ,
                                                __uid_t __owner ,
                                                __gid_t __group )  __attribute__((__nonnull__(1))) ;
#line 457
extern  __attribute__((__nothrow__)) int chdir(char const   *__path )  __attribute__((__nonnull__(1))) ;
#line 461
extern  __attribute__((__nothrow__)) int fchdir(int __fd ) ;
#line 471
extern  __attribute__((__nothrow__)) char *getcwd(char *__buf , size_t __size ) ;
#line 484
extern  __attribute__((__nothrow__)) char *getwd(char *__buf )  __attribute__((__nonnull__(1),
__deprecated__)) ;
#line 490
extern  __attribute__((__nothrow__)) int dup(int __fd ) ;
#line 493
extern  __attribute__((__nothrow__)) int dup2(int __fd , int __fd2 ) ;
#line 496
extern char **__environ ;
#line 504
extern  __attribute__((__nothrow__)) int execve(char const   *__path ,
                                                char * const  *__argv ,
                                                char * const  *__envp )  __attribute__((__nonnull__(1))) ;
#line 516
extern  __attribute__((__nothrow__)) int execv(char const   *__path ,
                                               char * const  *__argv )  __attribute__((__nonnull__(1))) ;
#line 521
extern  __attribute__((__nothrow__)) int execle(char const   *__path ,
                                                char const   *__arg  , ...)  __attribute__((__nonnull__(1))) ;
#line 526
extern  __attribute__((__nothrow__)) int execl(char const   *__path ,
                                               char const   *__arg  , ...)  __attribute__((__nonnull__(1))) ;
#line 531
extern  __attribute__((__nothrow__)) int execvp(char const   *__file ,
                                                char * const  *__argv )  __attribute__((__nonnull__(1))) ;
#line 537
extern  __attribute__((__nothrow__)) int execlp(char const   *__file ,
                                                char const   *__arg  , ...)  __attribute__((__nonnull__(1))) ;
#line 543
extern  __attribute__((__nothrow__)) int nice(int __inc ) ;
#line 548
extern  __attribute__((__noreturn__)) void _exit(int __status ) ;
#line 557
extern  __attribute__((__nothrow__)) long pathconf(char const   *__path ,
                                                   int __name )  __attribute__((__nonnull__(1))) ;
#line 561
extern  __attribute__((__nothrow__)) long fpathconf(int __fd , int __name ) ;
#line 564
extern  __attribute__((__nothrow__)) long sysconf(int __name )  __attribute__((__const__)) ;
#line 568
extern  __attribute__((__nothrow__)) size_t confstr(int __name , char *__buf ,
                                                    size_t __len ) ;
#line 573
extern  __attribute__((__nothrow__)) __pid_t getpid(void) ;
#line 576
extern  __attribute__((__nothrow__)) __pid_t getppid(void) ;
#line 581
extern  __attribute__((__nothrow__)) __pid_t getpgrp(void) ;
#line 591
extern  __attribute__((__nothrow__)) __pid_t __getpgid(__pid_t __pid ) ;
#line 600
extern  __attribute__((__nothrow__)) int setpgid(__pid_t __pid , __pid_t __pgid ) ;
#line 617
extern  __attribute__((__nothrow__)) int setpgrp(void) ;
#line 634
extern  __attribute__((__nothrow__)) __pid_t setsid(void) ;
#line 642
extern  __attribute__((__nothrow__)) __uid_t getuid(void) ;
#line 645
extern  __attribute__((__nothrow__)) __uid_t geteuid(void) ;
#line 648
extern  __attribute__((__nothrow__)) __gid_t getgid(void) ;
#line 651
extern  __attribute__((__nothrow__)) __gid_t getegid(void) ;
#line 656
extern  __attribute__((__nothrow__)) int getgroups(int __size , __gid_t *__list ) ;
#line 667
extern  __attribute__((__nothrow__)) int setuid(__uid_t __uid ) ;
#line 672
extern  __attribute__((__nothrow__)) int setreuid(__uid_t __ruid ,
                                                  __uid_t __euid ) ;
#line 677
extern  __attribute__((__nothrow__)) int seteuid(__uid_t __uid ) ;
#line 684
extern  __attribute__((__nothrow__)) int setgid(__gid_t __gid ) ;
#line 689
extern  __attribute__((__nothrow__)) int setregid(__gid_t __rgid ,
                                                  __gid_t __egid ) ;
#line 694
extern  __attribute__((__nothrow__)) int setegid(__gid_t __gid ) ;
#line 723
extern  __attribute__((__nothrow__)) __pid_t fork(void) ;
#line 730
extern  __attribute__((__nothrow__)) __pid_t vfork(void) ;
#line 736
extern  __attribute__((__nothrow__)) char *ttyname(int __fd ) ;
#line 740
extern  __attribute__((__nothrow__)) int ttyname_r(int __fd , char *__buf ,
                                                   size_t __buflen )  __attribute__((__nonnull__(2))) ;
#line 745
extern  __attribute__((__nothrow__)) int isatty(int __fd ) ;
#line 751
extern  __attribute__((__nothrow__)) int ttyslot(void) ;
#line 756
extern  __attribute__((__nothrow__)) int link(char const   *__from ,
                                              char const   *__to )  __attribute__((__nonnull__(1,2))) ;
#line 769
extern  __attribute__((__nothrow__)) int symlink(char const   *__from ,
                                                 char const   *__to )  __attribute__((__nonnull__(1,2))) ;
#line 775
extern  __attribute__((__nothrow__)) ssize_t readlink(char const   * __restrict  __path ,
                                                      char * __restrict  __buf ,
                                                      size_t __len )  __attribute__((__nonnull__(1,2))) ;
#line 792
extern  __attribute__((__nothrow__)) int unlink(char const   *__name )  __attribute__((__nonnull__(1))) ;
#line 801
extern  __attribute__((__nothrow__)) int rmdir(char const   *__path )  __attribute__((__nonnull__(1))) ;
#line 805
extern  __attribute__((__nothrow__)) __pid_t tcgetpgrp(int __fd ) ;
#line 808
extern  __attribute__((__nothrow__)) int tcsetpgrp(int __fd , __pid_t __pgrp_id ) ;
#line 815
extern char *getlogin(void) ;
#line 823
extern int getlogin_r(char *__name , size_t __name_len )  __attribute__((__nonnull__(1))) ;
#line 828
extern  __attribute__((__nothrow__)) int setlogin(char const   *__name )  __attribute__((__nonnull__(1))) ;
#line 59 "/usr/include/getopt.h"
extern char *optarg ;
#line 73
extern int optind ;
#line 78
extern int opterr ;
#line 82
extern int optopt ;
#line 152
extern  __attribute__((__nothrow__)) int getopt(int ___argc ,
                                                char * const  *___argv ,
                                                char const   *__shortopts ) ;
#line 845 "/usr/include/unistd.h"
extern  __attribute__((__nothrow__)) int gethostname(char *__name ,
                                                     size_t __len )  __attribute__((__nonnull__(1))) ;
#line 852
extern  __attribute__((__nothrow__)) int sethostname(char const   *__name ,
                                                     size_t __len )  __attribute__((__nonnull__(1))) ;
#line 857
extern  __attribute__((__nothrow__)) int sethostid(long __id ) ;
#line 863
extern  __attribute__((__nothrow__)) int getdomainname(char *__name ,
                                                       size_t __len )  __attribute__((__nonnull__(1))) ;
#line 865
extern  __attribute__((__nothrow__)) int setdomainname(char const   *__name ,
                                                       size_t __len )  __attribute__((__nonnull__(1))) ;
#line 872
extern  __attribute__((__nothrow__)) int vhangup(void) ;
#line 875
extern  __attribute__((__nothrow__)) int revoke(char const   *__file )  __attribute__((__nonnull__(1))) ;
#line 883
extern  __attribute__((__nothrow__)) int profil(unsigned short *__sample_buffer ,
                                                size_t __size ,
                                                size_t __offset ,
                                                unsigned int __scale )  __attribute__((__nonnull__(1))) ;
#line 891
extern  __attribute__((__nothrow__)) int acct(char const   *__name ) ;
#line 895
extern  __attribute__((__nothrow__)) char *getusershell(void) ;
#line 896
extern  __attribute__((__nothrow__)) void endusershell(void) ;
#line 897
extern  __attribute__((__nothrow__)) void setusershell(void) ;
#line 903
extern  __attribute__((__nothrow__)) int daemon(int __nochdir , int __noclose ) ;
#line 910
extern  __attribute__((__nothrow__)) int chroot(char const   *__path )  __attribute__((__nonnull__(1))) ;
#line 914
extern char *getpass(char const   *__prompt )  __attribute__((__nonnull__(1))) ;
#line 923
extern int fsync(int __fd ) ;
#line 930
extern long gethostid(void) ;
#line 933
extern  __attribute__((__nothrow__)) void sync(void) ;
#line 938
extern  __attribute__((__nothrow__)) int getpagesize(void)  __attribute__((__const__)) ;
#line 943
extern  __attribute__((__nothrow__)) int getdtablesize(void) ;
#line 948
extern  __attribute__((__nothrow__)) int truncate(char const   *__file ,
                                                  __off_t __length )  __attribute__((__nonnull__(1))) ;
#line 970
extern  __attribute__((__nothrow__)) int ftruncate(int __fd , __off_t __length ) ;
#line 990
extern  __attribute__((__nothrow__)) int brk(void *__addr ) ;
#line 996
extern  __attribute__((__nothrow__)) void *sbrk(intptr_t __delta ) ;
#line 1011
extern  __attribute__((__nothrow__)) long syscall(long __sysno  , ...) ;
#line 1065
extern int fdatasync(int __fildes ) ;
#line 207 "/usr/include/sys/stat.h"
__inline extern  __attribute__((__nothrow__)) int stat(char const   * __restrict  __path ,
                                                       struct stat * __restrict  __statbuf )  __attribute__((__nonnull__(1,2))) ;
#line 212
__inline extern  __attribute__((__nothrow__)) int fstat(int __fd ,
                                                        struct stat *__statbuf )  __attribute__((__nonnull__(2))) ;
#line 259
__inline extern  __attribute__((__nothrow__)) int lstat(char const   * __restrict  __path ,
                                                        struct stat * __restrict  __statbuf )  __attribute__((__nonnull__(1,2))) ;
#line 280
extern  __attribute__((__nothrow__)) int chmod(char const   *__file ,
                                               __mode_t __mode )  __attribute__((__nonnull__(1))) ;
#line 287
extern  __attribute__((__nothrow__)) int lchmod(char const   *__file ,
                                                __mode_t __mode )  __attribute__((__nonnull__(1))) ;
#line 293
extern  __attribute__((__nothrow__)) int fchmod(int __fd , __mode_t __mode ) ;
#line 307
extern  __attribute__((__nothrow__)) __mode_t umask(__mode_t __mask ) ;
#line 316
extern  __attribute__((__nothrow__)) int mkdir(char const   *__path ,
                                               __mode_t __mode )  __attribute__((__nonnull__(1))) ;
#line 331
__inline extern  __attribute__((__nothrow__)) int mknod(char const   *__path ,
                                                        __mode_t __mode ,
                                                        __dev_t __dev )  __attribute__((__nonnull__(1))) ;
#line 345
extern  __attribute__((__nothrow__)) int mkfifo(char const   *__path ,
                                                __mode_t __mode )  __attribute__((__nonnull__(1))) ;
#line 380
extern  __attribute__((__nothrow__)) int __fxstat(int __ver , int __fildes ,
                                                  struct stat *__stat_buf )  __attribute__((__nonnull__(3))) ;
#line 382
extern  __attribute__((__nothrow__)) int __xstat(int __ver ,
                                                 char const   *__filename ,
                                                 struct stat *__stat_buf )  __attribute__((__nonnull__(2,3))) ;
#line 384
extern  __attribute__((__nothrow__)) int __lxstat(int __ver ,
                                                  char const   *__filename ,
                                                  struct stat *__stat_buf )  __attribute__((__nonnull__(2,3))) ;
#line 386
extern  __attribute__((__nothrow__)) int __fxstatat(int __ver , int __fildes ,
                                                    char const   *__filename ,
                                                    struct stat *__stat_buf ,
                                                    int __flag )  __attribute__((__nonnull__(3,4))) ;
#line 423
extern  __attribute__((__nothrow__)) int __xmknod(int __ver ,
                                                  char const   *__path ,
                                                  __mode_t __mode ,
                                                  __dev_t *__dev )  __attribute__((__nonnull__(2,4))) ;
#line 426
extern  __attribute__((__nothrow__)) int __xmknodat(int __ver , int __fd ,
                                                    char const   *__path ,
                                                    __mode_t __mode ,
                                                    __dev_t *__dev )  __attribute__((__nonnull__(3,5))) ;
#line 433
__inline extern  __attribute__((__nothrow__)) int stat(char const   * __restrict  __path ,
                                                       struct stat * __restrict  __statbuf )  __attribute__((__nonnull__(1,2))) ;
#line 433 "/usr/include/sys/stat.h"
__inline extern int stat(char const   * __restrict  __path ,
                         struct stat * __restrict  __statbuf ) 
{ int tmp ;

  {
#line 436
  tmp = __xstat(1, (char const   *)__path, (struct stat *)__statbuf);
#line 436
  return (tmp);
}
}
#line 440
__inline extern  __attribute__((__nothrow__)) int lstat(char const   * __restrict  __path ,
                                                        struct stat * __restrict  __statbuf )  __attribute__((__nonnull__(1,2))) ;
#line 440 "/usr/include/sys/stat.h"
__inline extern int lstat(char const   * __restrict  __path ,
                          struct stat * __restrict  __statbuf ) 
{ int tmp ;

  {
#line 443
  tmp = __lxstat(1, (char const   *)__path, (struct stat *)__statbuf);
#line 443
  return (tmp);
}
}
#line 447
__inline extern  __attribute__((__nothrow__)) int fstat(int __fd ,
                                                        struct stat *__statbuf )  __attribute__((__nonnull__(2))) ;
#line 447 "/usr/include/sys/stat.h"
__inline extern int fstat(int __fd , struct stat *__statbuf ) 
{ int tmp ;

  {
#line 450
  tmp = __fxstat(1, __fd, __statbuf);
#line 450
  return (tmp);
}
}
#line 463
__inline extern  __attribute__((__nothrow__)) int mknod(char const   *__path ,
                                                        __mode_t __mode ,
                                                        __dev_t __dev )  __attribute__((__nonnull__(1))) ;
#line 463 "/usr/include/sys/stat.h"
__inline extern int mknod(char const   *__path , __mode_t __mode ,
                          __dev_t __dev ) 
{ int tmp ;

  {
#line 466
  tmp = __xmknod(0, __path, __mode, & __dev);
#line 466
  return (tmp);
}
}
#line 49 "/usr/include/sys/times.h"
extern  __attribute__((__nothrow__)) clock_t times(struct tms *__buffer ) ;
#line 5179 "bzip2.c"
Int32 verbosity  ;
#line 5180 "bzip2.c"
Bool keepInputFiles  ;
#line 5180 "bzip2.c"
Bool smallMode  ;
#line 5180 "bzip2.c"
Bool deleteOutputOnInterrupt  ;
#line 5181 "bzip2.c"
Bool forceOverwrite  ;
#line 5181 "bzip2.c"
Bool testFailsExist  ;
#line 5181 "bzip2.c"
Bool unzFailsExist  ;
#line 5181 "bzip2.c"
Bool noisy  ;
#line 5182 "bzip2.c"
Int32 numFileNames  ;
#line 5182 "bzip2.c"
Int32 numFilesProcessed  ;
#line 5182 "bzip2.c"
Int32 blockSize100k  ;
#line 5183 "bzip2.c"
Int32 exitValue  ;
#line 5195 "bzip2.c"
Int32 opMode  ;
#line 5196 "bzip2.c"
Int32 srcMode  ;
#line 5200 "bzip2.c"
Int32 longestFileName  ;
#line 5201 "bzip2.c"
Char inName[1034]  ;
#line 5202 "bzip2.c"
Char outName[1034]  ;
#line 5203 "bzip2.c"
Char tmpName[1034]  ;
#line 5204 "bzip2.c"
Char *progName  ;
#line 5205 "bzip2.c"
Char progNameReally[1034]  ;
#line 5206 "bzip2.c"
FILE *outputHandleJustInCase  ;
#line 5207 "bzip2.c"
Int32 workFactor  ;
#line 5209
static  __attribute__((__noreturn__)) void panic(Char *s ) ;
#line 5210
static  __attribute__((__noreturn__)) void ioError(void) ;
#line 5211
static  __attribute__((__noreturn__)) void outOfMemory(void) ;
#line 5212
static  __attribute__((__noreturn__)) void configError(void) ;
#line 5213
static  __attribute__((__noreturn__)) void crcError(void) ;
#line 5214
static  __attribute__((__noreturn__)) void cleanUpAndFail(Int32 ec ) ;
#line 5215
static  __attribute__((__noreturn__)) void compressedStreamEOF(void) ;
#line 5217
static void copyFileName(Char *to , Char *from ) ;
#line 5218
static void *myMalloc(Int32 n ) ;
#line 5232 "bzip2.c"
static void uInt64_from_UInt32s(UInt64 *n , UInt32 lo32 , UInt32 hi32 ) 
{ 

  {
#line 5235
  n->b[7] = (unsigned char )((hi32 >> 24) & 255U);
#line 5236
  n->b[6] = (unsigned char )((hi32 >> 16) & 255U);
#line 5237
  n->b[5] = (unsigned char )((hi32 >> 8) & 255U);
#line 5238
  n->b[4] = (unsigned char )(hi32 & 255U);
#line 5239
  n->b[3] = (unsigned char )((lo32 >> 24) & 255U);
#line 5240
  n->b[2] = (unsigned char )((lo32 >> 16) & 255U);
#line 5241
  n->b[1] = (unsigned char )((lo32 >> 8) & 255U);
#line 5242
  n->b[0] = (unsigned char )(lo32 & 255U);
#line 5243
  return;
}
}
#line 5246 "bzip2.c"
static double uInt64_to_double(UInt64 *n ) 
{ Int32 i ;
  double base ;
  double sum ;

  {
#line 5250
  base = 1.0;
#line 5251
  sum = 0.0;
#line 5252
  i = 0;
#line 5252
  while (i < 8) {
#line 5253
    sum += base * (double )n->b[i];
#line 5254
    base *= 256.0;
#line 5252
    i ++;
  }
#line 5256
  return (sum);
}
}
#line 5260 "bzip2.c"
static Bool uInt64_isZero(UInt64 *n ) 
{ Int32 i ;

  {
#line 5264
  i = 0;
#line 5264
  while (i < 8) {
#line 5265
    if ((int )n->b[i] != 0) {
#line 5265
      return ((unsigned char)0);
    } else {

    }
#line 5264
    i ++;
  }
#line 5266
  return ((unsigned char)1);
}
}
#line 5271 "bzip2.c"
static Int32 uInt64_qrm10(UInt64 *n ) 
{ UInt32 rem ;
  UInt32 tmp ;
  Int32 i ;

  {
#line 5276
  rem = 0U;
#line 5277
  i = 7;
#line 5277
  while (i >= 0) {
#line 5278
    tmp = rem * 256U + (UInt32 )n->b[i];
#line 5279
    n->b[i] = (unsigned char )(tmp / 10U);
#line 5280
    rem = tmp % 10U;
#line 5277
    i --;
  }
#line 5282
  return ((int )rem);
}
}
#line 5289 "bzip2.c"
static void uInt64_toAscii(char *outbuf , UInt64 *n ) 
{ Int32 i ;
  Int32 q ;
  UChar buf[32] ;
  Int32 nBuf ;
  UInt64 n_copy ;
  Bool tmp ;

  {
#line 5294
  nBuf = 0;
#line 5295
  n_copy = *n;
#line 5296
  while (1) {
#line 5297
    q = uInt64_qrm10(& n_copy);
#line 5298
    buf[nBuf] = (unsigned char )(q + 48);
#line 5299
    nBuf ++;
#line 5296
    tmp = uInt64_isZero(& n_copy);
#line 5296
    if (tmp) {
#line 5296
      break;
    } else {

    }
  }
#line 5301
  *(outbuf + nBuf) = (char)0;
#line 5302
  i = 0;
#line 5302
  while (i < nBuf) {
#line 5303
    *(outbuf + i) = (char )buf[(nBuf - i) - 1];
#line 5302
    i ++;
  }
#line 5304
  return;
}
}
#line 5314 "bzip2.c"
static void compressStream(FILE *stream , FILE *zStream ) 
{ BZFILE *bzf ;
  UChar ibuf[5000] ;
  Int32 nIbuf ;
  UInt32 nbytes_in_lo32 ;
  UInt32 nbytes_in_hi32 ;
  UInt32 nbytes_out_lo32 ;
  UInt32 nbytes_out_hi32 ;
  Int32 bzerr ;
  Int32 bzerr_dummy ;
  Int32 ret ;
  int tmp ;
  int tmp___0 ;
  Bool tmp___1 ;
  size_t tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  Char buf_nin[32] ;
  Char buf_nout[32] ;
  UInt64 nbytes_in ;
  UInt64 nbytes_out ;
  double nbytes_in_d ;
  double nbytes_out_d ;

  {
#line 5317
  bzf = (void *)0;
#line 5327
  tmp = ferror(stream);
#line 5327
  if (tmp) {
    goto errhandler_io;
  } else {

  }
#line 5328
  tmp___0 = ferror(zStream);
#line 5328
  if (tmp___0) {
    goto errhandler_io;
  } else {

  }
#line 5330
  bzf = BZ2_bzWriteOpen(& bzerr, zStream, blockSize100k, verbosity, workFactor);
#line 5332
  if (bzerr != 0) {
    goto errhandler;
  } else {

  }
#line 5334
  if (verbosity >= 2) {
#line 5334
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n");
  } else {

  }
#line 5336
  while (1) {
#line 5338
    tmp___1 = myfeof(stream);
#line 5338
    if (tmp___1) {
#line 5338
      break;
    } else {

    }
#line 5339
    tmp___2 = fread((void */* __restrict  */)(ibuf), sizeof(UChar ), 5000UL,
                    (FILE */* __restrict  */)stream);
#line 5339
    nIbuf = (int )tmp___2;
#line 5340
    tmp___3 = ferror(stream);
#line 5340
    if (tmp___3) {
      goto errhandler_io;
    } else {

    }
#line 5341
    if (nIbuf > 0) {
#line 5341
      BZ2_bzWrite(& bzerr, bzf, (void *)(ibuf), nIbuf);
    } else {

    }
#line 5342
    if (bzerr != 0) {
      goto errhandler;
    } else {

    }
  }
#line 5346
  BZ2_bzWriteClose64(& bzerr, bzf, 0, & nbytes_in_lo32, & nbytes_in_hi32,
                     & nbytes_out_lo32, & nbytes_out_hi32);
#line 5349
  if (bzerr != 0) {
    goto errhandler;
  } else {

  }
#line 5351
  tmp___4 = ferror(zStream);
#line 5351
  if (tmp___4) {
    goto errhandler_io;
  } else {

  }
#line 5352
  ret = fflush(zStream);
#line 5353
  if (ret == -1) {
    goto errhandler_io;
  } else {

  }
#line 5354
  if ((unsigned long )zStream != (unsigned long )stdout) {
#line 5355
    ret = fclose(zStream);
#line 5356
    outputHandleJustInCase = (FILE *)((void *)0);
#line 5357
    if (ret == -1) {
      goto errhandler_io;
    } else {

    }
  } else {

  }
#line 5359
  outputHandleJustInCase = (FILE *)((void *)0);
#line 5360
  tmp___5 = ferror(stream);
#line 5360
  if (tmp___5) {
    goto errhandler_io;
  } else {

  }
#line 5361
  ret = fclose(stream);
#line 5362
  if (ret == -1) {
    goto errhandler_io;
  } else {

  }
#line 5364
  if (verbosity >= 1) {
#line 5365
    if (nbytes_in_lo32 == 0U) {
#line 5365
      if (nbytes_in_hi32 == 0U) {
#line 5366
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)" no data compressed.\n");
      } else {
        goto _L;
      }
    } else {
      _L: /* CIL Label */ 
#line 5371
      uInt64_from_UInt32s(& nbytes_in, nbytes_in_lo32, nbytes_in_hi32);
#line 5373
      uInt64_from_UInt32s(& nbytes_out, nbytes_out_lo32, nbytes_out_hi32);
#line 5375
      nbytes_in_d = uInt64_to_double(& nbytes_in);
#line 5376
      nbytes_out_d = uInt64_to_double(& nbytes_out);
#line 5377
      uInt64_toAscii(buf_nin, & nbytes_in);
#line 5378
      uInt64_toAscii(buf_nout, & nbytes_out);
#line 5379
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%6.3f:1, %6.3f bits/byte, %5.2f%% saved, %s in, %s out.\n",
              nbytes_in_d / nbytes_out_d, (8.0 * nbytes_out_d) / nbytes_in_d,
              100.0 * (1.0 - nbytes_out_d / nbytes_in_d), buf_nin, buf_nout);
    }
  } else {

  }
#line 5390
  return;
  errhandler: 
#line 5393
  BZ2_bzWriteClose64(& bzerr_dummy, bzf, 1, & nbytes_in_lo32, & nbytes_in_hi32,
                     & nbytes_out_lo32, & nbytes_out_hi32);
#line 5396
  switch (bzerr) {
  case -9: 
#line 5398
  configError();
#line 5398
  break;
  case -3: 
#line 5400
  outOfMemory();
#line 5400
  break;
  case -6: 
  errhandler_io: 
#line 5403
  ioError();
#line 5403
  break;
  default: 
#line 5405
  panic((Char *)"compress:unexpected error");
  }
#line 5408
  panic((Char *)"compress:end");
}
}
#line 5415 "bzip2.c"
static Bool uncompressStream(FILE *zStream , FILE *stream ) 
{ BZFILE *bzf ;
  Int32 bzerr ;
  Int32 bzerr_dummy ;
  Int32 ret ;
  Int32 nread ;
  Int32 streamNo ;
  Int32 i ;
  UChar obuf[5000] ;
  UChar unused[5000] ;
  Int32 nUnused ;
  UChar *unusedTmp ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  Bool tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  Bool tmp___5 ;
  size_t tmp___6 ;
  int tmp___7 ;
  int tmp___8 ;

  {
#line 5418
  bzf = (void *)0;
#line 5425
  nUnused = 0;
#line 5426
  streamNo = 0;
#line 5431
  tmp = ferror(stream);
#line 5431
  if (tmp) {
    goto errhandler_io;
  } else {

  }
#line 5432
  tmp___0 = ferror(zStream);
#line 5432
  if (tmp___0) {
    goto errhandler_io;
  } else {

  }
#line 5434
  while (1) {
#line 5436
    bzf = BZ2_bzReadOpen(& bzerr, zStream, verbosity, (int )smallMode,
                         (void *)(unused), nUnused);
#line 5440
    if ((unsigned long )bzf == (unsigned long )((void *)0)) {
      goto errhandler;
    } else {
#line 5440
      if (bzerr != 0) {
        goto errhandler;
      } else {

      }
    }
#line 5441
    streamNo ++;
#line 5443
    while (bzerr == 0) {
#line 5444
      nread = BZ2_bzRead(& bzerr, bzf, (void *)(obuf), 5000);
#line 5445
      if (bzerr == -5) {
        goto trycat;
      } else {

      }
#line 5446
      if (bzerr == 0) {
        goto _L;
      } else {
#line 5446
        if (bzerr == 4) {
          _L: /* CIL Label */ 
#line 5446
          if (nread > 0) {
#line 5447
            fwrite((void const   */* __restrict  */)(obuf), sizeof(UChar ),
                   (unsigned long )nread, (FILE */* __restrict  */)stream);
          } else {

          }
        } else {

        }
      }
#line 5448
      tmp___1 = ferror(stream);
#line 5448
      if (tmp___1) {
        goto errhandler_io;
      } else {

      }
    }
#line 5450
    if (bzerr != 4) {
      goto errhandler;
    } else {

    }
#line 5452
    BZ2_bzReadGetUnused(& bzerr, bzf, (void **)(& unusedTmp), & nUnused);
#line 5453
    if (bzerr != 0) {
#line 5453
      panic((Char *)"decompress:bzReadGetUnused");
    } else {

    }
#line 5455
    i = 0;
#line 5455
    while (i < nUnused) {
#line 5455
      unused[i] = *(unusedTmp + i);
#line 5455
      i ++;
    }
#line 5457
    BZ2_bzReadClose(& bzerr, bzf);
#line 5458
    if (bzerr != 0) {
#line 5458
      panic((Char *)"decompress:bzReadGetUnused");
    } else {

    }
#line 5460
    if (nUnused == 0) {
#line 5460
      tmp___2 = myfeof(zStream);
#line 5460
      if (tmp___2) {
#line 5460
        break;
      } else {

      }
    } else {

    }
  }
  closeok: 
#line 5464
  tmp___3 = ferror(zStream);
#line 5464
  if (tmp___3) {
    goto errhandler_io;
  } else {

  }
#line 5465
  ret = fclose(zStream);
#line 5466
  if (ret == -1) {
    goto errhandler_io;
  } else {

  }
#line 5468
  tmp___4 = ferror(stream);
#line 5468
  if (tmp___4) {
    goto errhandler_io;
  } else {

  }
#line 5469
  ret = fflush(stream);
#line 5470
  if (ret != 0) {
    goto errhandler_io;
  } else {

  }
#line 5471
  if ((unsigned long )stream != (unsigned long )stdout) {
#line 5472
    ret = fclose(stream);
#line 5473
    outputHandleJustInCase = (FILE *)((void *)0);
#line 5474
    if (ret == -1) {
      goto errhandler_io;
    } else {

    }
  } else {

  }
#line 5476
  outputHandleJustInCase = (FILE *)((void *)0);
#line 5477
  if (verbosity >= 2) {
#line 5477
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n    ");
  } else {

  }
#line 5478
  return ((unsigned char)1);
  trycat: 
#line 5481
  if (forceOverwrite) {
#line 5482
    rewind(zStream);
#line 5483
    while (1) {
#line 5484
      tmp___5 = myfeof(zStream);
#line 5484
      if (tmp___5) {
#line 5484
        break;
      } else {

      }
#line 5485
      tmp___6 = fread((void */* __restrict  */)(obuf), sizeof(UChar ), 5000UL,
                      (FILE */* __restrict  */)zStream);
#line 5485
      nread = (int )tmp___6;
#line 5486
      tmp___7 = ferror(zStream);
#line 5486
      if (tmp___7) {
        goto errhandler_io;
      } else {

      }
#line 5487
      if (nread > 0) {
#line 5487
        fwrite((void const   */* __restrict  */)(obuf), sizeof(UChar ),
               (unsigned long )nread, (FILE */* __restrict  */)stream);
      } else {

      }
#line 5488
      tmp___8 = ferror(stream);
#line 5488
      if (tmp___8) {
        goto errhandler_io;
      } else {

      }
    }
    goto closeok;
  } else {

  }
  errhandler: 
#line 5494
  BZ2_bzReadClose(& bzerr_dummy, bzf);
#line 5495
  switch (bzerr) {
  case -9: 
#line 5497
  configError();
#line 5497
  break;
  case -6: 
  errhandler_io: 
#line 5500
  ioError();
#line 5500
  break;
  case -4: 
#line 5502
  crcError();
  case -3: 
#line 5504
  outOfMemory();
  case -7: 
#line 5506
  compressedStreamEOF();
  case -5: 
#line 5508
  if ((unsigned long )zStream != (unsigned long )stdin) {
#line 5508
    fclose(zStream);
  } else {

  }
#line 5509
  if ((unsigned long )stream != (unsigned long )stdout) {
#line 5509
    fclose(stream);
  } else {

  }
#line 5510
  if (streamNo == 1) {
#line 5511
    return ((unsigned char)0);
  } else {
#line 5513
    if (noisy) {
#line 5514
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"\n%s: %s: trailing garbage after EOF ignored\n",
              progName, inName);
    } else {

    }
#line 5517
    return ((unsigned char)1);
  }
  default: 
#line 5520
  panic((Char *)"decompress:unexpected error");
  }
#line 5523
  panic((Char *)"decompress:end");
#line 5524
  return ((unsigned char)1);
}
}
#line 5529 "bzip2.c"
static Bool testStream(FILE *zStream ) 
{ BZFILE *bzf ;
  Int32 bzerr ;
  Int32 bzerr_dummy ;
  Int32 ret ;
  Int32 nread ;
  Int32 streamNo ;
  Int32 i ;
  UChar obuf[5000] ;
  UChar unused[5000] ;
  Int32 nUnused ;
  UChar *unusedTmp ;
  int tmp ;
  Bool tmp___0 ;
  int tmp___1 ;

  {
#line 5532
  bzf = (void *)0;
#line 5539
  nUnused = 0;
#line 5540
  streamNo = 0;
#line 5543
  tmp = ferror(zStream);
#line 5543
  if (tmp) {
    goto errhandler_io;
  } else {

  }
#line 5545
  while (1) {
#line 5547
    bzf = BZ2_bzReadOpen(& bzerr, zStream, verbosity, (int )smallMode,
                         (void *)(unused), nUnused);
#line 5551
    if ((unsigned long )bzf == (unsigned long )((void *)0)) {
      goto errhandler;
    } else {
#line 5551
      if (bzerr != 0) {
        goto errhandler;
      } else {

      }
    }
#line 5552
    streamNo ++;
#line 5554
    while (bzerr == 0) {
#line 5555
      nread = BZ2_bzRead(& bzerr, bzf, (void *)(obuf), 5000);
#line 5556
      if (bzerr == -5) {
        goto errhandler;
      } else {

      }
    }
#line 5558
    if (bzerr != 4) {
      goto errhandler;
    } else {

    }
#line 5560
    BZ2_bzReadGetUnused(& bzerr, bzf, (void **)(& unusedTmp), & nUnused);
#line 5561
    if (bzerr != 0) {
#line 5561
      panic((Char *)"test:bzReadGetUnused");
    } else {

    }
#line 5563
    i = 0;
#line 5563
    while (i < nUnused) {
#line 5563
      unused[i] = *(unusedTmp + i);
#line 5563
      i ++;
    }
#line 5565
    BZ2_bzReadClose(& bzerr, bzf);
#line 5566
    if (bzerr != 0) {
#line 5566
      panic((Char *)"test:bzReadGetUnused");
    } else {

    }
#line 5567
    if (nUnused == 0) {
#line 5567
      tmp___0 = myfeof(zStream);
#line 5567
      if (tmp___0) {
#line 5567
        break;
      } else {

      }
    } else {

    }
  }
#line 5571
  tmp___1 = ferror(zStream);
#line 5571
  if (tmp___1) {
    goto errhandler_io;
  } else {

  }
#line 5572
  ret = fclose(zStream);
#line 5573
  if (ret == -1) {
    goto errhandler_io;
  } else {

  }
#line 5575
  if (verbosity >= 2) {
#line 5575
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n    ");
  } else {

  }
#line 5576
  return ((unsigned char)1);
  errhandler: 
#line 5579
  BZ2_bzReadClose(& bzerr_dummy, bzf);
#line 5580
  if (verbosity == 0) {
#line 5581
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: %s: ", progName, inName);
  } else {

  }
#line 5582
  switch (bzerr) {
  case -9: 
#line 5584
  configError();
#line 5584
  break;
  case -6: 
  errhandler_io: 
#line 5587
  ioError();
#line 5587
  break;
  case -4: 
#line 5589
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"data integrity (CRC) error in data\n");
#line 5591
  return ((unsigned char)0);
  case -3: 
#line 5593
  outOfMemory();
  case -7: 
#line 5595
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"file ends unexpectedly\n");
#line 5597
  return ((unsigned char)0);
  case -5: 
#line 5599
  if ((unsigned long )zStream != (unsigned long )stdin) {
#line 5599
    fclose(zStream);
  } else {

  }
#line 5600
  if (streamNo == 1) {
#line 5601
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"bad magic number (file not created by bzip2)\n");
#line 5603
    return ((unsigned char)0);
  } else {
#line 5605
    if (noisy) {
#line 5606
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"trailing garbage after EOF ignored\n");
    } else {

    }
#line 5608
    return ((unsigned char)1);
  }
  default: 
#line 5611
  panic((Char *)"test:unexpected error");
  }
#line 5614
  panic((Char *)"test:end");
#line 5615
  return ((unsigned char)1);
}
}
#line 5624 "bzip2.c"
static void setExit(Int32 v ) 
{ 

  {
#line 5627
  if (v > exitValue) {
#line 5627
    exitValue = v;
  } else {

  }
#line 5628
  return;
}
}
#line 5632 "bzip2.c"
static void cadvise(void) 
{ 

  {
#line 5635
  if (noisy) {
#line 5636
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\nIt is possible that the compressed file(s) have become corrupted.\nYou can use the -tvv option to test integrity of such files.\n\nYou can use the `bzip2recover\' program to attempt to recover\ndata from undamaged sections of corrupted files.\n\n");
  } else {

  }
#line 5643
  return;
}
}
#line 5647 "bzip2.c"
static void showFileNames(void) 
{ 

  {
#line 5650
  if (noisy) {
#line 5651
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\tInput file = %s, output file = %s\n",
            inName, outName);
  } else {

  }
#line 5656
  return;
}
}
#line 5660
static  __attribute__((__noreturn__)) void cleanUpAndFail(Int32 ec ) ;
#line 5660 "bzip2.c"
static void cleanUpAndFail(Int32 ec ) 
{ IntNative retVal ;
  struct stat statBuf ;

  {
#line 5666
  if (srcMode == 3) {
#line 5666
    if (opMode != 3) {
#line 5666
      if (deleteOutputOnInterrupt) {
#line 5675
        retVal = stat((char const   */* __restrict  */)(inName),
                      (struct stat */* __restrict  */)(& statBuf));
#line 5676
        if (retVal == 0) {
#line 5677
          if (noisy) {
#line 5678
            fprintf((FILE */* __restrict  */)stderr,
                    (char const   */* __restrict  */)"%s: Deleting output file %s, if it exists.\n",
                    progName, outName);
          } else {

          }
#line 5681
          if ((unsigned long )outputHandleJustInCase != (unsigned long )((void *)0)) {
#line 5682
            fclose(outputHandleJustInCase);
          } else {

          }
#line 5683
          retVal = remove((char const   *)(outName));
#line 5684
          if (retVal != 0) {
#line 5685
            fprintf((FILE */* __restrict  */)stderr,
                    (char const   */* __restrict  */)"%s: WARNING: deletion of output file (apparently) failed.\n",
                    progName);
          } else {

          }
        } else {
#line 5690
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"%s: WARNING: deletion of output file suppressed\n",
                  progName);
#line 5693
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"%s:    since input file no longer exists.  Output file\n",
                  progName);
#line 5696
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"%s:    `%s\' may be incomplete.\n",
                  progName, outName);
#line 5699
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"%s:    I suggest doing an integrity test (bzip2 -tv) of it.\n",
                  progName);
        }
      } else {

      }
    } else {

    }
  } else {

  }
#line 5706
  if (noisy) {
#line 5706
    if (numFileNames > 0) {
#line 5706
      if (numFilesProcessed < numFileNames) {
#line 5707
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: WARNING: some files have not been processed:\n%s:    %d specified on command line, %d not processed yet.\n\n",
                progName, progName, numFileNames,
                numFileNames - numFilesProcessed);
      } else {

      }
    } else {

    }
  } else {

  }
#line 5713
  setExit(ec);
#line 5714
  exit(exitValue);
}
}
#line 5719
static  __attribute__((__noreturn__)) void panic(Char *s ) ;
#line 5719 "bzip2.c"
static void panic(Char *s ) 
{ 

  {
#line 5722
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"\n%s: PANIC -- internal consistency error:\n\t%s\n\tThis is a BUG.  Please report it to me at:\n\tjseward@acm.org\n",
          progName, s);
#line 5728
  showFileNames();
#line 5729
  cleanUpAndFail(3);
}
}
#line 5734
static  __attribute__((__noreturn__)) void crcError(void) ;
#line 5734 "bzip2.c"
static void crcError(void) 
{ 

  {
#line 5737
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"\n%s: Data integrity error when decompressing.\n",
          progName);
#line 5740
  showFileNames();
#line 5741
  cadvise();
#line 5742
  cleanUpAndFail(2);
}
}
#line 5747
static  __attribute__((__noreturn__)) void compressedStreamEOF(void) ;
#line 5747 "bzip2.c"
static void compressedStreamEOF(void) 
{ 

  {
#line 5750
  if (noisy) {
#line 5751
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n%s: Compressed file ends unexpectedly;\n\tperhaps it is corrupted?  *Possible* reason follows.\n",
            progName);
#line 5755
    perror((char const   *)progName);
#line 5756
    showFileNames();
#line 5757
    cadvise();
  } else {

  }
#line 5759
  cleanUpAndFail(2);
}
}
#line 5764
static  __attribute__((__noreturn__)) void ioError(void) ;
#line 5764 "bzip2.c"
static void ioError(void) 
{ 

  {
#line 5767
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"\n%s: I/O or other error, bailing out.  Possible reason follows.\n",
          progName);
#line 5771
  perror((char const   *)progName);
#line 5772
  showFileNames();
#line 5773
  cleanUpAndFail(1);
}
}
#line 5778 "bzip2.c"
static void mySignalCatcher(IntNative n ) 
{ 

  {
#line 5781
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"\n%s: Control-C or similar caught, quitting.\n",
          progName);
#line 5784
  cleanUpAndFail(1);
}
}
#line 5789 "bzip2.c"
static void mySIGSEGVorSIGBUScatcher(IntNative n ) 
{ 

  {
#line 5792
  if (opMode == 1) {
#line 5793
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n%s: Caught a SIGSEGV or SIGBUS whilst compressing.\n\n   Possible causes are (most likely first):\n   (1) This computer has unreliable memory or cache hardware\n       (a surprisingly common problem; try a different machine.)\n   (2) A bug in the compiler used to create this executable\n       (unlikely, if you didn\'t compile bzip2 yourself.)\n   (3) A real bug in bzip2 -- I hope this should never be the case.\n   The user\'s manual, Section 4.3, has more info on (1) and (2).\n   \n   If you suspect this is a bug in bzip2, or are unsure about (1)\n   or (2), feel free to report it to me at: jseward@acm.org.\n   Section 4.3 of the user\'s manual describes the info a useful\n   bug report should have.  If the manual is available on your\n   system, please try and read it before mailing me.  If you don\'t\n   have the manual or can\'t be bothered to read it, mail me anyway.\n\n",
            progName);
  } else {
#line 5814
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"\n%s: Caught a SIGSEGV or SIGBUS whilst decompressing.\n\n   Possible causes are (most likely first):\n   (1) The compressed data is corrupted, and bzip2\'s usual checks\n       failed to detect this.  Try bzip2 -tvv my_file.bz2.\n   (2) This computer has unreliable memory or cache hardware\n       (a surprisingly common problem; try a different machine.)\n   (3) A bug in the compiler used to create this executable\n       (unlikely, if you didn\'t compile bzip2 yourself.)\n   (4) A real bug in bzip2 -- I hope this should never be the case.\n   The user\'s manual, Section 4.3, has more info on (2) and (3).\n   \n   If you suspect this is a bug in bzip2, or are unsure about (2)\n   or (3), feel free to report it to me at: jseward@acm.org.\n   Section 4.3 of the user\'s manual describes the info a useful\n   bug report should have.  If the manual is available on your\n   system, please try and read it before mailing me.  If you don\'t\n   have the manual or can\'t be bothered to read it, mail me anyway.\n\n",
            progName);
  }
#line 5837
  showFileNames();
#line 5838
  if (opMode == 1) {
#line 5839
    cleanUpAndFail(3);
  } else {
#line 5840
    cadvise();
#line 5840
    cleanUpAndFail(2);
  }
}
}
#line 5845
static  __attribute__((__noreturn__)) void outOfMemory(void) ;
#line 5845 "bzip2.c"
static void outOfMemory(void) 
{ 

  {
#line 5848
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"\n%s: couldn\'t allocate enough memory\n",
          progName);
#line 5851
  showFileNames();
#line 5852
  cleanUpAndFail(1);
}
}
#line 5857
static  __attribute__((__noreturn__)) void configError(void) ;
#line 5857 "bzip2.c"
static void configError(void) 
{ 

  {
#line 5860
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"bzip2: I\'m not configured correctly for this platform!\n\tI require Int32, Int16 and Char to have sizes\n\tof 4, 2 and 1 bytes to run properly, and they don\'t.\n\tProbably you can fix this by defining them correctly,\n\tand recompiling.  Bye!\n");
#line 5866
  setExit(3);
#line 5867
  exit(exitValue);
}
}
#line 5881 "bzip2.c"
static void pad(Char *s ) 
{ Int32 i ;
  size_t tmp ;
  size_t tmp___0 ;

  {
#line 5885
  tmp = strlen((char const   *)s);
#line 5885
  if ((int )tmp >= longestFileName) {
#line 5885
    return;
  } else {

  }
#line 5886
  i = 1;
#line 5886
  while (1) {
#line 5886
    tmp___0 = strlen((char const   *)s);
#line 5886
    if (i <= longestFileName - (int )tmp___0) {

    } else {
#line 5886
      break;
    }
#line 5887
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)" ");
#line 5886
    i ++;
  }
#line 5888
  return;
}
}
#line 5892 "bzip2.c"
static void copyFileName(Char *to , Char *from ) 
{ size_t tmp ;

  {
#line 5895
  tmp = strlen((char const   *)from);
#line 5895
  if (tmp > 1024UL) {
#line 5896
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"bzip2: file name\n`%s\'\nis suspiciously (more than %d chars) long.\nTry using a reasonable file name instead.  Sorry! :-)\n",
            from, 1024);
#line 5903
    setExit(1);
#line 5904
    exit(exitValue);
  } else {

  }
#line 5907
  strncpy((char */* __restrict  */)to, (char const   */* __restrict  */)from,
          1024UL);
#line 5908
  *(to + 1024) = (char )'\000';
#line 5909
  return;
}
}
#line 5913 "bzip2.c"
static Bool fileExists(Char *name ) 
{ FILE *tmp ;
  FILE *tmp___0 ;
  Bool exists ;

  {
#line 5916
  tmp___0 = fopen((char const   */* __restrict  */)name,
                  (char const   */* __restrict  */)"rb");
#line 5916
  tmp = tmp___0;
#line 5917
  exists = (Bool )((unsigned long )tmp != (unsigned long )((void *)0));
#line 5918
  if ((unsigned long )tmp != (unsigned long )((void *)0)) {
#line 5918
    fclose(tmp);
  } else {

  }
#line 5919
  return (exists);
}
}
#line 5933 "bzip2.c"
FILE *fopen_output_safely(Char *name , char const   *mode ) 
{ FILE *fp ;
  IntNative fh ;

  {
#line 5938
  fh = open((char const   *)name, 193, 384);
#line 5939
  if (fh == -1) {
#line 5939
    return ((FILE *)((void *)0));
  } else {

  }
#line 5940
  fp = fdopen(fh, mode);
#line 5941
  if ((unsigned long )fp == (unsigned long )((void *)0)) {
#line 5941
    close(fh);
  } else {

  }
#line 5942
  return (fp);
}
}
#line 5953 "bzip2.c"
static Bool notAStandardFile(Char *name ) 
{ IntNative i ;
  struct stat statBuf ;

  {
#line 5959
  i = lstat((char const   */* __restrict  */)name,
            (struct stat */* __restrict  */)(& statBuf));
#line 5960
  if (i != 0) {
#line 5960
    return ((unsigned char)1);
  } else {

  }
#line 5961
  if ((statBuf.st_mode & 61440U) == 32768U) {
#line 5961
    return ((unsigned char)0);
  } else {

  }
#line 5962
  return ((unsigned char)1);
}
}
#line 5970 "bzip2.c"
static Int32 countHardLinks(Char *name ) 
{ IntNative i ;
  struct stat statBuf ;

  {
#line 5976
  i = lstat((char const   */* __restrict  */)name,
            (struct stat */* __restrict  */)(& statBuf));
#line 5977
  if (i != 0) {
#line 5977
    return (0);
  } else {

  }
#line 5978
  return ((int )(statBuf.st_nlink - 1UL));
}
}
#line 6006 "bzip2.c"
static struct stat fileMetaInfo  ;
#line 6010 "bzip2.c"
static void saveInputFileMetaInfo(Char *srcName ) 
{ IntNative retVal ;

  {
#line 6016
  retVal = stat((char const   */* __restrict  */)srcName,
                (struct stat */* __restrict  */)(& fileMetaInfo));
#line 6017
  if (retVal != 0) {
#line 6017
    ioError();
  } else {

  }
#line 6019
  return;
}
}
#line 6022 "bzip2.c"
static void applySavedMetaInfoToOutputFile(Char *dstName ) 
{ IntNative retVal ;
  struct utimbuf uTimBuf ;

  {
#line 6029
  uTimBuf.actime = fileMetaInfo.st_atim.tv_sec;
#line 6030
  uTimBuf.modtime = fileMetaInfo.st_mtim.tv_sec;
#line 6032
  retVal = chmod((char const   *)dstName, fileMetaInfo.st_mode);
#line 6033
  if (retVal != 0) {
#line 6033
    ioError();
  } else {

  }
#line 6035
  retVal = utime((char const   *)dstName, (struct utimbuf  const  *)(& uTimBuf));
#line 6036
  if (retVal != 0) {
#line 6036
    ioError();
  } else {

  }
#line 6038
  retVal = chown((char const   *)dstName, fileMetaInfo.st_uid,
                 fileMetaInfo.st_gid);
#line 6043
  return;
}
}
#line 6047 "bzip2.c"
static Bool containsDubiousChars(Char *name ) 
{ 

  {
#line 6054
  return ((unsigned char)0);
}
}
#line 6069 "bzip2.c"
Char *zSuffix[4]  = {      (Char *)".bz2",      (Char *)".bz",      (Char *)".tbz2",      (Char *)".tbz"};
#line 6071 "bzip2.c"
Char *unzSuffix[4]  = {      (Char *)"",      (Char *)"",      (Char *)".tar",      (Char *)".tar"};
#line 6074 "bzip2.c"
static Bool hasSuffix(Char *s , Char *suffix ) 
{ Int32 ns ;
  size_t tmp ;
  Int32 nx ;
  size_t tmp___0 ;
  int tmp___1 ;

  {
#line 6077
  tmp = strlen((char const   *)s);
#line 6077
  ns = (Int32 )tmp;
#line 6078
  tmp___0 = strlen((char const   *)suffix);
#line 6078
  nx = (Int32 )tmp___0;
#line 6079
  if (ns < nx) {
#line 6079
    return ((unsigned char)0);
  } else {

  }
#line 6080
  tmp___1 = strcmp((char const   *)((s + ns) - nx), (char const   *)suffix);
#line 6080
  if (tmp___1 == 0) {
#line 6080
    return ((unsigned char)1);
  } else {

  }
#line 6081
  return ((unsigned char)0);
}
}
#line 6084 "bzip2.c"
static Bool mapSuffix(Char *name , Char *oldSuffix , Char *newSuffix ) 
{ Bool tmp ;
  size_t tmp___0 ;
  size_t tmp___1 ;

  {
#line 6088
  tmp = hasSuffix(name, oldSuffix);
#line 6088
  if (tmp) {

  } else {
#line 6088
    return ((unsigned char)0);
  }
#line 6089
  tmp___0 = strlen((char const   *)name);
#line 6089
  tmp___1 = strlen((char const   *)oldSuffix);
#line 6089
  *(name + (tmp___0 - tmp___1)) = (char)0;
#line 6090
  strcat((char */* __restrict  */)name,
         (char const   */* __restrict  */)newSuffix);
#line 6091
  return ((unsigned char)1);
}
}
#line 6096 "bzip2.c"
static void compress(Char *name ) 
{ FILE *inStr ;
  FILE *outStr ;
  Int32 n ;
  Int32 i ;
  struct stat statBuf ;
  Bool tmp ;
  int *tmp___0 ;
  char *tmp___1 ;
  Bool tmp___2 ;
  Bool tmp___3 ;
  Bool tmp___4 ;
  Bool tmp___5 ;
  char const   *tmp___6 ;
  int tmp___7 ;
  int tmp___8 ;
  int tmp___9 ;
  int tmp___10 ;
  int *tmp___11 ;
  char *tmp___12 ;
  int *tmp___13 ;
  char *tmp___14 ;
  int *tmp___15 ;
  char *tmp___16 ;
  IntNative retVal ;
  int tmp___17 ;

  {
#line 6104
  deleteOutputOnInterrupt = (unsigned char)0;
#line 6106
  if ((unsigned long )name == (unsigned long )((void *)0)) {
#line 6106
    if (srcMode != 1) {
#line 6107
      panic((Char *)"compress: bad modes\n");
    } else {

    }
  } else {

  }
#line 6109
  switch (srcMode) {
  case 1: 
#line 6111
  copyFileName(inName, (Char *)"(stdin)");
#line 6112
  copyFileName(outName, (Char *)"(stdout)");
#line 6113
  break;
  case 3: 
#line 6115
  copyFileName(inName, name);
#line 6116
  copyFileName(outName, name);
#line 6117
  strcat((char */* __restrict  */)(outName),
         (char const   */* __restrict  */)".bz2");
#line 6118
  break;
  case 2: 
#line 6120
  copyFileName(inName, name);
#line 6121
  copyFileName(outName, (Char *)"(stdout)");
#line 6122
  break;
  }
#line 6125
  if (srcMode != 1) {
#line 6125
    tmp = containsDubiousChars(inName);
#line 6125
    if (tmp) {
#line 6126
      if (noisy) {
#line 6127
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: There are no files matching `%s\'.\n",
                progName, inName);
      } else {

      }
#line 6129
      setExit(1);
#line 6130
      return;
    } else {

    }
  } else {

  }
#line 6132
  if (srcMode != 1) {
#line 6132
    tmp___2 = fileExists(inName);
#line 6132
    if (tmp___2) {

    } else {
#line 6133
      tmp___0 = __errno_location();
#line 6133
      tmp___1 = strerror(*tmp___0);
#line 6133
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%s: Can\'t open input file %s: %s.\n",
              progName, inName, tmp___1);
#line 6135
      setExit(1);
#line 6136
      return;
    }
  } else {

  }
#line 6138
  i = 0;
#line 6138
  while (i < 4) {
#line 6139
    tmp___3 = hasSuffix(inName, zSuffix[i]);
#line 6139
    if (tmp___3) {
#line 6140
      if (noisy) {
#line 6141
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: Input file %s already has %s suffix.\n",
                progName, inName, zSuffix[i]);
      } else {

      }
#line 6144
      setExit(1);
#line 6145
      return;
    } else {

    }
#line 6138
    i ++;
  }
#line 6148
  if (srcMode == 3) {
    goto _L;
  } else {
#line 6148
    if (srcMode == 2) {
      _L: /* CIL Label */ 
#line 6149
      stat((char const   */* __restrict  */)(inName),
           (struct stat */* __restrict  */)(& statBuf));
#line 6150
      if ((statBuf.st_mode & 61440U) == 16384U) {
#line 6151
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: Input file %s is a directory.\n",
                progName, inName);
#line 6154
        setExit(1);
#line 6155
        return;
      } else {

      }
    } else {

    }
  }
#line 6158
  if (srcMode == 3) {
#line 6158
    if (! forceOverwrite) {
#line 6158
      tmp___4 = notAStandardFile(inName);
#line 6158
      if (tmp___4) {
#line 6159
        if (noisy) {
#line 6160
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"%s: Input file %s is not a normal file.\n",
                  progName, inName);
        } else {

        }
#line 6162
        setExit(1);
#line 6163
        return;
      } else {

      }
    } else {

    }
  } else {

  }
#line 6165
  if (srcMode == 3) {
#line 6165
    tmp___5 = fileExists(outName);
#line 6165
    if (tmp___5) {
#line 6166
      if (forceOverwrite) {
#line 6167
        remove((char const   *)(outName));
      } else {
#line 6169
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: Output file %s already exists.\n",
                progName, outName);
#line 6171
        setExit(1);
#line 6172
        return;
      }
    } else {

    }
  } else {

  }
#line 6175
  if (srcMode == 3) {
#line 6175
    if (! forceOverwrite) {
#line 6175
      n = countHardLinks(inName);
#line 6175
      if (n > 0) {
#line 6177
        if (n > 1) {
#line 6177
          tmp___6 = "s";
        } else {
#line 6177
          tmp___6 = "";
        }
#line 6177
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: Input file %s has %d other link%s.\n",
                progName, inName, n, tmp___6);
#line 6179
        setExit(1);
#line 6180
        return;
      } else {

      }
    } else {

    }
  } else {

  }
#line 6183
  if (srcMode == 3) {
#line 6186
    saveInputFileMetaInfo(inName);
  } else {

  }
#line 6189
  switch (srcMode) {
  case 1: 
#line 6192
  inStr = stdin;
#line 6193
  outStr = stdout;
#line 6194
  tmp___7 = fileno(stdout);
#line 6194
  tmp___8 = isatty(tmp___7);
#line 6194
  if (tmp___8) {
#line 6195
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: I won\'t write compressed data to a terminal.\n",
            progName);
#line 6198
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: For help, type: `%s --help\'.\n",
            progName, progName);
#line 6200
    setExit(1);
#line 6201
    return;
  } else {

  }
#line 6203
  break;
  case 2: 
#line 6206
  inStr = fopen((char const   */* __restrict  */)(inName),
                (char const   */* __restrict  */)"rb");
#line 6207
  outStr = stdout;
#line 6208
  tmp___9 = fileno(stdout);
#line 6208
  tmp___10 = isatty(tmp___9);
#line 6208
  if (tmp___10) {
#line 6209
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: I won\'t write compressed data to a terminal.\n",
            progName);
#line 6212
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: For help, type: `%s --help\'.\n",
            progName, progName);
#line 6214
    if ((unsigned long )inStr != (unsigned long )((void *)0)) {
#line 6214
      fclose(inStr);
    } else {

    }
#line 6215
    setExit(1);
#line 6216
    return;
  } else {

  }
#line 6218
  if ((unsigned long )inStr == (unsigned long )((void *)0)) {
#line 6219
    tmp___11 = __errno_location();
#line 6219
    tmp___12 = strerror(*tmp___11);
#line 6219
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: Can\'t open input file %s: %s.\n",
            progName, inName, tmp___12);
#line 6221
    setExit(1);
#line 6222
    return;
  } else {

  }
#line 6224
  break;
  case 3: 
#line 6227
  inStr = fopen((char const   */* __restrict  */)(inName),
                (char const   */* __restrict  */)"rb");
#line 6228
  outStr = fopen_output_safely(outName, "wb");
#line 6229
  if ((unsigned long )outStr == (unsigned long )((void *)0)) {
#line 6230
    tmp___13 = __errno_location();
#line 6230
    tmp___14 = strerror(*tmp___13);
#line 6230
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: Can\'t create output file %s: %s.\n",
            progName, outName, tmp___14);
#line 6232
    if ((unsigned long )inStr != (unsigned long )((void *)0)) {
#line 6232
      fclose(inStr);
    } else {

    }
#line 6233
    setExit(1);
#line 6234
    return;
  } else {

  }
#line 6236
  if ((unsigned long )inStr == (unsigned long )((void *)0)) {
#line 6237
    tmp___15 = __errno_location();
#line 6237
    tmp___16 = strerror(*tmp___15);
#line 6237
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: Can\'t open input file %s: %s.\n",
            progName, inName, tmp___16);
#line 6239
    if ((unsigned long )outStr != (unsigned long )((void *)0)) {
#line 6239
      fclose(outStr);
    } else {

    }
#line 6240
    setExit(1);
#line 6241
    return;
  } else {

  }
#line 6243
  break;
  default: 
#line 6246
  panic((Char *)"compress: bad srcMode");
#line 6247
  break;
  }
#line 6250
  if (verbosity >= 1) {
#line 6251
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"  %s: ", inName);
#line 6252
    pad(inName);
#line 6253
    fflush(stderr);
  } else {

  }
#line 6257
  outputHandleJustInCase = outStr;
#line 6258
  deleteOutputOnInterrupt = (unsigned char)1;
#line 6259
  compressStream(inStr, outStr);
#line 6260
  outputHandleJustInCase = (FILE *)((void *)0);
#line 6263
  if (srcMode == 3) {
#line 6264
    applySavedMetaInfoToOutputFile(outName);
#line 6265
    deleteOutputOnInterrupt = (unsigned char)0;
#line 6266
    if (! keepInputFiles) {
#line 6267
      tmp___17 = remove((char const   *)(inName));
#line 6267
      retVal = tmp___17;
#line 6268
      if (retVal != 0) {
#line 6268
        ioError();
      } else {

      }
    } else {

    }
  } else {

  }
#line 6272
  deleteOutputOnInterrupt = (unsigned char)0;
#line 6273
  return;
}
}
#line 6277 "bzip2.c"
static void uncompress(Char *name ) 
{ FILE *inStr ;
  FILE *outStr ;
  Int32 n ;
  Int32 i ;
  Bool magicNumberOK ;
  Bool cantGuess ;
  struct stat statBuf ;
  Bool tmp ;
  Bool tmp___0 ;
  int *tmp___1 ;
  char *tmp___2 ;
  Bool tmp___3 ;
  Bool tmp___4 ;
  Bool tmp___5 ;
  char const   *tmp___6 ;
  int tmp___7 ;
  int tmp___8 ;
  int *tmp___9 ;
  char *tmp___10 ;
  int *tmp___11 ;
  char *tmp___12 ;
  int *tmp___13 ;
  char *tmp___14 ;
  IntNative retVal ;
  int tmp___15 ;
  IntNative retVal___0 ;
  int tmp___16 ;

  {
#line 6287
  deleteOutputOnInterrupt = (unsigned char)0;
#line 6289
  if ((unsigned long )name == (unsigned long )((void *)0)) {
#line 6289
    if (srcMode != 1) {
#line 6290
      panic((Char *)"uncompress: bad modes\n");
    } else {

    }
  } else {

  }
#line 6292
  cantGuess = (unsigned char)0;
#line 6293
  switch (srcMode) {
  case 1: 
#line 6295
  copyFileName(inName, (Char *)"(stdin)");
#line 6296
  copyFileName(outName, (Char *)"(stdout)");
#line 6297
  break;
  case 3: 
#line 6299
  copyFileName(inName, name);
#line 6300
  copyFileName(outName, name);
#line 6301
  i = 0;
#line 6301
  while (i < 4) {
#line 6302
    tmp = mapSuffix(outName, zSuffix[i], unzSuffix[i]);
#line 6302
    if (tmp) {
      goto zzz;
    } else {

    }
#line 6301
    i ++;
  }
#line 6304
  cantGuess = (unsigned char)1;
#line 6305
  strcat((char */* __restrict  */)(outName),
         (char const   */* __restrict  */)".out");
#line 6306
  break;
  case 2: 
#line 6308
  copyFileName(inName, name);
#line 6309
  copyFileName(outName, (Char *)"(stdout)");
#line 6310
  break;
  }
  zzz: 
#line 6314
  if (srcMode != 1) {
#line 6314
    tmp___0 = containsDubiousChars(inName);
#line 6314
    if (tmp___0) {
#line 6315
      if (noisy) {
#line 6316
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: There are no files matching `%s\'.\n",
                progName, inName);
      } else {

      }
#line 6318
      setExit(1);
#line 6319
      return;
    } else {

    }
  } else {

  }
#line 6321
  if (srcMode != 1) {
#line 6321
    tmp___3 = fileExists(inName);
#line 6321
    if (tmp___3) {

    } else {
#line 6322
      tmp___1 = __errno_location();
#line 6322
      tmp___2 = strerror(*tmp___1);
#line 6322
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%s: Can\'t open input file %s: %s.\n",
              progName, inName, tmp___2);
#line 6324
      setExit(1);
#line 6325
      return;
    }
  } else {

  }
#line 6327
  if (srcMode == 3) {
    goto _L;
  } else {
#line 6327
    if (srcMode == 2) {
      _L: /* CIL Label */ 
#line 6328
      stat((char const   */* __restrict  */)(inName),
           (struct stat */* __restrict  */)(& statBuf));
#line 6329
      if ((statBuf.st_mode & 61440U) == 16384U) {
#line 6330
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: Input file %s is a directory.\n",
                progName, inName);
#line 6333
        setExit(1);
#line 6334
        return;
      } else {

      }
    } else {

    }
  }
#line 6337
  if (srcMode == 3) {
#line 6337
    if (! forceOverwrite) {
#line 6337
      tmp___4 = notAStandardFile(inName);
#line 6337
      if (tmp___4) {
#line 6338
        if (noisy) {
#line 6339
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"%s: Input file %s is not a normal file.\n",
                  progName, inName);
        } else {

        }
#line 6341
        setExit(1);
#line 6342
        return;
      } else {

      }
    } else {

    }
  } else {

  }
#line 6344
  if (cantGuess) {
#line 6345
    if (noisy) {
#line 6346
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%s: Can\'t guess original name for %s -- using %s\n",
              progName, inName, outName);
    } else {

    }
  } else {

  }
#line 6351
  if (srcMode == 3) {
#line 6351
    tmp___5 = fileExists(outName);
#line 6351
    if (tmp___5) {
#line 6352
      if (forceOverwrite) {
#line 6353
        remove((char const   *)(outName));
      } else {
#line 6355
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: Output file %s already exists.\n",
                progName, outName);
#line 6357
        setExit(1);
#line 6358
        return;
      }
    } else {

    }
  } else {

  }
#line 6361
  if (srcMode == 3) {
#line 6361
    if (! forceOverwrite) {
#line 6361
      n = countHardLinks(inName);
#line 6361
      if (n > 0) {
#line 6363
        if (n > 1) {
#line 6363
          tmp___6 = "s";
        } else {
#line 6363
          tmp___6 = "";
        }
#line 6363
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: Input file %s has %d other link%s.\n",
                progName, inName, n, tmp___6);
#line 6365
        setExit(1);
#line 6366
        return;
      } else {

      }
    } else {

    }
  } else {

  }
#line 6369
  if (srcMode == 3) {
#line 6372
    saveInputFileMetaInfo(inName);
  } else {

  }
#line 6375
  switch (srcMode) {
  case 1: 
#line 6378
  inStr = stdin;
#line 6379
  outStr = stdout;
#line 6380
  tmp___7 = fileno(stdin);
#line 6380
  tmp___8 = isatty(tmp___7);
#line 6380
  if (tmp___8) {
#line 6381
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: I won\'t read compressed data from a terminal.\n",
            progName);
#line 6384
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: For help, type: `%s --help\'.\n",
            progName, progName);
#line 6386
    setExit(1);
#line 6387
    return;
  } else {

  }
#line 6389
  break;
  case 2: 
#line 6392
  inStr = fopen((char const   */* __restrict  */)(inName),
                (char const   */* __restrict  */)"rb");
#line 6393
  outStr = stdout;
#line 6394
  if ((unsigned long )inStr == (unsigned long )((void *)0)) {
#line 6395
    tmp___9 = __errno_location();
#line 6395
    tmp___10 = strerror(*tmp___9);
#line 6395
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: Can\'t open input file %s:%s.\n",
            progName, inName, tmp___10);
#line 6397
    if ((unsigned long )inStr != (unsigned long )((void *)0)) {
#line 6397
      fclose(inStr);
    } else {

    }
#line 6398
    setExit(1);
#line 6399
    return;
  } else {

  }
#line 6401
  break;
  case 3: 
#line 6404
  inStr = fopen((char const   */* __restrict  */)(inName),
                (char const   */* __restrict  */)"rb");
#line 6405
  outStr = fopen_output_safely(outName, "wb");
#line 6406
  if ((unsigned long )outStr == (unsigned long )((void *)0)) {
#line 6407
    tmp___11 = __errno_location();
#line 6407
    tmp___12 = strerror(*tmp___11);
#line 6407
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: Can\'t create output file %s: %s.\n",
            progName, outName, tmp___12);
#line 6409
    if ((unsigned long )inStr != (unsigned long )((void *)0)) {
#line 6409
      fclose(inStr);
    } else {

    }
#line 6410
    setExit(1);
#line 6411
    return;
  } else {

  }
#line 6413
  if ((unsigned long )inStr == (unsigned long )((void *)0)) {
#line 6414
    tmp___13 = __errno_location();
#line 6414
    tmp___14 = strerror(*tmp___13);
#line 6414
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: Can\'t open input file %s: %s.\n",
            progName, inName, tmp___14);
#line 6416
    if ((unsigned long )outStr != (unsigned long )((void *)0)) {
#line 6416
      fclose(outStr);
    } else {

    }
#line 6417
    setExit(1);
#line 6418
    return;
  } else {

  }
#line 6420
  break;
  default: 
#line 6423
  panic((Char *)"uncompress: bad srcMode");
#line 6424
  break;
  }
#line 6427
  if (verbosity >= 1) {
#line 6428
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"  %s: ", inName);
#line 6429
    pad(inName);
#line 6430
    fflush(stderr);
  } else {

  }
#line 6434
  outputHandleJustInCase = outStr;
#line 6435
  deleteOutputOnInterrupt = (unsigned char)1;
#line 6436
  magicNumberOK = uncompressStream(inStr, outStr);
#line 6437
  outputHandleJustInCase = (FILE *)((void *)0);
#line 6440
  if (magicNumberOK) {
#line 6441
    if (srcMode == 3) {
#line 6442
      applySavedMetaInfoToOutputFile(outName);
#line 6443
      deleteOutputOnInterrupt = (unsigned char)0;
#line 6444
      if (! keepInputFiles) {
#line 6445
        tmp___15 = remove((char const   *)(inName));
#line 6445
        retVal = tmp___15;
#line 6446
        if (retVal != 0) {
#line 6446
          ioError();
        } else {

        }
      } else {

      }
    } else {

    }
  } else {
#line 6450
    unzFailsExist = (unsigned char)1;
#line 6451
    deleteOutputOnInterrupt = (unsigned char)0;
#line 6452
    if (srcMode == 3) {
#line 6453
      tmp___16 = remove((char const   *)(outName));
#line 6453
      retVal___0 = tmp___16;
#line 6454
      if (retVal___0 != 0) {
#line 6454
        ioError();
      } else {

      }
    } else {

    }
  }
#line 6457
  deleteOutputOnInterrupt = (unsigned char)0;
#line 6459
  if (magicNumberOK) {
#line 6460
    if (verbosity >= 1) {
#line 6461
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"done\n");
    } else {

    }
  } else {
#line 6463
    setExit(2);
#line 6464
    if (verbosity >= 1) {
#line 6465
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"not a bzip2 file.\n");
    } else {
#line 6466
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%s: %s is not a bzip2 file.\n",
              progName, inName);
    }
  }
#line 6471
  return;
}
}
#line 6475 "bzip2.c"
static void testf(Char *name ) 
{ FILE *inStr ;
  Bool allOK ;
  struct stat statBuf ;
  Bool tmp ;
  int *tmp___0 ;
  char *tmp___1 ;
  Bool tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int *tmp___5 ;
  char *tmp___6 ;

  {
#line 6482
  deleteOutputOnInterrupt = (unsigned char)0;
#line 6484
  if ((unsigned long )name == (unsigned long )((void *)0)) {
#line 6484
    if (srcMode != 1) {
#line 6485
      panic((Char *)"testf: bad modes\n");
    } else {

    }
  } else {

  }
#line 6487
  copyFileName(outName, (Char *)"(none)");
#line 6488
  switch (srcMode) {
  case 1: 
#line 6489
  copyFileName(inName, (Char *)"(stdin)");
#line 6489
  break;
  case 3: 
#line 6490
  copyFileName(inName, name);
#line 6490
  break;
  case 2: 
#line 6491
  copyFileName(inName, name);
#line 6491
  break;
  }
#line 6494
  if (srcMode != 1) {
#line 6494
    tmp = containsDubiousChars(inName);
#line 6494
    if (tmp) {
#line 6495
      if (noisy) {
#line 6496
        fprintf((FILE */* __restrict  */)stderr,
                (char const   */* __restrict  */)"%s: There are no files matching `%s\'.\n",
                progName, inName);
      } else {

      }
#line 6498
      setExit(1);
#line 6499
      return;
    } else {

    }
  } else {

  }
#line 6501
  if (srcMode != 1) {
#line 6501
    tmp___2 = fileExists(inName);
#line 6501
    if (tmp___2) {

    } else {
#line 6502
      tmp___0 = __errno_location();
#line 6502
      tmp___1 = strerror(*tmp___0);
#line 6502
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%s: Can\'t open input %s: %s.\n",
              progName, inName, tmp___1);
#line 6504
      setExit(1);
#line 6505
      return;
    }
  } else {

  }
#line 6507
  if (srcMode != 1) {
#line 6508
    stat((char const   */* __restrict  */)(inName),
         (struct stat */* __restrict  */)(& statBuf));
#line 6509
    if ((statBuf.st_mode & 61440U) == 16384U) {
#line 6510
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%s: Input file %s is a directory.\n",
              progName, inName);
#line 6513
      setExit(1);
#line 6514
      return;
    } else {

    }
  } else {

  }
#line 6518
  switch (srcMode) {
  case 1: 
#line 6521
  tmp___3 = fileno(stdin);
#line 6521
  tmp___4 = isatty(tmp___3);
#line 6521
  if (tmp___4) {
#line 6522
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: I won\'t read compressed data from a terminal.\n",
            progName);
#line 6525
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: For help, type: `%s --help\'.\n",
            progName, progName);
#line 6527
    setExit(1);
#line 6528
    return;
  } else {

  }
#line 6530
  inStr = stdin;
#line 6531
  break;
  case 2: 
  case 3: 
#line 6534
  inStr = fopen((char const   */* __restrict  */)(inName),
                (char const   */* __restrict  */)"rb");
#line 6535
  if ((unsigned long )inStr == (unsigned long )((void *)0)) {
#line 6536
    tmp___5 = __errno_location();
#line 6536
    tmp___6 = strerror(*tmp___5);
#line 6536
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"%s: Can\'t open input file %s:%s.\n",
            progName, inName, tmp___6);
#line 6538
    setExit(1);
#line 6539
    return;
  } else {

  }
#line 6541
  break;
  default: 
#line 6544
  panic((Char *)"testf: bad srcMode");
#line 6545
  break;
  }
#line 6548
  if (verbosity >= 1) {
#line 6549
    fprintf((FILE */* __restrict  */)stderr,
            (char const   */* __restrict  */)"  %s: ", inName);
#line 6550
    pad(inName);
#line 6551
    fflush(stderr);
  } else {

  }
#line 6555
  outputHandleJustInCase = (FILE *)((void *)0);
#line 6556
  allOK = testStream(inStr);
#line 6558
  if (allOK) {
#line 6558
    if (verbosity >= 1) {
#line 6558
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"ok\n");
    } else {

    }
  } else {

  }
#line 6559
  if (! allOK) {
#line 6559
    testFailsExist = (unsigned char)1;
  } else {

  }
#line 6560
  return;
}
}
#line 6564 "bzip2.c"
static void license(void) 
{ char const   *tmp ;

  {
#line 6567
  tmp = BZ2_bzlibVersion();
#line 6567
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"bzip2, a block-sorting file compressor.  Version %s.\n   \n   Copyright (C) 1996-2002 by Julian Seward.\n   \n   This program is free software; you can redistribute it and/or modify\n   it under the terms set out in the LICENSE file, which is included\n   in the bzip2-1.0 source distribution.\n   \n   This program is distributed in the hope that it will be useful,\n   but WITHOUT ANY WARRANTY; without even the implied warranty of\n   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n   LICENSE file for more details.\n   \n",
          tmp);
#line 6585
  return;
}
}
#line 6589 "bzip2.c"
static void usage(Char *fullProgName ) 
{ char const   *tmp ;

  {
#line 6592
  tmp = BZ2_bzlibVersion();
#line 6592
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"bzip2, a block-sorting file compressor.  Version %s.\n\n   usage: %s [flags and input files in any order]\n\n   -h --help           print this message\n   -d --decompress     force decompression\n   -z --compress       force compression\n   -k --keep           keep (don\'t delete) input files\n   -f --force          overwrite existing output files\n   -t --test           test compressed file integrity\n   -c --stdout         output to standard out\n   -q --quiet          suppress noncritical error messages\n   -v --verbose        be verbose (a 2nd -v gives more)\n   -L --license        display software version & license\n   -V --version        display software version & license\n   -s --small          use less memory (at most 2500k)\n   -1 .. -9            set block size to 100k .. 900k\n   --fast              alias for -1\n   --best              alias for -9\n\n   If invoked as `bzip2\', default action is to compress.\n              as `bunzip2\',  default action is to decompress.\n              as `bzcat\', default action is to decompress to stdout.\n\n   If no file names are given, bzip2 compresses or decompresses\n   from standard input to standard output.  You can combine\n   short flags, so `-v -4\' means the same as -v4 or -4v, &c.\n\n",
          tmp, fullProgName);
#line 6629
  return;
}
}
#line 6633 "bzip2.c"
static void redundant(Char *flag ) 
{ 

  {
#line 6636
  fprintf((FILE */* __restrict  */)stderr,
          (char const   */* __restrict  */)"%s: %s is redundant in versions 0.9.5 and above\n",
          progName, flag);
#line 6640
  return;
}
}
#line 6667 "bzip2.c"
static void *myMalloc(Int32 n ) 
{ void *p ;

  {
#line 6672
  p = malloc((unsigned long )n);
#line 6673
  if ((unsigned long )p == (unsigned long )((void *)0)) {
#line 6673
    outOfMemory();
  } else {

  }
#line 6674
  return (p);
}
}
#line 6679 "bzip2.c"
static Cell *mkCell(void) 
{ Cell *c ;
  void *tmp ;

  {
#line 6684
  tmp = myMalloc((int )sizeof(Cell ));
#line 6684
  c = (Cell *)tmp;
#line 6685
  c->name = (Char *)((void *)0);
#line 6686
  c->link = (struct zzzz *)((void *)0);
#line 6687
  return (c);
}
}
#line 6692 "bzip2.c"
static Cell *snocString(Cell *root , Char *name ) 
{ Cell *tmp ;
  Cell *tmp___0 ;
  size_t tmp___1 ;
  void *tmp___2 ;
  Cell *tmp___3 ;

  {
#line 6695
  if ((unsigned long )root == (unsigned long )((void *)0)) {
#line 6696
    tmp___0 = mkCell();
#line 6696
    tmp = tmp___0;
#line 6697
    tmp___1 = strlen((char const   *)name);
#line 6697
    tmp___2 = myMalloc((int )(5UL + tmp___1));
#line 6697
    tmp->name = (Char *)tmp___2;
#line 6698
    strcpy((char */* __restrict  */)tmp->name,
           (char const   */* __restrict  */)name);
#line 6699
    return (tmp);
  } else {
#line 6701
    tmp___3 = root;
#line 6702
    while ((unsigned long )tmp___3->link != (unsigned long )((void *)0)) {
#line 6702
      tmp___3 = tmp___3->link;
    }
#line 6703
    tmp___3->link = snocString(tmp___3->link, name);
#line 6704
    return (root);
  }
}
}
#line 6710 "bzip2.c"
static void addFlagsFromEnvVar(Cell **argList , Char *varName ) 
{ Int32 i ;
  Int32 j ;
  Int32 k ;
  Char *envbase ;
  Char *p ;
  unsigned short const   **tmp ;
  unsigned short const   **tmp___0 ;

  {
#line 6716
  envbase = getenv((char const   *)varName);
#line 6717
  if ((unsigned long )envbase != (unsigned long )((void *)0)) {
#line 6718
    p = envbase;
#line 6719
    i = 0;
#line 6720
    while (! ((int )*(p + i) == 0)) {
#line 6722
      p += i;
#line 6723
      i = 0;
#line 6724
      while (1) {
#line 6724
        tmp = __ctype_b_loc();
#line 6724
        if ((int const   )*(*tmp + (int )*(p + 0)) & 8192) {

        } else {
#line 6724
          break;
        }
#line 6724
        p ++;
      }
#line 6725
      while (1) {
#line 6725
        if ((int )*(p + i) != 0) {
#line 6725
          tmp___0 = __ctype_b_loc();
#line 6725
          if ((int const   )*(*tmp___0 + (int )*(p + i)) & 8192) {
#line 6725
            break;
          } else {

          }
        } else {
#line 6725
          break;
        }
#line 6725
        i ++;
      }
#line 6726
      if (i > 0) {
#line 6727
        k = i;
#line 6727
        if (k > 1024) {
#line 6727
          k = 1024;
        } else {

        }
#line 6728
        j = 0;
#line 6728
        while (j < k) {
#line 6728
          tmpName[j] = *(p + j);
#line 6728
          j ++;
        }
#line 6729
        tmpName[k] = (char)0;
#line 6730
        *argList = snocString(*argList, tmpName);
      } else {

      }
    }
  } else {

  }
#line 6734
  return;
}
}
#line 6740 "bzip2.c"
IntNative main(IntNative argc , Char **argv ) 
{ Int32 i ;
  Int32 j ;
  Char *tmp ;
  Cell *argList ;
  Cell *aa ;
  Bool decode ;
  int tmp___0 ;
  size_t tmp___1 ;
  size_t tmp___2 ;
  char *tmp___3 ;
  char *tmp___4 ;
  char *tmp___5 ;
  char *tmp___6 ;
  char *tmp___7 ;
  char *tmp___8 ;
  int tmp___9 ;
  int tmp___10 ;
  int tmp___11 ;
  int tmp___12 ;
  int tmp___13 ;
  int tmp___14 ;
  int tmp___15 ;
  int tmp___16 ;
  int tmp___17 ;
  int tmp___18 ;
  int tmp___19 ;
  int tmp___20 ;
  int tmp___21 ;
  int tmp___22 ;
  int tmp___23 ;
  int tmp___24 ;
  int tmp___25 ;
  int tmp___26 ;
  int tmp___27 ;
  int tmp___28 ;
  int tmp___29 ;
  int tmp___30 ;
  int tmp___31 ;
  Cell *aa2 ;

  {
#line 6749
  if (sizeof(Int32 ) != 4UL) {
#line 6752
    configError();
  } else {
#line 6749
    if (sizeof(UInt32 ) != 4UL) {
#line 6752
      configError();
    } else {
#line 6749
      if (sizeof(Int16 ) != 2UL) {
#line 6752
        configError();
      } else {
#line 6749
        if (sizeof(UInt16 ) != 2UL) {
#line 6752
          configError();
        } else {
#line 6749
          if (sizeof(Char ) != 1UL) {
#line 6752
            configError();
          } else {
#line 6749
            if (sizeof(UChar ) != 1UL) {
#line 6752
              configError();
            } else {

            }
          }
        }
      }
    }
  }
#line 6755
  outputHandleJustInCase = (FILE *)((void *)0);
#line 6756
  smallMode = (unsigned char)0;
#line 6757
  keepInputFiles = (unsigned char)0;
#line 6758
  forceOverwrite = (unsigned char)0;
#line 6759
  noisy = (unsigned char)1;
#line 6760
  verbosity = 0;
#line 6761
  blockSize100k = 9;
#line 6762
  testFailsExist = (unsigned char)0;
#line 6763
  unzFailsExist = (unsigned char)0;
#line 6764
  numFileNames = 0;
#line 6765
  numFilesProcessed = 0;
#line 6766
  workFactor = 30;
#line 6767
  deleteOutputOnInterrupt = (unsigned char)0;
#line 6768
  exitValue = 0;
#line 6769
  j = 0;
#line 6769
  i = j;
#line 6772
  signal(11, & mySIGSEGVorSIGBUScatcher);
#line 6775
  signal(7, & mySIGSEGVorSIGBUScatcher);
#line 6779
  copyFileName(inName, (Char *)"(none)");
#line 6780
  copyFileName(outName, (Char *)"(none)");
#line 6782
  copyFileName(progNameReally, *(argv + 0));
#line 6783
  progName = & progNameReally[0];
#line 6784
  tmp = & progNameReally[0];
#line 6784
  while ((int )*tmp != 0) {
#line 6785
    if ((int )*tmp == 47) {
#line 6785
      progName = tmp + 1;
    } else {

    }
#line 6784
    tmp ++;
  }
#line 6791
  argList = (Cell *)((void *)0);
#line 6792
  addFlagsFromEnvVar(& argList, (Char *)"BZIP2");
#line 6793
  addFlagsFromEnvVar(& argList, (Char *)"BZIP");
#line 6794
  i = 1;
#line 6794
  while (i <= argc - 1) {
#line 6795
    argList = snocString(argList, *(argv + i));
#line 6794
    i ++;
  }
#line 6799
  longestFileName = 7;
#line 6800
  numFileNames = 0;
#line 6801
  decode = (unsigned char)1;
#line 6802
  aa = argList;
#line 6802
  while ((unsigned long )aa != (unsigned long )((void *)0)) {
#line 6803
    tmp___0 = strcmp((char const   *)aa->name, "--");
#line 6803
    if (tmp___0 == 0) {
#line 6803
      decode = (unsigned char)0;
      goto __Cont;
    } else {

    }
#line 6804
    if ((int )*(aa->name + 0) == 45) {
#line 6804
      if (decode) {
        goto __Cont;
      } else {

      }
    } else {

    }
#line 6805
    numFileNames ++;
#line 6806
    tmp___2 = strlen((char const   *)aa->name);
#line 6806
    if (longestFileName < (int )tmp___2) {
#line 6807
      tmp___1 = strlen((char const   *)aa->name);
#line 6807
      longestFileName = (int )tmp___1;
    } else {

    }
    __Cont: /* CIL Label */ 
#line 6802
    aa = aa->link;
  }
#line 6812
  if (numFileNames == 0) {
#line 6813
    srcMode = 1;
  } else {
#line 6813
    srcMode = 3;
  }
#line 6818
  opMode = 1;
#line 6820
  tmp___3 = strstr((char const   *)progName, "unzip");
#line 6820
  if ((unsigned long )tmp___3 != (unsigned long )((char *)0)) {
#line 6822
    opMode = 2;
  } else {
#line 6820
    tmp___4 = strstr((char const   *)progName, "UNZIP");
#line 6820
    if ((unsigned long )tmp___4 != (unsigned long )((char *)0)) {
#line 6822
      opMode = 2;
    } else {

    }
  }
#line 6824
  tmp___5 = strstr((char const   *)progName, "z2cat");
#line 6824
  if ((unsigned long )tmp___5 != (unsigned long )((char *)0)) {
    goto _L;
  } else {
#line 6824
    tmp___6 = strstr((char const   *)progName, "Z2CAT");
#line 6824
    if ((unsigned long )tmp___6 != (unsigned long )((char *)0)) {
      goto _L;
    } else {
#line 6824
      tmp___7 = strstr((char const   *)progName, "zcat");
#line 6824
      if ((unsigned long )tmp___7 != (unsigned long )((char *)0)) {
        goto _L;
      } else {
#line 6824
        tmp___8 = strstr((char const   *)progName, "ZCAT");
#line 6824
        if ((unsigned long )tmp___8 != (unsigned long )((char *)0)) {
          _L: /* CIL Label */ 
#line 6828
          opMode = 2;
#line 6829
          if (numFileNames == 0) {
#line 6829
            srcMode = 1;
          } else {
#line 6829
            srcMode = 2;
          }
        } else {

        }
      }
    }
  }
#line 6834
  aa = argList;
#line 6834
  while ((unsigned long )aa != (unsigned long )((void *)0)) {
#line 6835
    tmp___9 = strcmp((char const   *)aa->name, "--");
#line 6835
    if (tmp___9 == 0) {
#line 6835
      break;
    } else {

    }
#line 6836
    if ((int )*(aa->name + 0) == 45) {
#line 6836
      if ((int )*(aa->name + 1) != 45) {
#line 6837
        j = 1;
#line 6837
        while ((int )*(aa->name + j) != 0) {
#line 6838
          switch ((int )*(aa->name + j)) {
          case 99: 
#line 6839
          srcMode = 2;
#line 6839
          break;
          case 100: 
#line 6840
          opMode = 2;
#line 6840
          break;
          case 122: 
#line 6841
          opMode = 1;
#line 6841
          break;
          case 102: 
#line 6842
          forceOverwrite = (unsigned char)1;
#line 6842
          break;
          case 116: 
#line 6843
          opMode = 3;
#line 6843
          break;
          case 107: 
#line 6844
          keepInputFiles = (unsigned char)1;
#line 6844
          break;
          case 115: 
#line 6845
          smallMode = (unsigned char)1;
#line 6845
          break;
          case 113: 
#line 6846
          noisy = (unsigned char)0;
#line 6846
          break;
          case 49: 
#line 6847
          blockSize100k = 1;
#line 6847
          break;
          case 50: 
#line 6848
          blockSize100k = 2;
#line 6848
          break;
          case 51: 
#line 6849
          blockSize100k = 3;
#line 6849
          break;
          case 52: 
#line 6850
          blockSize100k = 4;
#line 6850
          break;
          case 53: 
#line 6851
          blockSize100k = 5;
#line 6851
          break;
          case 54: 
#line 6852
          blockSize100k = 6;
#line 6852
          break;
          case 55: 
#line 6853
          blockSize100k = 7;
#line 6853
          break;
          case 56: 
#line 6854
          blockSize100k = 8;
#line 6854
          break;
          case 57: 
#line 6855
          blockSize100k = 9;
#line 6855
          break;
          case 86: 
          case 76: 
#line 6857
          license();
#line 6857
          break;
          case 118: 
#line 6858
          verbosity ++;
#line 6858
          break;
          case 104: 
#line 6859
          usage(progName);
#line 6860
          exit(0);
#line 6861
          break;
          default: 
#line 6862
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"%s: Bad flag `%s\'\n",
                  progName, aa->name);
#line 6864
          usage(progName);
#line 6865
          exit(1);
#line 6866
          break;
          }
#line 6837
          j ++;
        }
      } else {

      }
    } else {

    }
#line 6834
    aa = aa->link;
  }
#line 6873
  aa = argList;
#line 6873
  while ((unsigned long )aa != (unsigned long )((void *)0)) {
#line 6874
    tmp___10 = strcmp((char const   *)aa->name, "--");
#line 6874
    if (tmp___10 == 0) {
#line 6874
      break;
    } else {

    }
#line 6875
    tmp___28 = strcmp((char const   *)aa->name, "--stdout");
#line 6875
    if (tmp___28 == 0) {
#line 6875
      srcMode = 2;
    } else {
#line 6876
      tmp___27 = strcmp((char const   *)aa->name, "--decompress");
#line 6876
      if (tmp___27 == 0) {
#line 6876
        opMode = 2;
      } else {
#line 6877
        tmp___26 = strcmp((char const   *)aa->name, "--compress");
#line 6877
        if (tmp___26 == 0) {
#line 6877
          opMode = 1;
        } else {
#line 6878
          tmp___25 = strcmp((char const   *)aa->name, "--force");
#line 6878
          if (tmp___25 == 0) {
#line 6878
            forceOverwrite = (unsigned char)1;
          } else {
#line 6879
            tmp___24 = strcmp((char const   *)aa->name, "--test");
#line 6879
            if (tmp___24 == 0) {
#line 6879
              opMode = 3;
            } else {
#line 6880
              tmp___23 = strcmp((char const   *)aa->name, "--keep");
#line 6880
              if (tmp___23 == 0) {
#line 6880
                keepInputFiles = (unsigned char)1;
              } else {
#line 6881
                tmp___22 = strcmp((char const   *)aa->name, "--small");
#line 6881
                if (tmp___22 == 0) {
#line 6881
                  smallMode = (unsigned char)1;
                } else {
#line 6882
                  tmp___21 = strcmp((char const   *)aa->name, "--quiet");
#line 6882
                  if (tmp___21 == 0) {
#line 6882
                    noisy = (unsigned char)0;
                  } else {
#line 6883
                    tmp___20 = strcmp((char const   *)aa->name, "--version");
#line 6883
                    if (tmp___20 == 0) {
#line 6883
                      license();
                    } else {
#line 6884
                      tmp___19 = strcmp((char const   *)aa->name, "--license");
#line 6884
                      if (tmp___19 == 0) {
#line 6884
                        license();
                      } else {
#line 6885
                        tmp___18 = strcmp((char const   *)aa->name,
                                          "--exponential");
#line 6885
                        if (tmp___18 == 0) {
#line 6885
                          workFactor = 1;
                        } else {
#line 6886
                          tmp___17 = strcmp((char const   *)aa->name,
                                            "--repetitive-best");
#line 6886
                          if (tmp___17 == 0) {
#line 6886
                            redundant(aa->name);
                          } else {
#line 6887
                            tmp___16 = strcmp((char const   *)aa->name,
                                              "--repetitive-fast");
#line 6887
                            if (tmp___16 == 0) {
#line 6887
                              redundant(aa->name);
                            } else {
#line 6888
                              tmp___15 = strcmp((char const   *)aa->name,
                                                "--fast");
#line 6888
                              if (tmp___15 == 0) {
#line 6888
                                blockSize100k = 1;
                              } else {
#line 6889
                                tmp___14 = strcmp((char const   *)aa->name,
                                                  "--best");
#line 6889
                                if (tmp___14 == 0) {
#line 6889
                                  blockSize100k = 9;
                                } else {
#line 6890
                                  tmp___13 = strcmp((char const   *)aa->name,
                                                    "--verbose");
#line 6890
                                  if (tmp___13 == 0) {
#line 6890
                                    verbosity ++;
                                  } else {
#line 6891
                                    tmp___12 = strcmp((char const   *)aa->name,
                                                      "--help");
#line 6891
                                    if (tmp___12 == 0) {
#line 6891
                                      usage(progName);
#line 6891
                                      exit(0);
                                    } else {
#line 6893
                                      tmp___11 = strncmp((char const   *)aa->name,
                                                         "--", 2UL);
#line 6893
                                      if (tmp___11 == 0) {
#line 6894
                                        fprintf((FILE */* __restrict  */)stderr,
                                                (char const   */* __restrict  */)"%s: Bad flag `%s\'\n",
                                                progName, aa->name);
#line 6895
                                        usage(progName);
#line 6896
                                        exit(1);
                                      } else {

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
                  }
                }
              }
            }
          }
        }
      }
    }
#line 6873
    aa = aa->link;
  }
#line 6900
  if (verbosity > 4) {
#line 6900
    verbosity = 4;
  } else {

  }
#line 6901
  if (opMode == 1) {
#line 6901
    if (smallMode) {
#line 6901
      if (blockSize100k > 2) {
#line 6902
        blockSize100k = 2;
      } else {

      }
    } else {

    }
  } else {

  }
#line 6904
  if (opMode == 3) {
#line 6904
    if (srcMode == 2) {
#line 6905
      fprintf((FILE */* __restrict  */)stderr,
              (char const   */* __restrict  */)"%s: -c and -t cannot be used together.\n",
              progName);
#line 6907
      exit(1);
    } else {

    }
  } else {

  }
#line 6910
  if (srcMode == 2) {
#line 6910
    if (numFileNames == 0) {
#line 6911
      srcMode = 1;
    } else {

    }
  } else {

  }
#line 6913
  if (opMode != 1) {
#line 6913
    blockSize100k = 0;
  } else {

  }
#line 6915
  if (srcMode == 3) {
#line 6916
    signal(2, & mySignalCatcher);
#line 6917
    signal(15, & mySignalCatcher);
#line 6919
    signal(1, & mySignalCatcher);
  } else {

  }
#line 6923
  if (opMode == 1) {
#line 6924
    if (srcMode == 1) {
#line 6925
      compress((Char *)((void *)0));
    } else {
#line 6927
      decode = (unsigned char)1;
#line 6928
      aa = argList;
#line 6928
      while ((unsigned long )aa != (unsigned long )((void *)0)) {
#line 6929
        tmp___29 = strcmp((char const   *)aa->name, "--");
#line 6929
        if (tmp___29 == 0) {
#line 6929
          decode = (unsigned char)0;
          goto __Cont___0;
        } else {

        }
#line 6930
        if ((int )*(aa->name + 0) == 45) {
#line 6930
          if (decode) {
            goto __Cont___0;
          } else {

          }
        } else {

        }
#line 6931
        numFilesProcessed ++;
#line 6932
        compress(aa->name);
        __Cont___0: /* CIL Label */ 
#line 6928
        aa = aa->link;
      }
    }
  } else {
#line 6938
    if (opMode == 2) {
#line 6939
      unzFailsExist = (unsigned char)0;
#line 6940
      if (srcMode == 1) {
#line 6941
        uncompress((Char *)((void *)0));
      } else {
#line 6943
        decode = (unsigned char)1;
#line 6944
        aa = argList;
#line 6944
        while ((unsigned long )aa != (unsigned long )((void *)0)) {
#line 6945
          tmp___30 = strcmp((char const   *)aa->name, "--");
#line 6945
          if (tmp___30 == 0) {
#line 6945
            decode = (unsigned char)0;
            goto __Cont___1;
          } else {

          }
#line 6946
          if ((int )*(aa->name + 0) == 45) {
#line 6946
            if (decode) {
              goto __Cont___1;
            } else {

            }
          } else {

          }
#line 6947
          numFilesProcessed ++;
#line 6948
          uncompress(aa->name);
          __Cont___1: /* CIL Label */ 
#line 6944
          aa = aa->link;
        }
      }
#line 6951
      if (unzFailsExist) {
#line 6952
        setExit(2);
#line 6953
        exit(exitValue);
      } else {

      }
    } else {
#line 6958
      testFailsExist = (unsigned char)0;
#line 6959
      if (srcMode == 1) {
#line 6960
        testf((Char *)((void *)0));
      } else {
#line 6962
        decode = (unsigned char)1;
#line 6963
        aa = argList;
#line 6963
        while ((unsigned long )aa != (unsigned long )((void *)0)) {
#line 6964
          tmp___31 = strcmp((char const   *)aa->name, "--");
#line 6964
          if (tmp___31 == 0) {
#line 6964
            decode = (unsigned char)0;
            goto __Cont___2;
          } else {

          }
#line 6965
          if ((int )*(aa->name + 0) == 45) {
#line 6965
            if (decode) {
              goto __Cont___2;
            } else {

            }
          } else {

          }
#line 6966
          numFilesProcessed ++;
#line 6967
          testf(aa->name);
          __Cont___2: /* CIL Label */ 
#line 6963
          aa = aa->link;
        }
      }
#line 6970
      if (testFailsExist) {
#line 6970
        if (noisy) {
#line 6971
          fprintf((FILE */* __restrict  */)stderr,
                  (char const   */* __restrict  */)"\nYou can use the `bzip2recover\' program to attempt to recover\ndata from undamaged sections of corrupted files.\n\n");
#line 6976
          setExit(2);
#line 6977
          exit(exitValue);
        } else {

        }
      } else {

      }
    }
  }
#line 6984
  aa = argList;
#line 6985
  while ((unsigned long )aa != (unsigned long )((void *)0)) {
#line 6986
    aa2 = aa->link;
#line 6987
    if ((unsigned long )aa->name != (unsigned long )((void *)0)) {
#line 6987
      free((void *)aa->name);
    } else {

    }
#line 6988
    free((void *)aa);
#line 6989
    aa = aa2;
  }
#line 6992
  return (exitValue);
}
}
