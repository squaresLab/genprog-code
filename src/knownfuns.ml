(*
 *
 * Copyright (c) 2012-2018,
 *  Wes Weimer          <weimerw@umich.edu>
 *  Stephanie Forrest   <steph@asu.edu>
 *  Claire Le Goues     <clegoues@cs.cmu.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <jlacomis@cmu.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
open Global

(*
 * Models of standard library functions for program analysis.
 *
 * For example, it is often helpful to know that "sqrt" is a pure function
 * that does not change any global state.
 *)

let pure_function_names =
  "abs labs llabs fabs div ldiv lldiv fmod remainder remquo fma fmax fmin fdim nan nanf nanl exp exp2 expm1 log log2 log10 log1p ilogb logb sqrt cbrt hypot pow sin cos tan asin acos atan atan2 sinh cosh tanh asinh acosh atanh erf erfc lgamma tgamma ceil floor trunc round lround llround nearbyint rint lrint llrint frexp ldexp modf scalbn scalbln nextafter nexttoward copysign isfinite isinf isnan isnormal signbit cabs carg cimag creal conj cproj cexp clog csqrt cpow csin ccos ctan casin cacos catan csinh ccosh ctanh casinh cacosh catanh iswalnum iswalpha iswlower iswupper iswdigit iswxdigit iswcntrl iswgraph iswspace iswblank iswprint iswpunct towlower towupper iswctype towctrans wctype wctrans difftime time clock asctime ctime strftime wcsftime gmtime localtime mktime CLOCKS_PER_SEC tm time_t clock_t malloc calloc tmpfile tmpnam localeconv getenv strlen wcslen strcmp wcscmp strncmp wcsncmp strcoll wcscoll strchr index rindex wcschr strrchr wcsrchr strspn wcsspn strcspn wcscspn strpbrk wcspbrk strstr wcsstr strerror memcmp bcmp memchr mblen mbsinit mbrlen atof atoi atol atoll strtof strtod strtold wcstof wcstod wcstold strtol strtoll wcstol wcstoll strtoul strtoull wcstoul wcstoull strdup _ctype_b_loc strcmp strncmp strcasecmp strncasecmp rawmemchr memrchr strchrnul memmem strnlen strsignal"

let io_function_names =
  "abort exit atexit quick_exit at_quick_exit system fopen freopen fflush fclose setbuf setvbuf fwide fread fwrite fgetc fgetwc getc getwc fgets fgetws fputc fputwc putc putwc fputs fputws getchar getwchar gets putchar putwchar puts ungetc ungetwc scanf wscanf fscanf fwscanf vscanf vwscanf vfscanf vfwscanf printf fprintf wprintf fwprintf vprintf vfprintf vwprintf vfwprintf perror ftell fgetpos fseek fsetpos rewind clearerr feof ferror remove rename"

let whitespace_newline_regexp = Str.regexp "[ \t\r\n]+"

let pure_function_set : StringSet.t =
  let names = Str.split whitespace_newline_regexp pure_function_names in
  List.fold_left (fun acc elt ->
      StringSet.add elt (StringSet.add ("_" ^ elt) acc)
    ) (StringSet.empty) names

let io_function_set : StringSet.t =
  let names = Str.split whitespace_newline_regexp io_function_names in
  List.fold_left (fun acc elt ->
      StringSet.add elt (StringSet.add ("_" ^ elt)
                           (StringSet.add ("_IO_" ^ elt) acc))
    ) (StringSet.empty) names

let is_pure_function name =
  StringSet.mem (name) pure_function_set

let is_io_function name =
  StringSet.mem (name) io_function_set
