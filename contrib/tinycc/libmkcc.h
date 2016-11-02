#ifndef LIBMKCC_H
#define LIBMKCC_H

#ifndef LIBMKCCAPI
# define LIBMKCCAPI
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct MKCCState;

typedef struct MKCCState MKCCState;

/* create a new MKCC compilation context */
LIBMKCCAPI MKCCState *mkcc_new(void);

/* free a MKCC compilation context */
LIBMKCCAPI void mkcc_delete(MKCCState *s);

/* set CONFIG_MKCCDIR at runtime */
LIBMKCCAPI void mkcc_set_lib_path(MKCCState *s, const char *path);

/* set error/warning display callback */
LIBMKCCAPI void mkcc_set_error_func(MKCCState *s, void *error_opaque,
    void (*error_func)(void *opaque, const char *msg));

/* set options as from command line (multiple supported) */
LIBMKCCAPI int mkcc_set_options(MKCCState *s, const char *str);

/*****************************/
/* preprocessor */

/* add include path */
LIBMKCCAPI int mkcc_add_include_path(MKCCState *s, const char *pathname);

/* add in system include path */
LIBMKCCAPI int mkcc_add_sysinclude_path(MKCCState *s, const char *pathname);

/* define preprocessor symbol 'sym'. Can put optional value */
LIBMKCCAPI void mkcc_define_symbol(MKCCState *s, const char *sym, const char *value);

/* undefine preprocess symbol 'sym' */
LIBMKCCAPI void mkcc_undefine_symbol(MKCCState *s, const char *sym);

/*****************************/
/* compiling */

/* add a file (C file, dll, object, library, ld script). Return -1 if error. */
LIBMKCCAPI int mkcc_add_file(MKCCState *s, const char *filename, int filetype);
#define MKCC_FILETYPE_BINARY 1
#define MKCC_FILETYPE_C      2
#define MKCC_FILETYPE_ASM    3
#define MKCC_FILETYPE_ASM_PP 4
#define MKCC_FILETYPE_AR_WHOLE_OFF 5
#define MKCC_FILETYPE_AR_WHOLE_ON  6

/* compile a string containing a C source. Return -1 if error. */
LIBMKCCAPI int mkcc_compile_string(MKCCState *s, const char *buf);

/*****************************/
/* linking commands */

/* set output type. MUST BE CALLED before any compilation */
LIBMKCCAPI int mkcc_set_output_type(MKCCState *s, int output_type);
#define MKCC_OUTPUT_MEMORY   1 /* output will be run in memory (default) */
#define MKCC_OUTPUT_EXE      2 /* executable file */
#define MKCC_OUTPUT_DLL      3 /* dynamic library */
#define MKCC_OUTPUT_OBJ      4 /* object file */
#define MKCC_OUTPUT_PREPROCESS 5 /* only preprocess (used internally) */

/* equivalent to -Lpath option */
LIBMKCCAPI int mkcc_add_library_path(MKCCState *s, const char *pathname);

/* the library name is the same as the argument of the '-l' option */
LIBMKCCAPI int mkcc_add_library(MKCCState *s, const char *libraryname);

/* add a symbol to the compiled program */
LIBMKCCAPI int mkcc_add_symbol(MKCCState *s, const char *name, const void *val);

/* output an executable, library or object file. DO NOT call
   mkcc_relocate() before. */
LIBMKCCAPI int mkcc_output_file(MKCCState *s, const char *filename);

/* link and run main() function and return its value. DO NOT call
   mkcc_relocate() before. */
LIBMKCCAPI int mkcc_run(MKCCState *s, int argc, char **argv);

/* do all relocations (needed before using mkcc_get_symbol()) */
LIBMKCCAPI int mkcc_relocate(MKCCState *s1, void *ptr);
/* possible values for 'ptr':
   - MKCC_RELOCATE_AUTO : Allocate and manage memory internally
   - NULL              : return required memory size for the step below
   - memory address    : copy code to memory passed by the caller
   returns -1 if error. */
#define MKCC_RELOCATE_AUTO (void*)1

/* return symbol value or NULL if not found */
LIBMKCCAPI void *mkcc_get_symbol(MKCCState *s, const char *name);

#ifdef __cplusplus
}
#endif

#endif
