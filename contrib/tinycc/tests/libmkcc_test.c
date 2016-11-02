/*
 * Simple Test program for libmkcc
 *
 * libmkcc can be useful to use mkcc as a "backend" for a code generator.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libmkcc.h"

/* this function is called by the generated code */
int add(int a, int b)
{
    return a + b;
}

char my_program[] =
"#include <mkcclib.h>\n" /* include the "Simple libc header for MKCC" */
"extern int add(int a, int b);\n"
"int fib(int n)\n"
"{\n"
"    if (n <= 2)\n"
"        return 1;\n"
"    else\n"
"        return fib(n-1) + fib(n-2);\n"
"}\n"
"\n"
"int foo(int n)\n"
"{\n"
"    printf(\"Hello World!\\n\");\n"
"    printf(\"fib(%d) = %d\\n\", n, fib(n));\n"
"    printf(\"add(%d, %d) = %d\\n\", n, 2 * n, add(n, 2 * n));\n"
"    return 0;\n"
"}\n";

int main(int argc, char **argv)
{
    MKCCState *s;
    int i;
    int (*func)(int);

    s = mkcc_new();
    if (!s) {
        fprintf(stderr, "Could not create mkcc state\n");
        exit(1);
    }

    /* if mkcclib.h and libmkcc1.a are not installed, where can we find them */
    for (i = 1; i < argc; ++i) {
        char *a = argv[i];
        if (a[0] == '-') {
            if (a[1] == 'B')
                mkcc_set_lib_path(s, a+2);
            else if (a[1] == 'I')
                mkcc_add_include_path(s, a+2);
            else if (a[1] == 'L')
                mkcc_add_library_path(s, a+2);
        }
    }

    /* MUST BE CALLED before any compilation */
    mkcc_set_output_type(s, MKCC_OUTPUT_MEMORY);

    if (mkcc_compile_string(s, my_program) == -1)
        return 1;

    /* as a test, we add a symbol that the compiled program can use.
       You may also open a dll with mkcc_add_dll() and use symbols from that */
    mkcc_add_symbol(s, "add", add);

    /* relocate the code */
    if (mkcc_relocate(s, MKCC_RELOCATE_AUTO) < 0)
        return 1;

    /* get entry symbol */
    func = mkcc_get_symbol(s, "foo");
    if (!func)
        return 1;

    /* run the code */
    func(32);

    /* delete the state */
    mkcc_delete(s);

    return 0;
}
