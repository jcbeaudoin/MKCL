#+(or ecl mkcl)
(ffi:clines "
extern MKCL_DLLEXPORT int sample_function(void);

int sample_function(void)
{
	return 42;
}
")
