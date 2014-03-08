(in-package :test-package)
#+windows
(ffi:clines "extern __declspec(dllimport) int sample_function(void);")
#-windows
(ffi:clines "extern int sample_function(void);")
(ffi:def-function "sample_function" () :returning :int)
