void * R_getNativeReference(SEXP arg, const char *type, const char *tag);
#define GET_REF(val, type) (type *)  R_getNativeReference((val), #type, #type)
