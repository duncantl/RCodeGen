
void *
R_getNativeReference(SEXP arg, const char *type, const char *tag)
{
 SEXP el, elTag; 
 void *ans;

 el = arg;
 if(TYPEOF(el) != EXTPTRSXP)
     el = GET_SLOT(arg, Rf_install("ref"));

 // Check the tag on the external ptr.
 
 return(R_ExternalPtrAddr(el));
 
#if 0
 if(R_isVariableReference(arg)) {
     void *tmp;
     tmp = getVariableReference(arg, el, type, tag);
     if(!tmp) {
        PROBLEM "Got null value for variable reference %s", type
        ERROR;
     }
     return(tmp);
 }
#endif
}

/*
  Finalize for deallocating the space we allocate for references to structures
  created in S as part of the automatically generated code.
 */
void
SimpleAllocFinalizer(SEXP ans)
{
    void *ptr = R_ExternalPtrAddr(ans);
    if(ptr) {
	fprintf(stderr, "Finalizing %p\n", ptr); fflush(stderr);
#ifdef DEBUG_R_RUNTIME
#endif
	free(ptr);
	R_ClearExternalPtr(ans);
    }
}
 
