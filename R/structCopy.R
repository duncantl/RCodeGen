externC =
  c("#ifdef __cplusplus",
    'extern "C"',
    "#endif")

getStructCopyRoutineName =
  # function to produce the name of the C routine responsible for copying an
  # instance of the structure given by 'def' to an R object.
  # This is short but centralized to avoid synchronizing code in different locations
  # and thus ensuring that the name used to define the routine is the same as the name
  # used to call that routine in generated code.
  
function(def, name = if(length(def@alias)) def@alias else def@name)
{
  if(is(def, "CXCursor"))
    def = getType(def)
  
  if(is(def, "PointerType"))
#    def = def@type
     def = getPointeeType(def)

  type = getCanonicalType(def)
  name = gsub("struct +", "", getName(type))
  
  paste("R_copyStruct_", name, sep = "")
}  

createCopyStruct =
  #
  #  Copy a C/C++ structure or a class to an R object.
  #  Copy the publically accessible fields into a
  #
  #  Expects an already resolved type.
  #
  #
  # The code
  # Make certain a class is defined in R to accept the result
  # Then assign each field to the slot
function(def,  className = def@name, isClass = FALSE, typeMap = list())
{
  # Need to change this for RCIndex
if(FALSE) {  
  if(is(def, "TypedefDefinition")) {
    if(missing(className))
      className = def@name
    decl = def@name
    def = def@type
  } else 
     decl = getNativeDeclaration(character(), def, character(), addSemiColon = FALSE, const = TRUE)
} else {
   decl = sprintf("const %s ", def$name)
}

   routineName = getStructCopyRoutineName(def, className)
   native = paste("SEXP",  routineName, "(", decl, "*value)")
   native = c(externC, native,
              "{",
               "SEXP r_ans = R_NilValue, klass;",
               paste('klass = MAKE_CLASS("', className, '");', sep = ""),
               'if(klass == R_NilValue) {',
               paste('   PROBLEM "Cannot find R class', className, '"'),
               "    ERROR;",
               "}",
               "\n",
               "PROTECT(klass);",
               "PROTECT(r_ans = NEW(klass));\n")
   els =
     sapply(names(def$fields),
           function(fieldName) {
             cv = convertValueToR(def$fields[[fieldName]], paste('value ->', fieldName), typeMap = typeMap)
             fieldName = fixupFieldName(fieldName)
             paste('PROTECT(r_ans = SET_SLOT(r_ans, Rf_install("', fieldName, '"), ', cv, " ));", sep = "")
           })

  if(FALSE) {
   native = paste(c(native[1],
                        paste("\t", c(native[-1],
                                      els,
                                      paste("UNPROTECT(", length(def$fields) + 2, ");"),
                                      "",
                                      "return(r_ans);")),
                   "}"),
                 collapse = "\n")
 }

   native = c(native,
              els,
              paste("UNPROTECT(", length(def$fields) + 2, ");"),
              "",
              "return(r_ans);",
              "}")
  
  CRoutineDefinition(name = routineName, code = native, 1L)
}  



SpecialFieldNames = c("names" = "Names")
fixupFieldName =
function(name)
{
  idx = match(name, names(SpecialFieldNames))
  w = !is.na(idx)
  if(any(w))
    warning("switching field name ", paste(name[w], collapse = ", "), " in struct. Don't ask!!!")
  name[w] = SpecialFieldNames[idx[w]]  
  name
}  

fixupStructNames =
function(type)
{
  idx = match("names", names(type@fields))
  if(is.na(idx))
    return(type)

  warning("switching field name of names in struct", type@name, "to Names. Don't ask!!!")
  names(type@fields)[idx] = "Names"
  type@fields[[idx]]@name = "Names"
  type
}  



generateStructCreation =
function(type, name = type@name, alloc = "calloc", addFinalizer = TRUE)
{
  decl = getNativeDeclaration("", type, addSemiColon = FALSE)
  rname = paste("R_new_", name, sep = "")
  txt = 
   c(externC,
     "SEXP", 
     paste(rname, "()"), 
     "{", 
     "SEXP r_ans = R_NilValue;",
     paste(decl, "* ans;"),
     "",
     paste("ans = (", decl, "*)", alloc, "(1, sizeof(", decl, "));"),
     paste(" r_ans = ", createNativeReference("ans", type, type@name, FALSE), ";"),  #XXX second type was type@name then type but back again!
     "return(r_ans);",
     "}")

   cdef = CRoutineDefinition(rname, txt, 0L)

   fieldNames = backtick(names(type@fields))
  
   id = getRConstructorFunctionName(type, name)
   destroy = getDestructorNames(type)
   txt = c(
           paste("ans = .Call('", rname, "')", sep = ""),
           "if((is.logical(.finalizer) && .finalizer) || length(.finalizer) > 0)",
           paste(Indent, "addFinalizer(ans, .finalizer, ",  sQuote(destroy["finalizer"]), ")", sep = ""),
           "",
#           "args = list(...)",
           paste("if(!missing(", fieldNames, ")) ans$", fieldNames, " = ", fieldNames, sep = ""),
           paste("na = pmatch(names(args), names(getSlots('", name, "')))", sep = ""),
           "if(any(is.na(na)))",
           paste("     ", "stop('no fields match ', paste(names(args)[is.na(na)], collapse = ', '))"),
           "",
           "for(i in names(args))",
           '  do.call("$<-", list(ans, i, args[[i]]))',
           "",
           "ans"
          )


  structure(list(c = cdef,
                 r = RFunctionDefinition(id, txt, c(fieldNames, ".finalizer"), c(.finalizer = as.character(addFinalizer)))),
            class = "ConstructorCode")
}



createStructFree =
  #
  # create a routine that can be registered as a R_CFinalizer_t
  # and another one that can be invoked via a .Call() to explicitly
  # free an RC++ExternalPtr, i.e. the object that has the @ref slot.
  #
  # These routines are named   R_free_<name>_finalizer and R_free_<name>
  #
function(type, name = type$name, free = "free", typeMap = list())
{
  decl = getNativeDeclaration(character(), type, addSemiColon = FALSE, const = FALSE)  
  type = PointerType(type)
  ids = getDestructorNames(type)
#XXX
  deref = paste(convertRValue("", "val", type, character(), typeMap = typeMap), ";")
  finalizer = 
   c(externC, "void", 
     paste(ids["finalizer"], "(SEXP val)"),
     "{", 
     paste(decl, "* ans = NULL;"),
     paste("ans = (", decl, "* ) R_ExternalPtrAddr(val);"),  
#     paste(' if(ans) { fprintf(stderr, "freeing', name, ' %p\\n", ans); free(ans);}'),
     ' if(ans) {',
     "#ifdef DEBUG_R_FINALIZERS",
     paste('   fprintf(stderr, "freeing', name, ' %p\\n", ans);'),
     "#endif",
     '    free(ans);',
     '}',     
     "}")

  free = c(externC, "SEXP",
     paste(ids["r"], "(SEXP val)"), 
     "{", 
     paste(decl, "* ans = NULL;"),
     paste("ans = ", deref),
     "", 
#     paste(' if(ans) { fprintf(stderr, "freeing', name, ' %p\\n", ans); free(ans);}'),
     ' if(ans) {',
     "#ifdef DEBUG_R_FINALIZERS",
     paste('   fprintf(stderr, "freeing', name, ' %p\\n", ans);'),
     "#endif",
     '    free(ans);',
     '}',         
     "return(R_ScalarLogical(ans ? TRUE : FALSE));",
     "}")    

  structure(list(finalizer = CRoutineDefinition(ids["finalizer"], finalizer, 1L),
                 free = CRoutineDefinition(ids["r"], free, 1L)),
            class = "FreeCode")
}  



generateStructSetAs =
  #
  # 2 coercions - from R to C and from C to R. 
  #   classNames comes as R and C type (pointer)
  #
  #
  # XXX need to deal with the name and the type.
  # i.e. struct name or simply name
  #
function(type, classNames = c(type$name, getReferenceClassName(type)),
         localVarDecl = "",
         ptype = new("PointerType", type = type, depth = as.integer(1), typeName = type@alias)) 
{

  qnames = paste('"', classNames, '"', sep = "")
  rname = paste("R_coerce", classNames[1], classNames[2], sep = "_")
  iname = paste("coerce", classNames[1], classNames[2], sep = "_")  

  txt = c(paste("setAs(", qnames[1], ", ", qnames[2], ", "),
            "function(from)",
            paste(".Call('", rname, "', from )", sep = ""),
          ")")

  #  Generate the C routine code to do this.
  # allocate a new struct instance, and then loop over the slots
  # and copy the R value to each field
  # and return a reference to the allocated struct instance.
  
  decl = getNativeDeclaration("ans", ptype, const = FALSE)

    # The R version
  rnative = c(externC, "SEXP",
             paste(rname, " ( SEXP r_from )"),
            "{",
             paste("return ( R_createNativeReference( (void *)", iname, "(r_from, NULL),", dQuote(classNames[2]), ",", dQuote(classNames[2]), "));"),    
            "}"    
            )

  
    # The internal version
  native = c(externC,
             getNativeDeclaration("", ptype, const = FALSE, addSemiColon = FALSE),
             paste(iname, "( SEXP r_from, ", getNativeDeclaration("ans", ptype, const = FALSE, addSemiColon = FALSE), ")"),
             "{",
#             decl,
             "",
             "SEXP tmp;",
             "",
    #XXX should be classNames[1] (for both) but then need to change default.
             "if(!ans) {",
             paste("    ans = (", getNativeDeclaration("", ptype, addSemiColon = FALSE, const = FALSE), ")",
                           "malloc( sizeof(", getNativeDeclaration("", type, addSemiColon = FALSE, const = FALSE), "));"),
             "    if(!ans) return(ans);",
             "}",
             "",
             sapply(names(type@fields),
                    function(field) {

                       c(paste("tmp = GET_SLOT(r_from, Rf_install(\"", field ,"\"));", sep = ""),
                             # This is really convertRValue, i.e. from R to C but better copy the object.
                             # was  just the field, not the type
                         getCopyFieldCode(field, type@fields[[field]]@type, c(to = "ans", from = "tmp")))
                    }),
             "return(ans);",
             "}",
             "")

  toRef = CRoutineDefinition(name = rname, code = unlist(rnative), 1L)
  internalToRef = CRoutineDefinition(name = iname, code = unlist(native), 2L)        
   # Coerce from the Ref to the R class. We already  have most of this
   # as 
  rname = paste("R_coerce", classNames[2], classNames[1], sep = "_")
  txt = c("", txt,
          paste("setAs(", qnames[2], ", ", qnames[1], ", "),
            "function(from)",
            paste(".Call('", rname, "', from)", sep = ""),
          ")")
   code = c(externC, "SEXP",
            paste(rname, "( SEXP from )"),
            "{",
            decl,
#            paste("ans = R_GET_REF_TYPE(from,", classNames[1] , " );"),
             paste("ans = (", getNativeDeclaration("", ptype, addSemiColon = FALSE, const = FALSE), ")", derefNativeReference("from", type, type@name), ";", sep = ""),
             paste("return(", getStructCopyRoutineName(type, classNames[1]), "( ans ) );"),
           "}"
           )


  list(r = paste(txt, collapse = "\n"),
       routines = list(internalToRef = internalToRef, toRef = toRef,
                       fromRef = CRoutineDefinition(name = rname, code = code, 1L)))
}  




getDestructorNames =
function(type, name = getReferenceClassName(type))
{
  rname = paste("R_free_", name, sep = "")
  c(r = rname, finalizer = paste(rname, "_finalizer", sep = ""))
}
