
makeStructCode =
  # handle different inputs, i.e. not the StructDescription, but CXType, CXCursor
function(desc, name = desc@name[1], isOpaque = FALSE, typeMap = NULL)
{
   # generate  a class for both the Struct represented in R
   # and a C pointer to an instance.
   #
   # For the C pointer version, e.g. <name>Ref,
   #    we extend ExternalReference or RC++Reference
   #  + define $ and $<- operator functions to get/set fields in it
   #    corresponding field set/get routines
   #    constructor routine - that allocates an instance and populates (some) of the fields
   #       add a finalizer, optionally
   #    finalizer callable directly from R.
   #    routine to copy the ref to an R object of class <name>
   #    clone/duplicate routine
  
   #   a coercion method from the Struct class to the Ref type and vice versa
  
   # For the R version,
   #   + define a class with a slot for each field
   #   + a constructor function
   #
   # 
   #     

   list(r = makeRStructCode(desc, name, isOpaque, typeMap),
        native = makeCStructCode(desc, name, isOpaque, typeMap))
}


makeRStructCode = 
function(desc, name = desc@name[1], isOpaque = FALSE, typeMap = NULL, ptrClassName = sprintf("%sPtr", name))
{
   fieldDefs = lapply(desc@fields, getRTypeName, typeMap = typeMap)
   classDef = sprintf("setClass('%s', representation(%s))",
                       name, paste(names(fieldDefs), sQuote(fieldDefs), sep = " = ", collapse = ", "))

   ptrClassDef = sprintf("setClass('%s', contains = 'RC++StructReference')", ptrClassName)
   
   list(classDef= classDef,
        ptrClassDef = ptrClassDef,
        getMethod =  makeRStructMethod(desc@fields, name),
        setMethod =  makeRStructMethod(desc@fields, name, FALSE),        
        constructor = makeRConstructor(desc, name),
        namesMethod = makeNamesMethod(desc, desc@fields, ptrClassName))
}

makeFieldAccessorRoutineName =
function(name, op = "get")
   sprintf("R_%s_%s", name, op)  

makeFieldMethod =
function(desc, name, op = "get")
{
  code = c(sprintf(".fieldNames = c(%s)", paste(sQuote(names(desc)), collapse = ", ")),
           "if(is.na( i <- pmatch(name, .fieldNames)))",
           sprintf("    stop(name, ' is not a field name for', '%s')", name),
           sprintf(".Call(sprintf('%s_%%s', .fieldNames[i]), x %s)",
                        makeFieldAccessorRoutineName(name, op),
                        if(op == "get") "" else ", value")
        )
}

makeRStructMethod =
function(desc, name, get = TRUE, ops = if(get) c("get", "$") else c("set", "$<-"))
{
  fun = RFunctionDefinition(character(), makeFieldMethod(desc, name, ops[1]), c("x", "name", if(!get) "value"))
  sprintf("setMethod('%s', '%s',\n %s\n)\n",
            ops[2L], name, as(fun, "character"))
}


makeRConstructor =
function(desc, name = desc@name[1], fields = desc@fields)
{
   if(is(desc, "CXCursor"))
      desc = getStructDef(desc)
   
   coerceCode = mapply(function(f, id)
                         makeCoerceArg(name = id, type= f),
                         fields, names(fields))
   set = sprintf("if(!missing(%s))\n       .ans@%s = %s",
                     names(fields), names(fields),
                      coerceCode)
 
  
   code = c("if(is.character(.ans))",
            "   .ans = new(.ans)",
            "",
            set,
            "",
            ".ans")
     
    RFunctionDefinition(name, code, c(names(fields), ".ans"), defaults = list(.ans = sprintf("new('%s')", name)))
}


makeCStructCode =
function(desc, name = desc@name[1], isOpaque = FALSE, typeMap = NULL, fields = desc@fields)
{
     # $ method
  list(getAccessors = mapply(makeCStructFieldAccessor, names(fields), fields, name, MoreArgs = list(typeMap = typeMap)),
       setAccessors = mapply(makeCStructFieldAccessor, names(fields), fields, name, MoreArgs = list(get = FALSE, typeMap = typeMap)),
       copyToR = makeCCopyStructCode(desc),
       alloc = makeAllocStruct(desc, name)
      )       
}

makeAllocStruct =
function(desc, name = desc@name[1])
{
  fnName = sprintf("R_alloc_%s", name)
  k = c("SEXP",
        paste0(fnName, "(int addFinalizer)"),
        "{",
         "void *ptr = NULL;",
         sprintf("ptr = calloc(1, sizeof(%s));", name),
         "if(!ptr) {",
         sprintf('   PROBLEM "cannot allocate %%lu bytes for a \\"%s\\"", sizeof(%s)\n\tERROR;', name, name),
         "}",
         "SEXP ans;",
         sprintf('ans = R_MakeExternalPtr(ptr, Rf_install("%s"), R_NilValue);', name),
         "PROTECT(ans);",
         "R_RegisterCFinalizer(ans, SimpleAllocFinalizer);",
         "UNPROTECT(1);",
         "return(ans);",
      "}")

   CRoutineDefinition(fnName, k)  
}

makeCStructFieldAccessor =
function(fieldName, type, structName, get = TRUE, typeMap = NULL)
{
  fnName = sprintf("%s_%s", makeFieldAccessorRoutineName(structName, if(get) "get" else "set"), fieldName)
  if(get) {
    ans = convertValueToR(type, sprintf("obj->%s", fieldName), typeMap = typeMap)
    if(!( is(ans, "AsIs") || length(ans)  > 1))
       ans = sprintf("r_ans = %s;", ans)
  }
  
  code = c("SEXP",
    sprintf("%s(SEXP r_obj%s)", fnName, if(get) "" else ", SEXP r_value"),
    "{",
    "SEXP r_ans = R_NilValue;",
    sprintf("%s * obj = GET_REF(r_obj, %s);", structName, structName),
    if(get) {
      ans
    } else {
      if(type$kind == CXType_ConstantArray) {
         len = getNumElements(type)
         elType = getElementType(type);
         
         sprintf("copyRVectorTo%sArray(%s, obj->%s, %d);", capitalize(getName(elType)), "r_value", fieldName, len);
        
      } else
         c(makeLocalVar(, "r_value", "value", type, addDecl = TRUE, typeMap = typeMap),
           sprintf("obj->%s = value;", fieldName))
    },
    "return(r_ans);",
    "}"
   )
   CRoutineDefinition(fnName, code)
}


makeCCopyStructCode =
    #
    #  This generates code to return a list.
    #  Why does it not set the slots of an S4 object.
    #
function(desc, funName = getStructCopyRoutineName(desc@def), typeMap = NULL)
{
  force(funName)   # if given a TypeDefinition, use its name and then get the Struct info.
  
  if(is(desc, "TypeDefinition"))
     desc = getStructDef(desc@type)

  copyFields =  mapply(function(f, name) {
                          v = convertValueToR(f, sprintf("obj->%s", name), typeMap = typeMap)
                          v[length(v)] = sprintf('SET_VECTOR_ELT(r_ans, i, %s);', gsub(";$", "", v))
                          a = if(length(v) > 1) 
                               c("{",
                                 v,
                                "}")
                                else
                                 v

                            c(a, sprintf('SET_STRING_ELT(r_names, i++, Rf_mkChar("%s"));', name))
                       },
                       desc@fields, names(desc@fields))
  nfields = length(desc@fields)
  sig = sprintf("%s(%s *obj)", funName, getName(getCanonicalType(desc@def)))
  code = c("SEXP",
            sig,
           "{",
           "int i = 0;",
           "SEXP r_ans, r_names;",
           sprintf("PROTECT(r_ans = NEW_LIST(%d));", nfields),
           sprintf("PROTECT(r_names = NEW_CHARACTER(%d));", nfields),    
           unlist(copyFields),
           "SET_NAMES(r_ans, r_names);",
           "UNPROTECT(2);",
           "return(r_ans);",
           "}")
  

  
  CRoutineDefinition(funName, code)
}



makeNamesMethod =
function(desc, fields = desc@fields, name = desc@name[1])
{

   code = sprintf("setMethod('names', '%s',\nfunction(x)\n c(%s))" ,
                 name,
                 paste(sQuote(names(fields)), collapse = ", "))
 
}
