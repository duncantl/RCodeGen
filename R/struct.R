
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

getPtrClassName =
function(name)
  sprintf("%sPtr", gsub(" ", "_", gsub("struct ", "", name))    )


makeRStructCode = 
function(desc, name = desc@name[1], isOpaque = FALSE, typeMap = NULL, ptrClassName = getPtrClassName(name))
{
   cName = name
   rname = gsub("struct ", "", name)
   fieldDefs = lapply(desc@fields, getRTypeName, typeMap = typeMap)
   classDef = sprintf("setClass('%s', representation(%s))",
                       rname, paste(names(fieldDefs), sQuote(fieldDefs), sep = " = ", collapse = ", "))

   ptrClassDef = sprintf("setClass('%s', contains = 'RC++StructReference')", ptrClassName)
   
   list(classDef= classDef,
        ptrClassDef = ptrClassDef,
        getMethod =  c(makeRStructMethod(desc@fields, ptrClassName, cName),
                       makeRStructMethod(desc@fields, ptrClassName, cName, ops = c("get", "[["))),
        setMethod =  c(makeRStructMethod(desc@fields, ptrClassName, cName, FALSE,),
                       makeRStructMethod(desc@fields, ptrClassName, cName, FALSE, c("set", "[[<-"))),    
        constructor = makeRConstructor(desc, rname),
        refConstructor = rRefConstructor(desc, rname, cName),
        namesMethod = makeNamesMethod(desc, desc@fields, ptrClassName),
        coercePtr = coerceStructToR(desc, rname, ptrClassName),
        coerceToR = coerceRToStruct(desc, rname, ptrClassName)
       )
}

coerceStructToR =
function(desc, rname, cname)
{
    sprintf("setAs('%s', '%s', function(from) .Call('%s', from))", cname, rname,
            getStructCopyRoutineName(desc@def))
}

coerceRToStruct =
function(desc, rname, cname, fields = desc@fields)
{
    # Perhaps for speed we should do this in C but leave that until later.
    code = c(sprintf("function(from, to = %s())", getPtrClassName(rname)),
             "{",
              sapply(names(fields),
                      function(x) sprintf("to$%s = from@%s", x, x)),
             "to",
             "}")
    sprintf("setAs('%s', '%s', %s)", rname, cname, paste(code, collapse = "\n"))
}


makeFieldAccessorRoutineName =
function(name, op = "get")
   sprintf("R_%s_%s", gsub(" ", "_", name), op)  

makeFieldMethod =
function(desc, rname, cname, op = "get")
{
  code = c(sprintf(".fieldNames = c(%s)", paste(sQuote(names(desc)), collapse = ", ")),
           "if(is.na( i <- pmatch(name, .fieldNames)))",
           sprintf("    stop(name, ' is not a field name for', '%s')", rname),
           sprintf(".Call(sprintf('%s_%%s', .fieldNames[i]), x %s)",
                        makeFieldAccessorRoutineName(cname, op),
                   if(op == "get") "" else ", value"),
           if(op != "get") "x"
      )

  if(op == "set") {
      rtypes = paste(sQuote(names(desc)), sQuote(sapply(desc, getRTypeName)), sep = " = ", collapse = ", ")
      code[1] = sprintf(".fieldNames = c(%s)", rtypes)
      code[c(2, 4)] = gsub(".fieldNames", "names(.fieldNames)", code[c(2, 4)])
      code = c(code[1:3],
               "value = if(.fieldNames[[i]] == 'numeric') as.numeric(value) else as(value, .fieldNames[[i]])",
               code[4:5])
  }
  
  code
}

makeRStructMethod =
function(desc, rname, cname, get = TRUE, ops = if(get) c("get", "$") else c("set", "$<-"))
{
    code = makeFieldMethod(desc, rname, cname, ops[1])
    if(substring(ops[2], 1, 2) == "[[")
        code = c("name = i", code)
    fun = RFunctionDefinition(character(), code, c("x", "name", if(!get) "value"))
    if(ops[2] == "[[<-") {
       fun@signature = c("x", "i", "j", "...", "value")
    } else if(ops[2] == "[[")
       fun@signature = c("x", "i", "j", "...")
    
    sprintf("setMethod('%s', '%s',\n %s\n)\n",
            ops[2L], rname, as(fun, "character"))
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
    rname = gsub(" ", "_", name)
    rclassName = gsub("struct ", "", name)
     # $ method
  list(getAccessors = mapply(makeCStructFieldAccessor, names(fields), fields, name, MoreArgs = list(typeMap = typeMap)),
       setAccessors = mapply(makeCStructFieldAccessor, names(fields), fields, name, MoreArgs = list(get = FALSE, typeMap = typeMap)),
       copyToR = makeCCopyStructCode(desc, rclassName),
       alloc = makeAllocStruct(desc, name)
      )       
}

allocRoutineName =
function(name)
  sprintf("R_alloc_%s", gsub(" ", "_", name))

makeAllocStruct =
function(desc, name = desc@name[1], cName = desc@name[1])
{
  fnName = allocRoutineName(name)
  k = c("SEXP",
        paste0(fnName, "(int addFinalizer)"),
        "{",
         "void *ptr = NULL;",
         sprintf("ptr = calloc(1, sizeof(%s));", cName),
         "if(!ptr) {",
         sprintf('   PROBLEM "cannot allocate %%lu bytes for a \\"%s\\"", sizeof(%s)\n\tERROR;', cName, name),
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

rRefConstructor =
function(desc, name = desc@name[1], structName = desc@name[1])
{    
    fnName = allocRoutineName(structName)
    
    code = c(sprintf(".ans = .Call('%s', as.logical(.addFinalizer))", fnName),
             sprintf(".ans = new('%s', ref = .ans)", getPtrClassName(name)),
             ".args = list(...)",
             "mapply(function(var, val)
                       .ans[[var]] = val,
                     names(.args), .args)",
             ".ans"
            )

    RFunctionDefinition(getPtrClassName(name), code, c("...", ".addFinalizer"), defaults = list(".addFinalizer" = TRUE))
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
    #  Why does it not set the slots of an S4 object. Does now.
    #
function(desc, rname, funName = getStructCopyRoutineName(desc@def), typeMap = NULL, s4 = TRUE)
{
  force(funName)   # if given a TypeDefinition, use its name and then get the Struct info.
  
  if(is(desc, "TypeDefinition"))
     desc = getStructDef(desc@type)

  copyFields =  mapply(function(f, name) {
                          v = convertValueToR(f, sprintf("obj->%s", name), typeMap = typeMap)
                          v[length(v)] = if(s4)
                                            sprintf('SET_SLOT(r_ans, Rf_install("%s"), %s);', name, gsub(";$", "", v))                             
                                         else 
                                            sprintf('SET_VECTOR_ELT(r_ans, i, %s);', gsub(";$", "", v))
                          a = if(length(v) > 1) 
                               c("{",
                                 v,
                                "}")
                                else
                                 v

                          if(s4)
                              a
                          else
                              c(a, sprintf('SET_STRING_ELT(r_names, i++, Rf_mkChar("%s"));', name))
                       },
                       desc@fields, names(desc@fields))
  nfields = length(desc@fields)
  ifunName = gsub("^R_", "Ri_", funName)
  sig = sprintf("%s(%s *obj)", ifunName, getName(getCanonicalType(desc@def)))

  code = if(s4) 
          c("SEXP",
            sig,
            "{",
            "SEXP r_ans, r_class;",
            sprintf('PROTECT(r_class = MAKE_CLASS("%s"));', rname),
            'PROTECT(r_ans = NEW_OBJECT(r_class));',            
            unlist(copyFields),
            "UNPROTECT(2);",
            "return(r_ans);",            
            "}")
         else
          c("SEXP",
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
  

  code2 = c("SEXP",
            sprintf("%s(SEXP r_obj)", funName),
            "{",
              sprintf("return(%s(GET_REF(r_obj, %s)));", ifunName,  getName(getCanonicalType(desc@def))),
            "}")
  list(internal = CRoutineDefinition(ifunName, code),
       r = CRoutineDefinition(funName, code2))
}



makeNamesMethod =
function(desc, fields = desc@fields, name = desc@name[1])
{

   code = sprintf("setMethod('names', '%s',\nfunction(x)\n c(%s))" ,
                 name,
                 paste(sQuote(names(fields)), collapse = ", "))
 
}
