createCppMethod =
    #
    # This is for objects created by RCIndex (not RGCCTranslationUnit)
    # 
function(mdef, className = getName(getCursorLexicalParent(mdef@def)), classPrefix = "",
          typeMap = NULL, allClassMethods = NULL)
{

  isStatic = getCursorTokens(mdef@def)[1] == "static"
  isConstructor = is(mdef, "C++ClassConstructor")
  
    # The name of the R proxy routine.
  Rname = getNativeProxyName(mdef, classMethods = allClassMethods)  # sprintf("R_%s_%s", className, mdef@name)

  params = mdef@params

    # Fix up missing names.
  if(length(params)) {
      pnames = names(params)
      if(is.null(pnames))
         names(params) = LETTERS[seq(along = params)]
      else if(any( i <- (pnames == "")))
         names(params)[i] = sprintf("param_%d", seq(1, sum(i)))
  }

    # The signature for the routine, adding r_tthis if this is a member method.
  decl = sprintf("SEXP %s(%s%s%s)" ,
                   Rname,
                   if(isStatic || isConstructor) "" else "SEXP r_tthis",
                   if(length(params) && !(isStatic || isConstructor)) ", " else "",
                   if(length(params))
                      paste("SEXP", paste0("r_", names(params)), sep = " ", collapse = ", ")
                   else "")
  
  if(length(mdef@params))
    cargs = paste(names(mdef@params), collapse = ", ")
  else
    cargs = ""


    # Add code for constructor and destructor methods
  call = if(isStatic)
            sprintf("%s::%s(%s);", className, mdef@name, cargs)
         else if(isConstructor)
            sprintf("new %s(%s);", className, cargs)            
         else if(is(mdef, "C++ClassDestructor"))
            "delete tthis;"
         else
            sprintf("tthis->%s(%s);", mdef@name, cargs)
  
  retType = getResultType(mdef@def$type)
  ansDecl = if(isConstructor)
               sprintf("%s *ans;", className)
            else
               getNativeDeclaration("ans", retType, typeMap = typeMap)

  rresult =  if(isConstructor)
                 sprintf('createRef(ans, "%s", NULL)', className) #XXX add the Ptr or not?   Add the finalizer.
             else
                convertValueToR(retType, "ans", typeMap = typeMap, rvar = "r_ans")

  if(length(params)) {
    args = mapply(createInitLocalVar, names(params),
                                      sprintf("r_%s", names(params)),
                                      params,
                   MoreArgs = list(typeMap = typeMap))
  } else
    args = character()
  
  # static and constructors and destructor methods are different
  code = c(if(!(isStatic || isConstructor))
              sprintf("%s%s *tthis = GET_REF(r_tthis, %s);", classPrefix, className, className),
           if(!isConstructor && isVoidType(retType))
             c(args, call, "return(R_NilValue);")
           else 
             c(args, ansDecl,
               sprintf("ans = %s", call),
               sprintf("SEXP r_ans = %s;", rresult),
               "return(r_ans);")
              )

  `C++MethodDefinition`(Rname, code, length(mdef@params) + 1, decl,
                          className = className)
}

createInitLocalVar =
function(id, rid, type, typeMap) {
   decl = getNativeDeclaration(id, type$type, typeMap = typeMap, fromRValue = TRUE)


   if(type$kind == CXCursor_ParmDecl)
       type = type$type
       
   if(getName(type) == "char *")
      cast = "(char *)"
   else
      cast = ""

   rhs = convertRValue(id, rid, type, typeMap, cast = cast)   

   c(decl, rhs)
}    
