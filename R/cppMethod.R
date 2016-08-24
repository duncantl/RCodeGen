createCppMethod =
    #
    # This is for RCIndex.
    # 
function(mdef, className = getName(getCursorLexicalParent(mdef@def)), classPrefix = "",
          typeMap = NULL)
{

  isStatic = getCursorTokens(mdef@def)[1] == "static"

    # The name of the R proxy routine.
  Rname = sprintf("R_%s_%s", className, mdef@name)

  params = mdef@params

    # The signature for the routine, adding r_tthis if this is a member method.
  decl = sprintf("SEXP %s(%s%s%s)" ,
                   Rname,
                   if(isStatic) "" else "SEXP r_tthis",
                   if(length(params) && !isStatic) ", " else "",
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
         else
            sprintf("tthis->%s(%s);", mdef@name, cargs)
  
  retType = getResultType(mdef@def$type)
  ansDecl = getNativeDeclaration("ans", retType, typeMap = typeMap)


  if(length(params)) {
    args = mapply(createInitLocalVar, names(params),
                                      sprintf("r_%s", names(params)),
                                      params,
                   MoreArgs = list(typeMap = typeMap))
  } else
    args = character()
  
  # static method is different
  code = c(if(!isStatic)
              sprintf("%s%s *tthis = GET_REF(r_tthis, %s);", classPrefix, className, className),
           if(isVoidType(retType))
             c(args, call, "return(R_NilValue);")
           else 
             c(args, ansDecl,
               sprintf("ans = %s", call),
               sprintf("SEXP r_ans = %s;",
                          convertValueToR(retType, "ans", typeMap = typeMap, rvar = "r_ans")),
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
