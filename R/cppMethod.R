createCppMethod =
function(mdef, className = getName(getCursorLexicalParent(mdef$def)), classPrefix = "llvm::",
          typeMap = NULL)
{

  isStatic = getCursorTokens(mdef$def)[1] == "static"
  
  Rname = sprintf("R_%s_%s", className, mdef$name)

  params = mdef$params
  
  decl = sprintf("SEXP %s(%s%s%s)" ,
                   if(isStatic) "" else "SEXP r_obj",
                   Rname, if(length(params)) ", " else "",
                   if(length(params))
                     paste("SEXP", paste0("r_", names(params)), sep = " ", collapse = ", ")
                   else "")
  
  if(length(mdef$params))
    cargs = paste(names(mdef$params), collapse = ", ")
  else
    cargs = ""


  call = if(isStatic)
            sprintf("%s::%s(%s);", className, mdef$name, cargs)
         else
            sprintf("obj->%s(%s);", mdef$name, cargs)
  
  retType = getResultType(mdef$def$type)
  ansDecl = getNativeDeclaration("ans", retType, typeMap = typeMap)


  if(length(params)) {
    args = mapply(function(id, rid, type) {
                    c(getNativeDeclaration(id, type$type, typeMap = typeMap),
                       convertRValue(id, rid, type$type, typeMap))
                  },
                  names(params), sprintf("r_%s", names(params)), params)
  } else
    args = character()
  
  # static method is different
  code = c(if(!isStatic)
              sprintf("%s%s *obj = GET_REF(r_obj, %s);", classPrefix, className, className),
           if(isVoidType(retType))
             c(args, call, "return(R_NilValue);")
           else 
             c(args, ansDecl,
               sprintf("ans = %s", call),
               sprintf("SEXP r_ans = %s;",
                          convertValueToR(retType, "ans", typeMap = typeMap, rvar = "r_ans")),
               "return(r_ans);")
              )

  `C++MethodDefinition`(Rname, code, length(mdef$params) + 1, decl,
                          className = className)
}

