#
# This generates the C++ methods for to implement a subclass of a C++ class
# that uses R functions to implement the methods.
# It implements each method in the class by creating a C++ routine with that 
# signature.  It looks up the list of R functions registered by the caller to
# implement the methods using the name of the method.  
#
# In our case, there is no overloading. 
#


createMethod =
function(fun, baseClassName = getName(getParent(fun@def)), className = paste0("R", baseClassName), typeMap = NULL, defaultValues = character())
{
   rt = fun@returnType

   isVoid = ( rt$kind == CXType_Void )

    # code if there is no R function.
   isPure = isPureVirtual(fun@def) # FALSE
   isStatic = isStatic(fun@def) # FALSE
   baseName = if(isStatic)
                 paste(baseClassName,  fun@name, sep = "::")
              else
                 fun@name
   super =  sprintf("%s(%s)",  baseName,  paste(names(fun@params), collapse = ", "))
   noFunCode = if(isVoid & isPure)
                   "return"
               else if(isPure)  # but not void
                   getDefaultReturnValue(fun@name, rt, defaultValues)       
               else if(isVoid)
                   sprintf("%s;\n\t   return;", super)
               else # So not pure and not void
                   sprintf("return(%s)", super)
               
   
   code = c( sprintf('SEXP fun = lookupRMethod("%s");', fun@name),
             "if(fun == R_NilValue) {",
               paste("  ", noFunCode, ";"),
#               if(isVoid) "return;" else character(),
              "}")

   params = fun@params
   if(length(names(params)) == 0 || all(names(params) == ""))
       names(params) = LETTERS[seq(along = params) ]
   numParams = length(params)

   body = 
         c(code,
           "",
           "",
           "SEXP e, cur;",
           sprintf("PROTECT(e = cur = allocVector(LANGSXP, %d));", numParams + 1 + !isStatic),
           "SETCAR(cur, fun); cur = CDR(cur);",
           if(!isStatic)
              sprintf('SETCAR(cur, R_createRef(this, "%s", NULL)); cur = CDR(cur);', className),
           sprintf("SETCAR(cur, %s); cur = CDR(cur);", mapply(convertCArgsToR, params, names(params), MoreArgs = list(typeMap = typeMap))),
           sprintf("%s invokeMethod(e);", if(isVoid) "" else "SEXP r_ans ="),
           if(!isVoid)
              paste(getCReturn(rt, c("r_ans", "ans"), typeMap), ";"),
           "UNPROTECT(1);",
           if(!isVoid)
            "return(ans);"
         )

   # signature could be just print the def and replace the name with className::name
#   sig = as(fun, "character")
# 
   sig = c(getName(rt), fun@name, "(", paste(mapply(mkParam, params, names(params)), collapse = ", "),  ")")
   sig = paste(sig, collapse = " ")
   osig = gsub(sprintf(" (%s) ?\\(", fun@name), sprintf(" %s::\\1(", className), sig)

   list(code = c(osig, "{", sprintf("\t%s", body), "}"), decl = sig)
}    

mkParam =
function(type, name)
{
    paste(getName(getType(type)), name)
}



convertCArgsToR =
function(param, name, typeMap = NULL)    
{
  convertValueToR(getType(param), name, typeMap = typeMap, rvar = character())
}

getCReturn =
function(type, vars, typeMap = NULL)
{
    #XXXX Fix this and synchronize getConvertRValue() and convertRValue()
#    expr = getConvertRValue(type, vars[2], vars[1], typeMap = typeMap)
    expr = convertRValue(vars[2], vars[1], type, typeMap = typeMap)
    paste(getName(type), expr)
}


getDefaultReturnValue  =
function(method, type, defaultValues = character())
{
  if(method %in% names(defaultValues))
     return(sprintf("return(%s)", defaultValues[[method]]))

  tyname = getName(type)
  if(tyname %in% names(defaultValues))
     return(sprintf("return(%s)", defaultValues[[tyname]]))

  if(tyname %in% c("GBool", "bool", "int"))
      return("return(0)")

  if(isPointerType(type))
      return("return(NULL)")

  "return()"
}


defineRSubclass =
    #
    # 
    #
    #
    #
    # Create the definition  of a C++ subclass that implements its methods using R functions 
    # The C++ code to implement the C++ methods is generated separately.
    # The declarations for the implemented methods are also generated separately and put in a separate header file.
    #
function(def, rclassName = paste0("R", getName(def)))
{

  decls = paste0("\t", sapply(def@methods, as, "character"), ";")
  g = split(decls, sapply(def@methods, slot, "access"))

  g = g[ setdiff(names(g), "private") ]
  
  decls = mapply(function(access, decls)
                    c("", paste0("  ", access, ":"), decls),
                  names(g), g)
  
  code = c( sprintf("class %s : public %s, public RFunctionsNativeMethods {", rclassName, getName(def)),
            "",
            unlist(decls),
            "",
            "  public",
            sprintf("\t%s(SEXP funs) {  setFunctions(funs, true); }", rclassName),
            "};")

    code
}
