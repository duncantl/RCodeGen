#
# A C++ method created in RCIndex via readCppClass() (called  from getCppClasses())
# is not a FunctionDecl object expected here, but instead a simple list with the same
# elements but not an S4 class. So let's convert that.


createRProxy =
    #
    # Create a simple R function to call a proxy C routine that we created
    # to call an actual routine. This coerces its arguments to the appropriate
    # R type and then uses .Call() to invoke the proxy C routine.
    #
    #  We've added new parameters so anthing calling this (code in CUDA, RCIndex but not in the NAMESPACE)
    #
    #
function(fun, name = getName(fun),
          params = fun@params,
          argNames = names(params),
          returnType = fun@returnType,
          nativeProxyName = sprintf("R_%s", name),
          PACKAGE = NA, defaultValues = character(), guessDefaults = FALSE,
          typeMap = NULL, libPrefix = "clang_")
{
    
   if(length(libPrefix))
      name = gsub(sprintf("^%s", libPrefix), "", name)
  
   if(any(w <- (argNames == ""))) 
      argNames[w] = sprintf("arg%d", which(w))
  
   coercedArgs = makeCoercedArgs(params, argNames)
   if(is(fun, "C++ClassMethod")) {
      coercedArgs = c(sprintf("as(this, '%s')", fun@className), coercedArgs)
      argNames = c("this", argNames)
   }
   
   call = c(sprintf(".Call('%s'",  nativeProxyName),
             if(length(coercedArgs)) ", ", 
             paste(c(coercedArgs, if(!is.na(PACKAGE)) c(PACKAGE = PACKAGE)), collapse = ", "),
            ")")
   call = paste(call, collapse = "")
   
#   sig = makeSignature(argNames, fun@params, defaultValues, guessDefaults)

   rt = returnType
   if(FALSE && length(fun$actualReturnType))
      rt = fun$actualReturnType

   map = lookupTypeMap(typeMap, getName(rt), "RcoerceResult", rt, name = "ans")   
   
   code = c(#paste(name, "<-"),
            #paste("function(", paste(sig, collapse = ", "), ")"),
            #"{",
            call, # paste("ans = ", call),
            map
           # "}"
          )

   RFunctionDefinition(name, code, argNames)
}



createRMethodProxy =
function(fun, typeMap = NULL)
{
#browser()    
    # If this is a C++ClassMethod, then we need to add the this as a parameter.
    # Unfortunately, we have the CXType for the class, not for the pointer to the class
    # and we cannot create a new type which isa a  pointer to this class.
    # If we were using the clang C++ API, we probably would be able to do this.
   
#   params = c(r_this = , fun@params)
   createRProxy(fun, params = params, typeMap= typeMap)
}

makeSignature =
function(argNames, params, defaultValues = character(), guessDefaults = !inherits(defaultValues, "AsIs"))
{
  if(length(defaultValues) == 0 && length(params) > 0 && guessDefaults)
      defaultValues = guessDefaultValues(params, argNames)

  argNames
}
  
makeCoercedArgs =
function(params, names)
{
  unlist(mapply(makeCoerceArg, params, names))
}


getBuiltinRTypeFromKind =
function(kind)  
   class = switch(names(kind),
                   LongLong= ,
                   Double=,
                   Float=,
                   Int128=,
                   UInt = "numeric",
                   Short=,
                   Int = "integer",
                   Bool = "logical",
                   NullPtr = "NULL",
                  character())


makeCoerceArg =
function(parm, name, type = getType(parm), kind = getTypeKind(type))
{

    class = getBuiltinRTypeFromKind(kind)

   if(length(class) == 0) {
     typeName = getName(type)
     
     if(kind == CXType_Enum || (kind == CXType_Unexposed && grepl("^enum ", typeName))) 
          class = gsub('^enum ', '', typeName)
     
     else if(kind == CXType_Typedef) {
       class = typeName
       #return(makeCoerceArg(type = getCanonicalType(type), name = name))
     } else if(kind == CXType_Record)
        class = typeName
     else if(kind == CXType_Pointer) {
         # XXX What should the name be for this representation
         # Make more specific
       info = getPointerInfo(type)
       if(info$depth <= 2 && getTypeKind(info$baseType) == CXType_Char_S)
         class = "character"
       else {
         class = getBuiltinRTypeFromKind(getTypeKind(info$baseType))
#         browser()
         if(length(class) == 0) {
           if(grepl("*", typeName, fixed = TRUE))
              #!!!! changed Jul 2 (2013). Was just getName(info$baseType), i.e. no Ptr.
             class = sprintf("%sPtr", getName(info$baseType))
           else
              class = typeName # name # assume we are using the name
         }
         #class = "RC++Reference"
       }
     } else if(kind == CXType_ConstantArray) {
        elTy = getArrayElementType(type)
        elKind = getTypeKind(elTy)
        if(elKind == CXType_Char_S) 
          class = "character"
        else 
          class = "raw"
     } else if(kind == CXType_Unexposed) {

         if(grepl("^struct ", typeName)) 
            class =  gsub("^struct ", "", typeName)
         #else
         #   browser()

     } else {
         #browser()
     }
   }

   sprintf("as(%s, '%s')", name, gsub("const ", "", class))
}
