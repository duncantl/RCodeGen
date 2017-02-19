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
    #  We've added new parameters so anything calling this (code in CUDA, RCIndex but not in the NAMESPACE)
    #
    #
function(fun, name = getName(fun),
          params = fun@params,
          argNames = names(params),
          returnType = fun@returnType,
          nativeProxyName = getNativeProxyName(fun, name, classMethods = allClassMethods),
          PACKAGE = NA, defaultValues = character(), guessDefaults = FALSE,
          typeMap = NULL, libPrefix = "clang_",
          allClassMethods = NULL,
          generics = list())
{
      # Figure out if we have to add a this argument for a C++ method
    isStatic = isStatic(fun@def)  
    takesThis = is(fun, "C++MethodDefinition") && !(is(fun, "C++ClassConstructor") || isStatic)


    
    if(length(libPrefix))
        name = gsub(sprintf("^%s", libPrefix), "", name)

      # If missing parameter names, add them now.
    if(any(w <- (argNames == ""))) 
        argNames[w] = sprintf("arg%d", which(w))

    hasGeneric = (length(generics) && name %in% names(generics))
    if(hasGeneric) {
        generic = generics[[ name ]]    
        argNames = names(formals(generic))
        if(is(fun, 'C++ClassMethod') && argNames[1] == 'this')
           argNames = argNames[-1]

        argNames = argNames[seq(along = params)]
    }

      # Generate the code for the body of the function that coerces the arguments to the correct types.
   coercedArgs = makeCoercedArgs(params, argNames, typeMap = typeMap)


      # Add a this to the parameters and coercion to the correct type if we need a this.
   if(takesThis) {
      coercedArgs = c(sprintf("as(this, '%s')", fun@className), coercedArgs)
      argNames = c("this", argNames)
   }

      #  Generate the call to the C routine.
   call = c(sprintf(".Call('%s'",  nativeProxyName),
             if(length(coercedArgs)) ", ", 
             paste(c(coercedArgs, if(!is.na(PACKAGE)) c(PACKAGE = PACKAGE)), collapse = ", "),
            ")")
   call = paste(call, collapse = "")
   
#   sig = makeSignature(argNames, fun@params, defaultValues, guessDefaults)

   rt = returnType
#   if(FALSE && length(fun$actualReturnType))
#      rt = fun$actualReturnType

       # Coerce the result back to R. Should probably be already done in C but a second chance.
   map = lookupTypeMap(typeMap, getName(rt), "RcoerceResult", rt, name = "ans")   
   
   code = c(#paste(name, "<-"),
            #paste("function(", paste(sig, collapse = ", "), ")"),
            #"{",
            call, # paste("ans = ", call),
            map
           # "}"
          )
    
   defaultVals = lapply(params, defaultParamValue)
   if(takesThis) {
       tmp = defaultVals
       defaultVals = list(character())
       defaultVals[seq(along = tmp) + 1] = tmp
   }


   if(hasGeneric) {
#if(name == "copy") browser()

# Get the missings in here also
      dispatch = if(length(params) == 0) fun@className else c(fun@className, sapply(params, makeCoerceArg, typeMap = typeMap))
      sig = ""
      RMethodDefinition(name, dispatch, code, sig, defaults = defaultVals)
   } else
      RFunctionDefinition(name, code, argNames, defaults = defaultVals)
}

getNativeProxyName =
    #
    # Connect this to 
    #
function(fun, name = getName(fun), isOverloaded = sum(name == names(classMethods)) > 1, classMethods = NULL)
{
  if(is(fun, "AbstractC++ClassStaticMethod")) {
    if(isOverloaded)
       sprintf("R_%s_%s%s", fun@className, name, getParamSig(fun@params))
    else
       sprintf("R_%s_%s", fun@className, name)     
  } else
    sprintf("R_%s", name)
}


getParamSig =
function(params)
{
  if(length(params))
     paste(sapply(params, function(x)  mangleType(getType(x))), collapse = "_")
  else
     ""
}

mangleType =
function(type, typeName = getName(type))
{
   gsub(" ", "_",  gsub("\\*", "Ptr", typeName))
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
function(params, names, typeMap = NULL)
{
  unlist(mapply(makeCoerceArg, params, names, MoreArgs = list(typeMap = typeMap)))
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
    #
    # parm is the CXCursor for the parameter declaration
    # name  parameter name
    # 
    #
function(parm, name = character(), type = getType(parm), kind = getTypeKind(type), typeMap = NULL)
{

   if(length(typeMap)) {
      ans = lookupTypeMap(typeMap, getName(type), "coerceRParam", type, name)
      if(length(ans) && nchar(ans) > 0)
        return(ans)
   }
    
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

   if(length(name) == 0 || is.na(name))
       return(class)
   
   sprintf("as(%s, '%s')", name, gsub("const ", "", class))
}
