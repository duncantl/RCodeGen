getReferenceClassName =
function(parm, name = character(), isClass = FALSE)
{
  # for RCIndex  - grow as needed.
  if(is(parm, "StructDescription"))
    return(sprintf("%sPtr", parm$name))

  
  if(is(parm, "ResolvedTypeReference"))
    parm = resolveType(parm)

  #XXX For classes
  #   parm@typeName
  if(is.character(parm))
    return(if(isClass) parm else paste(parm, "Ptr", sep = ""))

  if(is(parm, "BuiltinPrimitiveType"))
    name = capitalize(parm$name, first = FALSE)
  else if(is(parm, "EnumerationDefinition"))
    name = capitalize(parm$name[length(parm$name)], first = FALSE)
  else if(is(parm, "TypedefDefinition"))
     name = parm$name
  else if(is(parm, "C++ClassDefinition"))
     return(parm@name)
  else if(is(parm, "C++ReferenceType"))
     return( parm@type@name  )
  else if(is(parm, "ArrayType")) {
    parm = fixArrayElementTypeNames(parm)
    return(getCopyArrayName(parm, c("", ""), "Ptr"))
  } else if(is(parm, "PointerType")) {
    #XXX
     if(is(parm@type, "ResolvedTypeReference"))
       parm@type = resolveType(parm@type)
       
     if(is(parm@type, "C++ClassDefinition"))   
       return(parm@typeName) #XXX changed this from parm@depth - 1 
     
     name = paste(parm@typeName, paste(rep("Ptr", parm@depth - 1), collapse = ""), sep = "")
  } else
     name = parm@name

  name = gsub("^struct ", "", name)

  paste(name, "Ptr", sep = "")
}  
