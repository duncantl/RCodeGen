lookupTypeMap =
  # rvar is the name of the SEXP target variable
function(map, typeName, what, type, ..., cast = "")
{
   
   typeName = gsub("^const ", "", typeName)
   typeName = gsub("[[:space:]]*&$", "", typeName)   

   if(!(typeName %in% names(map)) && getTypeKind(type) == CXType_Typedef) {
#     message("looking for canonical type for ", typeName)
      ntype = getCanonicalType(type)
      typeName = getName(ntype)
   }
   
   if(!(typeName %in% names(map)))
      return(character())

   el = map[[typeName]]
   val = el[[what]]
   if(is.null(val))
     return(character())

   if(is.function(val))
      val(..., type = type, typeMap = map, cast = cast)
   else if(is.character(val)) {
#      if(val == "GooStringFromR") browser()       
      args = list(...)
      switch(what,
             convertValueToR = sprintf("%s%s(%s)", cast, val, args[[1]]),
             convertRValue = sprintf("%s = %s%s(%s);", args[[1]], cast, val, args[[2]]),
             character())
   }
}
