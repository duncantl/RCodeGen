# This copies an R object to its C-level equivalent, i.e. an R C++StructReference to a struct *
setGeneric("getCopyFieldCode",
             function(fieldName, type, varNames, typeMap = list())
               standardGeneric("getCopyFieldCode"))



setMethod("getCopyFieldCode", c("ANY", "ANY", "ANY"),
             function(fieldName, type, varNames, typeMap = list()) {
                 lhs = paste(varNames["to"], fieldName, sep = "->")
                 tmp = convertRValue(lhs, varNames["from"], type, character(), typeMap = typeMap)
                 paste(lhs, "=", tmp, ";")
             })

setMethod("getCopyFieldCode", c(type = "ResolvedTypeReference"),
             function(fieldName, type, varNames, typeMap = list()) {

               getCopyFieldCode(fieldName, resolveType(type), varNames)
             })

setMethod("getCopyFieldCode", c(type = "StructDefinition"),
             function(fieldName, type, varNames, typeMap = list()) {

              routine = paste("coerce", type@name, getReferenceClassName(type), sep = "_")
              paste(routine, "(", varNames["from"], ",", "&", varNames["to"], "->", fieldName, ");")
             })

setMethod("getCopyFieldCode", c(type = "ArrayType"),
             function(fieldName, type, varNames, typeMap = list()) {
                # Need to allocate space and then copy
                # Need to know the length.

      return( convertRValue(paste(varNames["to"], fieldName, sep = "->"),  varNames["from"], type, varNames, typeMap = typeMap))

if(FALSE) {               
               c("{",
                 "int i;",
                 paste("for(i = 0; i <", type@length, "; i++) {"),
                 
                 "}",
                 "}")
                paste("memcpy(", varNames["to"], ", ", varNames["from"], ", sizeof(", varNames["to"], "[0]) * ", type@length, ");")
}               
             })
