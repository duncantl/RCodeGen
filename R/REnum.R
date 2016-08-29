if(FALSE) {
    cenums = lapply(enums, makeEnumDef)
    renums = lapply(enums, makeEnumClass)
}

Indent = "   "

enumClassName =
function(def, name = def@name, removeTrailing = TRUE)
{
    name = gsub("^enum ", "", name)
    if(removeTrailing)
      gsub("_enum$", "", name)
    else
      name
}

makeEnumDef =
    #
    #  generate the C routine to convert an enum value to an R object.
    #
    # Compare to makeEnumConverter in nativeEnum.R
function(def, name = enumClassName(def), decl = getName(def@type), namespace = character())
{
  name;decl
  if(is(def, "EnumerationDefinition"))
    def = def@values

  if(length(namespace) && nchar(namespace) > 0)
      names(def) = sprintf("%s::%s", namespace, names(def))

  def = def[!duplicated(def)]

  c("SEXP",
    sprintf("Renum_convert_%s(%s val)", name, decl),
    "{",
    "const char *elName;",
    "switch(val) {",
    sprintf('%scase %s:\n\telName = "%s";\n\tbreak;', Indent, names(def), names(def)),
    paste(Indent, "default:", sep = ""),
    '\telName = "?";',
    "}",
    sprintf('return(R_makeEnumValue(val, elName, "%s"));', name),
    "}")
}


makeEnumClass =
    #
    # Generate the setClass(), setAs() methods and the Values and individual variables.
    # i.e. the whole thing.
    #
function(def, name = enumClassName(def), bitwise = FALSE, superClass = if(bitwise) "BitwiseValue" else "EnumValue", prefix = NA)
{
  classDef = sprintf('setClass("%s", contains = "%s")', name, superClass)

  values = as.integer(def@values)
  strVals = sprintf("%dL", values)
  i = is.na(values)
  if(any(i)) 
     strVals[i] = "NA"

      

  c(classDef, "",
    sprintf("%s = %sValues = structure(c(%s), .Names = c(%s))",
         name, name, paste(strVals, sep = "", collapse = ", "),  paste(sQuote(names(def@values)), collapse = ", ")),
    "",
    if(bitwise)
       makeBitwiseEnumValues(def, name)
    else
       sprintf("`%s` = GenericEnumValue('%s', %s, '%s')", names(def@values), names(def@values), strVals, rep(name, length(def@values))),

    "",

    makeEnumCoerce(def, name, bitwise, prefix = prefix)
    
   )
}

makeBitwiseEnumValues =
function(def, className = enumClassName(def))
{
 c(sprintf("%s = BitwiseValue(%dL, '%s', class = '%s')",
            names(def@values), def@values, def@values, className))
   
}


makeEnumCoerce =
function(def, name = def@name, bitwise = FALSE, prefix = NA, valuesSym = paste0(name, "Values"))
{
  prefix = if(length(prefix) > 1)
             sprintf("c(%s)", paste(sQuote(prefix),  collapse = ", "))
           else if(!is.na(prefix))
             sQuote(prefix)
           else
              prefix

 cvtCode = 
         sprintf('as%sValue(from, %s, "%s", prefix = %s)', if(bitwise) "Bitwise" else "Enum", valuesSym, name, prefix)
              
 sprintf(' setAs("%s", "%s", function(from) %s )', c("character", "integer", "numeric"), name, cvtCode)
}

sQuote =
function(x)
   sprintf("'%s'", x)
