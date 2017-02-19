library(RCodeGen)
library(RCIndex)

tu = createTU("simpleC.c", args = c("-ferror-limit=10000", "-fparse-all-comments"), verbose = FALSE)
r = getRoutines(tu)
ccode = lapply(r, createNativeProxy)

