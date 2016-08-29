library(RCIndex)
library(RCodeGen)

tu = createTU("MyClass.h", args = "-xc++")
kk = getCppClasses(tu)
names(kk)

source("genOutputDev.R")

# First method
m = createMethod(kk[[1]]@methods[[1]], baseClassName = getName(kk[[1]]))

# All methds
mm = lapply(kk[[1]]@methods, createMethod, baseClassName = getName(kk[[1]]))
invisible(sapply(mm, function(x) cat(x$code, sep = "\n")))


