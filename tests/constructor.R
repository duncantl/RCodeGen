library(RCodeGen)
library(RCIndex)

classes = getCppClasses("constructor.cc")

k = classes[[1]]
r = k@methods[[1]]

code = createCppMethod(r)
code

foo = createRProxy(k@methods$foo)

bar = createRProxy(k@methods$bar)

def = createRProxy(k@methods$def)



