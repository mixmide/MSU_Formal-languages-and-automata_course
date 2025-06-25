# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

# пустой список складывается с непустым и затем сразу индексируется

e:[int] = None
f:[int] = None
g:[int] = None
x:int = 0

f = [1, 2]
g = e + f

x = g[1]

print(x)
