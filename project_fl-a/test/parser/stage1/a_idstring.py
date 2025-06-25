# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -


# var_def с idstring-типом

x:"Foo" = None
print(x)
