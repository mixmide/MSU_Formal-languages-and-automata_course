# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

# сравнения строк “<” и “==” в теле

a:str = "abc"
b:str = "abd"

is_lt:bool = True
is_eq:bool = True
is_lt = a < b
is_eq = a == "abc"

print(is_lt)
print(is_eq)
