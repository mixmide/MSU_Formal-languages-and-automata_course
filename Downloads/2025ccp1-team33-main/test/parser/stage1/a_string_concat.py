# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

#проверка объявления строк и конкатенации
s:str = "Hello"

s = s + " World"
print(s)
