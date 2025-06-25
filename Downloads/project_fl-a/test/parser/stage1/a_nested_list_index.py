# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

#строим вложенный список и двойную индексацию в теле

lst:[int] = None
x:int = 0

lst = [[1,2], [3,4]]
x = lst[1][0]  

print(x)