# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

# for + плюс внутри тела комбинированный if-else

lst:[int] = None
total:int = 0

lst = [1, 2, 3, 4, 5]

for i in lst:
    if i % 2 == 0:
        total = total + i
    else:
        total = total + (i * 3)

print(total)
