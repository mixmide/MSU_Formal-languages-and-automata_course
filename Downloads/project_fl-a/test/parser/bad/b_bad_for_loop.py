# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

for x, y in lst:
    print(x)
    print(y)

print("OK")