# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

print(1, 2,)
