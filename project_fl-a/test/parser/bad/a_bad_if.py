# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

if True:

print("Ok")