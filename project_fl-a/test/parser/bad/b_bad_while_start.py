# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

while:
    pass

print("OK")