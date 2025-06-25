# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

else:
    print("else_OK")

print("after_else_OK")