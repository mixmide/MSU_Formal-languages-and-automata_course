# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

elif:
    print("elif_OK")

print("after_elif_OK")