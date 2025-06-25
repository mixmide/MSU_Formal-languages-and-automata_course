# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

class A(B, C):
    x: int = 1