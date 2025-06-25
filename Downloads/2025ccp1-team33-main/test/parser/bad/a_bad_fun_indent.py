# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

def foo():
x:int = 1
