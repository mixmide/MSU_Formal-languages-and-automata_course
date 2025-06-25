# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

x:int = 0
def foo():
    global x
    return "OK"
global x
