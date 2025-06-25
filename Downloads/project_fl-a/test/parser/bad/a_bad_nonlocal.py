# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

def foo():
    x:int = 0
    def foo2():
        nonlocal x
        x = x + 1
    return 0

nonlocal x
