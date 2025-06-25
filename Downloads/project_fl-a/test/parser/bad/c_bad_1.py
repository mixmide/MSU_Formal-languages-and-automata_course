# RUN: %chocopy-llvm %s 2>&1 | FileCheck %s.err

if True:
    x = 1 
  y = 2

