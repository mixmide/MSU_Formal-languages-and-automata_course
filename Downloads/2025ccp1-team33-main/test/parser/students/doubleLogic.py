# R# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

True == not False