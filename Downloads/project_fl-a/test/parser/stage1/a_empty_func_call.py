# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

# Тест “объявление функции + немедленный вызов без аргументов”
# (нигде в ALL_TESTS.txt не проверялось именно «def …; затем сразу f()»

def f() -> int:
    pass

f()
