# RUN: %chocopy-llvm %s -ast-dump | diff %s.ast -

# “call inside index”: get_list()[1]. 
# не было случая, когда CallExpr выступает в левой части index_expr.

val:int = 0
res_list:[int] = None
s:str = "xyz"
c:str = "ok"
res_upper:str = "ok"

def get_list() -> [int]:
    return [10, 20, 30]

#в теле вызов функции и сразу индекс:
res_list = get_list()
val = get_list()[1]

# строку превращаем в список символов
c = s[2]

# ниже берём значение и сразу вызываем метод (хотя метода у строк нет,
# синтаксически это DeclRef + CallExpr, а семант. ошибка будет ловить Sema)
# парсер лишь должен увидеть: (s[1]).upper()
# хотя ChocoPy вроде вообще не поддерживает upper().
# Итак, парсер должен разобрать s[1] → DeclRef("s") + IndexExpr, 
# затем MemberExpr("upper") => CallExpr (пустой args). 
# такая вложенность нигде не проверялась.
res_upper = s[1].upper()

print(val)
print(c)
print(res_upper)
