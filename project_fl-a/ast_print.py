import json

def format_ast(node, indent=0):
    lines = []
    prefix = '--' * indent
    
    if isinstance(node, dict):
        # Обработка узлов с 'kind'
        if 'kind' in node:
            kind = node['kind']
            
            # Формируем основную строку с дополнительной информацией
            parts = [kind]
            for key, value in node.items():
                if key == 'kind': 
                    continue
                if isinstance(value, (str, int, float, bool)) or value is None:
                    parts.append(str(value))
                elif key in ['name', 'value', 'className', 'operator'] and isinstance(value, str):
                    parts.append(value)
            
            lines.append(prefix + ' '.join(parts))
            
            # Рекурсивная обработка вложенных элементов
            for key, value in node.items():
                if key == 'kind':
                    continue
                if isinstance(value, (dict, list)):
                    lines.extend(format_ast(value, indent + 1))
        
        # Обработка других словарей
        else:
            for key, value in node.items():
                header = f"{prefix}{key}:"
                lines.append(header)
                lines.extend(format_ast(value, indent + 1))
    
    elif isinstance(node, list):
        # Обработка списков
        for i, item in enumerate(node):
            lines.extend(format_ast(item, indent))
    
    else:
        # Обработка примитивных значений
        lines.append(prefix + str(node))
    
    return lines

def remove_locations(obj):
    """Рекурсивно удаляет все поля 'location'"""
    if isinstance(obj, dict):
        return {k: remove_locations(v) for k, v in obj.items() if k != 'location'}
    elif isinstance(obj, list):
        return [remove_locations(item) for item in obj]
    return obj

def main():
    # Чтение и очистка AST
    with open("ast.txt", "r") as file:
        ast_data = json.load(file)
    cleaned_ast = remove_locations(ast_data)
    
    # Форматирование в древовидную структуру
    tree_lines = format_ast(cleaned_ast)
    
    # Вывод результата
    for line in tree_lines:
        print(line)

if __name__ == "__main__":
    main()