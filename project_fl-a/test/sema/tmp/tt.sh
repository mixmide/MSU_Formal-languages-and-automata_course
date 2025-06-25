#!/bin/bash

# Папка с тестами
TEST_DIR="/home/victo/sidedisk/msu/Form/chocopy-workdir/chocopy-llvm/test/sema/tmp"

# Проверяем, существует ли папка
if [ ! -d "$TEST_DIR" ]; then
    echo "Ошибка: папка $TEST_DIR не существует!"
    exit 1
fi

# Счетчики тестов
total_tests=0
passed_tests=0
failed_tests=0

# Проходим по всем .py файлам и запускаем chocopy-llvm
for test_file in "$TEST_DIR"/*.py; do
    ((total_tests++))
    filename=$(basename "$test_file")
    
    echo -n "Тест $total_tests: $filename ... "
    
    # Запускаем тест и захватываем вывод
    output=$(chocopy-llvm --run-sema "$test_file" 2>&1)
    exit_code=$?
    
    if [ $exit_code -eq 0 ] && [ -z "$output" ]; then
        echo "УСПЕХ"
        ((passed_tests++))
    else
        echo "ПРОВАЛ"
        ((failed_tests++))
        
        # Выводим дополнительную информацию для проваленных тестов
        echo "  Причина:"
        if [ $exit_code -ne 0 ]; then
            echo "  - Программа завершилась с кодом ошибки $exit_code"
        fi
        if [ -n "$output" ]; then
            echo "  - Программа вывела:"
            echo "$output" | sed 's/^/    /'
        fi
        echo "----------------------------------"
    fi
done

echo ""
echo "Итоговые результаты:"
echo "Всего тестов: $total_tests"
echo "Успешных:     $passed_tests (без вывода и без ошибок)"
echo "Проваленных:  $failed_tests"

if [ "$failed_tests" -eq 0 ]; then
    echo "Все тесты прошли успешно!"
    exit 0
else
    echo "Найдены ошибки в тестах!"
    exit 1
fi