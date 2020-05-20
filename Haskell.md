# Документация по Haskell

## Введение

Haskell - стандартизированный типизированный функциональный язык программирования, не имеющий специального назначения.

### Основные особенности

- Функциональность

Как и в любом функциональном языке в Haskell основная структура - функция.

- Строгая типизированность

В Haskell каждая функция и объект имеют свой тип

- Чистота 

Благодаря функциальнально чистым функциям в Haskell соблаюдает основной закон: вызов функции с одними и теми же параметрами всегда даст один и тот же результат.

- Ленивость

Haskell вычисляет значения только тогда, когда они необходимы, что позволяет очень просто работать с бесконечными структурами данных.

### Официальная документация

С полной документацией от разработчика можно ознакомиться по [ссылке](https://www.seas.upenn.edu/~cis194/spring13/lectures.html).

### Плюсы и минусы языка

Помимо общеизвестных преимуществ и недостатков всех функциональных языков программирования Haskell имеет свои.

Плюсы: 

- Краткость и гибкость
- Строгая типизация
- Простота вычисления благодаря ленивой модели
- Возможность компиляции

Минусы:

- Сложность понимания ленивой модели вычислений
- Не самый быстрый язык(по сравнению например с C)

### Применение

Haskell рекомендуется использовать в задачах, где высока цена ошибки. Например, в сфере безопасности или финанасов.

В целом язык достаточно универсален и может быть использован для решения различных задач благодаря своей строгости и краткости. Однако язык не очень удобен для написания промышленного кода используется редко из-за недостатка специалистов работающих с даннымм языком.

### Установка

Стандартный способ установки Haskell - Haskell Platform. Ссылки для скачивания и описание установки доступны по [ссылке](https://www.haskell.org/platform/).

Имеются 3 основные утилиты:
- Компилятор ghc
- Интерактивная оболочка ghci
- Для запуска программ без компиляции - runhaskell

### Исходный код

Для исходных файлов в Haskell существует 2 расширения: .hs и .lhs. Разница между ними заключается в обработке комментариев компилятором.

В .hs файле комментарии начинаются с '--' или заключаются в '{-/ ... -}'.

```haskell
{- This is 
    a multiline comment
 -}

-- It's simple comment
main = putStrLn "Hello World!"
```

В .lhs файле каждая строка считается комментарием, если она явно не помечена как код. Существует два способа пометить строки кода:
- с помощью префикса '>'
- с помощью блока code

```haskell
We can use '>'.
> main = putStrLn "Hello World!"

Or we can use code block:
\begin{code}
main = putStrLn "Hello World!"
\end{code}
```

### Пример простейшей программы

Создаем .hs файл, который выведет на экран Hello world. Сохраним этот файл с названием Example.hs.

```haskell
main = putStrLn "Hello World!"
```

Самый простой и удобный (пусть и не самый быстрый) способ запуска файлов - runhaskell. Поэтому будем использовать в примерах именно его.
Таким образом, чтобы запустить написанный код напишем следюущую команду:

```haskell
 ~ runhaskell ./Example.hs
Hello World!
```

### Создание функции

Синтаксис языка был продуман так, чтобы определение функции было максимально простым. Поэтому для определения простейшей функции можно использовать следущий код:

```haskell
f x y = x*x + y*y
```