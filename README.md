# Лабораторная работа 2

## Вариант `rb-dict-lazy`


---

  * Студент: `Морозов Ярослав Валерьевич`
  * Группа: `P3315`
  * ИСУ: `409178`
  * Функциональный язык: `Ocaml`

---

## Требования

Интерфейс — `Dict`, структура данных — `Red-Black Tree`.

1. Функции:
    * [x] добавление и удаление элементов;
    * [x] фильтрация;
    * [x] отображение (`map`);
    * [x] свертки (левая и правая);
    * [x] структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/%D0%9C%D0%BE%D0%BD%D0%BE%D0%B8%D0%B4).
2. Структуры данных должны быть **неизменяемыми**.
3. Библиотека должна быть протестирована в рамках **unit testing**.
4. Библиотека должна быть протестирована в рамках **property-based** тестирования (*как минимум 3 свойства*, включая свойства моноида).
5. Структура должна быть **полиморфной**.
6. Требуется использовать [идиоматичный для технологии стиль программирования](https://github.com/ocaml/ocaml/blob/trunk/stdlib/map.ml). Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка — необходимо реализовать их вручную и по возможности — обеспечить совместимость.

---

## Ключевые элементы реализации

Добавление, получение и удаление элементов:

```ocaml
let internal_add key value tree =
    let rec ins t =
      match t with
      | Leaf ->
          Node
            {
              color = Red;
              left = Leaf;
              key;
              value;
              is_deleted = false;
              right = Leaf;
            }
      | Node
          {
            color = c;
            left = l;
            key = k;
            value = v;
            is_deleted = del;
            right = r;
          } ->
          let cmp = Key.compare key k in
          if cmp < 0 then balance c (ins l) k v del r
          else if cmp > 0 then balance c l k v del (ins r)
          else
            Node
              {
                color = c;
                left = l;
                key = k;
                value;
                is_deleted = false;
                right = r;
              }
    in
    match ins tree with Node r -> Node { r with color = Black } | Leaf -> Leaf

  let rec internal_find_opt key t =
    match t with
    | Leaf -> None
    | Node { left = l; key = k; value = v; is_deleted = del; right = r; _ } ->
        let cmp = Key.compare key k in
        if cmp < 0 then internal_find_opt key l
        else if cmp > 0 then internal_find_opt key r
        else if del then None
        else Some v

  let internal_remove key t =
    let rec loop t =
      match t with
      | Leaf -> Leaf
      | Node
          {
            color = c;
            left = l;
            key = k;
            value = v;
            is_deleted = del;
            right = r;
          } ->
          let cmp = Key.compare key k in
          if cmp < 0 then
            Node
              {
                color = c;
                left = loop l;
                key = k;
                value = v;
                is_deleted = del;
                right = r;
              }
          else if cmp > 0 then
            Node
              {
                color = c;
                left = l;
                key = k;
                value = v;
                is_deleted = del;
                right = loop r;
              }
          else
            Node
              {
                color = c;
                left = l;
                key = k;
                value = v;
                is_deleted = true;
                right = r;
              }
    in
    loop t
```

Отображение (`map`):

```ocaml
 let map f t =
    let rec internal_map t =
      match t with
      | Leaf -> Leaf
      | Node ({ left = l; value = v; right = r; _ } as node_record) ->
          Node
            {
              node_record with
              left = internal_map l;
              value = f v;
              right = internal_map r;
            }
    in
    { t with root = internal_map t.root }
```

Сверткa:

```ocaml
let rec internal_fold f t acc =
    match t with
    | Leaf -> acc
    | Node { left = l; key = k; value = v; is_deleted = del; right = r; _ } ->
        let acc_l = internal_fold f l acc in
        let acc_curr = if del then acc_l else f k v acc_l in
        internal_fold f r acc_curr
```

### Соответствие свойству [моноида](https://ru.m.wikipedia.org/wiki/%D0%9C%D0%BE%D0%BD%D0%BE%D0%B8%D0%B4)

Определили пустой элемент:

```elixir
let empty = { root = Leaf; active = 0; total = 0 }
```

Определили бинарную операцию `union`:

```ocaml
let fold f t acc = internal_fold f t.root acc
let union t1 t2 = fold (fun k v acc -> add k v acc) t1 t2
```

## Тестирование

В рамках данной работы были применены два инструмента:

  * ounit2 - для модульного тестирования;
  * qcheck - для тестирования свойств (property-based).

## Выводы

В данной лабораторной работе была реализована структура данных "Красно-чёрное дерево" (Red-Black Tree) в виде функтора, предоставляющего интерфейс ассоциативного массива (Map). Это самобалансирующееся двоичное дерево поиска обеспечивает логарифмическую сложность поиска и вставки.

В реализации были использованы следующие приёмы программирования:

  * Модульная система и Функторы: Реализация выполнена в виде функтора Make, который принимает модуль Key (с интерфейсом OrderedType) и возвращает модуль с типом S. Это позволяет создавать типизированные деревья для любых типов ключей (числа, строки, пользовательские типы).
  * Алгебраические типы данных и Записи: Для представления узлов дерева используется алгебраический тип tree с конструкторами Leaf и Node. Узлы Node реализованы через record (запись), хранящую цвет, ключи, значения и служебные флаги.
  * Стратегия "Ленивого удаления" (Lazy Deletion): Вместо физического удаления узлов и сложной перебалансировки "на лету", используется поле is_deleted.
Узел помечается удалённым логически.
Физическая перестройка дерева (rebuild) происходит только тогда, когда количество "мусорных" узлов превышает допустимый порог (в данной реализации — более 50% от общего числа узлов).
  * Работа с последовательностями (Iterators/Sequences): Для реализации сравнения деревьев (compare) и генерации потока элементов использован модуль Seq (ленивые последовательности), что позволяет эффективно обходить дерево без создания промежуточных коллекций.
  * Pattern Matching (Сопоставление с образцом): Ключевой элемент реализации функции балансировки balance. Использование паттерн-матчинга (метод Окасаки) позволило свести 4 случая нарушения свойств красно-чёрного дерева к одному общему виду балансировки, что делает код лаконичным и надежным.
