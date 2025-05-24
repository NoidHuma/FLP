module Maybe

type Maybe<'T> =
    | Just of 'T
    | Nothing

// функтор
let map (f: 'T -> 'U) (m: Maybe<'T>) : Maybe<'U> =
    match m with
    | Just x -> Just (f x)
    | Nothing -> Nothing

// аппликативный функтор
// функция для подъема значения в Maybe
let pure (x: 'T) : Maybe<'T> = Just x

// функция для применения обернутой функции к значению в Maybe
let apply (mf: Maybe<'T -> 'U>) (mx: Maybe<'T>) : Maybe<'U> =
    match mf, mx with
    | Just f, Just x -> Just (f x)
    | _, _ -> Nothing

// монада
// функция для связывания двух вычислений в Maybe
let bind (m: Maybe<'T>) (f: 'T -> Maybe<'U>) : Maybe<'U> =
    match m with
    | Just x -> f x
    | Nothing -> Nothing


let id x = x
let checkFunctorLaws() =
    let x1 = Just 5
    let x2 = Nothing
    
    let f a = a + 2
    let g a = a * 3

    // Закон идентичности: fmap id = id
    let law1a = map id x1 = x1
    let law1b = map id x2 = x2

    // Закон композиции: fmap (f . g) = fmap f . fmap g
    // fmap (f >> g) x = fmap g (fmap f x)
    let law2a = map (f >> g) x1 = map g (map f x1)
    let law2b = map (f >> g) x2 = map g (map f x2)

    printfn "Функтор — закон идентичности: %b, %b" law1a law1b
    printfn "Функтор — закон композиции: %b, %b" law2a law2b

let checkApplicativeLaws() =
    let v = Just 10
    let f = fun x -> x + 3
    let u = Just f
    let y = 5

    // идентичность
    let law1 = apply (pure id) v = v
    // гомоморфизм
    let law2 = apply (pure f) (pure y) = pure (f y)
    // интерчанж
    let law3 = apply u (pure y) = apply (pure (fun f -> f y)) u

    printfn "Аппликатив — закон идентичности: %b" law1
    printfn "Аппликатив — закон гомоморфизма: %b" law2
    printfn "Аппликатив — закон интерчанжа: %b" law3

let checkMonadLaws() =
    let m = Just 5
    let f x = Just (x + 1)
    let g x = Just (x * 2)

    // закон левого нейтрального элемента: return a >>= f = f a
    let left = bind (pure 5) f = f 5

    // закон правого нейтрального элемента: m >>= return = m
    let right = bind m pure = m

    // Ассоциативность: (m >>= f) >>= g = m >>= (fun x -> f x >>= g)
    let assoc = bind (bind m f) g = bind m (fun x -> bind (f x) g)

    printfn "Монада — закон левой единицы: %b" left
    printfn "Монада — закон правой единицы: %b" right
    printfn "Монада — закон ассоциативности: %b" assoc