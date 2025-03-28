module first_steps


open System


let sumDigitsDown num =
    let rec sumDigitsDownLoop accomul num =
        if num = 0 then accomul
        else sumDigitsDownLoop (accomul + num % 10) (num / 10)
    sumDigitsDownLoop 0 num

let factDigit num =
    let rec factDigitLoop mult num =
        if num = 0 then mult
        else factDigitLoop (mult * num) (num - 1)
    factDigitLoop 1 num


let SumOrFact flag =
    match flag with
    | true -> sumDigitsDown
    | false -> factDigit

let rec thingsDigits num (func: int->int->int) acc =
    match num with
    | 0 -> acc
    | someth -> thingsDigits (someth / 10) func (func acc (abs(someth) % 10))

let rec thingsDigitsCondition num (func: int->int->int) acc (condition: int->bool) =
    match num with 
    | 0 -> acc
    | someth ->
        let digit = someth % 10
        let flag = condition digit
        match flag with
        | true -> thingsDigitsCondition (someth / 10) func (func acc (someth % 10)) condition
        | false -> thingsDigitsCondition (someth / 10) func acc condition

let chooseLanguage lang =
    match lang with
    | "F#" -> "Podliza"
    | "Prolog" -> "Ah podliza"
    | "Python" -> "Mt too)"
    | someth -> "Cool"

let chooseLanguageSuperPos () = 
    Console.Write("Введите любимый язык: ")
    (Console.ReadLine >> chooseLanguage >> Console.WriteLine)

let chooseLanguageCurry () = 
    Console.Write("Введите любимый язык: ")
    let subChooseLanguageCurry reader func writer =
        let answer = reader()
        let langResult = func answer
        writer langResult
    subChooseLanguageCurry Console.ReadLine chooseLanguage Console.WriteLine


let rec cifrFold num (func: int -> int -> int) acc (condition: int -> bool) =
    match num with
    | 0 -> acc
    | tmp ->
        let digit = abs (tmp % 10)
        let flag = condition digit
        match flag with
        | true -> cifrFold (tmp / 10) func (func acc (tmp % 10)) condition
        | false -> cifrFold (tmp / 10) func acc condition


let rec gcd x y = 
   match y with
   | 0 -> x
   | someth -> gcd y (x%y)

let obhodProst num (func: int->int->int) init  =
    let rec obhodProstLoop num func acc current =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                match result with
                | 1 -> func acc current
                | _ -> acc
            obhodProstLoop num func newAcc (current - 1)
    obhodProstLoop num func init num

let obhodProstCondition num (func: int->int->int) init (condition: int -> bool)  =
    let rec obhodProstLoop num func acc current condition =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                let flag = condition current
                match result, flag with
                | 1, true -> func acc current
                | _, _ -> acc
            obhodProstLoop num func newAcc (current - 1) condition
    obhodProstLoop num func init num condition


let EulerFinder num = 
    obhodProst num (fun x y -> x + 1) 0

let isPrime num =
    if num <= 1 then false
    else
        let rec isPrimeLoop curr =
            match curr with
            | _ when curr * curr > num -> true
            | _ when num % curr = 0 -> false
            | _ -> isPrimeLoop (curr + 1)
        isPrimeLoop 2
 

let sumPrimeDivs num =
    let rec sumDivisors acc current =
        match current with
        | 0 -> acc
        | _ when num % current = 0 && isPrime current -> 
            sumDivisors (acc + current) (current - 1)
        | _ -> sumDivisors acc (current - 1)
    sumDivisors 0 num

let oddDigitsGreater3 num =
    cifrFold num 
        (fun count digit -> if digit % 2 <> 0 && digit > 3 then count + 1 else count) 
        0 
        (fun _ -> true)

let productDivsLessDigitSum num =
    let originalSum = sumDigitsDown num
    let rec productDivisors acc current =
        match current with
        | 0 -> acc
        | _ when num % current = 0 && sumDigitsDown current < originalSum ->
            productDivisors (acc * current) (current - 1)
        | _ -> productDivisors acc (current - 1)
    productDivisors 1 num


let chooseFunc ans = 
    match ans with 
    | 1 -> sumPrimeDivs
    | 2 -> oddDigitsGreater3
    | 3 -> productDivsLessDigitSum
    | _ -> failwith "Ошибка"

let chooseFuncSuperPos =
    chooseFunc >> (fun f -> f) 

let chooseFuncCyrr ans num =
    chooseFunc ans num
    

let result = 
    Console.WriteLine("1 - сумма простых делителей")
    Console.WriteLine("2 - количество нечетных цифр, больших 3")
    Console.WriteLine("3 - произведение делителей числа, сумма цифр которых меньше суммы цифр исходного числа")
    Console.WriteLine("Введите два числа - номер метода и целевое значение:")
    let input = Console.ReadLine().Split()
    let tuple = (int input.[0], int input.[1])
    Console.WriteLine($"Суперпозиция:{(chooseFuncSuperPos (fst tuple)) (snd tuple)}")
    Console.WriteLine($"Каррирование:{chooseFuncCyrr (fst tuple) (snd tuple)}")



    0
