open System

let square (a:int64) = a * a

let cube (a:int64) = a * a * a

let build_num (x:int64) (y:int64) (z:int64) = square x * y * cube z + x * square y

let rec gcd (a:int64) (b:int64) =
    match b with
    | 0L -> abs a
    | _ -> gcd b (a % b)

let is_square (num:int64) =
    match num > 0L with
    | false -> false
    | true ->
        num
        |> float
        |> sqrt
        |> int64
        |> fun x -> x * x = num

let rec x_enumDown (x:int64) (y:int64) (z:int64) (n:int64) (sum:int64) =
    match x > (int64 (Math.Pow(float (n / (y * cube z)), 1.0 / 2.0))) with
    | false when build_num x y z
        |> is_square
        -> x_enumDown (x + 1L) y z n (sum + build_num x y z)
    | false -> x_enumDown (x + 1L) y z n sum
    | _ -> sum

let rec y_enumDown (y:int64) (z:int64) (n:int64) (sum:int64) =
    match y >= z with
    | false when gcd y z = 1L -> y_enumDown (y + 1L) z n (x_enumDown 1L y z n sum)
    | false -> y_enumDown (y + 1L) z n sum
    | _ -> sum
    
let rec z_enumDown (z:int64) (n:int64) (sum:int64) = 
    match z > (int64 (Math.Pow((float n), 1.0 / 3.0))) with
    | false -> z_enumDown (z + 1L) n (y_enumDown 1L z n sum)
    | _ -> sum
    
let result (n:int64) = z_enumDown 1L n 0L

let finalResult = result 100000L

printfn "Сумма прогрессирующих идеальных квадратов: %d" finalResult
