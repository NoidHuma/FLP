module church_lists


open System


let rec readList n = 
    if n=0 then []
    else
    let Head = System.Convert.ToInt32(System.Console.ReadLine())
    let Tail = readList (n-1)
    Head::Tail

let readData = 
    let n=System.Convert.ToInt32(System.Console.ReadLine())
    readList n

let rec writeList = function
    [] ->   let z = System.Console.ReadKey()
            0
    | (head : int)::tail -> 
                       System.Console.WriteLine(head)
                       writeList tail  


let rec accCond list (f : int -> int -> int) p acc = 
    match list with
    | [] -> acc
    | h::t ->
                let newAcc = f acc h
                if p h then accCond t f p newAcc
                else accCond t f p acc

let listMin list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list (fun x y -> if x < y then x else y) (fun x -> true) h

let listMax list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list (fun x y -> if x > y then x else y) (fun x -> true) h

let evenSum list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list (fun x y -> x + y) (fun x -> (x % 2 = 0)) 0

let unevenCount list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list (fun x y -> x + 1) (fun x -> (x % 2 = 1)) 0


// 5

let elemFrequencyRec list elem =
    match list with
    |[] -> 0
    | h::t -> accCond list (fun x y -> x + 1) (fun x -> x = elem) 0

let mostFrequentRec list =
    match list with
    |[] -> 0
    | h::t -> accCond list (fun x y -> (if elemFrequencyRec list y > elemFrequencyRec list x then y else x)) (fun x -> true) 0

// tree
type BinaryTree = 
    | Node of int * BinaryTree * BinaryTree 
    | Entry

let rec printInOrder tree = 
    match tree with 
    | Node (data, left, right) -> 
        printInOrder left 
        printfn "Node %d" data 
        printInOrder right 
    | Entry -> ()

(*
      2 
     / \ 
    1   4 
       / \ 
      3   5 
*)

let binTree() = 
    Node(2, 
        Node(1, Entry, Entry), 
        Node(4, 
            Node(3, Entry, Entry), 
            Node(5, Entry, Entry)
        )
    )


// 7 (list)
let mostFrequent list =
    list
    |> List.countBy id
    |> List.maxBy snd
    |> fst

// 8
let countSquareElements list =
    list
    |> List.filter (fun x -> 
        list |> List.exists (fun y -> y * y = x))
    |> List.length

//9
let rec sumOfDigits (n : int) : int = 
    if n = 0 then 0
    else (n % 10) + (sumOfDigits (n / 10))

let divsCount n = 
    let rec divsCount n div count = 
        if div = n then count + 1
        else    let divNew = div + 1
                if (n % div) = 0 then 
                        let countNew = count + 1
                        divsCount n divNew countNew
                else
                        divsCount n divNew count
    divsCount n 1 0

let ex9func list1 list2 list3 = List.zip3 (List.rev (List.sort list1)) (List.sortBy (fun x -> (sumOfDigits x)) list2) (List.rev (List.sortBy (fun x -> (divsCount x)) list3))

// 10 (string)
let rec readStringList n = 
    if n = 0 then []
    else
        printfn "Введите строку:"
        let head = System.Console.ReadLine()
        let tail = readStringList (n - 1)
        head :: tail

let readStringData() = 
    printfn "Введите количество строк:"
    let n = System.Convert.ToInt32(System.Console.ReadLine())
    readStringList n

let rec writeStringList = function
    | [] -> 
        let z = System.Console.ReadKey()
        0
    | (head:string) :: tail -> 
        System.Console.WriteLine(head)
        writeStringList tail

// 11-16
let LastMaxRec list =
    let rec loop currentMax lastMaxIndex currentIndex = function
        | [] -> List.length list - lastMaxIndex - 1
        | h :: t ->
            if h >= currentMax
            then loop h currentIndex (currentIndex + 1) t
            else loop currentMax lastMaxIndex (currentIndex + 1) t
    match list with
    | [] -> 0
    | head :: tail -> loop head 0 1 tail

let FirstMaxRec list =
    let rec loop currentMax acc = function
        | [] -> acc
        | h :: t ->
            if h > currentMax then 
                loop h [] t
            else 
                loop currentMax (acc @ [h]) t

    match list with
    | [] -> []
    | head :: tail -> 
        loop head [] tail

        
let findUniqueRec list =
    let rec findDifferent main = function
        | [] -> failwith "Нет уникального элемента"
        | x :: xs -> if x <> main then x else findDifferent main xs

    match list with
    | [] -> failwith "Пустой список"
    | [x] -> x
    | x1 :: x2 :: rest ->
        if x1 = x2 then
            findDifferent x1 list
        else
            match rest with
            | x3 :: _ ->
                if x3 = x1 then x2
                elif x3 = x2 then x1
            | [] -> failwith "Слишком короткий список"

let findUniqueElement list =
    list
    |> List.groupBy id
    |> List.pick (fun (key, group) -> 
        if List.length group = 1 then Some key else None)


let LastMax list newList=
    if List.isEmpty list then 0
    else
        let maxVal = List.max list
        let lastIndex = List.findIndexBack (fun x -> x = maxVal) list
        List.length list - lastIndex - 1

let FirstMax list =
    if List.isEmpty list then []
    else
        let maxValue = List.max list
        list
        |> List.tryFindIndex (fun x -> x = maxValue)
        |> Option.map (fun idx -> List.skip (idx + 1) list)
        |> Option.defaultValue []


let rec countEvenRec list =
    match list with
    | [] -> 0
    | head :: tail ->
        let increment = if head % 2 = 0 then 1 else 0
        increment + countEvenRec tail

let countEvenList list =
    list
    |> List.filter (fun x -> x % 2 = 0)
    |> List.length

let meanOfAbsRec list =
    let rec loop sum = function
        | [] -> sum
        | h :: t -> loop (sum + abs h) t
    
    if List.isEmpty list then 0.0
    else
        let total = loop 0 list
        float total / float (List.length list)


let meanOfAbs list =
    if List.isEmpty list then 0.0
    else
        list
        |> List.map abs
        |> List.averageBy float


let result = 
    let l = readData
    System.Console.WriteLine(listMin l)
    System.Console.WriteLine(listMax l)
    System.Console.WriteLine(evenSum l)
    System.Console.WriteLine(unevenCount l)
    System.Console.WriteLine(mostFrequentRec l)
    System.Console.WriteLine(mostFrequent l)
    System.Console.WriteLine(countSquareElements l)
    
    let strings = readStringData()
    printfn "Исходные строки:"
    writeStringList strings
    let sortedByLengthDesc = List.sortByDescending (fun (s: string) -> s.Length) strings
    printfn "\nСтроки, отсортированные по убыванию длины:"
    writeStringList sortedByLengthDesc
    
    let listik = readData
    System.Console.WriteLine (FirstMaxRec listik)
    System.Console.WriteLine (FirstMax listik)
    
    0

