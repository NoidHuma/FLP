module lab7main

open System
open GeometryShapes
open Figures
open Maybe
open Agent
open Parser
open Passport


let runLab7 =

    // 1 задание
    // let rectangle = new Rectangle(2.0, 6.0)
    // let square = new Square(5.0)
    // let circle = new Circle(4.5)

    // printfn "Реализация метода ToString():"
    // printfn "%s" (rectangle.ToString())
    // printfn "%s" (square.ToString())
    // printfn "%s" (circle.ToString())

    // let printInformation (shape: IPrint) =
    //     shape.Print()
 
    // printfn "Реализация метода интерфейса:"
    // printInformation rectangle
    // printInformation square
    // printInformation circle

    // printfn "Площадь прямоугольника: %.2f" (areaShape rectangle)
    // printfn "Площадь квадрата: %.2f" (areaShape square)
    // printfn "Площадь круга: %.2f" (areaShape circle)

    // 2 задание
    // checkFunctorLaws()
    // checkApplicativeLaws()
    // checkMonadLaws()

    // 3 задание
    // let expressions = [
    //     "1 + 2"
    //     "3 * (4 + 5)"
    //     "6 / 2 - 1"
    //     "10 + 2 * 3"
    // ]
    // for exp in expressions do
    //     printfn "\nВход: %s" exp
    //     parse exp |> ignore

    // 4 задание
    // agent.Post(Print "Привет, агент!")
    // agent.Post(ShowTime)
    // agent.Post(Print "Ещё одно сообщение")
    // agent.Post(Exit)
    // agent.PostAndAsyncReply(fun replyChannel ->
    //     replyChannel.Reply()
    //     Exit
    // )
    // |> Async.RunSynchronously
    // Console.ReadKey() |> ignore

    // 5 задание
    let vehicle = VehiclePassport(
        vin = "1HGBH41JXMN109186",
        mark = "Toyota",
        model = "Corolla",
        category = "Легковой",
        year = 2015,
        engineModelAndNumber = "2ZR-FE12345",
        color = "Красный",
        maxAllowedWeight = 1500.0,
        exportCountry = "Япония"
    )
    vehicle.PrintDocument()

    if vehicle.Validate() then
        printfn "Данные о транспортном средстве валидны."
    else
        printfn "Ошибка: Недопустимые данные о транспортном средстве."

    let vehicle2 = VehiclePassport(
        vin = "1HGBH41JXMN109187",  // другой VIN
        mark = "Honda",
        model = "Civic",
        category = "Легковой",
        year = 2017,
        engineModelAndNumber = "K24Z12345",
        color = "Синий",
        maxAllowedWeight = 1450.0,
        exportCountry = "Япония"
    )

    if vehicle.Equals(vehicle2) then
        printfn "Транспортные средства одинаковые."
    else
        printfn "Транспортные средства разные."

    0