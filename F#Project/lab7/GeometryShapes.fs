module GeometryShapes
open System

type IPrint =
    abstract member Print: unit -> unit
 
[<AbstractClass>]
type GeomShape() =
    // Виртуальный метод для вычисления площади
    abstract member Area : unit -> float
    default this.Area() = 0.0


type Rectangle(width: float, height: float) =
    inherit GeomShape()
    member this.Width = width
    member this.Height = height

    override this.Area() = this.Width * this.Height

    override this.ToString() = sprintf "Прямоугольник (ширина: %.2f, высота: %.2f, площадь: %.2f)" width height (this.Area())

    interface IPrint with
        member this.Print() = printfn "%s" (this.ToString())

type Square(sideLength: float) =
    inherit Rectangle(sideLength, sideLength)
    
    override this.ToString() = sprintf "Квадрат (длина стороны: %.2f, площадь: %.2f)" sideLength (this.Area())

    interface IPrint with
        member this.Print() = printfn "%s" (this.ToString())

type Circle(radius: float) =
    inherit GeomShape()
    member this.Radius = radius

    override this.Area() = Math.PI * this.Radius * this.Radius

    override this.ToString() = sprintf "Круг (радиус: %.2f, площадь: %.2f)" this.Radius (this.Area())

    interface IPrint with
        member this.Print() = printfn "%s" (this.ToString())