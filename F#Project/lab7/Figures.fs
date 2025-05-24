module Figures
 
type Figure =
    | Rectangle of width: float * height: float
    | Square of sideLength: float
    | Circle of radius: float

let areaShape (figure: Figure) : float =
    match figure with
    | Rectangle (width, height) -> width * height
    | Square sideLength -> sideLength * sideLength
    | Circle radius -> System.Math.PI * radius * radius