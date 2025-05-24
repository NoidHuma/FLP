module Passport
open System
open System.Text.RegularExpressions

// Паспорт транспортного средства
type VehiclePassport(vin: string, mark: string, model: string, category: string, year: int, engineModelAndNumber: string, color: string, maxAllowedWeight: float, exportCountry: string) =
    // Поля
    let mutable _vin = vin                                   // Идентификационный номер (VIN) автомобиля
    let mutable _mark = mark                                 // Марка автомобиля
    let mutable _model = model                               // Модель автомобиля
    let mutable _category = category                         // Категория транспортного средства
    let mutable _year = year                                 // Год изготовления
    let mutable _engineModelAndNumber = engineModelAndNumber // Модель и номер двигателя
    let mutable _color = color                               // Цвет кузова
    let mutable _maxAllowedWeight = maxAllowedWeight         // Максимальная разрешённая масса
    let mutable _exportCountry = exportCountry               // Страна вывоза

    // Валидации
    member this.ValidateVIN() = Regex.IsMatch(_vin.ToUpper(), @"^[A-HJ-NPR-Z0-9]{17}$")

    member this.ValidateYear() = _year >= 1886 && _year <= DateTime.Now.Year

    member this.ValidateMaxAllowedWeight() = _maxAllowedWeight > 0.0

    member this.Validate() =
        this.ValidateVIN() &&
        this.ValidateYear() &&
        this.ValidateMaxAllowedWeight()

    // Свойства
    member this.VIN
        with get() = _vin
        and set(value: string) =
            if Regex.IsMatch(value.ToUpper(), @"^[A-HJ-NPR-Z0-9]{17}$") then _vin <- value
            else failwith "Неверный формат VIN."

    member this.Mark
        with get() = _mark
        and set(value) = _mark <- value

    member this.Model
        with get() = _model
        and set(value) = _model <- value

    member this.Category
        with get() = _category
        and set(value) = _category <- value

    member this.Year
        with get() = _year
        and set(value) =
            if value >= 1886 && value <= DateTime.Now.Year then _year <- value
            else failwith "Недопустимый год изготовления."

    member this.EngineModelAndNumber
        with get() = _engineModelAndNumber
        and set(value) = _engineModelAndNumber <- value

    member this.Color
        with get() = _color
        and set(value) = _color <- value

    member this.MaxAllowedWeight
        with get() = _maxAllowedWeight
        and set(value) = 
            if value > 0.0 then _maxAllowedWeight <- value
            else failwith "Недопустимая максимальная масса."

    member this.ExportCountry
        with get() = _exportCountry
        and set(value) = _exportCountry <- value

    // Метод для вывода документа на экран
    member this.PrintDocument() =
        if this.Validate() then
            printfn "------ Паспорт транспортного средства (ПТС) ------"
            printfn "Идентификационный номер (VIN): %s" this.VIN
            printfn "Марка: %s" this.Mark
            printfn "Модель: %s" this.Model
            printfn "Категория: %s" this.Category
            printfn "Год изготовления: %d" this.Year
            printfn "Модель и номер двигателя: %s" this.EngineModelAndNumber
            printfn "Цвет кузова: %s" this.Color
            printfn "Максимальная разрешённая масса: %.2f кг" this.MaxAllowedWeight
            printfn "Страна-изготовитель: %s" this.ExportCountry
            printfn "-----------------------------------------------"
        else
            printfn "Ошибка: недопустимые данные ПТС."

    // Сравнение двух ПТС
    member this.Equals(other: VehiclePassport) =
        this.VIN = other.VIN &&
        this.Mark = other.Mark &&
        this.Model = other.Model &&
        this.Year = other.Year