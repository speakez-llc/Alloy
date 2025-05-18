module Alloy.Numerics

open Alloy.Core

[<AutoOpen>]
module Internal =
    // Basic numeric operations for primitive types
    
    [<AbstractClass; Sealed>]
    type BasicOps =
        // Add implementations
        static member inline Add(a: int, b: int) = a + b
        static member inline Add(a: float, b: float) = a + b
        static member inline Add(a: int64, b: int64) = a + b
        static member inline Add(a: uint64, b: uint64) = a + b
        static member inline Add(a: float32, b: float32) = a + b
        static member inline Add(a: decimal, b: decimal) = a + b
        static member inline Add(a: byte, b: byte) = a + b
        static member inline Add(a: uint32, b: uint32) = a + b
        static member inline Add(a: int16, b: int16) = a + b
        static member inline Add(a: uint16, b: uint16) = a + b
        
        static member inline Add(a: string, b: string) = 
            let aChars = if isNull a then [||] else Array.init a.Length (fun i -> a.[i])
            let bChars = if isNull b then [||] else Array.init b.Length (fun i -> b.[i])
            let result = Array.zeroCreate (aChars.Length + bChars.Length)
            for i = 0 to aChars.Length - 1 do
                result.[i] <- aChars.[i]
            for i = 0 to bChars.Length - 1 do
                result.[i + aChars.Length] <- bChars.[i]
            new string(result)
            
        // Subtract implementations
        static member inline Subtract(a: int, b: int) = a - b
        static member inline Subtract(a: float, b: float) = a - b
        static member inline Subtract(a: int64, b: int64) = a - b
        static member inline Subtract(a: uint64, b: uint64) = a - b
        static member inline Subtract(a: float32, b: float32) = a - b
        static member inline Subtract(a: decimal, b: decimal) = a - b
        static member inline Subtract(a: byte, b: byte) = a - b
        static member inline Subtract(a: uint32, b: uint32) = a - b
        static member inline Subtract(a: int16, b: int16) = a - b
        static member inline Subtract(a: uint16, b: uint16) = a - b
        
        // Multiply implementations
        static member inline Multiply(a: int, b: int) = a * b
        static member inline Multiply(a: float, b: float) = a * b
        static member inline Multiply(a: int64, b: int64) = a * b
        static member inline Multiply(a: uint64, b: uint64) = a * b
        static member inline Multiply(a: float32, b: float32) = a * b
        static member inline Multiply(a: decimal, b: decimal) = a * b
        static member inline Multiply(a: byte, b: byte) = a * b
        static member inline Multiply(a: uint32, b: uint32) = a * b
        static member inline Multiply(a: int16, b: int16) = a * b
        static member inline Multiply(a: uint16, b: uint16) = a * b
        
        // Divide implementations
        static member inline Divide(a: int, b: int) = a / b
        static member inline Divide(a: float, b: float) = a / b
        static member inline Divide(a: int64, b: int64) = a / b
        static member inline Divide(a: uint64, b: uint64) = a / b
        static member inline Divide(a: float32, b: float32) = a / b
        static member inline Divide(a: decimal, b: decimal) = a / b
        static member inline Divide(a: byte, b: byte) = a / b
        static member inline Divide(a: uint32, b: uint32) = a / b
        static member inline Divide(a: int16, b: int16) = a / b
        static member inline Divide(a: uint16, b: uint16) = a / b
    
    // Operations for unit-of-measure types - single measure
    
    [<AbstractClass; Sealed>]
    type MeasureOps =
        // Add implementations for unit-of-measure types
        static member inline AddInt(a: int<'u>, b: int<'u>) = a + b
        static member inline AddFloat(a: float<'u>, b: float<'u>) = a + b
        static member inline AddInt64(a: int64<'u>, b: int64<'u>) = a + b
        static member inline AddUInt64(a: uint64<'u>, b: uint64<'u>) = a + b
        static member inline AddFloat32(a: float32<'u>, b: float32<'u>) = a + b
        static member inline AddDecimal(a: decimal<'u>, b: decimal<'u>) = a + b
        
        // Subtract implementations for unit-of-measure types
        static member inline SubtractInt(a: int<'u>, b: int<'u>) = a - b
        static member inline SubtractFloat(a: float<'u>, b: float<'u>) = a - b
        static member inline SubtractInt64(a: int64<'u>, b: int64<'u>) = a - b
        static member inline SubtractUInt64(a: uint64<'u>, b: uint64<'u>) = a - b
        static member inline SubtractFloat32(a: float32<'u>, b: float32<'u>) = a - b
        static member inline SubtractDecimal(a: decimal<'u>, b: decimal<'u>) = a - b
        
        // Multiply: unit * scalar implementations
        static member inline MultiplyIntScalar(a: int<'u>, b: int) = a * b
        static member inline MultiplyFloatScalar(a: float<'u>, b: float) = a * b
        static member inline MultiplyInt64Scalar(a: int64<'u>, b: int64) = a * b
        static member inline MultiplyUInt64Scalar(a: uint64<'u>, b: uint64) = a * b
        static member inline MultiplyFloat32Scalar(a: float32<'u>, b: float32) = a * b
        static member inline MultiplyDecimalScalar(a: decimal<'u>, b: decimal) = a * b
        
        // Multiply: scalar * unit implementations (new)
        static member inline MultiplyScalarInt(a: int, b: int<'u>) = a * b
        static member inline MultiplyScalarFloat(a: float, b: float<'u>) = a * b
        static member inline MultiplyScalarInt64(a: int64, b: int64<'u>) = a * b
        static member inline MultiplyScalarUInt64(a: uint64, b: uint64<'u>) = a * b
        static member inline MultiplyScalarFloat32(a: float32, b: float32<'u>) = a * b
        static member inline MultiplyScalarDecimal(a: decimal, b: decimal<'u>) = a * b
        
        // Divide: unit / scalar implementations
        static member inline DivideIntScalar(a: int<'u>, b: int) = a / b
        static member inline DivideFloatScalar(a: float<'u>, b: float) = a / b
        static member inline DivideInt64Scalar(a: int64<'u>, b: int64) = a / b
        static member inline DivideUInt64Scalar(a: uint64<'u>, b: uint64) = a / b
        static member inline DivideFloat32Scalar(a: float32<'u>, b: float32) = a / b
        static member inline DivideDecimalScalar(a: decimal<'u>, b: decimal) = a / b
        
        // Divide: scalar / unit implementations (new)
        static member inline DivideScalarInt(a: int, b: int<'u>) = a / b
        static member inline DivideScalarFloat(a: float, b: float<'u>) = a / b
        static member inline DivideScalarInt64(a: int64, b: int64<'u>) = a / b
        static member inline DivideScalarUInt64(a: uint64, b: uint64<'u>) = a / b
        static member inline DivideScalarFloat32(a: float32, b: float32<'u>) = a / b
        static member inline DivideScalarDecimal(a: decimal, b: decimal<'u>) = a / b
    
    // Operations for unit-of-measure types - different measures
    
    [<AbstractClass; Sealed>]
    type MeasureMeasureOps =
        // Multiply implementations for different unit-of-measure types
        static member inline MultiplyIntUnits(a: int<'u1>, b: int<'u2>) = a * b
        static member inline MultiplyFloatUnits(a: float<'u1>, b: float<'u2>) = a * b
        static member inline MultiplyInt64Units(a: int64<'u1>, b: int64<'u2>) = a * b
        static member inline MultiplyUInt64Units(a: uint64<'u1>, b: uint64<'u2>) = a * b
        static member inline MultiplyFloat32Units(a: float32<'u1>, b: float32<'u2>) = a * b
        static member inline MultiplyDecimalUnits(a: decimal<'u1>, b: decimal<'u2>) = a * b
        
        // Divide implementations for different unit-of-measure types
        static member inline DivideIntUnits(a: int<'u1>, b: int<'u2>) = a / b
        static member inline DivideFloatUnits(a: float<'u1>, b: float<'u2>) = a / b
        static member inline DivideInt64Units(a: int64<'u1>, b: int64<'u2>) = a / b
        static member inline DivideUInt64Units(a: uint64<'u1>, b: uint64<'u2>) = a / b
        static member inline DivideFloat32Units(a: float32<'u1>, b: float32<'u2>) = a / b
        static member inline DivideDecimalUnits(a: decimal<'u1>, b: decimal<'u2>) = a / b
    
    // Collection operations for arrays of primitive types
    
    [<AbstractClass; Sealed>]
    type ArrayOps =
        static member inline Sum(xs: int[]) = 
            let mutable sum = 0
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: float[]) = 
            let mutable sum = 0.0
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: int64[]) = 
            let mutable sum = 0L
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: float32[]) = 
            let mutable sum = 0.0f
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: decimal[]) = 
            let mutable sum = 0M
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: byte[]) = 
            let mutable sum = 0uy
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: uint32[]) = 
            let mutable sum = 0u
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: int16[]) = 
            let mutable sum = 0s
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: uint16[]) = 
            let mutable sum = 0us
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Average(xs: int[]) = 
            if xs.Length = 0 then 0
            else
                let mutable sum = 0
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / xs.Length
                
        static member inline Average(xs: float[]) = 
            if xs.Length = 0 then 0.0
            else
                let mutable sum = 0.0
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / float xs.Length
                
        static member inline Average(xs: int64[]) = 
            if xs.Length = 0 then 0L
            else
                let mutable sum = 0L
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / int64 xs.Length
                
        static member inline Average(xs: float32[]) = 
            if xs.Length = 0 then 0.0f
            else
                let mutable sum = 0.0f
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / float32 xs.Length
                
        static member inline Average(xs: decimal[]) = 
            if xs.Length = 0 then 0M
            else
                let mutable sum = 0M
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / decimal xs.Length
                
        static member inline Average(xs: byte[]) = 
            if xs.Length = 0 then 0uy
            else
                let mutable sum = 0uy
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / byte xs.Length
                
        static member inline Average(xs: uint32[]) = 
            if xs.Length = 0 then 0u
            else
                let mutable sum = 0u
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / uint32 xs.Length
                
        static member inline Average(xs: int16[]) = 
            if xs.Length = 0 then 0s
            else
                let mutable sum = 0s
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / int16 xs.Length
                
        static member inline Average(xs: uint16[]) = 
            if xs.Length = 0 then 0us
            else
                let mutable sum = 0us
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / uint16 xs.Length
    
    // Collection operations for arrays of unit-of-measure types
    
    [<AbstractClass; Sealed>]
    type MeasureArrayOps =
        static member inline SumInt(xs: int<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<int<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum
            
        static member inline SumFloat(xs: float<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<float<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum
            
        static member inline SumInt64(xs: int64<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<int64<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum
            
        static member inline SumFloat32(xs: float32<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<float32<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum
                
        static member inline SumDecimal(xs: decimal<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<decimal<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum
                
        static member inline AverageInt(xs: int<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<int<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / xs.Length
                
        static member inline AverageFloat(xs: float<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<float<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / float xs.Length
                
        static member inline AverageInt64(xs: int64<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<int64<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / int64 xs.Length
                
        static member inline AverageFloat32(xs: float32<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<float32<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / float32 xs.Length
                
        static member inline AverageDecimal(xs: decimal<'u>[]) = 
            if xs.Length = 0 then LanguagePrimitives.GenericZero<decimal<'u>>
            else
                let mutable sum = xs.[0]
                for i = 1 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / decimal xs.Length
    
    // Min/Max operations for primitive types
    
    module MinMaxOperations =
        [<AbstractClass; Sealed>]
        type MinImpl =
            static member Min(a: int, b: int) : int = if a < b then a else b
            static member Min(a: float, b: float) : float = if a < b then a else b
            static member Min(a: int64, b: int64) : int64 = if a < b then a else b
            static member Min(a: uint64, b: uint64) : uint64 = if a < b then a else b
            static member Min(a: float32, b: float32) : float32 = if a < b then a else b
            static member Min(a: decimal, b: decimal) : decimal = if a < b then a else b
            static member Min(a: byte, b: byte) : byte = if a < b then a else b
            static member Min(a: uint32, b: uint32) : uint32 = if a < b then a else b
            static member Min(a: int16, b: int16) : int16 = if a < b then a else b
            static member Min(a: uint16, b: uint16) : uint16 = if a < b then a else b
        
        [<AbstractClass; Sealed>]
        type MaxImpl =
            static member Max(a: int, b: int) : int = if a > b then a else b
            static member Max(a: float, b: float) : float = if a > b then a else b
            static member Max(a: int64, b: int64) : int64 = if a > b then a else b
            static member Max(a: uint64, b: uint64) : uint64 = if a > b then a else b
            static member Max(a: float32, b: float32) : float32 = if a > b then a else b
            static member Max(a: decimal, b: decimal) : decimal = if a > b then a else b
            static member Max(a: byte, b: byte) : byte = if a > b then a else b
            static member Max(a: uint32, b: uint32) : uint32 = if a > b then a else b
            static member Max(a: int16, b: int16) : int16 = if a > b then a else b
            static member Max(a: uint16, b: uint16) : uint16 = if a > b then a else b
        
        // Min/Max operations for unit-of-measure types
        
        [<AbstractClass; Sealed>]
        type MeasureMinImpl =
            static member MinInt(a: int<'u>, b: int<'u>) : int<'u> = if a < b then a else b
            static member MinFloat(a: float<'u>, b: float<'u>) : float<'u> = if a < b then a else b
            static member MinInt64(a: int64<'u>, b: int64<'u>) : int64<'u> = if a < b then a else b
            static member MinUInt64(a: uint64<'u>, b: uint64<'u>) : uint64<'u> = if a < b then a else b
            static member MinFloat32(a: float32<'u>, b: float32<'u>) : float32<'u> = if a < b then a else b
            static member MinDecimal(a: decimal<'u>, b: decimal<'u>) : decimal<'u> = if a < b then a else b
        
        [<AbstractClass; Sealed>]
        type MeasureMaxImpl =
            static member MaxInt(a: int<'u>, b: int<'u>) : int<'u> = if a > b then a else b
            static member MaxFloat(a: float<'u>, b: float<'u>) : float<'u> = if a > b then a else b
            static member MaxInt64(a: int64<'u>, b: int64<'u>) : int64<'u> = if a > b then a else b
            static member MaxUInt64(a: uint64<'u>, b: uint64<'u>) : uint64<'u> = if a > b then a else b
            static member MaxFloat32(a: float32<'u>, b: float32<'u>) : float32<'u> = if a > b then a else b
            static member MaxDecimal(a: decimal<'u>, b: decimal<'u>) : decimal<'u> = if a > b then a else b
    
    // Main type interfaces for public operators
    
    [<AbstractClass; Sealed>]
    type Add =
        static member inline Invoke(a, b) =
            ((^a or ^b or BasicOps or MeasureOps) : (static member Add: ^a * ^b -> ^r) (a, b))
    
    [<AbstractClass; Sealed>]
    type Subtract =
        static member inline Invoke(a, b) =
            ((^a or ^b or BasicOps or MeasureOps) : (static member Subtract: ^a * ^b -> ^r) (a, b))
    
    [<AbstractClass; Sealed>]
    type Multiply =
        static member inline Invoke(a, b) =
            ((^a or ^b or BasicOps or MeasureOps or MeasureMeasureOps) : 
                (static member Multiply: ^a * ^b -> ^r) (a, b))
    
    [<AbstractClass; Sealed>]
    type Divide =
        static member inline Invoke(a, b) =
            ((^a or ^b or BasicOps or MeasureOps or MeasureMeasureOps) : 
                (static member Divide: ^a * ^b -> ^r) (a, b))
    
    [<AbstractClass; Sealed>]
    type Sum =
        static member inline Invoke(collection) =
            ((^collection or ^t or ArrayOps or MeasureArrayOps) : 
                (static member Sum: ^collection -> ^t) collection)
    
    [<AbstractClass; Sealed>]
    type Average =
        static member inline Invoke(collection) =
            ((^collection or ^t or ArrayOps or MeasureArrayOps) : 
                (static member Average: ^collection -> ^t) collection)
    
    [<Measure>] type internal TestUnit
    
    let private registerMinMaxImplementations() =
        let _ = MinMaxOperations.MinImpl.Min(0, 0)
        let _ = MinMaxOperations.MaxImpl.Max(0, 0)
        let _ = MinMaxOperations.MinImpl.Min(0.0, 0.0)
        let _ = MinMaxOperations.MaxImpl.Max(0.0, 0.0)
        let _ = MinMaxOperations.MinImpl.Min(0L, 0L)
        let _ = MinMaxOperations.MaxImpl.Max(0L, 0L)
        let _ = MinMaxOperations.MinImpl.Min(0UL, 0UL)
        let _ = MinMaxOperations.MaxImpl.Max(0UL, 0UL)
        let _ = MinMaxOperations.MinImpl.Min(0.0f, 0.0f)
        let _ = MinMaxOperations.MaxImpl.Max(0.0f, 0.0f)
        let _ = MinMaxOperations.MinImpl.Min(0M, 0M)
        let _ = MinMaxOperations.MaxImpl.Max(0M, 0M)
        let _ = MinMaxOperations.MinImpl.Min(0uy, 0uy)
        let _ = MinMaxOperations.MaxImpl.Max(0uy, 0uy)
        let _ = MinMaxOperations.MinImpl.Min(0u, 0u)
        let _ = MinMaxOperations.MaxImpl.Max(0u, 0u)
        let _ = MinMaxOperations.MinImpl.Min(0s, 0s)
        let _ = MinMaxOperations.MaxImpl.Max(0s, 0s)
        let _ = MinMaxOperations.MinImpl.Min(0us, 0us)
        let _ = MinMaxOperations.MaxImpl.Max(0us, 0us)
        let _ = MinMaxOperations.MeasureMinImpl.MinInt(LanguagePrimitives.Int32WithMeasure<TestUnit>(0), LanguagePrimitives.Int32WithMeasure<TestUnit>(0))
        let _ = MinMaxOperations.MeasureMaxImpl.MaxInt(LanguagePrimitives.Int32WithMeasure<TestUnit>(0), LanguagePrimitives.Int32WithMeasure<TestUnit>(0))
        ()
    
    do registerMinMaxImplementations()

    let inline intWithUnitInternal<[<Measure>] 'u> (value: int) : int<'u> = 
        LanguagePrimitives.Int32WithMeasure<'u>(value)

    let inline floatWithUnitInternal<[<Measure>] 'u> (value: float) : float<'u> = 
        LanguagePrimitives.FloatWithMeasure<'u>(value)

    let inline int64WithUnitInternal<[<Measure>] 'u> (value: int64) : int64<'u> = 
        LanguagePrimitives.Int64WithMeasure<'u>(value)

    let inline float32WithUnitInternal<[<Measure>] 'u> (value: float32) : float32<'u> = 
        LanguagePrimitives.Float32WithMeasure<'u>(value)

    let inline decimalWithUnitInternal<[<Measure>] 'u> (value: decimal) : decimal<'u> = 
        LanguagePrimitives.DecimalWithMeasure<'u>(value)

/// <summary>
/// Adds two values of the same type.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <typeparam name="T">The type of values to add.</typeparam>
/// <returns>The result of adding a and b.</returns>
let inline add a b = Internal.Add.Invoke(a, b)

/// <summary>
/// Subtracts the second value from the first value.
/// </summary>
/// <param name="a">The value to subtract from.</param>
/// <param name="b">The value to subtract.</param>
/// <typeparam name="T">The type of values to subtract.</typeparam>
/// <returns>The result of subtracting b from a.</returns>
let inline subtract a b = Internal.Subtract.Invoke(a, b)

/// <summary>
/// Multiplies two values.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <typeparam name="T">The type of the first value.</typeparam>
/// <typeparam name="U">The type of the second value.</typeparam>
/// <typeparam name="V">The resulting type.</typeparam>
/// <returns>The result of multiplying a and b.</returns>
let inline multiply a b = Internal.Multiply.Invoke(a, b)

/// <summary>
/// Divides the first value by the second value.
/// </summary>
/// <param name="a">The dividend.</param>
/// <param name="b">The divisor.</param>
/// <typeparam name="T">The type of the dividend.</typeparam>
/// <typeparam name="U">The type of the divisor.</typeparam>
/// <typeparam name="V">The resulting type.</typeparam>
/// <returns>The result of dividing a by b.</returns>
let inline divide a b = Internal.Divide.Invoke(a, b)

/// <summary>
/// Computes the sum of all elements in a collection.
/// </summary>
/// <param name="collection">The collection to sum.</param>
/// <typeparam name="Collection">The type of the collection.</typeparam>
/// <typeparam name="T">The type of the elements in the collection.</typeparam>
/// <returns>The sum of all elements in the collection.</returns>
let inline sum collection = Internal.Sum.Invoke(collection)

/// <summary>
/// Computes the average of all elements in a collection.
/// </summary>
/// <param name="collection">The collection to average.</param>
/// <typeparam name="Collection">The type of the collection.</typeparam>
/// <typeparam name="T">The type of the elements in the collection.</typeparam>
/// <returns>The average of all elements in the collection.</returns>
let inline average collection = Internal.Average.Invoke(collection)

/// <summary>
/// Attaches a unit of measure to an integer value.
/// </summary>
/// <param name="value">The value to attach the unit to.</param>
/// <typeparam name="u">The unit of measure to attach.</typeparam>
/// <returns>The value with the unit of measure attached.</returns>
let inline intWithUnit<[<Measure>] 'u> (value: int) : int<'u> = 
    LanguagePrimitives.Int32WithMeasure<'u>(value)

/// <summary>
/// Attaches a unit of measure to a floating-point value.
/// </summary>
/// <param name="value">The value to attach the unit to.</param>
/// <typeparam name="u">The unit of measure to attach.</typeparam>
/// <returns>The value with the unit of measure attached.</returns>
let inline floatWithUnit<[<Measure>] 'u> (value: float) : float<'u> = 
    LanguagePrimitives.FloatWithMeasure<'u>(value)

/// <summary>
/// Attaches a unit of measure to a 64-bit integer value.
/// </summary>
/// <param name="value">The value to attach the unit to.</param>
/// <typeparam name="u">The unit of measure to attach.</typeparam>
/// <returns>The value with the unit of measure attached.</returns>
let inline int64WithUnit<[<Measure>] 'u> (value: int64) : int64<'u> = 
    LanguagePrimitives.Int64WithMeasure<'u>(value)

/// <summary>
/// Attaches a unit of measure to a 32-bit floating-point value.
/// </summary>
/// <param name="value">The value to attach the unit to.</param>
/// <typeparam name="u">The unit of measure to attach.</typeparam>
/// <returns>The value with the unit of measure attached.</returns>
let inline float32WithUnit<[<Measure>] 'u> (value: float32) : float32<'u> = 
    LanguagePrimitives.Float32WithMeasure<'u>(value)

/// <summary>
/// Attaches a unit of measure to a decimal value.
/// </summary>
/// <param name="value">The value to attach the unit to.</param>
/// <typeparam name="u">The unit of measure to attach.</typeparam>
/// <returns>The value with the unit of measure attached.</returns>
let inline decimalWithUnit<[<Measure>] 'u> (value: decimal) : decimal<'u> = 
    LanguagePrimitives.DecimalWithMeasure<'u>(value)

/// <summary>
/// Removes the unit of measure from an integer value.
/// </summary>
/// <param name="value">The value from which to remove the unit.</param>
/// <typeparam name="u">The unit of measure to remove.</typeparam>
/// <returns>The value without the unit of measure.</returns>
let inline intFromUnit<[<Measure>] 'u> (value: int<'u>) : int = int value

/// <summary>
/// Removes the unit of measure from a floating-point value.
/// </summary>
/// <param name="value">The value from which to remove the unit.</param>
/// <typeparam name="u">The unit of measure to remove.</typeparam>
/// <returns>The value without the unit of measure.</returns>
let inline floatFromUnit<[<Measure>] 'u> (value: float<'u>) : float = float value

/// <summary>
/// Removes the unit of measure from a 64-bit integer value.
/// </summary>
/// <param name="value">The value from which to remove the unit.</param>
/// <typeparam name="u">The unit of measure to remove.</typeparam>
/// <returns>The value without the unit of measure.</returns>
let inline int64FromUnit<[<Measure>] 'u> (value: int64<'u>) : int64 = int64 value

/// <summary>
/// Removes the unit of measure from a 32-bit floating-point value.
/// </summary>
/// <param name="value">The value from which to remove the unit.</param>
/// <typeparam name="u">The unit of measure to remove.</typeparam>
/// <returns>The value without the unit of measure.</returns>
let inline float32FromUnit<[<Measure>] 'u> (value: float32<'u>) : float32 = float32 value

/// <summary>
/// Removes the unit of measure from a decimal value.
/// </summary>
/// <param name="value">The value from which to remove the unit.</param>
/// <typeparam name="u">The unit of measure to remove.</typeparam>
/// <returns>The value without the unit of measure.</returns>
let inline decimalFromUnit<[<Measure>] 'u> (value: decimal<'u>) : decimal = decimal value