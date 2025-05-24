namespace Alloy

open FSharp.NativeInterop
open Alloy.Memory.Platform

/// <summary>
/// Advanced memory types and operations
/// </summary>
[<AutoOpen>]
module Memory =
    /// <summary>
    /// Gets the platform memory implementation
    /// </summary>
    let private platformMemory = getImplementation()
    /// <summary>
    /// A region of memory with type safety
    /// </summary>
    /// <typeparam name="'T">The type associated with this memory</typeparam>
    /// <typeparam name="'region">The memory region measure type</typeparam>
    [<Struct>]
    type Memory<'T, [<Measure>] 'region> =
        { /// <summary>The underlying data buffer</summary>
          Data: byte[]
          /// <summary>The starting offset within the buffer</summary>
          Offset: int<offset>
          /// <summary>The length in bytes</summary>
          Length: int<bytes> }
    
    /// <summary>
    /// A memory buffer that can be written to
    /// </summary>
    /// <typeparam name="'T">The type associated with this buffer</typeparam>
    [<Struct>]
    type Buffer<'T> =
        { /// <summary>The underlying data buffer</summary>
          Data: byte[]
          /// <summary>The current write position</summary>
          mutable Position: int<offset> }
        
        /// <summary>
        /// Writes a single byte to the buffer and advances the position
        /// </summary>
        /// <param name="value">The byte to write</param>
        member this.Write(value: byte): unit =
            this.Data.[int this.Position] <- value
            this.Position <- add this.Position (intWithUnit<offset> 1)
            
        /// <summary>
        /// Writes multiple bytes to the buffer and advances the position
        /// </summary>
        /// <param name="values">The bytes to write</param>
        member this.WriteBytes(values: byte[]): unit =
            for i = 0 to subtract values.Length 1 do
                this.Data.[add (int this.Position) i] <- values.[i]
            this.Position <- add this.Position (intWithUnit<offset> values.Length)
            
        /// <summary>
        /// Creates a new buffer with the specified capacity
        /// </summary>
        /// <param name="capacity">The buffer capacity in bytes</param>
        /// <returns>A new buffer instance</returns>
        static member Create(capacity: int): Buffer<'T> =
            { Data = Array.zeroCreate capacity
              Position = intWithUnit<offset> 0 }
    
    /// <summary>
    /// Creates a memory region from a byte array
    /// </summary>
    /// <param name="data">The source byte array</param>
    /// <returns>A new memory region containing the entire array</returns>
    let fromArray<'T, [<Measure>] 'region> (data: byte[]) : Memory<'T, 'region> =
        { Data = data
          Offset = intWithUnit<offset> 0
          Length = intWithUnit<bytes> data.Length }
          
    /// <summary>
    /// Creates a memory region from a slice of an existing byte array
    /// </summary>
    /// <param name="data">The source byte array</param>
    /// <param name="offset">The starting offset in the array</param>
    /// <param name="length">The number of bytes to include</param>
    /// <returns>A new memory region representing the specified slice</returns>
    let fromArraySlice<'T, [<Measure>] 'region> (data: byte[]) (offset: int) (length: int) : Memory<'T, 'region> =
        { Data = data
          Offset = intWithUnit<offset> offset
          Length = intWithUnit<bytes> length }
    
    /// <summary>
    /// Copies data from one memory region to another
    /// </summary>
    /// <param name="source">The source memory region</param>
    /// <param name="destination">The destination memory region</param>
    /// <param name="count">The number of bytes to copy</param>
    /// <exception cref="System.Exception">Thrown when copy exceeds memory region bounds</exception>
    let copy<'T, 'U, [<Measure>] 'region1, [<Measure>] 'region2> 
            (source: Memory<'T, 'region1>) 
            (destination: Memory<'U, 'region2>) 
            (count: int<bytes>) : unit =
        
        if greaterThan count source.Length || greaterThan count destination.Length then
            failwith "Copy exceeds memory region bounds"
            
        let srcOffset = int source.Offset
        let dstOffset = int destination.Offset
        let copyCount = int count
        
        // Manual copy implementation without System dependencies
        for i = 0 to subtract copyCount 1 do
            destination.Data.[add dstOffset i] <- source.Data.[add srcOffset i]
    
    /// <summary>
    /// Creates a slice of a memory region
    /// </summary>
    /// <param name="memory">The source memory region</param>
    /// <param name="offset">The starting offset within the region</param>
    /// <param name="length">The number of bytes to include in the slice</param>
    /// <returns>A new memory region representing the specified slice</returns>
    /// <exception cref="System.Exception">Thrown when slice exceeds memory region bounds</exception>
    let slice<'T, 'U, [<Measure>] 'region> 
             (memory: Memory<'T, 'region>) 
             (offset: int<offset>) 
             (length: int<bytes>) : Memory<'U, 'region> =
             
        // Convert to raw ints for normal comparisons and calculations
        let rawOffset = int offset
        let rawLength = int length
        let memoryOffset = int memory.Offset
        let memoryLength = int memory.Length
         
        if lessThan rawOffset 0 || lessThan rawLength 0 || 
           greaterThan (add rawOffset rawLength) memoryLength then
            failwith "Slice exceeds memory region bounds"
            
        { Data = memory.Data
          Offset = add memory.Offset offset
          Length = length }
    
    /// <summary>
    /// Reads a byte from memory at the specified offset
    /// </summary>
    /// <param name="memory">The memory region to read from</param>
    /// <param name="offset">The offset within the region</param>
    /// <returns>The byte value at the specified offset</returns>
    /// <exception cref="System.Exception">Thrown when offset is out of bounds</exception>
    let readByte<'T, [<Measure>] 'region> 
                (memory: Memory<'T, 'region>) 
                (offset: int<offset>) : byte =
        
        let rawOffset = int offset
        let memoryOffset = int memory.Offset
        let memoryLength = int memory.Length
                
        if lessThan rawOffset 0 || greaterThanOrEqual rawOffset memoryLength then
            failwith "Offset out of bounds"
            
        memory.Data.[add memoryOffset rawOffset]
    
    /// <summary>
    /// Writes a byte to memory at the specified offset
    /// </summary>
    /// <param name="memory">The memory region to write to</param>
    /// <param name="offset">The offset within the region</param>
    /// <param name="value">The byte value to write</param>
    /// <exception cref="System.Exception">Thrown when offset is out of bounds</exception>
    let writeByte<'T, [<Measure>] 'region> 
                 (memory: Memory<'T, 'region>) 
                 (offset: int<offset>) 
                 (value: byte) : unit =
        
        let rawOffset = int offset
        let memoryOffset = int memory.Offset
        let memoryLength = int memory.Length
                 
        if lessThan rawOffset 0 || greaterThanOrEqual rawOffset memoryLength then
            failwith "Offset out of bounds"
            
        memory.Data.[add memoryOffset rawOffset] <- value
    
    /// <summary>
    /// Gets the absolute address of a location within a region
    /// </summary>
    /// <param name="memory">The memory region</param>
    /// <param name="relativeOffset">The relative offset within the region</param>
    /// <returns>An address representing the absolute location</returns>
    /// <exception cref="System.Exception">Thrown when address is outside region bounds</exception>
    let getAddress<'T, [<Measure>] 'region> 
                  (memory: Memory<'T, 'region>) 
                  (relativeOffset: int<offset>) : Address<'region> =
        
        let rawOffset = int relativeOffset
        let memoryLength = int memory.Length
                  
        if greaterThanOrEqual rawOffset memoryLength then
            failwith "Address outside region bounds"
            
        { Offset = add memory.Offset relativeOffset }
        
    /// <summary>
    /// Fills a memory region with a specific byte value
    /// </summary>
    /// <param name="memory">The memory region to fill</param>
    /// <param name="value">The byte value to fill with</param>
    let fill<'T, [<Measure>] 'region>
                (memory: Memory<'T, 'region>)
                (value: byte) : unit =
        
        let memoryOffset = int memory.Offset
        let memoryLength = int memory.Length
        
        for i = 0 to subtract memoryLength 1 do
            memory.Data.[add memoryOffset i] <- value
        
    /// <summary>
    /// Clears a memory region (fills with zeros)
    /// </summary>
    /// <param name="memory">The memory region to clear</param>
    let clear<'T, [<Measure>] 'region>
                 (memory: Memory<'T, 'region>) : unit =
        fill memory 0uy
        
    /// <summary>
    /// Compares two memory regions for equality
    /// </summary>
    /// <param name="memory1">The first memory region</param>
    /// <param name="memory2">The second memory region</param>
    /// <returns>True if the regions contain the same bytes, false otherwise</returns>
    let equals<'T, 'U, [<Measure>] 'region1, [<Measure>] 'region2>
                  (memory1: Memory<'T, 'region1>)
                  (memory2: Memory<'U, 'region2>) : bool =
        
        let memory1Offset = int memory1.Offset
        let memory1Length = int memory1.Length
        let memory2Offset = int memory2.Offset
        let memory2Length = int memory2.Length
        
        if memory1Length <> memory2Length then false
        else
            let mutable result = true
            let mutable i = 0
            
            while result && lessThan i memory1Length do
                if memory1.Data.[add memory1Offset i] <> memory2.Data.[add memory2Offset i] then
                    result <- false
                i <- add i 1
                
            result
            
    /// <summary>
    /// Creates a memory region from a native pointer
    /// </summary>
    /// <param name="ptr">The native pointer</param>
    /// <param name="size">The size in bytes</param>
    /// <returns>A view over the native memory</returns>
    let fromNativePointer<'T, [<Measure>] 'region> (ptr: nativeint) (size: int) : Memory<'T, 'region> =
        let data = Array.zeroCreate size
        platformMemory.CopyFromNative(ptr, data, 0, size)
        { Data = data; Offset = intWithUnit<offset> 0; Length = intWithUnit<bytes> size }