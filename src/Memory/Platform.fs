namespace Alloy.Memory

/// <summary>
/// Platform interface definitions for Alloy memory functions
/// </summary>
module Platform =
    /// <summary>
    /// Interface for platform-specific memory operations
    /// </summary>
    type IPlatformMemory =
        /// <summary>
        /// Copies memory from a native pointer to a byte array
        /// </summary>
        abstract member CopyFromNative: source:nativeint -> destination:byte[] -> offset:int -> length:int -> unit
        
        /// <summary>
        /// Copies memory from a byte array to a native pointer
        /// </summary>
        abstract member CopyToNative: source:byte[] -> offset:int -> destination:nativeint -> length:int -> unit
        
        /// <summary>
        /// Fills native memory with a specific byte value
        /// </summary>
        abstract member FillNative: ptr:nativeint -> value:byte -> length:int -> unit
        
        /// <summary>
        /// Compares two native memory regions
        /// </summary>
        abstract member CompareNative: ptr1:nativeint -> ptr2:nativeint -> length:int -> int

    /// <summary>
    /// Exception thrown when platform implementation cannot be determined
    /// </summary>
    exception PlatformNotSupportedException of string

    /// <summary>
    /// Registry for platform implementations
    /// </summary>
    module private PlatformRegistry =
        let mutable private implementation : IPlatformMemory option = None
        
        /// <summary>
        /// Registers a platform implementation
        /// </summary>
        let register (impl: IPlatformMemory) =
            implementation <- Some impl
            
        /// <summary>
        /// Gets the registered implementation
        /// </summary>
        let get() =
            match implementation with
            | Some impl -> impl
            | None -> None
    
    /// <summary>
    /// Function to get the appropriate platform implementation
    /// </summary>
    let getImplementation() = 
        match PlatformRegistry.get() with
        | Some impl -> impl
        | None -> 
            // Auto-detect and register platform implementation
            let impl = 
                #if WINDOWS
                Windows.createImplementation()
                #elif LINUX
                Linux.createImplementation()
                #elif MACOS
                MacOS.createImplementation()
                #else
                Portable.createImplementation()
                #endif
            PlatformRegistry.register impl
            impl
    
    /// <summary>
    /// Registers a platform implementation
    /// </summary>
    let registerImplementation (impl: IPlatformMemory) = PlatformRegistry.register impl