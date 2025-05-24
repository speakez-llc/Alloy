#nowarn "9"

namespace Alloy.Memory

open FSharp.NativeInterop
open Alloy.Time.NativeInterop
open Alloy.Memory.Platform

/// <summary>
/// Linux platform-specific memory implementation
/// </summary>
module Linux =
    /// <summary>
    /// Linux memory functions
    /// </summary>
    module LibC =
        let memcpyImport : NativeImport<nativeint -> nativeint -> uint64 -> nativeint> = 
            {
                LibraryName = "libc"
                FunctionName = "memcpy"
                CallingConvention = CallingConvention.Cdecl
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        let memsetImport : NativeImport<nativeint -> int -> uint64 -> nativeint> = 
            {
                LibraryName = "libc"
                FunctionName = "memset"
                CallingConvention = CallingConvention.Cdecl
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        let memcmpImport : NativeImport<nativeint -> nativeint -> uint64 -> int> = 
            {
                LibraryName = "libc"
                FunctionName = "memcmp"
                CallingConvention = CallingConvention.Cdecl
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        /// <summary>
        /// Copies memory from source to destination
        /// </summary>
        let memcpy (destination: nativeint) (source: nativeint) (size: uint64) =
            invokeFunc3 memcpyImport destination source size |> ignore
            
        /// <summary>
        /// Fills memory with a specific byte value
        /// </summary>
        let memset (destination: nativeint) (value: int) (size: uint64) =
            invokeFunc3 memsetImport destination value size |> ignore
            
        /// <summary>
        /// Compares two memory regions
        /// </summary>
        let memcmp (ptr1: nativeint) (ptr2: nativeint) (size: uint64) =
            invokeFunc3 memcmpImport ptr1 ptr2 size

    /// <summary>
    /// Linux platform implementation of IPlatformMemory
    /// </summary>
    type LinuxMemoryImplementation() =
        interface IPlatformMemory with
            /// <summary>
            /// Copies memory from a native pointer to a byte array
            /// </summary>
            member _.CopyFromNative(source: nativeint, destination: byte[], offset: int, length: int) =
                use destPin = fixed &destination[offset]
                let destPtr = NativePtr.toNativeInt destPin
                LibC.memcpy destPtr source (uint64 length)
            
            /// <summary>
            /// Copies memory from a byte array to a native pointer
            /// </summary>
            member _.CopyToNative(source: byte[], offset: int, destination: nativeint, length: int) =
                use srcPin = fixed &source[offset]
                let srcPtr = NativePtr.toNativeInt srcPin
                LibC.memcpy destination srcPtr (uint64 length)
            
            /// <summary>
            /// Fills native memory with a specific byte value
            /// </summary>
            member _.FillNative(ptr: nativeint, value: byte, length: int) =
                LibC.memset ptr (int value) (uint64 length)
            
            /// <summary>
            /// Compares two native memory regions
            /// </summary>
            member _.CompareNative(ptr1: nativeint, ptr2: nativeint, length: int) =
                LibC.memcmp ptr1 ptr2 (uint64 length)

    /// <summary>
    /// Factory function to create a Linux memory implementation
    /// </summary>
    let createImplementation() =
        LinuxMemoryImplementation() :> IPlatformMemory