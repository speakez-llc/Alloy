#nowarn "9"

namespace Alloy.Memory

open FSharp.NativeInterop
open Alloy.Time.NativeInterop
open Alloy.Memory.Platform

/// <summary>
/// Windows platform-specific memory implementation
/// </summary>
module Windows =
    /// <summary>
    /// Windows memory functions
    /// </summary>
    module Kernel32 =
        let rtlMoveMemoryImport : NativeImport<nativeint -> nativeint -> uint32 -> unit> = 
            {
                LibraryName = "kernel32"
                FunctionName = "RtlMoveMemory"
                CallingConvention = CallingConvention.StdCall
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        let rtlFillMemoryImport : NativeImport<nativeint -> uint32 -> byte -> unit> = 
            {
                LibraryName = "kernel32"
                FunctionName = "RtlFillMemory"
                CallingConvention = CallingConvention.StdCall
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        let rtlCompareMemoryImport : NativeImport<nativeint -> nativeint -> uint32 -> uint32> = 
            {
                LibraryName = "kernel32"
                FunctionName = "RtlCompareMemory"
                CallingConvention = CallingConvention.StdCall
                CharSet = CharSet.Ansi
                SupressErrorHandling = false
            }
            
        /// <summary>
        /// Copies memory from source to destination
        /// </summary>
        let copyMemory (destination: nativeint) (source: nativeint) (length: uint32) =
            invokeFunc3 rtlMoveMemoryImport destination source length
            
        /// <summary>
        /// Fills memory with a specific byte value
        /// </summary>
        let fillMemory (destination: nativeint) (length: uint32) (fill: byte) =
            invokeFunc3 rtlFillMemoryImport destination length fill
            
        /// <summary>
        /// Compares two memory regions
        /// </summary>
        let compareMemory (source1: nativeint) (source2: nativeint) (length: uint32) =
            let result = invokeFunc3 rtlCompareMemoryImport source1 source2 length
            if result = length then 0
            else -1

    /// <summary>
    /// Windows platform implementation of IPlatformMemory
    /// </summary>
    type WindowsMemoryImplementation() =
        interface IPlatformMemory with
            /// <summary>
            /// Copies memory from a native pointer to a byte array
            /// </summary>
            member _.CopyFromNative(source: nativeint, destination: byte[], offset: int, length: int) =
                use destPin = fixed &destination.[offset]
                let destPtr = NativePtr.toNativeInt destPin
                Kernel32.copyMemory destPtr source (uint32 length)
            
            /// <summary>
            /// Copies memory from a byte array to a native pointer
            /// </summary>
            member _.CopyToNative(source: byte[], offset: int, destination: nativeint, length: int) =
                use srcPin = fixed &source.[offset]
                let srcPtr = NativePtr.toNativeInt srcPin
                Kernel32.copyMemory destination srcPtr (uint32 length)
            
            /// <summary>
            /// Fills native memory with a specific byte value
            /// </summary>
            member _.FillNative(ptr: nativeint, value: byte, length: int) =
                Kernel32.fillMemory ptr (uint32 length) value
            
            /// <summary>
            /// Compares two native memory regions
            /// </summary>
            member _.CompareNative(ptr1: nativeint, ptr2: nativeint, length: int) =
                Kernel32.compareMemory ptr1 ptr2 (uint32 length)

    /// <summary>
    /// Factory function to create a Windows memory implementation
    /// </summary>
    let createImplementation() =
        WindowsMemoryImplementation() :> IPlatformMemory