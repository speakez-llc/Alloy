#nowarn "9"

namespace Alloy.Memory

open FSharp.NativeInterop
open Alloy.Memory.Platform
open Alloy.Numerics

/// <summary>
/// Portable memory implementation using pure F#
/// </summary>
module Portable =
    /// <summary>
    /// Portable implementation of IPlatformMemory that works across platforms
    /// </summary>
    type PortableMemoryImplementation() =
        interface IPlatformMemory with
            /// <summary>
            /// Copies memory from a native pointer to a byte array
            /// </summary>
            member _.CopyFromNative(source: nativeint, destination: byte[], offset: int, length: int) =
                let srcPtr = NativePtr.ofNativeInt<byte> source
                for i = 0 to subtract length 1 do
                    destination.[add offset i] <- NativePtr.get srcPtr i
            
            /// <summary>
            /// Copies memory from a byte array to a native pointer
            /// </summary>
            member _.CopyToNative(source: byte[], offset: int, destination: nativeint, length: int) =
                let destPtr = NativePtr.ofNativeInt<byte> destination
                for i = 0 to subtract length 1 do
                    NativePtr.set destPtr i source.[add offset i]
            
            /// <summary>
            /// Fills native memory with a specific byte value
            /// </summary>
            member _.FillNative(ptr: nativeint, value: byte, length: int) =
                let destPtr = NativePtr.ofNativeInt<byte> ptr
                for i = 0 to subtract length 1 do
                    NativePtr.set destPtr i value
            
            /// <summary>
            /// Compares two native memory regions
            /// </summary>
            member _.CompareNative(ptr1: nativeint, ptr2: nativeint, length: int) =
                let p1 = NativePtr.ofNativeInt<byte> ptr1
                let p2 = NativePtr.ofNativeInt<byte> ptr2
                let mutable result = 0
                let mutable i = 0
                while equals result 0 && lessThan i length do
                    let b1 = NativePtr.get p1 i
                    let b2 = NativePtr.get p2 i
                    if lessThan b1 b2 then result <- -1
                    elif greaterThan b1 b2 then result <- 1
                    i <- add i 1
                result

    /// <summary>
    /// Factory function to create a portable memory implementation
    /// </summary>
    let createImplementation() =
        PortableMemoryImplementation() :> IPlatformMemory