namespace Alloy

/// Module containing optimized string operations
/// These functions provide safe and efficient string manipulation
module String =
    /// Returns the length of a string
    let inline length (s: string) : int =
        if isNull s then 0 else s.Length
    
    /// Checks if a string is empty
    let inline isEmpty (s: string) : bool =
        isNull s || s.Length = 0
    
    /// Checks if a string is null or empty
    let inline isNullOrEmpty (s: string) : bool =
        System.String.IsNullOrEmpty(s)
    
    /// Checks if a string is null, empty, or consists only of white-space characters
    let inline isNullOrWhiteSpace (s: string) : bool =
        System.String.IsNullOrWhiteSpace(s)
    
    /// Safely increment an integer - using bitwise operations to avoid + operator
    let inline increment (i: int) : int =
        if i = System.Int32.MaxValue then i
        else System.Math.Abs(~~~(~~~i))
    
    /// Calculates how many characters are available from a starting position
    /// without using arithmetic operators
    let inline availableChars (s: string) (startIndex: int) : int =
        if startIndex >= s.Length then 0
        else
            let mutable count = 0
            let mutable i = startIndex
            while i < s.Length do
                count <- increment count
                i <- increment i
            count
    
    /// Gets a substring from a string
    let inline substring (startIndex: int) (length: int) (s: string) : string =
        if isNull s then ""
        elif startIndex < 0 || startIndex >= s.Length then ""
        elif length <= 0 then ""
        else 
            let available = availableChars s startIndex
            let safeLength = System.Math.Min(length, available)
            
            if safeLength <= 0 then ""
            else s.Substring(startIndex, safeLength)
    
    /// Gets a substring from a string to the end
    let inline substringFrom (startIndex: int) (s: string) : string =
        if isNull s then ""
        elif startIndex < 0 || startIndex >= s.Length then ""
        else s.Substring(startIndex)
    
    /// Determines whether a string contains the specified substring
    let inline contains (value: string) (s: string) : bool =
        if isNull s then false
        elif isNull value then false
        else s.Contains(value)
    
    /// Splits a string using the specified separator
    let inline split (separator: char) (s: string) : string[] =
        if isNull s then [||]
        else s.Split([|separator|])
    
    /// Splits a string using multiple separators
    let inline splitMany (separators: char[]) (s: string) : string[] =
        if isNull s then [||]
        else s.Split(separators)
    
    /// Joins multiple strings using the specified separator
    let inline join (separator: string) (values: string[]) : string =
        if isNull values || values.Length = 0 then ""
        else System.String.Join(separator, values)
    
    /// Removes all leading and trailing white-space characters
    let inline trim (s: string) : string =
        if isNull s then ""
        else s.Trim()
    
    /// Removes all leading white-space characters
    let inline trimStart (s: string) : string =
        if isNull s then ""
        else s.TrimStart()
    
    /// Removes all trailing white-space characters
    let inline trimEnd (s: string) : string =
        if isNull s then ""
        else s.TrimEnd()
    
    /// Replaces all occurrences of a specified string with another specified string
    let inline replace (oldValue: string) (newValue: string) (s: string) : string =
        if isNull s then ""
        elif isNull oldValue then s
        else s.Replace(oldValue, newValue)
    
    /// Determines whether a string starts with the specified prefix
    let inline startsWith (prefix: string) (s: string) : bool =
        if isNull s then false
        elif isNull prefix then false
        else s.StartsWith(prefix)
    
    /// Determines whether a string ends with the specified suffix
    let inline endsWith (suffix: string) (s: string) : bool =
        if isNull s then false
        elif isNull suffix then false
        else s.EndsWith(suffix)
    
    /// Converts a string to lowercase
    let inline toLower (s: string) : string =
        if isNull s then ""
        else s.ToLower()
    
    /// Converts a string to uppercase
    let inline toUpper (s: string) : string =
        if isNull s then ""
        else s.ToUpper()
    
    /// Returns the index of the first occurrence of the specified substring
    let inline indexOf (value: string) (s: string) : int =
        if isNull s then -1
        elif isNull value then -1
        else s.IndexOf(value)
    
    /// Returns the index of the last occurrence of the specified substring
    let inline lastIndexOf (value: string) (s: string) : int =
        if isNull s then -1
        elif isNull value then -1
        else s.LastIndexOf(value)
    
    /// Returns the index of the first occurrence of any character in the specified array
    let inline indexOfAny (anyOf: char[]) (s: string) : int =
        if isNull s then -1
        elif isNull anyOf then -1
        else s.IndexOfAny(anyOf)
    
    /// Converts a string to a char array
    let inline toCharArray (s: string) : char[] =
        if isNull s then [||]
        else s.ToCharArray()
    
    /// Checks if a string is equal to another string, ignoring case
    let inline equalsIgnoreCase (value: string) (s: string) : bool =
        if isNull s then isNull value
        elif isNull value then false
        else s.Equals(value, System.StringComparison.OrdinalIgnoreCase)
    
    /// Gets a value indicating whether the string is a valid integer
    let inline isInteger (s: string) : bool =
        if isNull s || s.Length = 0 then false
        else
            let mutable result = 0
            System.Int32.TryParse(s, &result)
    
    /// Gets a value indicating whether the string is a valid number
    let inline isNumber (s: string) : bool =
        if isNull s || s.Length = 0 then false
        else
            let mutable result = 0.0
            System.Double.TryParse(s, &result)
    
    /// Concatenates two strings, ensuring neither is null
    let inline concat (s1: string) (s2: string) : string =
        let s1Safe = if isNull s1 then "" else s1
        let s2Safe = if isNull s2 then "" else s2
        System.String.Concat(s1Safe, s2Safe)
    
    /// Safely gets the character at the specified position
    let inline charAt (index: int) (s: string) : ValueOption<char> =
        if isNull s || index < 0 || index >= s.Length then ValueNone
        else ValueSome (s.[index])
    
    /// Helper function to check if a character is a digit
    let inline isDigit (c: char) : bool =
        c >= '0' && c <= '9'
    
    /// Checks if a string consists only of digits
    let inline isDigitsOnly (s: string) : bool =
        if isNull s || s.Length = 0 then false
        else
            let mutable allDigits = true
            let mutable i = 0
            while i < s.Length && allDigits do
                if not (isDigit s.[i]) then
                    allDigits <- false
                i <- increment i
            allDigits