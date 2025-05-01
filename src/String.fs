namespace Alloy

/// String operations implemented without any external dependencies
module String =
   /// Length of a string
   let length (s: string) = s.Length
   
   /// Check if string is empty
   let isEmpty (s: string) = s.Length = 0
   
   /// Check if string is null or empty
   let isNullOrEmpty (s: string) = 
       match s with
       | null -> true
       | s -> s.Length = 0
   
   /// Check if string contains only whitespace
   let isWhiteSpace (s: string) =
       let mutable i = 0
       let mutable result = true
       
       while i < s.Length && result do
           let c = s.[i]
           if c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r' then
               result <- false
           i <- i + 1
       
       result
   
   /// Check if string is null or whitespace
   let isNullOrWhiteSpace (s: string) =
       match s with
       | null -> true
       | s -> isWhiteSpace s
   
   /// Get substring
   let substring (startIndex: int) (length: int) (s: string) =
       s.Substring(startIndex, length)
   
   /// Get substring from start to end
   let substringFrom (startIndex: int) (s: string) =
       s.Substring(startIndex)
   
   /// Split string by delimiter
   let split (separator: char) (s: string) =
       let result = ResizeArray<string>()
       let mutable startIndex = 0
       
       for i = 0 to s.Length - 1 do
           if s.[i] = separator then
               let segment = s.Substring(startIndex, i - startIndex)
               result.Add(segment)
               startIndex <- i + 1
               
       // Add final segment
       if startIndex <= s.Length then
           result.Add(s.Substring(startIndex))
           
       result.ToArray()
   
   /// Split string by multiple delimiters
   let splitMany (separators: char[]) (s: string) =
       let result = ResizeArray<string>()
       let mutable startIndex = 0
       
       for i = 0 to s.Length - 1 do
           let mutable isSeparator = false
           for sep in separators do
               if s.[i] = sep then
                   isSeparator <- true
           
           if isSeparator then
               let segment = s.Substring(startIndex, i - startIndex)
               result.Add(segment)
               startIndex <- i + 1
               
       // Add final segment
       if startIndex <= s.Length then
           result.Add(s.Substring(startIndex))
           
       result.ToArray()
   
   /// Join strings with separator
   let join (separator: string) (strings: string[]) =
       if strings.Length = 0 then ""
       else
           let sb = strings |> Array.fold 
                       (fun (sb: ResizeArray<char>) s -> 
                           if sb.Count > 0 then
                               // Add separator
                               for c in separator do
                                   sb.Add(c)
                           // Add string
                           for c in s do
                               sb.Add(c)
                           sb) 
                       (ResizeArray<char>())
           new string(sb.ToArray())
   
   /// Trim whitespace from start and end
   let trim (s: string) =
       let mutable startIndex = 0
       while startIndex < s.Length && 
             (s.[startIndex] = ' ' || 
              s.[startIndex] = '\t' || 
              s.[startIndex] = '\n' || 
              s.[startIndex] = '\r') do
           startIndex <- startIndex + 1
       
       let mutable endIndex = s.Length - 1
       while endIndex >= 0 && 
             (s.[endIndex] = ' ' || 
              s.[endIndex] = '\t' || 
              s.[endIndex] = '\n' || 
              s.[endIndex] = '\r') do
           endIndex <- endIndex - 1
       
       if startIndex > endIndex then ""
       else s.Substring(startIndex, endIndex - startIndex + 1)
   
   /// Trim whitespace from start
   let trimStart (s: string) =
       let mutable startIndex = 0
       while startIndex < s.Length && 
             (s.[startIndex] = ' ' || 
              s.[startIndex] = '\t' || 
              s.[startIndex] = '\n' || 
              s.[startIndex] = '\r') do
           startIndex <- startIndex + 1
       
       if startIndex = 0 then s
       elif startIndex >= s.Length then ""
       else s.Substring(startIndex)
   
   /// Trim whitespace from end
   let trimEnd (s: string) =
       let mutable endIndex = s.Length - 1
       while endIndex >= 0 && 
             (s.[endIndex] = ' ' || 
              s.[endIndex] = '\t' || 
              s.[endIndex] = '\n' || 
              s.[endIndex] = '\r') do
           endIndex <- endIndex - 1
       
       if endIndex < 0 then ""
       elif endIndex = s.Length - 1 then s
       else s.Substring(0, endIndex + 1)
   
   /// Replace substring
   let replace (oldValue: string) (newValue: string) (s: string) =
       let mutable result = ResizeArray<char>()
       let mutable i = 0
       
       while i < s.Length do
           if i <= s.Length - oldValue.Length && 
              s.Substring(i, oldValue.Length) = oldValue then
               // Add replacement
               for c in newValue do
                   result.Add(c)
               i <- i + oldValue.Length
           else
               // Add current character
               result.Add(s.[i])
               i <- i + 1
               
       new string(result.ToArray())
   
   /// Check if string starts with prefix
   let startsWith (prefix: string) (s: string) =
       if s.Length < prefix.Length then false
       else
           let mutable result = true
           let mutable i = 0
           
           while i < prefix.Length && result do
               if s.[i] <> prefix.[i] then
                   result <- false
               i <- i + 1
               
           result
   
   /// Check if string ends with suffix
   let endsWith (suffix: string) (s: string) =
       if s.Length < suffix.Length then false
       else
           let mutable result = true
           let mutable i = 0
           
           while i < suffix.Length && result do
               if s.[s.Length - suffix.Length + i] <> suffix.[i] then
                   result <- false
               i <- i + 1
               
           result
   
   /// Convert string to lowercase
   let toLower (s: string) =
       let chars = Array.zeroCreate s.Length
       for i = 0 to s.Length - 1 do
           let c = s.[i]
           if c >= 'A' && c <= 'Z' then
               chars.[i] <- char (int c + 32)
           else
               chars.[i] <- c
       new string(chars)
   
   /// Convert string to uppercase
   let toUpper (s: string) =
       let chars = Array.zeroCreate s.Length
       for i = 0 to s.Length - 1 do
           let c = s.[i]
           if c >= 'a' && c <= 'z' then
               chars.[i] <- char (int c - 32)
           else
               chars.[i] <- c
       new string(chars)
   
   /// Find index of substring
   let indexOf (substring: string) (s: string) =
       let mutable index = -1
       let mutable i = 0
       
       while i <= s.Length - substring.Length && index = -1 do
           let mutable matches = true
           let mutable j = 0
           
           while j < substring.Length && matches do
               if s.[i + j] <> substring.[j] then
                   matches <- false
               j <- j + 1
               
           if matches then
               index <- i
               
           i <- i + 1
           
       index
   
   /// Find last index of substring
   let lastIndexOf (substring: string) (s: string) =
       let mutable index = -1
       let mutable i = s.Length - substring.Length
       
       while i >= 0 && index = -1 do
           let mutable matches = true
           let mutable j = 0
           
           while j < substring.Length && matches do
               if s.[i + j] <> substring.[j] then
                   matches <- false
               j <- j + 1
               
           if matches then
               index <- i
               
           i <- i - 1
           
       index
   
   /// Check if string contains substring
   let contains (substring: string) (s: string) =
       indexOf substring s <> -1