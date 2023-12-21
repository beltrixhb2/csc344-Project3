# csc344-Project2

Write an OCaml program that performs pattern matching on strings, where patterns are expressed using only the concatenation, alternation (“|”), and optional (“?”) operators of regular expressions (no loops/”*”, no escape characters), and the tokens are letters and digits, plus period (“.”) to mean any letter. Each run of the program should accept a pattern, and then any number of strings, reporting only whether they match. Your program should represent expressions as trees and evaluate on the inputs, without using any regular expressions or OCaml’s regular expression libraries except for matching the individual alphanumeric characters, if you’d like. For example:
    
    pattern? ((h|j)ell. worl?d)|(42)
    string? hello world
    match
    string? jello word
    match
    string? jelly word
    match
    string? 42
    match
    string? 24
    no match
    string? hello world42
    no match


    pattern? I (like|love|hate)( (cat|dog))? people
    string? I like cat people
    match
    string? I love dog people
    match
    string? I hate people
    match
    string? I likelovehate people
    no match
    string? I people
    no match
