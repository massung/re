# The RE Package

The `re` package is a small, portable, lightweight, and quick, regular expression library for Common Lisp. It is a non-recursive, backtracing VM. The syntax is similar to [Lua](http://www.lua.org)-style pattern patching (found [here](http://www.lua.org/pil/20.2.html)), but has added support for additional regex features (see below). It's certainly not the fastest, but is very easy to understand and extend.

It makes heavy use of the monadic [`parse`](http://github.com/massung/parse) combinator library for parsing the regular expressions. If you'd like to understand the parsing and compiling of regular expressions, I recommend reading up on that library as well.

## Compiling Patterns

To create a `re` object, you can either use the `compile-re` function or the `#r` dispatch macro.

    CL-USER > (compile-re "%d+")
    #<RE "%d+">

    CL-USER > #r/%d+/
    #<RE "%d+">

Both work equally well, but the dispatch macro will compile the pattern at read-time. The `re` class has a [load form](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ld_.htm#make-load-form) and so can be saved to a FASL file.

*HINT: when using the read macro, use a backslash to escape the `/` and other characters that might mess with syntax coloring.*

Finally, the `with-re` macro let's you user either strings or `re` objects in a body of code. If a string is passed as the pattern, then it will be compiled before the body is evaluated.

    CL-USER > (with-re (re "%d+") re)
    #<RE "%d+">

*NOTE: All pattern matching functions use the `with-re` macro, and so the pattern argument can be either a string or a pre-compiled `re` object.*

## Basic Pattern Matching

The heart of all pattern matching is the `match-re` function.

    (match-re pattern string &key start end exact)

It will match `string` against `pattern` and return a `re-match` object on success or `nil` on failure. The `start` and `end` arguments limit the scope of the match and default to the entire string. If `exact` is `t` then the pattern has to consume the entire string (from start to end).

    CL-USER > (match-re "%d+" "abc 123")
    NIL

    CL-USER > (match-re "%a+" "abc 123")
    #<RE-MATCH "abc">

Once you have successfully matched and have a `re-match` object, you can use the following reader functions to inspect it:

* `match-string` returns the entire match
* `match-groups` returns a list of groups
* `match-pos-start` returns the index where the match began
* `match-pos-end` returns the index where the match ended

Try peeking into a match...

    CL-USER > (inspect (match-re "(a(b(c)))" "abc 123"))
    MATCH          "abc"
    GROUPS         ("abc" "bc" "c")
    START-POS      0
    END-POS        3

## Pattern Scanning

To find a pattern match anywhere in a string use the `find-re` function.

    (find-re pattern string &key start end all)

It will scan `string` looking for matches to `pattern`. If `all` is non-`nil` then a list of all matches found is returned, otherwise it will simply be the first match.

    CL-USER > (find-re "%d+" "abc 123")
    #<RE-MATCH "123">

    CL-USER > (find-re "[^%s]+" "abc 123" :all t)
    (#<RE-MATCH "abc">
     #<RE-MATCH "123">)

## Splitting by Pattern

Once patterns have been matched, splitting a string from the matches is trivial.

    (split-re pattern string &key start end all coalesce-seps)

If `all` is true, then a list of all sub-sequences in `string` (delimited by `pattern`) are returned, otherwise just the first and the rest of the string.

If `coalesce-seps` is true the sub-sequences that are empty will be excluded from the results. This argument is ignored if `all` is `nil`.

    CL-USER > (split-re "," "1,2,3")
    "1"
    "2,3"

    CL-USER > (split-re "," "1,2,,,abc,3,," :all t :coalesce-seps t)
    ("1" "2" "abc" "3")

## Replacing by Pattern

The `replace-re` function scans the string looking for matching sub-sequences that will be replaced with another string.

    (replace-re pattern with string &key start end all)

If `with` is a function, then the function is called with the `re-match` object, replacing the pattern with the return value. Otherwise the value is used as-is. As with `find-re` and `split-re`, if `all` is true, then the pattern is globally replaced.

    CL-USER > (replace-re "%d+" #\* "1 2 3")
    "* 2 3"

    CL-USER > (replace-re "%a+" #'(lambda (m) (length (match-string m))) "a bc def" :all t)
    "1 2 3"

*NOTE: The string returned by `replace-re` is a completely new string. This is true even if `pattern` isn't found in the string.*

## Groups

Using parenthesis in a pattern will cause the matching text to be groups in the returned `re-match` object. The `match-groups` function will return a list of all the captured strings in the match.

    CL-USER > (match-groups (match-re #r/(%d+)(%a+)/ "123abc"))
    ("123" "abc")

Captures can be nested, but are always returned in the order they are **opened**.

    CL-USER > (match-groups (match-re #r/(a(b(c)))(d)/ "abcd"))
    ("abc" "bc" "c" "d")

*HINT: you can always use the `match-string` function to get at the full text that was matched and there's no need to capture the entire pattern.*

## The `with-re-match` Macro

Whe `with-re-match` macro can be used to assist in extracting the matched patterns and groups.

    (with-re-match ((var match-expr &key no-match) &body body)

If the result of `match-expr` is `nil`, then `no-match` is returned and `body` is not executed.

While in the body of the macro, `$$` will be bound to the `match-string` and the groups will be bound to `$1`, `$2`, ..., `$9`. Any groups beyond the first 9 are bound in a list to `$_`. The symbol `$*` is bound to all the match groups.

    CL-USER > (with-re-match (m (match-re "(%a+)(%s+)(%d+)" "abc 123"))
                (string-append $3 $2 $1)))
    "123 abc"

    CL-USER > (flet ((initial (m)
                       (with-re-match (v m)
                         (format nil "~@(~a~)." $1))))
                (replace-re #r/(%a)%a+%s*/ #'initial "lisp in small pieces" :all t))
    "L.I.S.P."

## Additional Features

In addition to supporting all of what Lua pattern matching has to offer, it also supports branching with `|` and uncaptured groups: `(?..)`. For example...

    CL-USER > (match-re "(?a|b)+" "abbaaabbccc")
    #<RE-MATCH "abbaaabb">

Also, to have slightly better support for Windows line endings, there is also the `%r` character set that matches on return characters only (as opposed to `%n` which matches on return and linefeed). This allows for patterns that match a single Windows or Linux/Mac line ending.

    CL-USER > (match-re "%r?%n" (concatenate 'string '(#\return #\linefeed)))
    #<RE-MATCH "^M
    ">

Finally, the `re` package has one special feature: user-defined character set predicates! Using `%:`, you can provide a predicate function for the regexp VM to test characters against.

    CL-USER > (match-re #r"%:digit-char-p:+" "103")
    #<RE-MATCH "103">

The predicate must take a single character and return non-nil if the character matches the predicate function. *Note: this is especially handy when parsing unicode strings!*

# Thank You!

If you get some good use out of this package, please let me know; it's nice to know your work is valued by others.

I'm always improving it; it's the foundation for many of the other packages I've created for JSON parsing, XML parsing, HTTP header parsing, etc.

Should you find/fix a bug or add a nice feature, please feel free to send a pull request or let me know at [massung@gmail.com](mailto:massung@gmail.com).
