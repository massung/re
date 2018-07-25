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
* `match-captures` returns a vector of extended captured groups, each an instance of the `re-capture` class, comprising substring, optional name, start index, and end index
* `match-pos-start` returns the index where the match began
* `match-pos-end` returns the index where the match ended

Further functions exists specialized on the captured groups retrieval:

* `match-capture-at-index` returns the group at the given index
* `match-captures-by-name` returns a vector of named captured groups with the given name
* `match-capture-by-name` returns either the first or the last named captured group with the given name
* `match-capture-by-name-at-index` returns the `index`-th named captured group
* `match-has-capture-of-name` return `T` if at least one named captured group with the given name exists, otherwise `NIL`
* `match-extract-data` procures a generalized access to the captured groups and their data

The `re-capture` class offers following reader functions:
* `re-capture-substring` returns the substring of the input string enclosed in the group
* `re-capture-start-position` returns the start position of the captured group in the input string
* `re-capture-end-position` returns the end position of the captured group in the input string
* `re-capture-name` returns the name of the named captured group, or `NIL` if the group is unnamed

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

## Named Groups

An extended method of addressing captured groups, or simply “captures”, constitutes the definition of named captured groups. These are based upon the parentheses syntax of ordinary captured groups, with the opening parenthesis being followed by the “!” marker to designate the named variant. The basic syntax adheres to: `(!<GROUP-NAME>)`, with the `GROUP-NAME`, enclosed between the markers `<` and `>` being any character string — even white spaces — apart from the `>` group name end determinator.

    CL-USER > (match-captures (match-re "(!<number>%d+)" "123abc"))
    #(#<re-capture start=0, end=3, substring="123", name=number>)

These names are not confined to uniqueness, acting in some sense as “tags”: An arbitrary number of named captured groups with the same denomination may be defined. The following example retrieves four named captures, each with the name “letter”:

    CL-USER > (match-captures (match-re "(!<letter>%a)+" "abcd"))
    #(#<re-capture start=0, end=1, substring="a", name=letter> #<re-capture start=1, end=2, substring="b", name=letter> #<re-capture start=2, end=3, substring="c", name=letter> #<re-capture start=3, end=4, substring="d", name=letter>)


## The `with-re-match` Macro

The `with-re-match` macro can be used to assist in extracting the matched patterns and groups.

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

An additional local function `$->` permits a generalized access to captured groups.

    ($-> (set &optional (selection :all) (component :capture) (default NIL)))


The `set` determines the subset of captured groups to retrieve, always a vector of ``re-capture'' instances, valid options being:

* `:all-groups` selects all groups, named and unnamed
* `:named-groups` selects named groups only
* `:unnamed-groups` selects unnamed groups only
* `group-name` is a string designating the name of the groups to select

The `selection` filters the subset obtained with the `set` parameter, returning either a vector of a single ``re-capture'' object. The argument expects one of these:

* `:all` simply returns the existing subset
* `:first` obtains the first group, or the default value upon the subset's dearth of items
* `:last` obtains the last group, or the default value upon the subset's dearth of items
* `index` constitutes an integer greater or equal to 0 which references the item to select from the subset

As an `re-capture` in its completeness might not be desiderated, the vector or single ``re-capture'' instance(s) might be replaced via the `component` argument:

* `:capture` simply returns each `re-capture` instance unmodified
* `:substring` extracts the substring from each `re-capture`
* `:start-position` extracts the start position of the group in the input string
* `:end-position` extracts the end position of the group in the input string
* `:start-and-end-position` extracts the start and end position as a two-item list (start end)
* `:name` extract the group name, which might be `NIL` if an unnamed group

Failure to obtain a group usually returns the `NIL` value. This behavior might be modified by setting a different substitution through the `default` parameter.

    CL-USER > (with-re-match (match (match-re "(!<year>%d{=2,4})/(!<month>%d{[1,2]})/(!<day>%d{=2})" "2018/05/31"))
                (format T "~&Year:  ~a~%" ($-> "year"  :first :substring))
                (format T "~&Month: ~a~%" ($-> "month" :first :substring))
                (format T "~&Day:   ~a~%" ($-> "day"   :first :substring)))
    Year:  2018
    Month: 05
    Day:   31


## Additional Features

In addition to supporting all of what Lua pattern matching has to offer, it also supports branching with `|` and uncaptured groups: `(?..)`. For example...

    CL-USER > (match-re "(?a|b)+" "abbaaabbccc")
    #<RE-MATCH "abbaaabb">

Finally, the `re` package has one special feature: user-defined character set predicates! Using `%:`, you can provide a predicate function for the regexp VM to test characters against.

    CL-USER > (match-re #r"%:digit-char-p:+" "103")
    #<RE-MATCH "103">

The predicate must take a single character and return non-nil if the character matches the predicate function. *Note: this is especially handy when parsing unicode strings!*



## Ranged Quantifiers

Additional to the quantifiers `?` (zero or one repetitions), `*` (zero or more repetitions), and `+` (one or more repetitions), a “ranged quantifier” is available. This comprises a family of adjustable iteration checkers, by default enclosed within `{` and `}`.

The family consists of these three variants:

| Variant  | Description                                                  |
| -------- | ------------------------------------------------------------ |
| `{[…]}`  | A range of inclusive bounds.                                 |
| `{=…}`   | A sequence of valid repetitions.                             |
| `{%:…:`} | A user-defined function to check for desiderated iterations. |


### Ranges

The instigating brace `{` must be immediately followed by the opening square bracket `[` to form `{[`. Further white spaces are ignored. The complete syntax — albeit variations are extant — is as follows:

    {[MINIMUM, MAXIMUM]}

Only integer numbers greater or equal to 0, white spaces and omissions are valid. The syntax resembles that of Java and Perl, equal also in its variations:

| Syntax                       | Description | Example
| ---------------------------- | --- | --- |
| `{[*MINIMUM,MAXIMUM*]}`      | The number of repetitions must be between *MINIMUM* and *MAXIMUM*, both bounds construed as inclusive. | `{[2,8]}` |
| `{[*MINIMUM*,]}`             | The number of repetitions must be at least *MINIMUM*, with no maximum required. Hence, *MAXIMUM* defaults to “infinity”. | `{[2,]}` |
| `{[,*MAXIMUM*]}`             | The number of repetitions must be at most *MAXIMUM*, with no minimum required. Hence, *MINIMUM* defaults to zero. | `{[,10]}` |
| `{[*REPETITIONS*]}`          | The number of repetitions must exactly equal *REPETITIONS*. This is commensurately equivalent to the syntax `{[*MINIMUM*,*MINIMUM*]}`. | `{[4]}` |
| `{[,]}`                      | The number of repetitions is unbounded (“infinite”) on both ends. In consectary, this is synonymous to the `*` quantifier. |

Some examples shall demonstrate these features:

    (match-re "a{[2,4]}b" "aaab")
    #<RE-MATCH "aaab">

    ;; Too few "a" occurrences.
    (match-re "a{[2,4]}b" "ab")
    NIL

    ;; Too many "a" occurrences.
    (match-re "a{[2,4]}b" "aaaaab")
    NIL

    (match-re "a{[2,]}b" "aaaaaaaaaaaaaaaaab")
    #<RE-MATCH "aaaaaaaaaaaaaaaaab">

    (match-re "a{[,4]}b" "aaaab")
    #<RE-MATCH "aaaab">

    ;; Too many "a" occurrences.
    (match-re "a{[,4]}b" "aaaaaaaaaaaaaaaaab")
    NIL
    
    (match-re "a{[3,3]}b" "aaab")
    #<RE-MATCH "aaab">

    (match-re "a{[3]}b" "aaab")
    #<RE-MATCH "aaab">

    (match-re "a{[,]}b" "aaab")
    #<RE-MATCH "aaab">

### Number set

The number of repetitions must equal one of those numbers in the sequence. This sequence is a comma-separated list of integer values greater or equal to zero. Redudant entries are permitted but meaningless. The equal sign must follow immediately after the opening brace to form `{=`, further white spaces are ignored.

The syntax is:
    {= a, b, c, …, d}

Examples:

    CL-USER > (match-re "a{=1,3,5}b" "aaab")
    #<RE-MATCH "aaab">

    CL-USER > (match-re "a{=1,3,5}b" "aab")
    NIL


### User-defined Function

The indagation whether the number of repetitions is valid is delegated to a global function *USER-FUNCTION*, the same must accept exactly one argument, the current number of iterations, and return a generalized boolean being `T` if the iterations are valid, and `NIL` upon invalidity. The syntax is:

    {%:USER-FUNCTION%:}

The signature, in corollary, complies to:

    lambda(number-of-repetitions) => generalized-boolean

Some examples to illustrate:

    (defun even-number-of-repetitions-p (counter)
      (evenp counter))

    (match-re "a{%:even-number-of-repetitions-p:}b" "aab")
    #<RE-MATCH "aab">
    
    (match-re "a{%:even-number-of-repetitions-p:}b" "aaab")
    NIL


## Regular Engine Configuration

A few instances of the regular expression engine behavior are adjustable, especially with the purpose of counteracting against undesired reservation of common characters like `{`, `}`, and `!` by the engine's features, and to retain compatibility with previous versions of the `re` library. The configurating entity is the class `re-configuration`, whose global instance `*re-configuration*` can be queried and modified by the following accessor functions:

* `re-configuration-permit-named-captures`, adjustable with a generalized boolean, determines whether named captured groups are recognized at all (`T`) or not (`NIL`)
* `re-configuration-named-capture-marker` designates the character utilized to distinguish an unnamed or ignored group from a named one, defaulting to `#\!`
* `re-configuration-named-capture-name-starter` designates the character utilized to start the group name portion of a named captured group, defaulting to `#\<`
* `re-configuration-named-capture-name-ender` designates the character utilized to mark the end of the group name portion of a named captured group, defaulting to `#\>`
* `re-configuration-permit-ranged-quantifiers`, adjustable with a generalized boolean, determines whether ranged quantifiers of the type `{…}` are recognized at all

An example follows:

    (setf (re-configuration-named-capture-marker       *re-configuration*) #\+)
    (setf (re-configuration-named-capture-name-starter *re-configuration*) #\[)
    (setf (re-configuration-named-capture-name-ender   *re-configuration*) #\])
    
    (with-re-match (match (match-re "(+[year]%d{=2,4})/(+[month]%d{[1,2]})/(+[day]%d{=2})" "2018/05/31"))
      (format T "~&Year:  ~a~%" ($-> "year"  :first :substring))
      (format T "~&Month: ~a~%" ($-> "month" :first :substring))
      (format T "~&Day:   ~a~%" ($-> "day"   :first :substring)))


# Thank You!

If you get some good use out of this package, please let me know; it's nice to know your work is valued by others.

I'm always improving it; it's the foundation for many of the other packages I've created for JSON parsing, XML parsing, HTTP header parsing, etc.

Should you find/fix a bug or add a nice feature, please feel free to send a pull request or let me know at [massung@gmail.com](mailto:massung@gmail.com).
