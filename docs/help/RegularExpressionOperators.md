# Regular Expression Operators #

----------

Regular expressions are used to describe patterns in text. The following
characters are regular expression operators (or metacharacters) used by the
[Find and Delete Options](FindAndDeleteOptions.html) dialog to increase
the power and versatility of regular expressions.

^

|

Caret matches the beginning of the string or the beginning of a line within
the string. For example:^@chapter matches the "@chapter" at the beginning of a
string.  
  
---|---  
  
$

|

Dollar sign is similar to the caret, but it matches only at the end of a
string or the end of a line within the string. For example:p$ matches a record
that ends with a p.  
  
.

|

Period (.) matches any single character except a new line. For example: .P
matches any single character followed by a P in a string. Using concatenation
we can make regular expressions like 'U.A', which matches any three-character
sequence that begins with 'U' and ends with 'A'.  
  
[...]

|

This is called a character set. It matches any one of the characters that are
enclosed in the square brackets. For example: [MVX] matches any one of the
characters M, V, or X in a string. Ranges of characters are indicated by using
a hyphen between the beginning and ending characters, and enclosing the whole
thing in brackets. For example:[0-9] matches any digit. To match '-', write it
as '---', which is a range containing only '-'. You may also give '-' as the
first or last character in the set. To match '^', put it anywhere except as
the first character of a set. To match a ']', make it the first character in
the set. For example: []d^]matches either ']', 'd' or '^'.  
  
[^ ...]

|

This is a complemented character set. The first character after the [ must be
a ^. It matches any characters except those in the square brackets (or
newline). For example: [^0-9] matches any character that is not a digit.  
  
|

|

Vertical bar is the alternation operator and it is used to specify
alternatives. For example: ^P|[0-9]matches any string that matches either ^P
or [0-9]. This means it matches any string that contains a digit or starts
with P. The alternation applies to the largest possible regexps on either
side. No spaces are allowed between strings and the alternation operator.  
  
{...}

|

Brackets are used for grouping in regular expressions as in arithmetic. They
can be used to concatenate regular expressions containing the alternation
operator, |.  
  
*

|

Asterisk means that the preceding regular expression is to be repeated as many
times as possible to find a match. For example: ph* applies the * symbol to
the preceding h and looks for matches to one p followed by any number of h's.
This will also match just p if no h's are present. The * repeats the smallest
possible preceding expression (use parentheses if you wish to repeat a larger
expression). It finds as many repetitions as possible. For example:
(c[ad][ad]*r x) matches a string of the form (car x), (cdr x), (cadr x), and
so on.  
  
+

|

Plus sign is similar to *, but the preceding expression must be matched at
least once. This means that: wh+y would match "why" and "whhy" but not "wy,"
whereas wh*y would match all three of these strings. This is a simpler way of
writing the last * example: (c[ad]+r x)  
  
?

|

Question mark is similar to *, but the preceding expression can be matched
once or not at all. For example: fe?d will match fed and fd, but nothing else.  
  
\

|

Backslash is used to suppress the special meaning of a character when
matching. For example: \$ matches the character $.  
  
In regular expressions, the *, +, and ? operators have the highest precedence,
followed by concatenation, and finally by |.



