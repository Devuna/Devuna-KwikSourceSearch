# Using Regular Expressions #

----------

KSS is capable of finding the exact text you are looking for in any ASCII file
or files. However, sometimes you have only part of the information that you
want to match, or you want to find a wider range of information. In such
cases, KSS has the powerful capability to search for patterns of text using
regular expressions.

Regular expressions are a notation for specifying patterns of text, as opposed
to exact strings of characters. The notation uses literal characters and
metacharacters. Every character that does not have special meaning in the
regular expression syntax is a literal character and matches an occurrence of
that character. For example, letters and numbers are literal characters. A
metacharacter is a symbol with special meaning (an operator or delimiter) in
the regular-expression syntax.

The following table lists the metacharacters that the [Search
For](html\\SearchFor.htm) dialog accepts.

.

|

Period (.) matches any single character except a new line. For example: .P
matches any single character followed by a P in a string. Using concatenation
we can make regular expressions like 'U.A', which matches any three-character
sequence that begins with 'U' and ends with 'A'.  
  
---|---  
  
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
  
^

|

Caret matches the beginning of the string or the beginning of a line within
the string. For example:^@chapter matches the "@chapter" at the beginning of a
string.  
  
$

|

Dollar sign is similar to the caret, but it matches only at the end of a
string or the end of a line within the string. For example:p$ matches a record
that ends with a p.  
  
[class]

|

Character class: it matches any one of the characters that are enclosed in the
square brackets. For example: [MVX] matches any one of the characters M, V, or
X in a string. Ranges of characters are indicated by using a hyphen between
the beginning and ending characters, and enclosing the whole thing in
brackets. For example:[0-9] matches any digit. To match '-', write it as
'---', which is a range containing only '-'. You may also give '-' as the
first or last character in the set. To match '^', put it anywhere except as
the first character of a set. To match a ']', make it the first character in
the set. For example: []d^]matches either ']', 'd' or '^'.  
  
[^class]

|

Inverse class: the first character after the [ must be a ^. It matches any
characters except those in the square brackets (or newline). For example:
[^0-9] matches any character that is not a digit.  
  
\x

|

Backslash is used to suppress the special meaning of a character when
matching. For example: \$ matches the character $.  
  
\&lt;xyz

|

Word position: beginning of word  
  
xyz\&gt;

|

Word position: end of word

