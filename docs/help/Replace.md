# Replace #

----------

The Replace dialog is use to initiate a search and replace through the text
displayed in the Text Editor. The various check boxes control the search type,
which includes regular expression searches.

**Find What**      The text or pattern to search for.

**Replace With**      The text to be used for replacment.

**Case Sensitive**      A match only occurs with text that matches the case of the search string.

**Whole Word**      A match only occurs with text that matches the case of the search string.

**Word Start**      A match only occurs if the character before is not a word character.

**Regular Expression**      The search string should be interpreted as a regular expression.

**POSIX compatible**      Treat regular expression in a more POSIX compatible manner by interpreting bare ( and ) for tagged sections rather than \\( and \\).

In a regular expression, special characters interpreted are:

.

|

Matches any character.  
  
---|---  
  
\\(

|

This marks the start of a region for tagging a match.  
  
\\)

|

This marks the end of a tagged region.  
  
\n

|

Where n is 1 through 9 refers to the first through ninth tagged region.  
  
\&lt;

|

This matches the start of a word using Scintilla's definitions of words.  
  
\&gt;

|

This matches the end of a word using Scintilla's definition of words.  
  
\x

|

This matches the end of a word using Scintilla's definition of words.  
  
[...]

|

This indicates a set of characters, for example, [abc] means any of the
characters a, b or c. You can also use ranges, for example [a-z] for any lower
case character.  
  
[^...]

|

The complement of the characters in the set. For example, [^A-Za-z] means any
character except an alphabetic character.  
  
^

|

This matches the start of a line (unless used inside a set, see above).  
  
$

|

This matches the end of a line.  
  
*

|

This matches 0 or more times. For example, Sa*m matches Sm, Sam, Saam, Saaam
and so on.  
  
+

|

This matches 1 or more times. For example, Sa+m matches Sam, Saam, Saaam and
so on.  
  


**Word Wrap**      Check this option to continue searching through the file when the end of file (direction down) or beginning or file (direction up) is reached.

**Direction**      Specifies the direction of search from the current selection.

**Find Next**      Press this button to move to the next matching entry in the specified direction.

**Replace**      Press this button to replace the current selection with the replacement text.

**Replace All**      Press this button to replace all occurrances of the Find text within the docment.

**Replace in Selection**      Press this button to replace all occurrances of the Find text within the current selection.

**Replace in Results**      Press this button to replace all occurrances of the Find text within the result list items.

**Cancel**      Press this button to cancel the search.

