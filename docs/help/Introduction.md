# Introduction #

----------

With Kwik Source Search (KSS) you can perform powerful standard and regular
expression searches through one or multiple text files, such as source code,
logging, and debugging information.

KSS is capable of finding the exact text you are looking for in any ASCII file
or files. However, sometimes you have only part of the information that you
want to match, or you want to find a wider range of information. In such
cases, KSS has the powerful capability to search for patterns of text using
[regular expressions](UsingRegularExpressions.html).

The underlying search engine for KSS is the
[Findstr](http://technet.microsoft.com/en-us/library/bb490907.aspx) command.
KSS provides a convenient, user friendly, interface for the many command line
options. Search text is always treated as a literal search string [/l and /c:]
and files with non-printable characters are always skipped [/p]. The regular
expression functionality of the Findstr command is used for the main text
search. There are many examples that can be found on the internet.

KSS captures and filters the output of the Findstr command, opens the file
associated with the selected result list item, and displays it in a syntax
highlighting editor built on the [Scintilla](http://www.scintilla.org)
control. KSS has built-in styling for the Clarion language and can easily be
extended, through the use of special [property
files](PropertyFiles.html), to provide styling for other languages
supported by the Scintilla control.

