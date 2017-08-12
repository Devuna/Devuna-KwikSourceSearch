# Search For #

----------

The Search for... dialog allows the user to specify what they want to find,
how the matching is to be done, and where and what files are to be searched.

**Find**      This is the text to be searched for. It can be either a literal search string or a [regular expression](UsingRegularExpressions.html). Multiple search arguments can be specified here by using a vertical bar | to separate the arguments. To search for a vertical bar, place two vertical bars together like this ||.

**Get Find Text From Specified File**      Specifies that the search strings are to be loaded from the specified file. Enter multiple search strings, one per line, in a simple text file and supply that filename to search for the specified strings.

**Case Sensitive      **Specifies that the search is to be case-sensitive.

**Exclude Comment Lines      **Specifies that matches which occur only within a Clarion comment are to be excluded from the search results. Only certain files, identified on the [Options](UserOptions.html) dialog, receive this special Clarion processing.

**Match Pattern at Start of Line**      Matches the pattern if at the beginning of a line.

**Match Pattern at End of Line**      Matches the pattern if at the end of a line.

**Exact Match**      Only lines that match exactly.

**Exclude Matching Lines**      Only lines that do not contain a match.

**Use Regular Expressions      **The Find string as a [regular expression](UsingRegularExpressions.html). The search engine interprets all metacharacters as regular expressions.

![Help.png](images\\Help.png)      Press this button to get regular expression
help

**Filenames Only**      Select this option to return one result line for each file that contains a match. All matches are not identified, only that the file contains a match.

**Read File List from Specified File      **Specifies that the list of files to be searched is contained in a text file. Specify the full path and filename of files to be searched, one per line, in a simple text file and supply that file name to have the program search only the files in the supplied list.

**Paths**      Search a semicolon delimited list of directories. The user can enter or paste directories into the path, select from the drop down list, or use the two buttons located to the right of the path. The ellipsis [...] button displays the Windows File Dialog and replaces the path with the selected folder. The plus [+] button displays the Windows File Dialog and appends a semicolon and the selected folder to the path.

**Search Subdirectories**      Searches for matching files in the current directory and all subdirectories.

**Levels**      Specifies how many subdirectory levels are to be searched. If the level is blank then all subdirectory levels are searched.

**Files**      Specifies a file or files to search. Separate the file names with a semicolon, vertical bar, or space. Wildcards can be used in the filenames.

**Exclude**      Specifies a file or files to exclude from the Results List. Separate the file names with a semicolon, vertical bar, or space. Wildcards can be used in the filenames.

**Load Saved Results**      The [Save Results](SaveResults.html) dialog allows the user to save the results to a re-loadable result list file. This button is used to re-load a previously saved re-loadable result list file.

**Search New Tab**      Press this button to begin the search and display the results on a new tab.

**Search**      Press this button to begin the search.

**Cancel**      Press this button to close the dialog and return to the main window without doing a search.

