# Options #

----------

The Options dialog is used to configure the application.

**Send To Command**      Use this field to define one or more send to commands. If the command uses long file names the path and executable filename should be enclosed in double quotes.

KSS supports two special tokens for use in the edit commands (note: the tokens
are not case sensitive):  
%1 or %file      represents the currently selected filename.  
%2 or %line represents the currently selected line number for editors that
provide a command line option to open the file at a specific line number.

The special &lt;Use Windows Default&gt; command cannot be removed from the
list and instructs to program to use Windows file associations to determine
the Send To command.

Press the ellipsis [...] button to use the Windows Explorer to select the Send
To command executable.

**Prompt for Command**      If more than one send to command is defined and this option is checked, the user is prompted to select which command to use.

KSS performs special processing on Clarion file types. This special processing
consists of identifying procedure names, code and data sections, and optional
comment removal. To accomplish this special processing it is necessary to
parse the entire file; if there are many files to process the presentation of
the results will be delayed. For the best performance, it is recommended to
enumerate the file extensions that will receive this special processing.

**Clarion Help File**      Use the ellipsis button to the right of the field to select the Clarion Help file you would like the application to use. Any Hot Spots in the Text Editor will invoke the selected Clarion help file when clicked with the mouse.

**Apply Clarion processing to all files**      Check this option to ignore the Clarion File Extensions list and apply special processing to all files (not recommended).

**Clarion File Extensions**      Use the Insert, Edit, and Delete buttons to manage the extensions in the list. The listed extensions are matched anywhere in the file name so, and entry of '.CLW', for example will match files named xyz.clw, xyz.clw.old, xyz.bad.clw.copy, etc.

**Properties**      The Properties list contains the names of all property files found by the application. These special property files are used to define the keywords and styles used by the application to provide syntax and lexigraphical highlighting based on a files extension. You can set the font name, size and use of bold, italic and underline, foreground and background colors. You can also choose to hide text with a given style, display all characters as upper or lower case and mark language elements as Hot Spots to trigger a call to the Clarion Help file.

Press the [Edit] button below the properties list, or double click on an
entry, to open the [Property Editor](PropertyEditor.html) dialog.

**Confirm results deletions**      The default behaviour for the program is to display a warning message to the user when thay are about to delete more than 1 line from the results list. There are three choices in the associated drop list:   
     Never - the warning message is never displayed   
     Always - the warning message is always displayed   
     When - this choice displays and additional field   
     **removing at least N rows**. Use the spin control to set the row limit for displaying the warning message.

**Result List**      The font Name, Size, Style, and Fore color of the results list.

**Application**      The background color of the main application window.

**Toolbar**      The background color of the application toolbar.

**Selected**      The background color to be used for selected text.

**Bookmark**      The background color to use for bookmarks.

**Link Paths and Files with Find Text      **The [Search for](SearchFor.html) dialog has many independent fields and options. When this option is enabled, all options in effect when you press the Search button are saved with the Find text. The next time the Find text is selected from the Most Recently Used list, the options associated with that text are restored to their previous state.

**Allow Multiple Instances**      When this option is selected the user will be allowed to start multiple instances of the application. Note all instances of the application share the same file for linking paths and files with the find text. Much like concurrency in database appplications, the last search for a particular text string will overwrite any previous search for the same text even if it is from a different instance of the application.

**Auto Resize Result Columns**      With this option selected, the application will resize the columns to accommodate the data in the column when the result list is initially loaded**.**

**Restore Point Timer**      The application can maintain a restore point for your session. This is useful if you work within the application for extended periods and occassionally experience an abnormal termination of the program. The timer value represents the frequency, in minutes, at which Restore Points are created. Set this value to zero to disable the Restore Point feature. Restore Points are deleted when the application terminates normally. If a restore point is detected when the application starts, the user is given the opportunity to recover to the restore point.

**Application Function Keys**      The key combinations associated with the 'Jump to Previous Result Line' and 'Jump to Next Result Line' functions can be reassigned to support alternate keyboard types. Click the adjacent ellipsis buttons to display the [Application Function Key Assignment](ApplicationFunctionKeyAssig.html) dialog. 

**Register**      This button only appears on the unlicensed evaluation copy and is used to register the key provided when the product is purchased. Once the key is registered, the evaluation period is removed and the button will no longer be displayed on the Options dialog.

**Apply**      Closes the window and applies any changes to the application.

