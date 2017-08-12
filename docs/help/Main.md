# Main Window #

----------

The main KSS window consists of a tabbed results sheet, a toolbar, and a text
editor. The status bar at the bottom of the window displays the Last Modified
data and time for the currently highlighted file in the Results List, product
registration information and product version.

**Tabbed Results Sheet**

After a search request has been completed, the results are shown in a list on
the current tab unless the 'Search New Tab' button was pressed on the 
[Search for](SearchFor.html) dialog box.

A new tab for search results can also be created by clicking on the tab with
the plus sign [+] or by pressing [Ctrl + T].

Press [Alt + S] , [Ctrl + W], or [Esc] when the result list has focus to
display the [Search for](SearchFor.html) dialog used to initiate a new
search.

Press [Ctrl+C] to copy the full path and filename to the clipboard.

Right click on the results list header to display the 
[Results List Formatter](ListboxFormatter.html) dialog. This dialog can be used to
rearrange the order of the columns in the results list. If the Location column
is empty for all results, the column is hidden.

Double click on a column resize bar to change the column width to auto fit the
contents.

Right click on the results list to display the context menu.

**_context menu/toolbar_**

Most of the results list context menu commands are on the toolbar for easy
access.

![Search.png](images\Search.png)      Search [Alt + S]  
displays the [Search for](SearchFor.html) dialog used to initiate a new
search.

![reload_rotate.png](images\\reload_rotate.png)      Redo Search [F5]  
performs the same search again without displaying the 
[Search for](SearchFor.html) dialog.

Show Match Summary      the [Match Summary](MatchSummary.html) dialog
displays a summary list of the matches found.

----------

![PreviousFolder.png](images\\PreviousFolder.png)      Jump ?&gt; Previous
Folder  
selects the first match in the previous folder in the path column.

![PreviousFile.png](images\\PreviousFile.png)      Jump ?&gt; Previous File  
selects the first match in the previous file in the Filename + Ext columns.

![PreviousLine.png](images\\PreviousLine.png)      Jump ?&gt; Previous Line  
selects the previous line in the results list.

![NextLine.png](images\\NextLine.png)      Jump ?&gt; Next Line  
selects the next line in the results list.

![NextFile.png](images\\NextFile.png)      Jump ?&gt; Next File  
selects the first match in the next file in the Filename + Ext columns.

![NextFolder.png](images\\NextFolder.png)      Jump ?&gt; Next Folder  
selects the first match in the next folder in the path column.

----------

![DeleteLine.png](images\\DeleteLine.png)      Delete ?&gt; Selected ?&gt;
Line [delete key]  
deletes the currently selected line.

![DeleteFile.png](images\\DeleteFile.png)      Delete ?&gt; Selected ?&gt;
Filename  
deletes all occurrences of the currently selected Filename + Ext columns; a
delete confirmation message is displayed.

![DeleteExtension.png](images\\DeleteExtension.png)      Delete ?&gt; Selected
?&gt; Extension  
deletes all occurrences of the currently selected Ext column; a delete
confirmation message is displayed.

![DeletePath.png](images\\DeletePath.png)      Delete ?&gt; Selected ?&gt;
Path  
deletes all occurrences of the currently selected Path column; a delete
confirmation message is displayed.

![DeleteComment.png](images\\DeleteComment.png)      Delete ?&gt; Comment
Lines  
deletes all lines in which the match only occurs within a Clarion comment; a
delete confirmation message is displayed.

![DeleteLabel.png](images\\DeleteLabel.png)      Delete ?&gt; Label Lines  
deletes all lines that do not have a space [ ] or exclamation mark [!] as the
first character; a delete confirmation message is displayed.

![DeleteCode.png](images\\DeleteCode.png)      Delete ?&gt; Matches in CODE  
deletes all lines that occur in Clarion CODE sections; a delete confirmation
message is displayed.

![DeleteData.png](images\\DeleteData.png)      Delete ?&gt; Matches in DATA  
deletes all lines that occur in Clarion DATA sections; a delete confirmation
message is displayed.

![FindDelete.png](images\\FindDelete.png)      Delete ?&gt; Find and Delete
[Ctrl + Delete]  
displays the [Find and Delete Options](FindAndDeleteOptions.html) dialog
used to search for text within a column in the results list and delete
matching or non-matching entries; a delete confirmation message is displayed.

![UndoDelete.png](images\\UndoDelete.png)      Undo Delete [Ctrl + Z]  
restores deleted results to the list.

Macro ?&gt; Player      the [Macro Player](MacroPlayer.html) dialog
window is displayed. Use this dialog to Load, Save and Play delete commands
that have been previously recorded.

Macro ?&gt; Record      starts recording all delete command actions performed
by the user.

Macro ?&gt; Stop Recording      stops the recording of delete command actions.

----------

![Save.png](images\\Save.png)      Save Results  
displays the [Save Results](SaveResults.html) dialog used to save results
to the clipboard or a file.

![SendTo.png](images\\SendTo.png)      Send To [Ctrl + E]  
executes the send to command specified in the [Options](UserOptions.html)
dialog. If more than one send to command has been defined and the Prompt for
Command check box is checked, the [Send To](SelectSendToCommand.html)
dialog is displayed and then the selected command is executed.

----------

Copy Fullname to Clipboard [Ctrl + C]      copies the full path and filename
of the currently selected result list item to the clipboard.

Copy Path to Clipboard      copies the file path of the currently selected
result list item to the clipboard.

Copy Findstr to Clipboard      copies the findstr command used to provide the
search results to the clipboard. This can be useful for debugging unexpected
results.

Reveal in Explorer      open the folder that contains the currently selected
file.

----------

![UserOptions.png](images\\UserOptions.png)      Options  
displays the [Options](UserOptions.html) dialog.

![SplitH.png](images\\SplitH.png)![SplitV.png](images\\SplitV.png)      Change
Layout  
alternately switches the arrangement of the results list and the text editor
between' Results on Left and Editor on Right' and 'Results on Top and Editor
on Bottom'.

Center Resizer Bar [Crtl + |]      Centers the resizer bar on the window.

Change Results List Format      displays the 
[Results List Formatter](ListboxFormatter.html) dialog which which can be used to
rearrange the results list columns.

Auto Size Columns      resizes all results list columns to fit the contents.

----------

![images/main19.jpg](images\\main19.jpg)      Close Tab [Ctrl + F4] or [Ctrl +
W]  
closes the currently selected results tab. note: the first tab cannot be
closed and pressing this when the first tab is selected causes the resul;ts
list to be cleared and a new search initiated.

![images/main20.jpg](images\\main20.jpg)      Cancel Search  
terminates a search in progress.

----------

![CheckBox_On.png](images\\CheckBox_On.png)![CheckBox_Off.png](images\\CheckBox_Off.png)
Hide Edit Panel  
Hides or unhides the edit panel.

----------

**Text Editor**

The Text Editor displays the currently selected file and result line each time
a new result list row is selected. The Text Editor provides Clarion syntax
highlighting and all matches found in the currently selected file are
bookmarked.

The 'File Modified' indicator ![save-warn.png](images\\save-warn.png) is
enabled if the file currently being viewed has been modified by the user. The
indicator is a button which can be pressed to save the file contents.

Right click on the Text Editor to display the context menu. Most context menu
commands have a corresponding key combination.

**context menu**

Save      [Ctrl + S]  
     saves the Text Editor buffer to the file.

Save As      [Ctrl + Shift + S]  
     displays the Windows Save As dialog and saves the Text Editor buffer to the selected file.

Auto Save  
     when enabled, modified files are automatically saved (without prompting the user) when moving to another file on the result list.

Send To      [Ctrl + E]  
     executes the send to command specified in the [Options](html\\UserOptions.htm) dialog. If more than one send to command has been defined and the Prompt for Command check box is checked, the [Send To](html\\SelectSendToCommand.htm) dialog is displayed and then the selected command is executed.

Print      [Ctrl + P]  
     prints the file currently displayed in the text editor. The file is printed with full syntax colouring to the printer you select.   
note:      there is no print preview. If you want to preview select a PDF or
XPS printer. For XPS Printer go to printer preferences, XPS documents, and
make sure Auto Open is checked.

Cut      [Ctrl + X]  
     copies the currently selected text to the clipboard and deletes the text.

Copy      [Ctrl + C]  
     copies the currently selected text to the clipboard.

Paste      [Ctrl + V]  
     pastes the current contents of the clipboard into the document at the current cursor position.

Delete      [Delete]  
     deletes the currently selected text from the document.

File Mode -&gt; selects the desired language syntax highlighting  
     default languages are clarion and text; the choices can be expanded by the use of special [property files](html\\PropertyFiles.htm).

Folding -&gt; Toggle Fold [Ctrl + Shift + M]  
     expands or contracts the current fold based on its fold state.

Folding -&gt; Toggle All Folds [Ctrl + Shift + T]  
     expands or contracts all folds based on the fold state of the first fold marker in the file.

Folding -&gt; Fold Margin  
     displays or hides the fold margin.

Find      [Alt + F3]  
     displays the [Find](html\\Find.htm) dialog used to locate text within the text editor.

Find Next      [F3]  
     finds the next matching text in the file. A Find operation must first be initiated in order to set the text to look for.

Replace      [Ctrl + R]  
     displays the [Replace](html\\Find.htm) dialog used to locate and replace text within the text editor.

Toggle Bookmark      [Ctrl + F2]  
     adds or removes a bookmark from the currently selected line.

Previous Bookmark      [Shift + F2]  
     scrolls the previous bookmark into view.

Next Bookmark      [F2]  
     scrolls the next bookmark into view.

Clear All Bookmarks      [Ctrl + Shift + F2]  
     removes all bookmarks.

Hot Spots Enabled  
     enables or disables hot spot support. When enabled, clicking on a hot spot causes the program to attempt to find help for the clicked item. For Clarion, help is obtained from the help file specified on the [Options](html\\UserOptions.htm) dialog. For all other languages a search on the internet is initiated.

Go To Line Number      [Ctrl + G]  
     displays the [Go To Line Number](html\\GoTo.htm) dialog used to scroll the specified line number into view.

Move -&gt; Top      [Ctrl + Home]  
     scrolls to the top of the file.

Move -&gt; Page Up      [PgUp]  
     scrolls up one page.

Move -&gt; Page Down      [PgDn]  
     scrolls down one page.

Move -&gt; Bottom      [Ctrl + End]  
     scrolls to the bottom of the file.

Hide Results List  
     Hide or show the results list.

