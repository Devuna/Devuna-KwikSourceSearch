# Property Editor #

----------

The Property Editor dialog provides a convenient way to override the
application styling defaults used to display files in the editor window. There
are four tabs each with several fields.

###  _General Tab_

**Description**      A brief description of the property file contents

**File Patterns**      A space delimited list of file patterns used to identify the files that will receive the associated styling.



###  _Options Tab_

**Options      **Each lexer has certain options that control its behaviour. All available options are listed and a brief description of the option is displayed below the list as well as in the tooltip for that option.



###  _Keywords Tab_

**Keywords      **Each lexer uses different sets of keywords to provide its functionality. There are a maximum of nine Keyword sets. The Keyword Set Description is shown in the list on the left and the Keywords in the set are shown in the text box on the right.

Right click on the text box to display the text box context menu. This menu
has one option "View as List" which displays the Keywords in a sorted list.

Right click on the list to display the list context menu. This menu has
options to Insert, Edit, or Delete rows from the list as well as an option to
"View as Text" whic reverts back to the text box view. When the list has
focus, the [Insert], [Ctrl+Enter], and [Delete] keys can also be used to
initiate editing of the list.

###  

###  _Styles Tab_

**Styles      **The lexers determine a style number for each lexical type, such as keyword, comment or number. These settings determine the visual style to be used for each style number of each lexer.

The **&lt;Default&gt;** style appears first in the Styles list and **has
special significance.**  
**This style provides the styling options that will be used for all other styles where a specific styling option has not been selected. **

The Property Editor indicates options that are inherited from the
&lt;Default&gt; style with an italic prompt. Options that are specifically set
for a style, overriding the default option, are indicated with a bold prompt.
The Font option may have a mix of default and specific settings in which case
the prompt is shown in bold italic_.  
  
italic_**                        **inherited from default **  
****bold****                  **      specifically set, overrides default**   
****_bold italic_****      ** mix of inherited and override options

**Font**      Press the ellipsis [...] button to the right of the field to display the font selection dialog to select the desired font. 

**Fore Color**      Press the ellipsis [...] button to the right of the field to display the color selection dialog to select the desired text color. 

**Back Color      **Press the ellipsis [...] button to the right of the field to display the font selection dialog to select the desired background color. 

**Options**      Additional text editor display options.   
     Hide      The language element is hidden in the text editor when checked.   
     Hot Spot      This option is used to mark language elements in the text that can detect mouse clicks. The cursor changes to a hand over hotspots and an underline appears to indicate that these areas are sensitive to clicking. This is used to allow hyperlinks to Clarion help for the language element.   
     EOL Filled      If the last character in the line has a style with this attribute set, the remainder of the line up to the right edge of the window is filled with the background colour set for the last character. This is useful when a document contains embedded sections in another language such as html pages with embedded JavaScript.

**Case**      Select the case in which the language element is to be displayed.   
     Mixed      the case of the language element is not altered.   
     Upper      the language element is converted to upper case.   
     Lower      the language element is converted to lower case.

**Save**      Press the Save button to save your changes. You will be asked to provide the path and filename for the modified properties file.

**Cancel**      Press the Cancel button to discard any changes and return to the [Options](UserOptions.html) dialog.

