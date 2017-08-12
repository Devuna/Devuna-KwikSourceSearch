# Property Files #

----------

Keystone Source Search can be easily extended to support styling for other
languages using special property files. These are simply text files that
provide lexer, keyword, and styling options for the Scintilla control. These
are similar to, but not as sophisticated as, the property files used with the
[sciTE editor](http://www.scintilla.org/SciTEDoc.html).

**File Locations**   
KSS searches for the special property files in two locations:  
%ProgramFiles%/Keystone Computer Resources/KSS - this folder is reserved for
special property files that may be shipped as part of the basic product. These
files should not be modified.

%AppData%/Keystone Computer Resources/KSS - this folder is reserved for user
defined special property files. Property files in this folder override
similarly named property files located in the %ProgramFiles%/Keystone Computer
Resources/KSS folder.

**File Names   
**Property files have a required naming format which is _language_.properties where _language_ is the name that will appear on the KSS [File Mode menu](main.html#FileModeMenu).

**File Contents   
**Blank lines and Clarion style comments, line begins with a !, are ignored.

The first non-comment line in the file should specify the lexer to be used
like this:  
**Lexer=****_lexer_name_**   
where _lexer_name_ is one of **clarion**, **cpp**, **html**, or **text**;
these are the only lexers currently supported. Invalid values are equated to
text.

The remainder of the file contents are organized into sections denoted by [ ]
and followed by the data for that section. Data for each section can be
specified on multiple lines. Sections should be laid out in the following
order:

**[FilePatterns]**   
Specify each of the file patterns, separated by a space, to be associated with
this property file. If the same extension appears in multiple property files,
only the first occurrance found will be used. The property files are processed
alphabetically.

example:  
[FilePatterns]  
*.xml *.xsl *.svg *.xul *.xsd *.dtd   
*.xslt *.axl *.xrc *.rdf

**[Options]**   
A lexer may have special options that control its behaviour. Use this section
to specify the lexer options, one per line. The format for this section is
option, "=", the option value, ";", the option description.

example:  
fold=1;Folding is turned on by setting fold=true.

**[Keywords#]**   
Each lexer uses different sets of keywords to provide its functionality. The
**#** refers to the Scintilla keyword list that is to contain the specified
keywords. Depending on the lexer, the case of the keywords can be significant.
Most lexers use lower case, however, the Clarion lexer requires upper case
keywords.

example:

! Comment that describes Keyword Set  
[Keywords1]  
abstract as ascending base bool break by byte case catch char checked  
class const continue decimal default delegate descending do double else enum  
equals event explicit extern false finally fixed float for foreach from goto
group if

! Comment that describes Keyword Set  
[Keywords2]  
file group region

**[Styles]**   
The lexers determine a style number for each lexical type, such as keyword,
comment or number. These settings determine the visual style to be used for
each style number of each lexer.

The value of each setting is a set of ',' separated fields, some of which have
a subvalue after a ':'. Each lexer uses several styles to highlight various
aspects of the text. The styles are numbered and can contain the following,
comma separated, values in any order:

Style_#_=font:_font_name_,size:_font_size_,bold,italic,underline,fore:_fore_color_,back:_back_color
_,eolfilled,case:_case_opt_,hide,hotspot

Style#=      specify the style number being defined.

font:_font_name_      specify the name of the font to be used

size:_font_size_      specify the point size of the font

bold      specify this for a bold font style

italic      specify this for an italic font style

underline      specify this for an underline font effect

fore:_fore_color      _specify the foreground color as a hexadecimal rgb value
[#RRGGBB]

back:_back_color      _specify the background color as a hexadecimal rgb value
[#RRGGBB]

eolfilled      if the last character in the line has a style with this
attribute set, the remainder of the line up to the right edge of the window is
filled with the background colour set for the last character. This is useful
when a document contains embedded sections in another language such as html
pages with embedded JavaScript.

case:_case_opt_      specify 0 for Mixed Case, 1 for Upper Case, or 2 for
Lower Case

hide      specify this if the lexical type is to be hidden

hotspot      specify this if the lexical type is to act like a hyperlink.
Within KSS, this is only useful for the Clarion lexer. Hotspots attempt to
display the Clarion Help for the lexical type.

example:  
[Styles]  
! **&lt;Default&gt;**      use this format for the Default style to cause it
to sort to the top of the list  
style32=font:Verdana,size:10  
! Doc Comment Line: line comments beginning with /// or //!.  
style15=fore:#3F703F,font:Comic Sans MS,size:9

