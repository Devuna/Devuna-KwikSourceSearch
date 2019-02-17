# Devuna-KwikSourceSearch #

**February 18, 2019 Release**
This release includes bug fixes and many enhancements suggested by Mark Riffy's fork of this project.  

***Release Notes***

1) Added 'Include Files with Non-printable Characters' to the search parameters window.  When checked the /p flag is removed from the findstr command line.
   
2) The 'Read File List from Specified File' now supports selecting an application filelist.xml file.     This causes KSS to process all clw, inc, def, equ, trn and int files listed in the xml file
   
3) Added code to force the main window onto the primary monitor on startup when only one monitor is available.  This resolves the issue of non-accessible windows in certain situations when multiple monitors are involved.

4) Fixed bug in exclude MATCH code, to achieve the same effect as Mark Riffy's additional toolbar button that deletes generated built-in Clarion files.  Simply place this (without quotes) in the Exclude field '*_BC*.clw;*_SF.clw;*_R*.clw' to have them excluded from the results.

5) Added Copy for Skype and Copy for Slack to editor context menu

6) CheckRegistration procedure uses GetUserName() for unregistered public domain versions of the program.

7) Compiled and tested with Clarion 11.0.13244

**August 2017 Initial Public Release**

With Kwik Source Search (KSS) you can perform powerful standard and regular expression searches through one or multiple text files, such as source code, log files, and debugging information.

KSS is capable of finding the exact text you are looking for in any ASCII file or files. Sometimes you have only part of the information that you want to match, or you want to find a wider range of information. In such cases, KSS has the powerful capability to search for patterns of text using regular expressions.

The underlying search engine for KSS is the Findstr command. KSS provides a convenient, user friendly, interface for the many command line options. Files with non-printable characters are always skipped and the regular expression functionality of the Findstr command is used for the main text search.

KSS captures and filters the output of the Findstr command, opens the file associated with the selected result list item, and displays it in a syntax highlighting editor. KSS has built-in styling for Clarion, C++, C#, HTML, Java, and XML source files and can easily be extended, through the use of special property files, to provide styling for other languages.


This version of KSS is slightly different from the last commercial release.

Most notably the Results List 'Print' and the 'CheckForUpdate' features have been removed as they required other 3rd party tools, some of which are no longer available.

