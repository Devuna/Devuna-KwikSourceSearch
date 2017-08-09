   MEMBER

!region Notices
!========================================================================================
!                                 Scintilla Viewer Class
!========================================================================================
!Notice : Copyright (C) 2017, Devuna
!Author : Randy Rogers <rrogers@devuna.com>
!         Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of Devuna-Scintilla(https://github.com/Devuna/Devuna-KwikSourceSearch)
!
!    Devuna-KwikSourceSearch is free software: you can redistribute it and/or modify
!    it under the terms of the MIT License as published by
!    the Open Source Initiative.
!
!    Devuna-KwikSourceSearch is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    MIT License for more details.
!
!    You should have received a copy of the MIT License
!    along with Devuna-KwikSourceSearch.  If not, see <https://opensource.org/licenses/MIT>.
!
!Revisions:
!==========
!2003.10.25 KCR modified to match Ron Schofields enhanced lexer for enhanced
!               reserved word processing
!2003.11.28 KCR added deprecated keyword lists for use with new
!               lexer dated 2003.11.27 or later
!2003.12.21 KCR added themed support and a default set of styles
!2004.12.09 KCR changed 'Family' from ABC to SCI
!========================================================================================
!endregion Notices

  ! Include the class declaration
  INCLUDE('CSciViewer.inc'),ONCE
  INCLUDE('ABREPORT.INC'),ONCE
  INCLUDE('KEYSTONE.CLW','equates'),ONCE
  
  ! Procedure Map
  MAP
    GetFileExtension(*CSTRING szFile),STRING
    GetMaximum(LONG, LONG),LONG
    EnumPrinters(*QUEUE pQueue, *CSTRING pPrinterName),LONG,PROC
    GetPrinterDevice(*CSTRING szPrinterName, *HGLOBAL hDevNames, *HGLOBAL hDevMode),BOOL,PROC
   
    MODULE('SciUtil.c')
      CallSciMsgProc(LONG fn,       |
                     LONG ptr,      |
                     LONG uMsg,     |
                     LONG wParam,   |
                     LONG lParam),LONG,RAW,PROC,NAME('_CallSciMsgProc@FPFllll_lllll')
      GetXPMData(LONG lImage),LONG,NAME('_GetXPMData')                     
    END

    MODULE('win32API')
      CallWindowProc(UNSIGNED wndprc,   |
                    UNSIGNED hWnd,      |
                    UNSIGNED uMsg,      |
                    LONG wParam,        |
                    LONG lParam         |
                   ),LONG,PASCAL,NAME('CallWindowProcA')

      CreateWindowEx(UNSIGNED dwExStyle,     |
                    *CSTRING lpClassName,    |
                    <*CSTRING lpWindowName>, |
                    UNSIGNED dwStyle,        |
                    SIGNED x,                |
                    SIGNED y,                |
                    SIGNED nWidth,           |
                    SIGNED nHeight,          |
                    UNSIGNED hWndParent,     |
                    UNSIGNED hMenu,          |
                    UNSIGNED hInstance,      |
                    LONG lpParam             |
                   ),LONG,PASCAL,RAW,NAME('CreateWindowExA')

      DestroyWindow(UNSIGNED hWnd),BOOL,PASCAL,PROC

      GetWindowRect(UNSIGNED hWnd,    | handle to window
                  *RECT lpRect        | window coordinates
                  ),BOOL,PASCAL,RAW,PROC

      SetWindowPos(UNSIGNED hWnd,            |
                  UNSIGNED hWndInsertAfter,  |
                  SIGNED X,                  |
                  SIGNED Y,                  |
                  SIGNED cx,                 |
                  SIGNED cy,                 |
                  UNSIGNED uFlags            |
                ),BOOL,PASCAL,PROC

      SendMessage(UNSIGNED hWnd,     |
                  UNSIGNED uMsg,     |
                  UNSIGNED wParam,   |
                  LONG lParam        |
                 ),LONG,PASCAL,PROC,NAME('SendMessageA')


      LoadLibrary(*CSTRING pszModuleFileName), UNSIGNED, PASCAL, RAW, NAME('LoadLibraryA')
      FreeLibrary(UNSIGNED hModule), LONG, PASCAL, PROC
      GetLastError(),ULONG,PASCAL

      InvalidateRect(UNSIGNED,ULONG,BOOL),BOOL,RAW,PASCAL,PROC

      ScreenToClient(UNSIGNED,*POINT),PASCAL,RAW

      GlobalAlloc(UNSIGNED,ULONG),UNSIGNED,PASCAL
      GlobalFree(UNSIGNED),UNSIGNED,PASCAL,PROC
      GlobalHandle(UNSIGNED),UNSIGNED,PASCAL
      GlobalLock(UNSIGNED hMem),ULONG,PASCAL,PROC
      GlobalUnlock(UNSIGNED hMem),BOOL,PASCAL,PROC
      SetWindowLong(UNSIGNED hWnd, SIGNED Index, LONG dwNewLong),LONG,PASCAL,RAW,NAME('SetWindowLongA'),PROC
      GetWindowLong(UNSIGNED hWnd, SIGNED Index),LONG,PASCAL,RAW,NAME('GetWindowLongA')
      GetClassName(UNSIGNED hWnd, *CSTRING ClassName, LONG nClassName),LONG,PASCAL,RAW,PROC,NAME('GetClassNameA')
      
      CreateFile(*CSTRING szFileName, LONG dwDesiredAccess, LONG dwShareMode, LONG lpSecurityAttributes, |
               LONG dwCreationDisposition, LONG dwFlagsAndAttributes, UNSIGNED hTemplateFile),UNSIGNED, |
               NAME('CreateFileA'),PASCAL,RAW
      ReadFile(UNSIGNED hFile, LONG lpBuffer, LONG dwBytes, *LONG dwBytesRead, LONG lpOverlapped),BOOL,RAW,PASCAL
      GetFileSize(UNSIGNED hFile, *LONG FileSizeHigh),LONG,RAW,PASCAL  
      CloseHandle(UNSIGNED),BOOL,RAW,PASCAL,PROC
      CreateFileMapping(UNSIGNED hFile, LONG lpAttributes, LONG flProtect, LONG dwMaximumSizeHigh, LONG dwMaximumSizeLow, LONG lpName),UNSIGNED,PASCAL,NAME('CreateFileMappingA')
      MapViewOfFile(UNSIGNED hFileMappingObject, LONG dwDesiredAccess, LONG dwFileOffsetHigh, LONG dwFileOffsetLow, LONG dwNumberOfBytesToMap),LONG,PASCAL
      MoveMemory(LONG lpDestination, LONG lpSource, LONG nLength),PASCAL,NAME('RtlMoveMemory')
      UnmapViewOfFile(LONG pBuf),BOOL,PROC,PASCAL
      GetTempPath(LONG nBufferLength, *CSTRING szBuffer),LONG,PASCAL,RAW,PROC,NAME('GetTempPathA')
      
      PrintDlg(*?),BOOL,RAW,PASCAL,NAME('PrintDlgA')
      GetDeviceCaps(HDC, SIGNED),SIGNED,PASCAL,NAME('GetDeviceCaps')
      GetLocaleInfo(ULONG,ULONG,*CSTRING,SIGNED),SIGNED,PASCAL,RAW,NAME('GetLocaleInfoA'),PROC
      MulDiv(SIGNED,SIGNED,SIGNED),SIGNED,PASCAL
      DPtoLP(HDC hDC, LONG lpPoints, SIGNED nCount),BOOL,PASCAL,RAW,PROC
      CreatePen( SIGNED, SIGNED, COLORREF),HPEN,PASCAL
      CreateFont(LONG nHeight,LONG nWidth,LONG nEsc,LONG nOri,LONG fnWeight,ULONG fItalic,ULONG fUnderline,ULONG fStrikeOut,ULONG fCharSet,ULONG fOutPutPrecs,ULONG fClipPrecs,ULONG fQualit,ULONG fPitchAndFamily,ULONG lpszFace),UNSIGNED,RAW,PASCAL,NAME('CreateFontA')
      GetTextMetrics(HDC hdc, LONG lptm),SHORT,PASCAL,PROC,NAME('GetTextMetricsA')
      SelectObject(HDC hdc, HGDIOBJ hgdiobj),HGDIOBJ,PASCAL,PROC,NAME('SelectObject')
      DeleteDC(HDC),BOOL,PASCAL,PROC
      DeleteObject(HGDIOBJ hgdiobj),BOOL,PASCAL,PROC
      StartDoc(HDC,*kcr_DOCINFO),SIGNED,PASCAL,RAW,NAME('StartDocA'),PROC
      StartPage(HDC hdc),LONG,PASCAL,PROC
      SetBkColor(HDC hdc, LONG crColor),BOOL,PASCAL,PROC
      SetTextColor(HDC hdc, LONG crColor),LONG,PASCAL,PROC    
      SetTextAlign(HDC hdc, UNSIGNED uMode),UNSIGNED,PASCAL,PROC
      ExtTextOut(HDC hdc, LONG X, LONG Y, long fuOptions, *Sci_Rectangle rc, <*cstring lpString>, long cbCount, long lpDx),BOOL,RAW,PASCAL,NAME('ExtTextOutA'),PROC
      MoveToEx(HDC hdc, SIGNED x, SIGNED y, LONG lpPoint),BOOL,RAW,PASCAL,PROC
      LineTo(HDC hdc, SIGNED x, SIGNED y),BOOL,PASCAL,PROC
      EndDoc(HDC),SIGNED,PASCAL,PROC
      EndPage(HDC hdc),LONG,PASCAL,PROC
      
      RegOpenKeyEx(ULONG,*CSTRING,ULONG,ULONG,*ULONG),LONG,RAW,PASCAL,NAME('RegOpenKeyExA'),PROC
      RegQueryValueEx(ULONG,*CSTRING,ULONG,*ULONG,ULONG,*ULONG),LONG,RAW,PASCAL,NAME('RegQueryValueExA'),PROC
      RegCloseKey(ULONG),LONG,RAW,PASCAL,NAME('RegCloseKey'),PROC
      RegEnumKeyEx(ULONG hKey, ULONG dwIndex, *CSTRING szName, *ULONG cName, ULONG reserved=0, LONG lpszClass, LONG lpcchClass, ULONG lpftLastWriteTime=0),LONG,RAW,PASCAL,NAME('RegEnumKeyExA'),PROC
      CreateDC(*CSTRING szDriver,*CSTRING szDevice, LONG=0,LONG=0),LONG,RAW,PASCAL,NAME('CreateDCA')
    END
    MODULE('winspool')
        OpenPrinter(*CSTRING pPrinterName, *HANDLE phPrinter, *kcr_PRINTER_DEFAULTS pDefaults),BOOL,PASCAL,RAW,PROC,NAME('OpenPrinterA')
        GetPrinter(HANDLE hPrinter, LONG Level, LONG pPrinter, LONG cbBuf, *LONG  pcbNeeded),BOOL,PASCAL,RAW,PROC,NAME('GetPrinterA')
        ClosePrinter(HANDLE hPrinter),BOOL,PROC,PASCAL
    END            
  END

_O_RDONLY         EQUATE(00000h) ! open for reading only
_O_BINARY         EQUATE(08000h) ! file mode is binary (untranslated)

!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! Module Level Data - implicitly PRIVATE
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
OrigClientWndProc   UNSIGNED,THREAD,STATIC              ! original subclassed ClientWndProc
FindGroup           LIKE(FindGrp),THREAD,STATIC

ILoader              INTERFACE,COM
Release                 PROCEDURE(),LONG,VIRTUAL
AddData                 PROCEDURE(LONG lpData, LONG nLength),LONG,VIRTUAL
ConvertToDocument       PROCEDURE(),LONG,VIRTUAL
                     END
thisLoaderClass      &ILoader
lpDocument           LONG                     

!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! CSciViewer methods
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

! =======================================================================================
! CSciViewer.Init
! purpose:  The Init method initializes the CSciControl object and returns a value
!           indicating whether it successfully initialised and is ready to proceed.
! inputs :  *WINDOW W - Window control will appear on
!           LONG feq - feq of clarion control to use for positioning and events
!           UNSIGNED id - ID used to identify control
! returns:  BYTE    Level:Benign if control is initialised
!                   Level:Notify if initialisation failed
! =======================================================================================
CSciViewer.Init PROCEDURE(*WINDOW W, LONG feq, UNSIGNED id, BOOL Themed = 0) !,BYTE,VIRTUAL   ! Initialise the class

ReturnValue BYTE,AUTO
i           LONG

  CODE                                      ! Enter Procedure
  SELF.ErrorStatus &= NEW ErrorStatusClass  !2004.05.15 RR - 6.1
  SELF.ErrorMgr &= NEW ErrorClass
  SELF.ErrorMgr.Init(SELF.ErrorStatus)      !2004.05.15 RR - 6.1
  SELF.ErrorMgr.AddErrors(ClassErrors)
  
  ReturnValue = PARENT.Init(W, feq, id, Themed)

  SELF.Popup &= NEW PopupClass
  SELF.Popup.Init
  SELF.Popup.AddMenu('&Copy|&Find|&Print|-|&Move{{&Top|Page &Up|Page &Down|Bottom}|-|&Goto|-|Toggle All Folds|Fold Margin')
  SELF.Popup.AddItemEvent('Top',EVENT:ScrollTop,feq)
  SELF.Popup.AddItemEvent('PageUp',EVENT:PageUp,feq)
  SELF.Popup.AddItemEvent('PageDown',EVENT:PageDown,feq)
  SELF.Popup.AddItemEvent('Bottom',EVENT:ScrollBottom,feq)
  SELF.Popup.SetItemCheck('FoldMargin',SELF.bFoldMargin)
  SELF.Popup.SetItemEnable('ToggleAllFolds',SELF.bFoldMargin)

  DO InitializeViewerStyles

  SELF.ClarionKeywords         &= rgClarionKeywords
  SELF.CompilerDirectives      &= rgCompilerDirectives
  SELF.RuntimeExpressions      &= rgRuntimeExpressions          !2004.12.18 KCR
  SELF.BuiltinProcsFuncs       &= rgBuiltinProcsFuncs
  SELF.StructDataTypes         &= rgStructDataTypes
  SELF.Attributes              &= rgAttributes
  SELF.StandardEquates         &= rgStandardEquates
  SELF.ReservedWordsLabels     &= rgReservedWordsLabels         !2003.10.25 KCR
  SELF.ReservedWordsProcLabels &= rgReservedWordsProcLabels     !2003.10.25 KCR
  
  SELF.Typeface = 'MS Sans Serif'

  FindGroup.What = ''
  FindGroup.Direction = 'Down'
  FindGroup.MatchCase = FALSE
  FindGroup.WholeWord = FALSE
  FindGroup.WordStart = FALSE
  FindGroup.RegExp    = FALSE
  FindGroup.POSIX     = FALSE
  
  
  RETURN(ReturnValue)                       ! Exit Procedure


InitializeViewerStyles   ROUTINE
  DATA
I           LONG,AUTO
J           LONG,AUTO
K           LONG,AUTO
ViewerFore  ULONG,AUTO
ViewerBack  ULONG,AUTO
ViewerHot   LONG,AUTO

  CODE
  LOOP K = 1 TO SCE_CLW_LAST
     EXECUTE K
        BEGIN
           ViewerFore = COLOR:BLACK;  ViewerBack = COLOR:WHITE; ViewerHot = 0  !default
        END
        BEGIN
           ViewerFore = COLOR:RED;    ViewerBack = COLOR:WHITE; ViewerHot = 0  !label
        END
        BEGIN
           ViewerFore = COLOR:MAROON; ViewerBack = COLOR:WHITE; ViewerHot = 0  !comment
        END
        BEGIN
           ViewerFore = COLOR:GRAY;   ViewerBack = COLOR:WHITE; ViewerHot = 0  !string
        END
        BEGIN
           ViewerFore = COLOR:BLACK;  ViewerBack = COLOR:WHITE; ViewerHot = 0  !identifier
        END
        BEGIN
           ViewerFore = COLOR:BLACK;  ViewerBack = COLOR:WHITE; ViewerHot = 0  !integer constant
        END
        BEGIN
           ViewerFore = COLOR:BLACK;  ViewerBack = COLOR:WHITE; ViewerHot = 0  !real constant
        END
        BEGIN
           ViewerFore = COLOR:BLACK;  ViewerBack = COLOR:WHITE; ViewerHot = 0  !picture string
        END
        BEGIN
           ViewerFore = COLOR:NAVY;   ViewerBack = COLOR:WHITE; ViewerHot = 1  !keyword
        END
        BEGIN
           ViewerFore = COLOR:BLUE;   ViewerBack = COLOR:WHITE; ViewerHot = 0  !directive
        END
        BEGIN
           ViewerFore = COLOR:BLACK;  ViewerBack = COLOR:WHITE; ViewerHot = 0  !runtime expression
        END
        BEGIN
           ViewerFore = COLOR:NAVY;   ViewerBack = COLOR:WHITE; ViewerHot = 1  !builtin procedure
        END
        BEGIN
           ViewerFore = COLOR:NAVY;   ViewerBack = COLOR:WHITE; ViewerHot = 1  !structure
        END
        BEGIN
           ViewerFore = COLOR:NAVY;   ViewerBack = COLOR:WHITE; ViewerHot = 1  !attribute
        END
        BEGIN
           ViewerFore = COLOR:GREEN;  ViewerBack = COLOR:WHITE; ViewerHot = 1  !equate
        END
        BEGIN
           ViewerFore = COLOR:WHITE;  ViewerBack = COLOR:RED;   ViewerHot = 0  !error
        END
        BEGIN
           ViewerFore = COLOR:TEAL;   ViewerBack = COLOR:WHITE; ViewerHot = 0  !depricated
        END
     END
     SELF.Style.StyleGroup[K].Font = 'Courier New'
     SELF.Style.StyleGroup[K].FontSize = 10
     SELF.Style.StyleGroup[K].FontStyle = 700
     SELF.Style.StyleGroup[K].Bold = TRUE
     SELF.Style.StyleGroup[K].Italic = FALSE
     SELF.Style.StyleGroup[K].Underline = FALSE
     SELF.Style.StyleGroup[K].Fore = ViewerFore
     SELF.Style.StyleGroup[K].Back = ViewerBack
     SELF.Style.StyleGroup[K].EolFilled = CHOOSE(K=1,TRUE,FALSE)
     SELF.Style.StyleGroup[K].CaseOpt = 0
     SELF.Style.StyleGroup[K].Visible = TRUE
     SELF.Style.StyleGroup[K].HotSpot = ViewerHot
  END
  EXIT


! =======================================================================================
! CSciViewer.OpenFile
! purpose:  Open a file and load contents into Scintilla Control buffer
! inputs :  *CSTRING szFilename - Name of file to open
! outputs:  Scintilla Control is loaded with file contents
! returns:  BYTE
!           Level:Benign to indicate success
!           Level:Notify to indicate failure
! =======================================================================================
CSciViewer.OpenFile PROCEDURE(*CSTRING szFilename) !,BOOL,PROC,VIRTUAL

ReturnValue         BOOL,AUTO

lFileSize           LONG(0)

szExtension         CSTRING(33)
szMsgText           CSTRING(240)
szMsg_NoCanDo       CSTRING('Not possible.')

szFileExtension     CSTRING(33)

szAAFileName        CSTRING(256),STATIC
A_A                 FILE,DRIVER('DOS'),NAME(szAAFileName),PRE(AA),CREATE
Record                RECORD
Bytes                    STRING(65535)
                      END
                    END
fh                  SHORT
fm                  LONG
ppBuf               LONG 
pBuf                LONG
cch                 LONG                
shortName           CSTRING(260)    
lFileSizeHigh       LONG
!clock1start         LONG
!clock1end           LONG
!clock2start         LONG
!clock2end           LONG
FILE_MAP_READ       EQUATE(00004h)

  CODE                                      ! Enter Procedure
  
  szAAFileName = szFileName
  SELF.szFileName &= szAAFileName

  !method 1
  !clock1start = clock()
  fh = CreateFile(SELF.szFileName, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
  IF fh <> -1
     lFileSize = GetFileSize(fh, lFileSizeHigh)
     IF lFileSize <> -1
        IF ~SELF.szTextBuffer &= NULL                          ! If we already have a buffer allocated
           DISPOSE(SELF.szTextBuffer)                          !   dispose buffer
           SELF.szTextBuffer &= NULL                           !   clear reference
        END
        SELF.szTextBuffer &= NEW(CSTRING(lFileSize+1))         ! Create a buffer to hold the file
?       ASSERT(~SELF.szTextBuffer &= NULL)
        IF ~SELF.szTextBuffer &= NULL
           fm = CreateFileMapping(fh, 0, PAGE_READONLY, lFileSizeHigh, lFileSize, 0)
           IF fm 
              pBuf = MapViewOfFile(fm, FILE_MAP_READ,0,0,lFileSize)
              MoveMemory(ADDRESS(SELF.szTextBuffer), pBuf, lFileSize)
              UnmapViewOfFile(pBuf)
              CloseHandle(fm)
           END 
        END   
        SELF.szTextBuffer[lFileSize + 1] = '<0>'
       
        SELF.SetBuffer()
        ReturnValue = TRUE
     END
     CloseHandle(fh)
  ELSE
     ReturnValue = FALSE
  END  
  !clock1end = CLOCK()
  RETURN ReturnValue

  OMIT('__OLDCODE__')  
  !method2
  clock2start = CLOCK()
  OPEN(A_A,ReadOnly+DenyWrite)
  IF ~ERRORCODE()
     lFileSize = BYTES(A_A)
     CLOSE(A_A)
 
     IF ~SELF.szTextBuffer &= NULL                          ! If we already have a buffer allocated
        DISPOSE(SELF.szTextBuffer)                          !   dispose buffer
        SELF.szTextBuffer &= NULL                           !   clear reference
     END
     SELF.szTextBuffer &= NEW(CSTRING(lFileSize+1))         ! Create a buffer to hold the file

?    ASSERT(~SELF.szTextBuffer &= NULL)
     IF ~SELF.szTextBuffer &= NULL
        DO LoadFile
        SELF.SetBuffer()
        !!may need to add to classviewer
        !!SELF.SetClarionLexer()
        ReturnValue = TRUE
     ELSE
?       SELF.ErrorMgr.ThrowMessage(CSciViewerMsg:BufferAllocationError, lFileSize+1)
        ReturnValue = FALSE
     END
  ELSE
?    SELF.ErrorMgr.ThrowFile(CSciViewerMsg:OpenFailed, CLIP(szAAFileName))
     ReturnValue = FALSE
  END
  !clock2end = CLOCK()
  
  RETURN(ReturnValue)                       ! Exit Procedure

! Procedure Routines
!-------------------------------------------
LoadFile    ROUTINE
!-------------------------------------------
  DATA

lRecSize            LONG(0)                 ! Note new variables to keep track of bytes read
lBytesRead          LONG(0)                 ! from file and Bytes written to buffer.
lBytes2Write        LONG(0)
lBytePtr            LONG(0)
cc                  LONG

  CODE                                      ! Enter Routine
  
  OPEN(A_A,ReadOnly+DenyWrite)
  IF ~ERRORCODE()
     lBytesRead = 0                                         ! We haven't read any bytes yet
     lFileSize  = BYTES(A_A)
     lRecSize   = SIZE(AA:Bytes)


     SETCURSOR(CURSOR:Wait)

     SET(A_A)
     LOOP
       NEXT(A_A)                                                        ! Get the next record sized chunk
       IF ERRORCODE()
          BREAK
       ELSE
          IF (lBytesRead + lrecSize >= lFileSize)                       ! If we have read up to or past the file size
            lBytes2Write = lFileSize - lBytesRead                       ! Bytes to write to blob is the last "partial" chunk
          ELSE
            lBytes2Write = lRecSize                                     ! Else the Byte to write is the full record
          END

          LOOP lBytePtr =1 TO lBytes2Write                              ! Loop through the record
            SELF.szTextBuffer[(lBytesRead + lBytePtr)] = A_A:Bytes[lBytePtr] ! Storing the bytes
          END

          lBytesRead = lBytesRead + lBytes2Write                        ! Increment the byte read
          IF (lBytesRead >= lFileSize)                                  ! Break if we are up to the file size
             BREAK
          END
       END
     END
     CLOSE(A_A)
     SELF.szTextBuffer[lBytesRead + 1] = '<0>'                          ! *** IMPORTANT ***
    ! cc = thisLoaderClass.AddData(ADDRESS(SELF.szTextBuffer),lBytesRead)
     

     SETCURSOR()
  END

  EXIT                                      !Exit Routine
  !__OLDCODE__

! =======================================================================================
! CSciViewer.ClearBuffer
! purpose:  Clear class text buffer and Scintilla Control buffer
! inputs :  
! outputs:  Class text buffer and Scintilla Control buffer are cleared
! returns:  
! =======================================================================================
CSciViewer.ClearBuffer PROCEDURE() !,VIRTUAL

lBytes  LONG

  CODE                                      ! Enter Procedure

  CLEAR(SELF.szTextBuffer)
  SELF.ClearAll()
  lBytes = SELF.GetText(SIZE(SELF.szTextBuffer), SELF.szTextBuffer)


  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetBuffer
! purpose:  Set the Scintilla Control buffer
! inputs :  
! outputs:  the Scintilla Control buffer is filled
! returns:  
! =======================================================================================
CSciViewer.SetBuffer   PROCEDURE() !,VIRTUAL
szEmpty  CSTRING('')

  CODE                                      ! Enter Procedure

  SELF.SetUndoCollection(FALSE)
  SELF.EmptyUndoBuffer()

  SELF.SetText(SELF.szTextBuffer)

  SELF.SetUndoCollection(TRUE)
  SELF.SetSavePoint()

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetLexer
! purpose:  Set the Scintilla Control lexer
! inputs :  *CSTRING szFileType - the file extension or type
! outputs:  The Scintilla Control lexer is set
! returns:  
! =======================================================================================
CSciViewer.SetLexerType    PROCEDURE(STRING szFileType) !,VIRTUAL

  CODE                                      ! Enter Procedure

  CASE (UPPER(CLIP(szFileType)))
  OF    'CLW'
  OROF  'CLN'
  OROF  'EQU'
  OROF  'INC'
  OROF  'INT'
  OROF  'PRJ'
  OROF  'TPL'
  OROF  'TPW'
  OROF  'TRN'
  OROF  'TXA'
  OROF  'TXR'
  OROF  'TXR'
    SELF.SetClarionLexer()                  ! Clarion (clw inc tpl tpw prj txd txa txr)
  ELSE
    SELF.SetTextLexer()                     ! Simple Text (txt asc csv)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetTextLexer
! purpose:  Set the Scintilla Control Text lexer
! inputs :  
! outputs:  The Scintilla Control Text lexer is set
! returns:  
! =======================================================================================
CSciViewer.SetTextLexer    PROCEDURE !,VIRTUAL

  CODE                                      ! Enter Procedure
  SELF.SetLexer(SCLEX_NULL)
  SELF.ClearDocumentStyle()  
  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetClarionLexer
! purpose:  Set the Scintilla Control Text lexer
! inputs :  
! outputs:  The Scintilla Control Text lexer is set
! returns:  
! =======================================================================================
CSciViewer.SetClarionLexer     PROCEDURE !,VIRTUAL

  CODE                                      ! Enter Procedure
  
  SELF.SetLexer(SCLEX_CLWNOCASE)
  SELF.SetKeywords(0, SELF.ClarionKeywords)
  SELF.SetKeywords(1, SELF.CompilerDirectives)
  SELF.SetKeywords(2, SELF.RuntimeExpressions)          !2004.12.18 KCR
  SELF.SetKeywords(3, SELF.BuiltinProcsFuncs)           !2004.12.18 KCR
  SELF.SetKeywords(4, SELF.StructDataTypes)             !2004.12.18 KCR
  SELF.SetKeywords(5, SELF.Attributes)                  !2004.12.18 KCR
  SELF.SetKeywords(6, SELF.StandardEquates)             !2004.12.18 KCR
  SELF.SetKeywords(7, SELF.ReservedWordsLabels)         !2004.12.18 KCR
  SELF.SetKeywords(8, SELF.ReservedWordsProcLabels)     !2004.12.18 KCR

  SELF.SetColors(SELF.Style)

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetDefaultMonoFont
! purpose:  Set the Scintilla Control default font to mono
! inputs :  
! outputs:  The Scintilla Control default font is set to mono
! returns:  
! =======================================================================================
CSciViewer.SetDefaultMonoFont  PROCEDURE !,VIRTUAL

lMonoPoint      LONG(10)                    ! Point size for mono font
szMonoFont      CSTRING('Courier New')      ! Face name for mono font

  CODE                                      ! Enter Procedure

  SELF.SetDefaultFont(szMonoFont, lMonoPoint)

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetDefaultFont
! purpose:  Set the Scintilla Control default font
! inputs :  
! outputs:  The Scintilla Control default font is set
! returns:  
! =======================================================================================
CSciViewer.SetDefaultFont  PROCEDURE(*CSTRING szFontName, LONG lFontPoint) !,VIRTUAL

  CODE                                      ! Enter Procedure
  !-----------------------------------------------------------------!
  ! Font Setup - Notice that this is the default for all styles.    !
  !-----------------------------------------------------------------!
  SELF.StyleSetFont(STYLE_DEFAULT, szFontName)
  SELF.StyleSetSize(STYLE_DEFAULT, lFontPoint)
  SELF.StyleSetFore(STYLE_DEFAULT, COLOR:Black)
  SELF.StyleSetBack(STYLE_DEFAULT, COLOR:White)
  SELF.StyleClearAll()

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetDefaults
! purpose:  Set the Scintilla Control defaults
! inputs :  
! outputs:  The Scintilla Control defaults are set
! returns:  
! =======================================================================================
CSciViewer.SetDefaults  PROCEDURE() !,VIRTUAL

szNumberWidth   CSTRING('_99999')
lNumberWidth    LONG(0)
lMonoPoint      LONG(10)                    ! Point size for mono font
szMonoFont      CSTRING('Courier New')      ! Face name for mono font

  CODE                                      ! Enter Procedure

  SELF.SetDefaultMonoFont()

  !-----------------------------------------------------------------!
  ! Font Setup -                                                    !
  !-----------------------------------------------------------------!
  SELF.SetColors(SELF.Style)

  !-------------------------------------------------------------!
  ! Tab, Indent Setup - This is global. Style does not matter.  !
  !-------------------------------------------------------------!
  SELF.SetStyleBits(7)
  SELF.SetTabWidth(SELF.lTabWidth)
  SELF.SetUseTabs(SELF.bUseTabs)
  SELF.SetTabIndents(SELF.bTabIndents)
  SELF.SetBackspaceUnindents(SELF.bBackSpaceUnindents)
  SELF.SetMarginLeft(4)
  SELF.SetMarginRight(4)

  SELF.SetProperty(szFoldProp,szPropVal1)               !2004.12.18 KCR
  SELF.SetProperty(szFoldCompactProp,szPropVal0)        !2004.12.18 KCR

  !-----------------------------------------------------------------------------!
  ! Whitespace Visibility Setup - Really usefull if you want check out stuff.   !
  !-----------------------------------------------------------------------------!
  SELF.SetViewWS(SELF.lWsMode)

  !-------------------------------------------------!
  ! Set up for the three (0, 1, 2) defined margins.    !
  !-------------------------------------------------!
  SELF.StyleSetFont(STYLE_LINENUMBER, szMonoFont)
  SELF.StyleSetSize(STYLE_LINENUMBER, lMonoPoint)

  !---------------------------------------------!
  ! Margin 0 is the one used for line numbers.  !
  !---------------------------------------------!
  SELF.SetMarginTypeN(0, SC_MARGIN_NUMBER)
  lNumberWidth = SELF.TextWidth(STYLE_LINENUMBER, szNumberWidth)
  SELF.SetMarginWidthN(0, lNumberWidth)
  SELF.StyleSetVisible(STYLE_LINENUMBER, TRUE)

  !-------------------------------------------------!
  ! Margin 1 is used for bookmarks                  !
  !-------------------------------------------------!
  SELF.MarkerSetFore(markerBookmark,07F0000h)
  SELF.MarkerSetBack(markerBookmark,0FFFF80h)
!  SELF.MarkerSetAlpha(markerBookmark,SC_ALPHA_NOALPHA)
!  SELF.MarkerDefine(markerBookmark,SC_MARK_CIRCLE)
  SELF.MarkerDefinePixmap(markerBookmark,GetXPMData(1))
  SELF.RegisterImage(1,GetXPMData(1))

  !-------------------------------------------------!
  ! Margin 2 is used for folding symbols.           !
  !-------------------------------------------------!
  SELF.SetMarginTypeN(MARGIN_SCRIPT_FOLD_INDEX, SC_MARGIN_SYMBOL)
  SELF.SetMarginMaskN(MARGIN_SCRIPT_FOLD_INDEX, SC_MASK_FOLDERS)
  SELF.SetMarginSensitiveN(MARGIN_SCRIPT_FOLD_INDEX,1)
  SELF.SetMarginWidthN(MARGIN_SCRIPT_FOLD_INDEX, 16)
  
  !following are for debugging folding
  !SELF.SetMarginWidthN(0, lNumberWidth+8)
  !SELF.SetFoldFlags(64) !16

  !-------------------------------------------------!
  ! Set some visual preferences.                    !
  !-------------------------------------------------!
  SELF.MarkerDefine(SC_MARKNUM_FOLDER, SC_MARK_BOXPLUS)
  SELF.MarkerDefine(SC_MARKNUM_FOLDEROPEN, SC_MARK_BOXMINUS)
  SELF.MarkerDefine(SC_MARKNUM_FOLDEREND, SC_MARK_BOXPLUSCONNECTED)
  SELF.MarkerDefine(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_TCORNER)
  SELF.MarkerDefine(SC_MARKNUM_FOLDEROPENMID, SC_MARK_BOXMINUSCONNECTED)
  SELF.MarkerDefine(SC_MARKNUM_FOLDERSUB, SC_MARK_VLINE)
  SELF.MarkerDefine(SC_MARKNUM_FOLDERTAIL, SC_MARK_LCORNER)

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.AskGoToLine
! purpose:  Ask User what line to go to
! inputs :  
! outputs:  The given line is scrolled to the top of the window
! returns:  
! =======================================================================================
CSciViewer.AskGoToLine  PROCEDURE() !,VIRTUAL

LineNo  LONG,STATIC
OKGo    BYTE(False)
I       LONG,AUTO
J       LONG,AUTO

GotoDialog WINDOW('Go To Line Number'),AT(,,105,38),CENTER,GRAY,FONT('Segoe UI',10,,FONT:regular), |
         DOUBLE,SYSTEM
      SPIN(@n_5),AT(40,5,50,10),USE(LineNo),RANGE(1,99999)
      PROMPT('&Line No:'),AT(5,5,32,10),USE(?Prompt1),TRN
      BUTTON('&Go'),AT(5,20,45,14),USE(?GoButton),DEFAULT,TIP('Go to selected Line')
      BUTTON('&Cancel'),AT(55,20,45,14),USE(?CancelButton),TIP('Cancel GoTo operation')
   END

  CODE
  OPEN(GotoDialog)
  J = LASTFIELD()
  LOOP I = FIRSTFIELD() TO J
    I{PROP:FontName} = SELF.Typeface
  END
  !IF ~SELF.Translator&=NULL THEN SELF.Translator.TranslateWindow.
  ACCEPT
    CASE EVENT()
    OF EVENT:Accepted
      CASE ACCEPTED()
      OF ?GoButton
        OKGo=True
      OROF ?CancelButton
        POST(EVENT:CloseWindow)
      END
    END
  END
  CLOSE(GotoDialog)
  IF OKGo THEN SELF.GoToLine(LineNo-1).
  
  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.GoToLine
! purpose:  enforce visibility in case folded
! inputs :  
! outputs:  
! returns:  
! =======================================================================================
CSciViewer.GoToLine  PROCEDURE(LONG lLine) !,VIRTUAL

  CODE
  
  IF SELF.bInitialised
     PARENT.EnsureVisibleEnforcePolicy(lLine)
     PARENT.GoToLine(lLine)
     !select the line here
     SELF.SetSelectionStart(SELF.PositionFromLine(lLine))
     SELF.SetSelectionEnd(SELF.GetLineEndPosition(lLine))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.TakeContextMenu
! purpose:  Handle the Context Menu Event
! inputs :
! outputs:
! returns:  BYTE
!           Level:Benign to indicate processing of this event should continue normally
!           Level:Notify to indicate processing is completed for this event
!                                and the ACCEPT loop should CYCLE
!           Level:Fatal to indicate the event could not be processed
!                                and the ACCEPT loop should BREAK
! =======================================================================================
CSciViewer.TakeContextMenu PROCEDURE !,BYTE,VIRTUAL

ReturnValue BYTE,AUTO
CurrentLine LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     CASE SELF.Popup.Ask()
     OF 'Copy'
        SELF.Copy()
     OF 'Find'
        SELF.SearchAsk()
     OF 'Print'
        SELF.PrintAsk()
     OF 'Goto'
        SELF.AskGotoLine()
     OF 'ToggleAllFolds'
        SELF.FoldAll()
     OF 'FoldMargin'
        SELF.FoldMargin()
     END
  ELSE
     ReturnValue = Level:Fatal
  END

  RETURN(ReturnValue)                       ! Exit Procedure

! =======================================================================================
! CSciViewer.SearchAsk
! purpose:  Ask User what to look for.  Provides wrap around option if file limit reached
! inputs :  
! outputs:  The given line is scrolled to the top of the window
! returns:  
! =======================================================================================
CSciViewer.SearchAsk PROCEDURE()
thisHH   &tagHTMLHelp
   CODE
      SELF.SearchAsk(TRUE,thisHH)

! =======================================================================================
! CSciViewer.SearchAsk
! purpose:  Ask User what to look for.  Provides wrap around option if file limit reached
! inputs :  
! outputs:  The given line is scrolled to the top of the window
! returns:  
! =======================================================================================
CSciViewer.SearchAsk PROCEDURE(BOOL bShowWindow)
thisHH   &tagHTMLHelp
   CODE
      SELF.SearchAsk(bShowWindow,thisHH)
! =======================================================================================
! CSciViewer.SearchAsk
! purpose:  Ask User what to look for.  Provides wrap around option if file limit reached
! inputs :  
! outputs:  The given line is scrolled to the top of the window
! returns:  
! =======================================================================================
CSciViewer.SearchAsk PROCEDURE(BOOL bShowWindow, tagHTMLHelp HTMLHelp)
! Static variables so that they persist
!======================================
WinInit             BYTE(FALSE),STATIC
WinXPos             SIGNED,AUTO,STATIC
WinYPos             SIGNED,AUTO,STATIC

OmitWindow          BYTE(FALSE)
Quit                BYTE(FALSE)
lFoundPosition      LONG,AUTO
lFoundNext          LONG,AUTO
I                   LONG,AUTO
J                   LONG,AUTO
buttonPressed       BOOL(FALSE)
markerHandle        LONG
currentPosition     LONG
SearchPerformed     BOOL(FALSE)

FindOptions WINDOW('Find'),AT(,,285,75),CENTER,GRAY,IMM,FONT('Segoe UI',10,COLOR:Black,FONT:regular,CHARSET:ANSI),HLP('Find.htm'),ALRT(F12Key),DOUBLE,SYSTEM
      PROMPT('Find What'),AT(5,5,40),USE(?Prompt1)
      ENTRY(@s64),AT(55,5,135,10),USE(FindGroup.What,,?FindGroup:What),IMM, |
            TIP('Enter the text to Find')
      CHECK(' &Case Sensitive'),AT(5,20,92,10),USE(FindGroup.MatchCase,,?FindGroup:MatchCase),TIP('A match' & |
            ' only occurs with text that matches the case of the search string.')
      CHECK(' W&hole Word Only'),AT(5,30,92,10),USE(FindGroup.WholeWord,,?FindGroup:WholeWord),TIP('A match' & |
            ' only occurs if the characters before and after are not word characters.')
      CHECK(' Word &Start'),AT(5,40,92,10),USE(FindGroup.WordStart,,?FindGroup:WordStart),TIP('A match' & |
            ' only occurs if the character before is not a word character.')
      CHECK(' &Regular Expression'),AT(5,50,92,10),USE(FindGroup.RegExp,,?FindGroup:RegExp), |
            TIP('The search string should be interpreted as a regular expression.')
      CHECK(' &POSIX compatible'),AT(100,50,92,10),USE(FindGroup.POSIX,,?FindGroup:POSIX), |
            TIP('Treat regular expression in a more POSIX compatible manner<13>' & |
            '<10>by interpreting bare ( and ) for tagged sections rather than \(' & |
            ' and \).')
      CHECK(' &Word Wrap'),AT(5,60,92,10),USE(FindGroup.bWordWrap,,?FindGroup:bWordWrap)
      OPTION('Direction'),AT(100,20,92,25),USE(FindGroup.Direction,,?FindGroup:Direction),BOXED
         RADIO('&Up'),AT(114,31),USE(?Radio1),TIP('Search towards the start of t' & |
               'he file'),VALUE('Up')
         RADIO('&Down'),AT(150,31),USE(?Radio2),TIP('Search towards the end of t' & |
               'he file'),VALUE('Down')
      END
      BUTTON('Find &Next'),AT(196,4,84,14),USE(?NextButton),DEFAULT,TIP('Find the nex' & |
            't occurrance of the selected text')
      BUTTON('&Bookmark All'),AT(196,20,84,14),USE(?cmdBookmarkAll),TIP('Find and Bookmark al' & |
            'l occurrances of the selected text')
      BUTTON('&Cancel'),AT(196,36,84,14),USE(?CancelButton),TIP('Cancel Find mod' & |
            'e and close the Find window'),HIDE
   END

  CODE                                      ! Enter Procedure
  IF SELF.bInitialised
     lFoundPosition = SELF.GetCurrentPos()
 
     OmitWindow = 1 - bShowWindow
     
     LOOP
       IF ~OmitWindow
          OPEN(FindOptions)
          SELF.FindOptionsWindow &= FindOptions
          IF WinInit
             SETPOSITION(0,WinXPos,WinYPos)
          ELSE
             GETPOSITION(0,WinXPos,WinYPos)
             WinInit=True
          END

          J = LASTFIELD()
          LOOP I = FIRSTFIELD() TO J
            I{PROP:FontName} = SELF.Typeface
          END

          !IF ~SELF.Translator&=NULL THEN SELF.Translator.TranslateWindow.

          ACCEPT
            CASE KEYCODE()
            OF EscKey
               Quit = TRUE
               BREAK
            END

            CASE EVENT()
              OF EVENT:AlertKey
                 CASE KEYCODE()
                   OF F12Key
                      IF NOT HTMLHelp &= NULL
                         HTMLHelp.ShowTopic('Find.htm')
                      END   
                 END

            OF EVENT:CloseDown
               POST(EVENT:CloseWindow)
               
            OF EVENT:CloseWindow
               SELF.FindWindowTakeCloseWindow()
               IF buttonPressed = FALSE
                  Quit = TRUE
               !ELSE
               !   buttonPressed = FALSE
               END
               
            OF EVENT:OpenWindow
               SELF.FindWindowTakeOpenWindow()
               IF FindGroup.What = ''
                  SELF.GetSelText(FindGroup.What)
               ELSE
                  !lFoundPosition = SELF.GetCurrentPos()
                  SearchPerformed = FALSE
               END   
               lFoundNext = INVALID_POSITION
               DO UpdateWindow
               SELECT(?FindGroup:What)

            OF EVENT:Accepted
               CASE FIELD()
               OF ?FindGroup:MatchCase
                  DO UpdateWindow
               OF ?FindGroup:WholeWord
                  DO UpdateWindow
               OF ?FindGroup:WordStart
                  DO UpdateWindow
               OF ?FindGroup:RegExp
                  DO UpdateWindow
               OF ?FindGroup:POSIX
                  DO UpdateWindow
               
               OF ?NextButton
                  SearchPerformed = TRUE
                  buttonPressed = ?NextButton
                  POST(EVENT:CloseWindow)

               OF ?cmdBookmarkAll
                  SearchPerformed = TRUE
                  buttonPressed = ?cmdBookmarkAll
                  POST(EVENT:CloseWindow)
                  
               OF ?CancelButton
                  Quit = TRUE
                  buttonPressed = ?CancelButton
                  POST(EVENT:CloseWindow)
               END

            OF EVENT:Moved
               GETPOSITION(0,WinXPos,WinYPos)
               
            OF EVENT:NewSelection
               CASE FIELD()
                 OF ?FindGroup:What
                    DO UpdateWindow
               END     
            END

            UPDATE(?FindGroup:What)
            ?NextButton{PROP:Disable} = CHOOSE(FindGroup.What = '')
            ?cmdBookmarkAll{PROP:Disable} = CHOOSE(FindGroup.What = '')
            

          END   !ACCEPT

          CLOSE(FindOptions)
       END ! IF ~OmitWindow

       OmitWindow=FALSE

       IF Quit
          IF SearchPerformed = FALSE
             SELF.GotoPos(SELF.GetSelectionStart())
          END   
          BREAK
       ELSE
          CASE buttonPressed
            OF ?NextButton
               IF FindGroup.Direction = 'Down'
                  SELF.GoToPos(lFoundPosition + (LEN(FindGroup.What) + 1))
               ELSE
                  SELF.GoToPos(lFoundPosition - 1)
               END
               SELF.SetAnchor(SELF.GetCurrentPos())

               lFoundNext = SELF.SearchNext(FindGroup)
               IF (lFoundNext = INVALID_POSITION)
                  IF FindGroup.bWordWrap = TRUE
                     IF FindGroup.Direction = 'Down'
                        lFoundPosition = 0
                     ELSE
                        lFoundPosition = SELF.GetLength()
                     END
                     SELF.GoToPos(lFoundPosition)
                     SELF.SetSel(lFoundPosition,lFoundPosition + LEN(FindGroup.What))
                     OmitWindow = TRUE
                  END
               ELSE
                  lFoundPosition = lFoundNext
                  SELF.SetSel(lFoundPosition + LEN(FindGroup.What), lFoundPosition)
                  SELF.GrabFocus()
                  IF bShowWindow = FALSE
                     BREAK
                  END   
               END
               
            OF ?cmdBookmarkAll
               currentPosition = SELF.GetCurrentPos()
               IF FindGroup.Direction = 'Down'
                  lFoundPosition = 0
               ELSE
                  lFoundPosition = SELF.GetLength()
               END
               SELF.GoToPos(lFoundPosition)
               LOOP
                  lFoundNext = SELF.SearchNext(FindGroup)
                  CASE lFoundNext
                    OF INVALID_POSITION
                       BREAK
                    OF -1
                       BREAK
                  ELSE      
                       markerHandle = SELF.MarkerAdd(SELF.LineFromPosition(lFoundNext),1)
                       IF FindGroup.Direction = 'Down'
                          SELF.GotoPos(lFoundNext + LEN(FindGroup.What) + 1)
                       END   
                  END
               END
               SELF.GotoPos(currentPosition)
               SELF.SetAnchor(SELF.GetCurrentPos())
               BREAK
          END 
          buttonPressed = FALSE
       END
     END
     SELF.FindOptionsWindow &= NULL
  END

  RETURN                                    ! Exit Procedure
  
UpdateWindow   ROUTINE
   UPDATE()
   ?NextButton{PROP:Disable} = CHOOSE(FindGroup.What = '')
   ?cmdBookmarkAll{PROP:Disable} = CHOOSE(FindGroup.What = '')

   IF FindGroup.MatchCase = TRUE |
   OR FindGroup.WholeWord = TRUE |
   OR FindGroup.WordStart = TRUE
      FindGroup.RegExp = FALSE
      FindGroup.POSIX  = FALSE
      DISABLE(?FindGroup:RegExp)
      DISABLE(?FindGroup:POSIX)
   ELSE
      ENABLE(?FindGroup:RegExp)
   END
   IF FindGroup.RegExp = TRUE
      DISABLE(?FindGroup:MatchCase,?FindGroup:WordStart)
      ENABLE(?FindGroup:POSIX)
   ELSE   
      FindGroup.POSIX = FALSE
      DISABLE(?FindGroup:POSIX)
      ENABLE(?FindGroup:MatchCase,?FindGroup:WordStart)
   END
   DISPLAY(?FindGroup:MatchCase,?FindGroup:POSIX)
   EXIT  

! =======================================================================================
! CSciViewer.SearchNext(*FindGrp  Find)
! purpose:  To perform a search based on the passed FindGroup
! inputs :  *FindGrp Find group with flags and search text
! outputs:  The found text is scrolled into the view
! returns:  The starting position where the text was found
! =======================================================================================
CSciViewer.SearchNext PROCEDURE(*FindGrp Find) !LONG,VIRTUAL

ReturnValue LONG,AUTO
SearchFlags LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SearchFlags = 0
     IF Find.MatchCase
        SearchFlags = BOR(SearchFlags,SCFIND_MATCHCASE)
     END
     IF Find.WholeWord
        SearchFlags = BOR(SearchFlags,SCFIND_WHOLEWORD)
     END
     IF Find.WordStart
        SearchFlags = BOR(SearchFlags,SCFIND_WORDSTART)
     END
     IF Find.RegExp
        SearchFlags = BOR(SearchFlags,SCFIND_REGEXP)
     END
     IF Find.POSIX
        SearchFlags = BOR(SearchFlags,SCFIND_POSIX)
     END
     SELF.SearchAnchor()
     IF Find.Direction = 'Down'
        ReturnValue = SELF.SearchNext(SearchFlags, Find.What)
     ELSE
        ReturnValue = SELF.SearchPrev(SearchFlags, Find.What)
     END
  ELSE
     ReturnValue = INVALID_POSITION
  END

  RETURN(ReturnValue)                       ! Exit Procedure

! =======================================================================================
! CSciViewer.PrintAsk
! purpose:  Print the document
! inputs :  
! outputs:  The document is printed
! returns:  
! RAS.2003.05.22 (File Print) - Created procedure and tested
! KCR.2003.07.15 - modified for CSciViewer
! =======================================================================================
CSciViewer.PrintAsk PROCEDURE()

LOC:RunDate             LONG
LOC:RunTime             LONG
LOC:DetailLine          CSTRING(4096)
LOC:LineCount           LONG
LOC:LineLength          LONG
LOC:LineNumber          LONG
LOC:LineShow            LONG

PrintPreviewQueue   PreviewQueue
Previewer           PrintPreviewClass

SciPrint REPORT('Editor Listing'),AT(500,750,7500,9552),PAPER(PAPER:LETTER),PRE(RPT),FONT('Courier New',10,,,CHARSET:ANSI), |
         THOUS
       HEADER,AT(500,521,7500,167),USE(?Header),FONT('Courier New',10,,FONT:regular,CHARSET:ANSI)
         STRING(@s60),AT(0,0,6490,208),USE(SELF.szFileName),TRN
         STRING(@pPage: <<<#p),AT(5469,0,2031,208),PAGENO,USE(?String2),TRN,RIGHT
         LINE,AT(0,167,7500,0),USE(?Line1),COLOR(COLOR:Black)
       END
DataDetail DETAIL,AT(,,7500,135),USE(?DataDetail),FONT('Courier New',8,,FONT:regular,CHARSET:ANSI)
         STRING(@n_5),AT(0,0,,150),USE(LOC:LineShow),TRN
         TEXT,AT(417,0,7083,150),USE(LOC:DetailLine),TRN,RESIZE
       END
       FOOTER,AT(500,10302,7500,198),USE(?Footer),FONT('Courier New',10,,FONT:regular,CHARSET:ANSI)
         LINE,AT(0,10,7500,0),USE(?Line2),COLOR(COLOR:Black)
         STRING('Run:'),AT(5469,42),USE(?String9),TRN,LEFT
         STRING(@D010-B),AT(5885,42),USE(LOC:RunDate),TRN
         STRING(@T04B),AT(6823,42),USE(LOC:RunTime),TRN
       END
       FORM,AT(500,490,7500,10500),USE(?Form)
       END
     END

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     LOC:RunDate           = TODAY()
     LOC:RunTime           = CLOCK()
     LOC:LineNumber        = 0
     LOC:LineCount         = SELF.GetLineCount()

     Previewer.Init(PrintPreviewQueue)
     Previewer.SetZoomPercentile(100)
     Previewer.AllowUserZoom = TRUE
     Previewer.Maximize = TRUE

     SETCURSOR(CURSOR:Wait)
     OPEN(SciPrint)
     SciPrint{PROP:Preview} = PrintPreviewQueue.FileName

     LOOP LOC:LineNumber = 0 TO LOC:LineCount
        LOC:LineShow   = LOC:LineNumber + 1
        LOC:DetailLine = ''
        LOC:LineLength = SELF.LineLength(LOC:LineNumber)                   

        IF (LOC:LineLength < SIZE(LOC:DetailLine)) THEN
          LOC:LineLength = SELF.GetLine(LOC:LineNumber, LOC:DetailLine)    
          IF LOC:LineLength > 1                                            
             LOC:DetailLine[LOC:LineLength-1] = '<0>'                      
          END                                                              
        ELSE
          LOC:DetailLine    = '*** LINE IS TOO BIG ***'
        END

        IF NOT ((LOC:LineNumber = LOC:LineCount) AND (LOC:DetailLine = ''))
           PRINT(RPT:DataDetail)
        END
     END
     ENDPAGE(SciPrint)

     
     SciPrint{PROP:flushpreview} = Previewer.Display()

     CLOSE(SciPrint)
     SETCURSOR()

  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.SetColors
! purpose:  Set the colors used by the control
! inputs :  *COLORGROUPTYPE color
! outputs:  The colors are set as defined in the passed colorgroup
! returns:  
! KCR.2003.07.19 - Initial Version
! =======================================================================================
CSciViewer.SetColors PROCEDURE(*COLORGROUPTYPE color) !,VIRTUAL

styleNumber LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     
     SELF.Style = color                     ! Save as current style

     IF color.StyleGroup[SCE_CLW_DEFAULT+1].Font
        SELF.StyleSetFont(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].Font)
     END
     IF color.StyleGroup[SCE_CLW_DEFAULT+1].FontSize
        SELF.StyleSetSize(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].FontSize)
     END

     SELF.StyleSetFore(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].Fore)
     SELF.StyleSetBack(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].Back)

     SELF.SetHotSpotActiveBack(TRUE, color.StyleGroup[SCE_CLW_DEFAULT+1].Back)

     SELF.StyleSetBold(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].Bold)
     SELF.StyleSetItalic(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].Italic)
     SELF.StyleSetUnderline(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].Underline)

     SELF.StyleSetEolFilled(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].EolFilled)
     SELF.StyleSetCase(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].CaseOpt)
     SELF.StyleSetVisible(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].Visible)
     SELF.StyleSetHotSpot(STYLE_DEFAULT, color.StyleGroup[SCE_CLW_DEFAULT+1].HotSpot)

     SELF.StyleClearAll()

     LOOP styleNumber =  SCE_CLW_DEFAULT+1 TO SCE_CLW_LAST-1
       IF color.StyleGroup[styleNumber+1].Font
          SELF.StyleSetFont(styleNumber, color.StyleGroup[styleNumber+1].Font)
       END
       IF color.StyleGroup[styleNumber+1].FontSize
          SELF.StyleSetSize(styleNumber, color.StyleGroup[styleNumber+1].FontSize)
       END

       SELF.StyleSetFore(styleNumber, color.StyleGroup[styleNumber+1].Fore)
       SELF.StyleSetBack(styleNumber, color.StyleGroup[styleNumber+1].Back)

       SELF.StyleSetBold(styleNumber, color.StyleGroup[styleNumber+1].Bold)
       SELF.StyleSetItalic(styleNumber, color.StyleGroup[styleNumber+1].Italic)
       SELF.StyleSetUnderline(styleNumber, color.StyleGroup[styleNumber+1].Underline)

       SELF.StyleSetEolFilled(styleNumber, color.StyleGroup[styleNumber+1].EolFilled)
       SELF.StyleSetCase(styleNumber, color.StyleGroup[styleNumber+1].CaseOpt)
       SELF.StyleSetVisible(styleNumber, color.StyleGroup[styleNumber+1].Visible)
       SELF.StyleSetHotSpot(styleNumber, color.StyleGroup[styleNumber+1].HotSpot)
     END

     SELF.MarkerSetFore(SC_MARKNUM_FOLDER, COLOR:WHITE)
     SELF.MarkerSetBack(SC_MARKNUM_FOLDER, COLOR:GRAY)
     SELF.MarkerSetFore(SC_MARKNUM_FOLDEROPEN, COLOR:WHITE)
     SELF.MarkerSetBack(SC_MARKNUM_FOLDEROPEN, COLOR:GRAY)
     SELF.MarkerSetFore(SC_MARKNUM_FOLDEREND, COLOR:WHITE)
     SELF.MarkerSetBack(SC_MARKNUM_FOLDEREND, COLOR:GRAY)
     SELF.MarkerSetFore(SC_MARKNUM_FOLDERMIDTAIL, COLOR:WHITE)
     SELF.MarkerSetBack(SC_MARKNUM_FOLDERMIDTAIL, COLOR:GRAY)
     SELF.MarkerSetFore(SC_MARKNUM_FOLDEROPENMID, COLOR:WHITE)
     SELF.MarkerSetBack(SC_MARKNUM_FOLDEROPENMID, COLOR:GRAY)
     SELF.MarkerSetFore(SC_MARKNUM_FOLDERSUB, COLOR:WHITE)
     SELF.MarkerSetBack(SC_MARKNUM_FOLDERSUB, COLOR:GRAY)
     SELF.MarkerSetFore(SC_MARKNUM_FOLDERTAIL, COLOR:WHITE)
     SELF.MarkerSetBack(SC_MARKNUM_FOLDERTAIL, COLOR:GRAY)

     SELF.Colourise(0,-1)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.FoldMargin
! purpose:  Toggle Fold Margin on or off
! inputs :  
! outputs:  
! returns:  
! KCR.2004.12.20 - Initial Version
! =======================================================================================
CSciViewer.FoldMargin PROCEDURE() !,VIRTUAL
lLine       LONG(1)
I           LONG,AUTO
J           LONG,AUTO

  CODE                                      ! Enter Procedure
  IF SELF.bInitialised
     IF SELF.bFoldMargin
        J = SELF.GetLineCount()
        LOOP I = 1 TO J
           SELF.EnsureVisible(I)
        END
        SELF.SetProperty(szFoldProp,szPropVal0)               !2004.12.18 KCR
        SELF.SetMarginWidthN(MARGIN_SCRIPT_FOLD_INDEX, 0)
        SELF.bFoldMargin = FALSE
     ELSE
        SELF.SetProperty(szFoldProp,szPropVal1)               !2004.12.18 KCR
        SELF.SetProperty(szFoldCompactProp,szPropVal0)        !2004.12.18 KCR
        SELF.SetMarginWidthN(MARGIN_SCRIPT_FOLD_INDEX, 20)
        SELF.bFoldMargin = TRUE
     END
     SELF.Popup.SetItemCheck('FoldMargin',SELF.bFoldMargin)
     SELF.Popup.SetItemEnable('ToggleAllFolds',SELF.bFoldMargin)
  END
  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.FoldAll
! purpose:  Expand or Contract all folds
! inputs :  
! outputs:  
! returns:  
! KCR.2004.12.20 - Initial Version
! =======================================================================================
CsciViewer.FoldAll  PROCEDURE()
line            LONG
lineSeek        LONG
maxLine         LONG
expanding       BOOL(TRUE)
level           LONG
lineMaxSubord   LONG
CurrentLine     LONG

  CODE
  CurrentLine = SELF.LineFromPosition(SELF.GetCurrentPos())
  SELF.Colourise(0,-1)
  maxLine = SELF.GetLineCount() - 1
  LOOP lineSeek = 0 TO maxLine
     IF BAND(SELF.GetFoldLevel(lineSeek),SC_FOLDLEVELHEADERFLAG)
        expanding = CHOOSE(SELF.GetFoldExpanded(lineSeek),0,1)
        BREAK
     END
  END

  LOOP line = 0 TO maxLine
     level = SELF.GetFoldLevel(line)
     IF (BAND(level,SC_FOLDLEVELHEADERFLAG) AND (SC_FOLDLEVELBASE = BAND(level,SC_FOLDLEVELNUMBERMASK)))
        IF (expanding)
           SELF.SetFoldExpanded(line, 1)
           SELF.Expand(line, true, false, 0, level)
           line -= 1
        ELSE
           lineMaxSubord = SELF.GetLastChild(line, -1)
           SELF.SetFoldExpanded(line, 0)
           IF (lineMaxSubord > line)
              SELF.HideLines(line + 1, lineMaxSubord)
           END
        END
     END
  END
  SELF.GoToLine(CurrentLine)

! =======================================================================================
! CSciViewer.Expand
! purpose:  Expand or Contract levels
! inputs :  
! outputs:  
! returns:  
! KCR.2004.12.20 - Initial Version
! =======================================================================================
CsciViewer.Expand   PROCEDURE (*LONG line, BOOL doExpand, BOOL force, LONG visLevels, LONG level)
lineMaxSubord   LONG
levelLine       LONG

  CODE
  lineMaxSubord = SELF.GetLastChild(line, BAND(level,SC_FOLDLEVELNUMBERMASK))
  line += 1
  LOOP WHILE (line <= lineMaxSubord)
     IF (force)
        IF (visLevels > 0)
           SELF.ShowLines(line, line)
        ELSE
           SELF.HideLines(line, line)
        END
     ELSE
        IF (doExpand)
           SELF.ShowLines(line, line)
        END
        levelLine = level
        IF (levelLine = -1)
           levelLine = SELF.GetFoldLevel(line)
        END
        IF (BAND(levelLine,SC_FOLDLEVELHEADERFLAG))
           IF (force)
              IF (visLevels > 1)
                 SELF.SetFoldExpanded(line, 1)
              ELSE
                 SELF.SetFoldExpanded(line, 0)
              END
              SELF.Expand(line, doExpand, force, visLevels - 1, -1)
           ELSE
              IF (doExpand)
                 IF (~SELF.GetFoldExpanded(line))
                    SELF.SetFoldExpanded(line, 1)
                 END
                 SELF.Expand(line, true, force, visLevels - 1, -1)
              ELSE
                 SELF.Expand(line, false, force, visLevels - 1, -1)
              END
           END
        ELSE
           line += 1
        END
     END
  END
  RETURN

! =======================================================================================
! CSciViewer.GetFindGroup()
! purpose:  Return FindGroup
! inputs :  
! outputs:  
! returns:  
! =======================================================================================
CSciViewer.GetFindGroup          PROCEDURE() !,STRING,VIRTUAL                    ! 
   CODE
      RETURN FindGroup

! =======================================================================================
! CSciViewer.SetFindGroup()
! purpose:  Set FindGroup
! inputs :  
! outputs:  
! returns:  
! =======================================================================================
CSciViewer.SetFindGroup          PROCEDURE(*FindGrp pFindGroup) !,VIRTUAL        !
   CODE
      FindGroup = pFindGroup
      RETURN

! =======================================================================================
! CSciViewer.GetFindWhat()
! purpose:  Return value in FindGroup.what
! inputs :  
! outputs:  
! returns:  
! =======================================================================================
CSciViewer.GetFindWhat           PROCEDURE() !,STRING,VIRTUAL                    ! 
   CODE
      RETURN FindGroup.What

! =======================================================================================
! CSciViewer.SetFindWhat()
! purpose:  Set the value in FindGroup.what
! inputs :  
! outputs:  
! returns:  
! =======================================================================================
CSciViewer.SetFindWhat           PROCEDURE(szWhat) !,VIRTUAL                    ! 
   CODE
      FindGroup.What = szWhat
      RETURN

! =======================================================================================
! CSciViewer.FindWindowTakeCloseWindow()
! purpose:  To provide virtual access to the Find window CloseWindow Event
! inputs :  
! outputs:  
! returns:  
! =======================================================================================
CSciViewer.FindWindowTakeCloseWindow PROCEDURE() !,VIRTUAL
  CODE                                      ! Enter Procedure
  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.FindWindowTakeOpenWindow()
! purpose:  To provide virtual access to the Find window OpenWindow Event
! inputs :  
! outputs:  
! returns:  
! =======================================================================================
CSciViewer.FindWindowTakeOpenWindow PROCEDURE() !,VIRTUAL
  CODE                                      ! Enter Procedure
  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciViewer.SetDefaultMonoFont
! purpose:  Set the Scintilla Control default font to mono
! inputs :  
! outputs:  The Scintilla Control default font is set to mono
! returns:  
! =======================================================================================
CSciViewer.SetTypeface  PROCEDURE(*STRING sTypeface) !,VIRTUAL

  CODE                                      ! Enter Procedure

  SELF.Typeface = CLIP(sTypeface)

  RETURN                                    ! Exit Procedure


CSciViewer.GetSelection   PROCEDURE(*Sci_CharacterRange crange)   !,VIRTUAL
   CODE
      crange.cpMin = SELF.GetSelectionStart()
      crange.cpMax = SELF.GetSelectionEnd()
   RETURN


! =======================================================================================
! CSciViewer.GetCurrentLineNumber
! purpose:  Return the current line number
! inputs :  
! outputs:  
! returns:  
! =======================================================================================   
CSciViewer.GetCurrentLineNumber PROCEDURE()
   CODE
      RETURN SELF.LineFromPosition(SELF.GetCurrentPos())
      
      
! =======================================================================================
! CSciViewer.BookmarkAdd
! purpose:  Add a bookmark
! inputs :  line number to be bookmarked
! outputs:  
! returns:  
! =======================================================================================   
CSciViewer.BookmarkAdd PROCEDURE(LONG lineno)
markerHandle   LONG

  CODE
  	   IF lineno = -1
  		   lineno = SELF.GetCurrentLineNumber()
  		END   
  	   IF NOT SELF.BookmarkPresent(lineno)
  		   markerHandle = SELF.MarkerAdd(lineno, markerBookmark)
  		END   

  		
! =======================================================================================
! CSciViewer.BookmarkDelete
! purpose:  Delete bookmark
! inputs :  line number to have bookmark removed
! outputs:  
! returns:  
! =======================================================================================   
CSciViewer.BookmarkDelete PROCEDURE(LONG lineno)
  CODE
  	   IF lineno = -1
  		   lineno = SELF.GetCurrentLineNumber()
  		END   
  	   IF SELF.BookmarkPresent(lineno)
  		   SELF.MarkerDelete(lineno, markerBookmark)
  		END  
  		
  		
! =======================================================================================
! CSciViewer.BookmarkPresent
! purpose:  Check if line in bookmarked
! inputs :  line number to be checked
! outputs:  
! returns:  
! =======================================================================================   
CSciViewer.BookmarkPresent PROCEDURE(LONG lineno)
state       LONG,AUTO

  CODE
      IF lineno = -1
  		   lineno = SELF.GetCurrentLineNumber()
  		END   
  	   state = SELF.MarkerGet(lineno)
  	   RETURN BAND(state,(BSHIFT(1,markerBookmark)))
  	   
! =======================================================================================
! CSciViewer.BookmarkToggle
! purpose:  Toggle a bookmark on and off
! inputs :  Line number with bookmark to toggle
! outputs:  
! returns:  
! =======================================================================================   
CSciViewer.BookmarkToggle PROCEDURE(LONG lineno)
  CODE
  	   IF lineno = -1
  		   lineno = SELF.GetCurrentLineNumber()
  		END   
  	   IF SELF.BookmarkPresent(lineno)
  		   SELF.BookmarkDelete(lineno)
  	   ELSE
  		   SELF.BookmarkAdd(lineno)
  		END   

! =======================================================================================
! CSciViewer.BookmarkNext
! purpose:  Go to next/previous bookmark
! inputs :  forwardScan true = next; false = previous
!           select = true sets anchor
! outputs:  
! returns:  
! =======================================================================================   
CSciViewer.BookmarkNext PROCEDURE(BOOL forwardScan, BOOL select)
lineno         LONG
lineStart      LONG
lineRetry      LONG
anchor         LONG
nextLine       LONG

  CODE
      lineno = SELF.GetCurrentLineNumber()
  	   anchor = SELF.GetAnchor()
      IF forwardScan
   	   lineStart = lineno + 1                 !Scan starting from next line
   	   lineRetry = 0  				            !If not found, try from the beginning
  		   nextLine = SELF.MarkerNext(lineStart, BSHIFT(1,markerBookmark))
  	   ELSE
  		   lineStart = lineno - 1                 !Scan starting from previous line
  		   lineRetry = SELF.GetLineCount()  !If not found, try from the end
  		   nextLine = SELF.MarkerPrevious(lineStart, BSHIFT(1,markerBookmark))
  		END   
  	   IF nextLine < 0
  	      IF forwardScan
  		      nextLine = SELF.MarkerNext(lineRetry, BSHIFT(1,markerBookmark))
  		   ELSE
  		      nextLine = SELF.MarkerPrevious(lineRetry, BSHIFT(1,markerBookmark))
         END		    
      END   
  	   IF nextLine < 0                           !No bookmark
  		   MESSAGE('No bookmarks!','Warning',ICON:Exclamation)
  	   ELSIF nextLine = lineno                   !Only one, and already on it
  		   MESSAGE('No more bookmarks!','Warning',ICON:Exclamation)
  		ELSE   
  			SELF.EnsureVisibleEnforcePolicy(nextLine)
  		   SELF.GotoLine(nextLine)
  		   IF select
  		      SELF.SetAnchor(anchor)
  		   END
  		END   
      
   
! =======================================================================================
! CSciViewer.OnPrint
! purpose:  Print the Document
! inputs :  bShowDialog
! outputs:  
! returns:  
! =======================================================================================   
CSciViewer.OnPrint   PROCEDURE(BOOL bShowDialog)   !,VIRTUAL

pdlg                 LIKE(kcr_PRINTDLG),PRE(pd)
di                   LIKE(kcr_DOCINFO),PRE(di)
hFont                HFONT
hOldFont             HFONT
tm                   LIKE(kcr_TEXTMETRIC)
dm                   &kcr_DEVMODE
hdc                  HDC
crange               LIKE(Sci_CharacterRange)
startPos             LONG
endPos               LONG 
rectMargins          LIKE(Sci_Rectangle)
rectPhysMargins      LIKE(Sci_Rectangle)
ptPage               LIKE(Sci_Point)
ptDpi                LIKE(Sci_Point)
rectSetup            LIKE(Sci_Rectangle)
localeInfo           CSTRING(4)
szDocName            CSTRING('Kwik Source Search')
szHeader             CSTRING(256)
szHeaderFont         CSTRING('Arial')
headerFormatSize     LONG(12)
szFooter             CSTRING(256)
szFooterFont         CSTRING('Arial Narrow')
footerFormatSize     LONG(10)
headerLineHeight     LONG
fontHeader           HFONT
footerLineHeight     LONG
fontFooter           HFONT
lengthDoc	         LONG
lengthDocMax	      LONG
lengthPrinted	      LONG(0)
frPrint              LIKE(Sci_RangeToFormat)
pageNum              LONG(1)
printPage            BOOL
ta                   UNSIGNED                  
rcw                  LIKE(Sci_Rectangle)                  
pen                  HPEN
penOld               HPEN 
nullFr               &Sci_RangeToFormat
szOutputName         CSTRING(260)
szDataType           CSTRING('WMF')
!	print.header.format=$(FileNameExt) -- Printed on $(CurrentDate), $(CurrentTime) -- Page $(CurrentPage)
!	print.footer.format=$(FilePath) -- File date: $(FileDate) -- File time: $(FileTime)
ProgressWindow WINDOW,AT(,,200,8),CENTER,FONT('Segoe UI',10),TIMER(10),NOFRAME
      PROGRESS,AT(0,0),FULL,USE(?ProgressBar),RANGE(0,100)
   END
   
i              LONG   
PrinterQueue   QUEUE
PrinterName       CSTRING(256)
               END
               
XPSDocumentWriterPresent   BOOL(FALSE)
szDriver          CSTRING('WINSPOOL') 
szDevice          CSTRING('Microsoft XPS Document Writer')
hPrinter          HANDLE
printerDefaults   &kcr_PRINTER_DEFAULTS
cbBuf    LONG
cbNeeded LONG
cc       LONG  
x        LONG
y        LONG
w        LONG
h        LONG                    
   CODE                                      ! Enter Procedure

   IF SELF.bInitialised = TRUE
      SELF.AnnotationClearAll()
   
      EnumPrinters(PrinterQueue,PrinterQueue.PrinterName)
      LOOP i = 1 TO RECORDS(PrinterQueue)
         GET(PrinterQueue,i)
         IF PrinterQueue.PrinterName = 'Microsoft XPS Document Writer'
            !XPSDocumentWriterPresent = TRUE
            BREAK
         END
      END
      
      !initialize the PrintDialog structure
      pdlg.lStructSize  = SIZE(pdlg)
      pdlg.hwndOwner    = SELF.GetWindowHandle()
      pdlg.Flags        = BOR(BOR(PD_USEDEVMODECOPIES, PD_RETURNDC), PD_ALLPAGES)
      pdlg.hInstance    = SYSTEM{PROP:AppInstance}
      pdlg.nFromPage    = 1
      pdlg.nToPage      = 1
      pdlg.nMinPage     = 1
      pdlg.nMaxPage     = -1  ! We do not know how many pages in the
                              ! document until the printer is selected and the paper size is known.
      pdlg.nCopies      = 1
      pdlg.hDC          = 0
      pdlg.hDevMode     = hDevMode
      pdlg.hDevNames    = hDevNames

      ! See if a range has been selected
      SELF.GetSelection(crange)
      startPos = crange.cpMin
      endPos = crange.cpMax

      IF (startPos = endPos)
         pdlg.Flags = BOR(pdlg.Flags,PD_NOSELECTION)
      ELSE
         pdlg.Flags = BOR(pdlg.Flags,PD_SELECTION)
      END
      IF NOT bShowDialog
         !Don't display dialog box, just use the default printer and options
         pdlg.Flags = BOR(pdlg.Flags,PD_RETURNDEFAULT)
      END   

      !get user printer selection
      IF XPSDocumentWriterPresent = TRUE 
         IF GetPrinterDevice(szDevice,hDevNames,hDevMode)
            hdc = CreateDC(szDriver,szDevice,0,0)
         ELSE   
            XPSDocumentWriterPresent = FALSE
            IF PrintDlg(pdlg)
       	      hDevMode = pdlg.hDevMode
      	      hDevNames = pdlg.hDevNames
	            hdc = pdlg.hDC
	         ELSE
	            RETURN
	         END
	      END
      ELSE
         IF PrintDlg(pdlg)
          	hDevMode = pdlg.hDevMode
         	hDevNames = pdlg.hDevNames
	         hdc = pdlg.hDC
	      ELSE
	         RETURN
	      END   
	   END   
	   
      DISPLAY()    
      
      ! Get printer resolution
      ptDpi.x = GetDeviceCaps(hdc, LOGPIXELSX)    ! dpi in X direction
      ptDpi.y = GetDeviceCaps(hdc, LOGPIXELSY)    ! dpi in Y direction

      ! Start by getting the physical page size (in device units).
      ptPage.x = GetDeviceCaps(hdc, PHYSICALWIDTH)   ! device units
      ptPage.y = GetDeviceCaps(hdc, PHYSICALHEIGHT)  ! device units

      ! Get the dimensions of the unprintable
      ! part of the page (in device units).
      rectPhysMargins.left = GetDeviceCaps(hdc, PHYSICALOFFSETX)
      rectPhysMargins.top  = GetDeviceCaps(hdc, PHYSICALOFFSETY)

      ! To get the right and lower unprintable area,
      ! we take the entire width and height of the paper and
      ! subtract everything else.
      rectPhysMargins.right = ptPage.x						       | ! total paper width
                              - GetDeviceCaps(hdc, HORZRES)  | ! printable width
                              - rectPhysMargins.left;				! left unprintable margin

      rectPhysMargins.bottom = ptPage.y						    | ! total paper height
                               - GetDeviceCaps(hdc, VERTRES) | !printable height
                               - rectPhysMargins.top;				! right unprintable margin

      ! At this point, rectPhysMargins contains the widths of the
      ! unprintable regions on all four sides of the page in device units.

      ! Take in account the page setup given by the user (if one value is not null)
      IF (pagesetupMargin.left <> 0 OR pagesetupMargin.right  <> 0 OR |
          pagesetupMargin.top  <> 0 OR pagesetupMargin.bottom <> 0)

         ! Convert the hundredths of millimeters (HiMetric) or
         ! thousandths of inches (HiEnglish) margin values
         ! from the Page Setup dialog to device units.
         ! (There are 2540 hundredths of a mm in an inch.)

         GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, localeInfo, 3)

         IF (localeInfo[1] = '0') 	! Metric system. '1' is US System
            rectSetup.left    = MulDiv(pagesetupMargin.left, ptDpi.x, 2540)
            rectSetup.top     = MulDiv(pagesetupMargin.top, ptDpi.y, 2540)
            rectSetup.right	= MulDiv(pagesetupMargin.right, ptDpi.x, 2540)
            rectSetup.bottom	= MulDiv(pagesetupMargin.bottom, ptDpi.y, 2540)
         ELSE
            rectSetup.left	   = MulDiv(pagesetupMargin.left, ptDpi.x, 1000)
            rectSetup.top	   = MulDiv(pagesetupMargin.top, ptDpi.y, 1000)
            rectSetup.right	= MulDiv(pagesetupMargin.right, ptDpi.x, 1000)
            rectSetup.bottom	= MulDiv(pagesetupMargin.bottom, ptDpi.y, 1000)
         END

         ! Dont reduce margins below the minimum printable area
         rectMargins.left	   = GetMaximum(rectPhysMargins.left, rectSetup.left)
         rectMargins.top	   = GetMaximum(rectPhysMargins.top, rectSetup.top)
         rectMargins.right	   = GetMaximum(rectPhysMargins.right, rectSetup.right)
         rectMargins.bottom	= GetMaximum(rectPhysMargins.bottom, rectSetup.bottom)
      ELSE
         rectMargins.left	   = rectPhysMargins.left
         rectMargins.top	   = rectPhysMargins.top
         rectMargins.right	   = rectPhysMargins.right
         rectMargins.bottom	= rectPhysMargins.bottom
      END

      ! rectMargins now contains the values used to shrink the printable
      ! area of the page.

      ! Convert device coordinates into logical coordinates
      DPtoLP(hdc, ADDRESS(rectMargins), 2)
      DPtoLP(hdc, ADDRESS(rectPhysMargins), 2)

      ! Convert page size to logical units and we're done!
      DPtoLP(hdc, ADDRESS(ptPage), 1)

      ! Print.header.style=font:Arial,size:12,bold
      headerLineHeight = MulDiv(headerFormatSize,ptDpi.y, 72)
      fontHeader = CreateFont(headerLineHeight,0,0,0,700,0,0,0,0,0,0,0,0,szHeaderFont)
      SelectObject(hdc, fontHeader)
      GetTextMetrics(hdc, ADDRESS(tm))
      headerLineHeight = tm.tmHeight + tm.tmExternalLeading
      
      ! Print.footer.style=font:Arial Narrow,size:10,italics
      footerLineHeight = MulDiv(footerFormatSize,ptDpi.y, 72)
      fontFooter = CreateFont(footerLineHeight,0,0,0,400,1,0,0,0,0,0,0,0,szFooterFont)
      SelectObject(hdc, fontHeader)
      GetTextMetrics(hdc, ADDRESS(tm))
      footerLineHeight = tm.tmHeight + tm.tmExternalLeading
      
      szOutputName = ''
      IF XPSDocumentWriterPresent = TRUE 
         IF GetTempPath(SIZE(szOutputName),szOutputName) > 0
            szOutputName = szOutputName & 'kss.xps'
            REMOVE(szOutputName)
         END
      END   

      di.cbSize       = SIZE(di)
      di.lpszDocName  = ADDRESS(szDocName)
      IF szOutputName = ''
         di.lpszOutput   = 0  !ADDRESS(szOutputName)
      ELSE
         di.lpszOutput   = ADDRESS(szOutputName)
      END
      di.lpszDatatype = 0  !ADDRESS(szDataType)
      di.fwType       = 0
      
      IF StartDoc(hdc, di) < 0
         MESSAGE('Can not start printer document.','Print Error',ICON:HAND)
      ELSE
         lengthDoc	 = SELF.GetLength()
         lengthDocMax = lengthDoc
         
         ! Requested to print selection
         IF BAND(pdlg.Flags,PD_SELECTION)
            IF startPos > endPos
               lengthPrinted = endPos
               lengthDoc = startPos
            ELSE
               lengthPrinted = startPos
               lengthDoc = endPos
            END

            IF lengthPrinted < 0
               lengthPrinted = 0
            END
            
            IF lengthDoc > lengthDocMax
               lengthDoc = lengthDocMax
            END   
         END       
 
         ! We must substract the physical margins from the printable area
         frPrint.hdc = hdc
         frPrint.hdcTarget = hdc
         frPrint.rc.left = rectMargins.left - rectPhysMargins.left
         frPrint.rc.top = rectMargins.top - rectPhysMargins.top
         frPrint.rc.right = ptPage.x - rectMargins.right - rectPhysMargins.left
         frPrint.rc.bottom = ptPage.y - rectMargins.bottom - rectPhysMargins.top
         frPrint.rcPage.left = 0
         frPrint.rcPage.top = 0
         frPrint.rcPage.right = ptPage.x - rectPhysMargins.left - rectPhysMargins.right - 1
         frPrint.rcPage.bottom = ptPage.y - rectPhysMargins.top - rectPhysMargins.bottom - 1
         IF headerFormatSize > 0
            frPrint.rc.top += headerLineHeight + headerLineHeight / 2
         END
         IF footerFormatSize > 0
            frPrint.rc.bottom -= footerLineHeight + footerLineHeight / 2
         END
         
         GETPOSITION(0,x,y,w,h)
         OPEN(ProgressWindow)
         SETPOSITION(0,x+w-286,y+h+16,,)
         ACCEPT
           CASE EVENT()
             OF EVENT:CloseDown
                POST(EVENT:CloseWindow,,,1)
             OF EVENT:CloseWindow   
                BREAK
             OF EVENT:OpenWindow
                ?ProgressBar{PROP:Progress} = 0
                DISPLAY(?ProgressBar)
             OF EVENT:Timer   
                IF LengthPrinted < lengthDoc
                   ?ProgressBar{PROP:Progress} = (LengthPrinted / lengthDoc) * 100
                   DISPLAY(?ProgressBar)
                   printPage = FALSE
                   IF (BAND(pdlg.Flags,PD_PAGENUMS) = 0) OR |
                      ((pageNum >= pdlg.nFromPage) AND (pageNum <= pdlg.nToPage))
                      printPage = TRUE
                   END
                   IF (printPage)
                      StartPage(hdc)

                      IF headerFormatSize > 0
                         szHeader = SELF.szFileName
                         SetTextColor(hdc, COLOR:Black)
                         SetBkColor(hdc, COLOR:White)
                         SelectObject(hdc, fontHeader)
                         ta = SetTextAlign(hdc, TA_BOTTOM)
                         rcw.left = frPrint.rc.left
                         rcw.top = frPrint.rc.top - headerLineHeight - headerLineHeight / 2
                         rcw.right = frPrint.rc.right
                         rcw.bottom = frPrint.rc.top - headerLineHeight / 2
                         rcw.bottom = rcw.top + headerLineHeight
                         ExtTextOut(hdc, frPrint.rc.left + 5, frPrint.rc.top - headerLineHeight / 2,  |
                                      ETO_OPAQUE, rcw, szHeader,LEN(szHeader), 0)
                         SetTextAlign(hdc, ta)
                         pen = CreatePen(0, 1, COLOR:Black)
                         penOld = SelectObject(hdc, pen)
                         MoveToEx(hdc, frPrint.rc.left, frPrint.rc.top - headerLineHeight / 4, 0)
                         LineTo(hdc, frPrint.rc.right, frPrint.rc.top - headerLineHeight / 4)
                         SelectObject(hdc, penOld)
                         DeleteObject(pen)
                      END
                   END

                   frPrint.chrg.cpMin = lengthPrinted
                   frPrint.chrg.cpMax = lengthDoc

                   lengthPrinted = SELF.FormatRange(printPage, frPrint)

                   IF (printPage) 
                      IF footerFormatSize > 0
                         !GUI::gui_string sFooter = GUI::StringFromUTF8(propsPrint.GetExpanded("print.footer.format").c_str());
                         szFooter = 'Printed on ' & FORMAT(TODAY(),@D3) & ' at ' & FORMAT(CLOCK(),@T3) & ' -- Page ' & pageNum
                         SetTextColor(hdc, COLOR:Black)
                         SetBkColor(hdc, COLOR:White)
                         SelectObject(hdc, fontFooter)
                         ta = SetTextAlign(hdc, TA_TOP)
                         rcw.left   = frPrint.rc.left
                         rcw.top    = frPrint.rc.bottom + footerLineHeight / 2
                         rcw.right  = frPrint.rc.right
                         rcw.bottom = frPrint.rc.bottom + footerLineHeight + footerLineHeight / 2
                         ExtTextOut(hdc, frPrint.rc.left + 5, frPrint.rc.bottom + footerLineHeight / 2, |
                                      ETO_OPAQUE, rcw, szFooter,   |
                                      LEN(szFooter), 0)
                         SetTextAlign(hdc, ta)
                         pen = CreatePen(0, 1, COLOR:Black)
                         penOld = SelectObject(hdc, pen)
                         SetBkColor(hdc, COLOR:Black)
                         MoveToEx(hdc, frPrint.rc.left, frPrint.rc.bottom + footerLineHeight / 4, 0)
                         LineTo(hdc, frPrint.rc.right, frPrint.rc.bottom + footerLineHeight / 4)
                         SelectObject(hdc, penOld)
                         DeleteObject(pen)
                      END

                      EndPage(hdc)
                   END
                   pageNum += 1

                   IF (BAND(pdlg.Flags,PD_PAGENUMS) AND (pageNum > pdlg.nToPage))
                      BREAK
                   END   
                ELSE
                   ProgressWindow{PROP:Timer} = 0
                   POST(EVENT:CloseWindow)
                END
           END !CASE
         END   !ACCEPT
         CLOSE(ProgressWindow)

         nullFr &= NULL
         SELF.FormatRange(FALSE, nullFr)

         EndDoc(hdc)
      END   !IF StartDoc
      
      IF XPSDocumentWriterPresent = TRUE 
          cc = GlobalFree(hDevMode)
          ASSERT(cc=0)
          cc = GlobalFree(hDevNames)
          ASSERT(cc=0)
      END   
          
      DeleteDC(hdc)
      DeleteObject(fontHeader)
      DeleteObject(fontFooter)
   END
   
   RETURN
   
   
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! WindowComponent Callback Methods
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

! =======================================================================================
! CSciViewer.Kill
! purpose:  The Kill method releases any memory allocated during the life of the object
!           and performs any other required termination code.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciViewer.Kill PROCEDURE

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised = TRUE

     ! Dispose Popup Manager
     IF ~SELF.Popup &= NULL
        SELF.Popup.Kill
        DISPOSE(SELF.Popup)
        SELF.Popup &= NULL
     END

     ! Dispose Error Manager
     IF ~SELF.ErrorMgr &= NULL
        !SELF.ErrorMgr.Kill
        DISPOSE(SELF.ErrorMgr)
        SELF.ErrorMgr &= NULL
     END
     
     ! Free dynamically allocated memory
     IF ~SELF.szTextBuffer &= NULL                             ! If we already have a buffer allocated
        DISPOSE(SELF.szTextBuffer)
        SELF.szTextBuffer &= NULL
     END
     
  END

  PARENT.Kill()

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.Reset
! purpose:  The Reset method resets the object's data if needed or if Force is TRUE.
! inputs :  (BYTE Force)
! outputs:
! returns:
! =======================================================================================
CSciViewer.Reset PROCEDURE(BYTE Force)

  CODE                                      ! Enter Procedure

  PARENT.Reset(Force)

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.ResetRequired
! purpose:  The ResetRequired method determines whether the objects data needs to be
!           refreshed.  A TRUE return value indicates a refresh occurred and a screen
!           redraw is necessary.
! inputs :
! outputs:
! returns:  BYTE
! =======================================================================================
CSciViewer.ResetRequired PROCEDURE!,BYTE

ReturnValue BYTE,AUTO

  CODE                                      ! Enter Procedure

  ReturnValue = PARENT.ResetRequired()

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciViewer.SetAlerts
! purpose:  The SetAlerts method alerts standard keystrokes for the control associated
!           with the window component's object.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciViewer.SetAlerts PROCEDURE

  CODE                                      ! Enter Procedure

  PARENT.SetAlerts()
  SELF.W{PROP:ALRT,255} = CtrlHome  !Document Top
  SELF.W{PROP:ALRT,255} = PgUpKey   !Page Up
  SELF.W{PROP:ALRT,255} = UpKey     !Line Up
  SELF.W{PROP:ALRT,255} = DownKey   !Line Down
  SELF.W{PROP:ALRT,255} = PgDnKey   !Page Down
  SELF.W{PROP:ALRT,255} = CtrlEnd   !Document Bottom
  SELF.W{PROP:ALRT,255} = CtrlG     !Goto Line
  SELF.W{PROP:ALRT,255} = F3Key     !Find Next
  SELF.W{PROP:ALRT,255} = CtrlF     !Find
  SELF.W{PROP:ALRT,255} = CtrlP     !Print
  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.TakeEvent
! purpose:  Process the current ACCEPT loop event.
!           The TakeEvent method processes all window events and returns a value indicating
!           whether ACCEPT loop processing is complete and should stop. 
! inputs :
! outputs:
! returns:  BYTE
!           Level:Benign to indicate processing of this event should continue normally
!           Level:Notify to indicate processing is completed for this event
!                                and the ACCEPT loop should CYCLE
!           Level:Fatal to indicate the event could not be processed
!                                and the ACCEPT loop should BREAK
! =======================================================================================
CSciViewer.TakeEvent PROCEDURE!,BYTE

ReturnValue         BYTE,AUTO
lFoundNext          LONG,AUTO
lFoundPosition      LONG,AUTO

  CODE                                      ! Enter the procedure

  ReturnValue = PARENT.TakeEvent()

  IF ReturnValue = Level:Benign
     CASE EVENT()                              ! What event occurred?
     OF SELF.lContextMenuEvent                 !   ContextMenu Event
        ReturnValue = SELF.TakeContextMenu()   !       call event handler
     OF EVENT:ScrollTop
        SELF.DocumentStart()
     OF EVENT:PageUp
        SELF.PageUp()
     OF EVENT:ScrollUp
        SELF.LineScrollUp()
     OF EVENT:ScrollDown
        SELF.LineScrollDown()
     OF EVENT:PageDown
        SELF.PageDown()
     OF EVENT:ScrollBottom
        SELF.DocumentEnd()
     OF EVENT:AlertKey
        CASE KEYCODE()
        OF CtrlHome
           SELF.DocumentStart()
        OF PgUpKey
           SELF.PageUp()
        OF UpKey
           SELF.LineScrollUp()
        OF DownKey
           SELF.LineScrollDown()
        OF PgDnKey
           SELF.PageDown()
        OF CtrlEnd
           SELF.DocumentEnd()
        OF CtrlG
           SELF.AskGoToLine()
        OF F3Key
           SELF.SearchAsk(FALSE)
        OF CtrlF
           SELF.SearchAsk(TRUE)
        OF CtrlP
           SELF.PrintAsk()
        END
     END                                       ! End of Event Processor
  END

  RETURN ReturnValue                        ! Exit the procedure


! =======================================================================================
! CSciViewer.Update
! purpose:  Get VIEW data for the selected item.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciViewer.Update PROCEDURE

  CODE                                      ! Enter Procedure

  PARENT.Update()

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciViewer.UpdateWindow
! purpose:  The UpdateWindow method updates the controls on the window based upon
!           certain conditions set by the WindowComponent object.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciViewer.UpdateWindow PROCEDURE

  CODE                                      ! Enter Procedure

  PARENT.UpdateWindow()

  RETURN                                    ! Exit Procedure


!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! Module Procedures - implicitly PRIVATE
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

! =======================================================================================
! GetFileExtension
! purpose:  Extract the file extension from passed filename
! inputs :  *CSTRING szFile - The filename from which to extract the extension
! outputs:
! returns:  STRING - The file extension
! =======================================================================================
GetFileExtension    PROCEDURE(*CSTRING szFile)          ! Declare Procedure
sExtension  STRING(33),AUTO
I           LONG,AUTO
lLength     LONG,AUTO
lStart      LONG,AUTO

  CODE                                      ! Enter Procedure
  lStart  = 0
  lLength = LEN(CLIP(szFile))
  LOOP I = lLength TO 1 BY -1
    IF (szFile[I] = '.') THEN
       lStart = I
       BREAK
    END
  END
  lStart   += 1
  lLength  = lLength - lStart + 1
  sExtension = SUB(szFile, lStart, lLength)
  RETURN(sExtension)
  
GetMaximum  PROCEDURE(Val1,Val2)
   CODE
     IF Val1 > Val2
        RETURN Val1
     ELSE
        RETURN Val2
     END

EnumPrinters         PROCEDURE  (*QUEUE pQueue, *CSTRING pPrinterName) !,LONG ! Declare Procedure
cc                      LONG                                  !completion code
cdwName                 ULONG(81)                             !
dwSubKey                ULONG                                 !
hKeyPrinters            ULONG                                 !
RetVal                  LONG                                  !
szName                  CSTRING(81)                           !
szSubKey                CSTRING(256)                          !

!HKEY_LOCAL_MACHINE      EQUATE(080000002h)
!KEY_ENUMERATE_SUB_KEYS  EQUATE(00008h)
!ERROR_MORE_ITEMS        EQUATE(234)

  CODE
 retval = ERROR_SUCCESS
 szSubKey = 'System\CurrentControlSet\control\Print\Printers'
 
 cc = RegOpenKeyEx(HKEY_LOCAL_MACHINE,szSubKey,0,KEY_ENUMERATE_SUB_KEYS,hKeyPrinters)
 if cc = ERROR_SUCCESS
   dwSubKey = 0
   loop
     szName = ''
     cdwName = 80
     cc = RegEnumKeyEx(hKeyPrinters,dwSubKey,szName,cdwName,0,0,0,0)
     case cc
       of ERROR_NO_MORE_ITEMS
          break
       of ERROR_MORE_ITEMS
     orof ERROR_SUCCESS
          pPrinterName = szName
          add(pQueue)
     else
          retval = cc
          break
     end
     dwSubKey += 1
   end
   cc = RegCloseKey(hKeyPrinters)
 end
 return(RetVal)

! returns a DEVMODE and DEVNAMES for the printer name specified
GetPrinterDevice  PROCEDURE(*CSTRING szPrinterName, *HGLOBAL phDevNames, *HGLOBAL phDevMode)  !,BOOL,PROC
hPrinter          HANDLE
printerDefaults   &kcr_PRINTER_DEFAULTS
ppi               LONG
printerInfo       &kcr_PRINTER_INFO_2
dwBytesReturned   LONG
dwBytesNeeded     LONG
pDevMode          LONG
_hDevMode         HGLOBAL  
dm                &kcr_DEVMODE
drvNameLen        LONG
ptrNameLen        LONG
porNameLen        LONG
pDevNames         LONG
_hDevNames        HGLOBAL 
tcOffset          LONG
dn                &kcr_DEVNAMES
cs                &CSTRING

   CODE
    ! Open printer
    printerDefaults &= NULL
    IF OpenPrinter(szPrinterName, hPrinter, printerDefaults) = FALSE
       RETURN FALSE
    END   

    ! obtain PRINTER_INFO_2 structure and close printer
    
    GetPrinter(hPrinter, 2, 0, 0, dwBytesNeeded)
    ppi = GlobalAlloc(GPTR, dwBytesNeeded)
    printerInfo &= (ppi)
    IF GetPrinter(hPrinter, 2, ADDRESS(printerInfo), dwBytesNeeded, dwBytesReturned) = 0
       GlobalFree(ppi)
       ClosePrinter(hPrinter)
       RETURN FALSE
    END
    ClosePrinter(hPrinter)

    ! Allocate a global handle for DEVMODE
    dm &= (printerInfo.pDevmode)
    
    _hDevMode = GlobalAlloc(GHND, dm.dmSize + dm.driverExtra)
    ASSERT(_hDevMode)
    
    pDevMode = GlobalLock(_hDevMode)
    ASSERT(pDevMode)

    ! copy DEVMODE data from PRINTER_INFO_2::pDevMode
    movememory(pDevMode, printerInfo.pDevMode, dm.dmSize + dm.driverExtra)
    GlobalUnlock(_hDevMode)

    ! Compute size of DEVNAMES structure from PRINTER_INFO_2's data
    cs &= (printerInfo.pDriverName)
    drvNameLen = LEN(cs)+1;  ! driver name
    cs &= (printerInfo.pPrinterName)
    ptrNameLen = LEN(cs)+1; ! printer name
    cs &= (printerInfo.pPortName)
    porNameLen = LEN(cs)+1;    ! port name

    ! Allocate a global handle big enough to hold DEVNAMES.
    _hDevNames = GlobalAlloc(GHND, SIZE(kcr_DEVNAMES) + (drvNameLen + ptrNameLen + porNameLen))
    ASSERT(_hDevNames)
    pDevNames = GlobalLock(_hDevNames)
    ASSERT(pDevNames)
    dn &= (pDevNames)

    ! Copy the DEVNAMES information from PRINTER_INFO_2
    ! tcOffset = TCHAR Offset into structure
    tcOffset = SIZE(kcr_DEVNAMES)

    dn.wDriverOffset = tcOffset
    movememory(pDevNames + tcOffset, printerInfo.pDriverName, drvNameLen)
    tcOffset += drvNameLen

    dn.wDeviceOffset = tcOffset
    movememory(pDevNames + tcOffset, printerInfo.pPrinterName, ptrNameLen)
    tcOffset += ptrNameLen

    dn.wOutputOffset = tcOffset
    movememory(pDevNames + tcOffset, printerInfo.pPortName, porNameLen)
    dn.wDefault = 0

    GlobalUnlock(_hDevNames)
    GlobalFree(ppi)

    ! set the new hDevMode and hDevNames
    phDevMode = _hDevMode
    phDevNames = _hDevNames
    RETURN TRUE

	