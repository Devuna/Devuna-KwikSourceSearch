   MEMBER
!region Notices
! ================================================================================
! Notice : Copyright (C) 2017, Devuna
!          Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of Devuna-Scintilla (https://github.com/Devuna/Devuna-Scintilla)
!
!    Devuna-Scintilla is free software: you can redistribute it and/or modify
!    it under the terms of the MIT License as published by
!    the Open Source Initiative.
!
!    Devuna-Scintilla is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    MIT License for more details.
!
!    You should have received a copy of the MIT License
!    along with Devuna-Scintilla.  If not, see <https://opensource.org/licenses/MIT>.
! ================================================================================
!endregion Notices
!========================================================================================
!Scintilla Control Class
!Author:        Randy Rogers <rrogers@devuna.com>
!
!Revisions:
!==========
! 2012.02.12    KCR Added missing methods
! 2003.07.15    KCR Added project PRAGMA statements to control class linking
!                   and inclusion of sciutil.c into the application project.
!                   Thanks to Mark Goldberg for the suggestion.
! 2003.11.28    KCR added deprecated keyword lists for use with new
!                   lexer dated 2003.11.27 or later
! 2003.12.20    KCR added GetWindowHandle() method
! 2003.12.21    KCR added themed support
!========================================================================================

  PRAGMA('compile(SCIUTIL.C)')                                      ! 2003.07.15 KCR

  ! Include the class declaration
  INCLUDE('CSciControl.inc'),ONCE


!================================================================
! windows.h (partial)
!================================================================
!!WS_CHILD            EQUATE(40000000h)
!!WS_VISIBLE          EQUATE(10000000h)
!!WS_EX_WINDOWEDGE    EQUATE(00000100h)
!!WS_EX_CLIENTEDGE    EQUATE(00000200h)
!!WS_BORDER           EQUATE(00800000h)

!!WM_PAINT            EQUATE(0000Fh)
!!WM_NOTIFY           EQUATE(004Eh)
!!WM_CONTEXTMENU      EQUATE(0007Bh)

!!SWP_NOZORDER        EQUATE(00004h)
!!SWP_SHOWWINDOW      EQUATE(00040h)

! ShowWindow() Command EQUATES
!!SW_HIDE             EQUATE(0)
!!SW_SHOW             EQUATE(5)

!!GWL_WNDPROC         EQUATE(-4)
!!GWL_USERDATA        EQUATE(-21)

!!POINT   GROUP,TYPE
!!x         LONG
!!y         LONG
!!        END

RECT    GROUP,TYPE
left      SIGNED
top       SIGNED
right     SIGNED
bottom    SIGNED
        END

BS_SOLID            EQUATE(0)
FLOODFILLSURFACE    EQUATE(1)

LOGBRUSH    GROUP,TYPE
lbStyle       UNSIGNED
lbColor       ULONG
lbHatch       LONG
            END

!================================================================
! Procedure Map
!================================================================
  MAP
    WindowProc(UNSIGNED hWnd, |
               UNSIGNED uMsg, |
               LONG wParam,   |
               LONG lParam),LONG,PASCAL

    ClientWndProc(UNSIGNED hWnd, |
                  UNSIGNED uMsg, |
                  LONG wParam,   |
                  LONG lParam),LONG,PASCAL

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

      DefWindowProc(UNSIGNED hWnd, |
                    UNSIGNED uMsg, |
                    LONG wParam,   |
                    LONG lParam),LONG,PASCAL,NAME('DefWindowProcA')

      DestroyWindow(UNSIGNED hWnd),BOOL,PASCAL,PROC

      GetDlgItem(UNSIGNED hDlg, UNSIGNED nIDDlgItem),LONG,PASCAL

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
      ShowWindow(UNSIGNED hWnd, LONG nCmdShow),BYTE,PASCAL,PROC

      CreateBrushIndirect(*LOGBRUSH),UNSIGNED,PASCAL,RAW
      DeleteObject(UNSIGNED hGDIOBJ),BOOL,PASCAL,PROC
      ExtFloodFill(UNSIGNED hwnd, SIGNED x, SIGNED y, ULONG color, UNSIGNED),BOOL,PASCAL,PROC
      GetDC(UNSIGNED hwnd),UNSIGNED,PASCAL
      GetPixel(UNSIGNED hdc, SIGNED x, SIGNED y),ULONG,PASCAL
      GetWindowDC(UNSIGNED hwnd),UNSIGNED,PASCAL
      ReleaseDC(UNSIGNED hwnd, UNSIGNED hdc),SIGNED,PASCAL,PROC
      SelectObject(UNSIGNED hdc, UNSIGNED hGDIOBJ),UNSIGNED,PASCAL,PROC

    END
  END


!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! Module Level Data - implicitly PRIVATE
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
OrigClientWndProc &UNSIGNED,THREAD,STATIC        ! original subclassed ClientWndProc

! LIFO Queue of windows on this thread that have been subclassed
SubClassedClient  QUEUE,PRE(SubClassedClient),THREAD,STATIC
hWnd                UNSIGNED
WndProc             UNSIGNED
                  END

OrigWindowProc    &UNSIGNED,THREAD,STATIC        ! original subclassed ClientWndProc

! Queue of windows on this thread that have been subclassed
SubClassed        QUEUE,PRE(SubClassed),THREAD,STATIC
hWnd                UNSIGNED
WndProc             UNSIGNED
                  END

!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! CSciControl methods
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

! =======================================================================================
! CSciControl.Construct
! purpose:  The Construct method is automatically called when the object is instantiated
!           It performs the necessary code to initialize the CSciControl object.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Construct PROCEDURE

szLibraryName    CSTRING('SciLexer.DLL')

  CODE                                      ! Enter Procedure

  SELF.bInitialised = FALSE
  SELF.hLib = LoadLibrary(szLibraryName)

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Destruct
! purpose:  The Destruct method is an automatic destructor that is called when the object
!           is removed from memory. This ensures that all data allocated by the object is
!           removed from memory.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Destruct PROCEDURE

  CODE                                      ! Enter Procedure

  IF SELF.hLib <> 0
     FreeLibrary(SELF.hLib)
     SELF.hLib = 0
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Init
! purpose:  The Init method initializes the CSciControl object and returns a value
!           indicating whether it successfully initialised and is ready to proceed.
! inputs :  *WINDOW W - Window control will appear on
!           LONG feq - feq of clarion control to use for positioning and events
!           UNSIGNED id - ID used to identify control
!           BOOL Themed - flag to indicate control is to be XP themed
! returns:  BYTE    Level:Benign if control is initialised
!                   Level:Notify if initialisation failed
! =======================================================================================
CSciControl.Init PROCEDURE(*WINDOW W, LONG feq, UNSIGNED id, BOOL Themed = 0) !,BYTE,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure
? ASSERT(SELF.hLib <> 0)
  IF SELF.hLib <> 0                             ! Make sure library got loaded
?    ASSERT(~W &= NULL AND feq <> 0 AND id <> 0)
     IF ~W &= NULL AND feq <> 0 AND id <> 0     ! Make sure parameters are not null
        SELF.W &= W                             ! The window the control appears on
        SELF.feq = feq                          ! Control that will receive notification events
        SELF.hwndRegion = feq{PROP:Handle}      ! Save window handle to use for positioning
        SELF.id = id                            ! Save control ID
        SELF.bThemed = Themed                   ! Save Themed option
        SELF.thread = THREAD()                  ! Save thread
        SELF.SetAlerts()                        ! Set alert keys

        ! Subclass the ClientWndProc
        ! note: ClientWndProc may already be subclassed by another instance of the class
        IF OrigClientWndProc &= NULL            ! if not already subclassed
           SubClassedClient.hWnd = SELF.W{PROP:ClientHandle}
           SubClassedClient.WndProc = SELF.W{PROP:ClientWndProc}
           ADD(SubClassedClient,1)
           OrigClientWndProc &= SubClassedClient.WndProc
           SELF.W{PROP:ClientWndProc} = ADDRESS(ClientWndProc)
        ELSE
           IF SubClassedClient.hWnd <> SELF.W{PROP:ClientHandle}   !If different window on same thread
              SubClassedClient.hWnd = SELF.W{PROP:ClientHandle}
              SubClassedClient.WndProc = SELF.W{PROP:ClientWndProc}
              ADD(SubClassedClient,1)
              SELF.W{PROP:ClientWndProc} = ADDRESS(ClientWndProc)
           END
        END
        SELF.bInitialised = TRUE                ! Set class initialised flag
     ELSE
        SELF.bInitialised = FALSE               ! Set class initialised flag
     END
  ELSE
     SELF.bInitialised = FALSE                  ! Set class initialised flag
  END

  IF SELF.bInitialised                          ! Set return value
     ReturnValue = Level:Benign                 !
  ELSE                                          !
     ReturnValue = Level:Notify                 !
  END                                           !

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetWindowHandle
! purpose:  Return the Window Handle
! inputs :
! outputs:
! returns:  BOOL the Control Hide Property
! =======================================================================================
CSciControl.GetWindowHandle PROCEDURE() !,ULONG,VIRTUAL

ReturnValue ULONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.hWnd
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetHide
! purpose:  Return the Control Hide Property
! inputs :
! outputs:
! returns:  BOOL the Control Hide Property
! =======================================================================================
CSciControl.GetHide PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.bHide
  ELSE
     ReturnValue = FALSE
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetHide
! purpose:  Set the Control Hide Property
! inputs :  BOOL True to Hide the control window, False to Show
! outputs:  The control window is hidden/shown
! returns:
! =======================================================================================
CSciControl.SetHide               PROCEDURE(BOOL bHide) !,VIRTUAL                 ! Set Hide Property

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.bHide = bHide
     ShowWindow(SELF.hWnd, CHOOSE(SELF.bHide=TRUE,SW_HIDE,SW_SHOW))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetContextMenuEvent
! purpose:  Get the Context Menu Event Property
! inputs :
! outputs:  The Event that is posted to the window when a WM_CONTEXTMENU event occurs
!           ie when the user right clicks on the control when the control default popup
!           menu has been disabled.
! returns:  LONG - the Event ID that the control uses for context menu notification
! =======================================================================================
CSciControl.GetContextMenuEvent PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.lContextMenuEvent
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetContextMenuEvent
! purpose:  Set the Control Context Menu Event Property
! inputs :  LONG The Event ID to be used for context menu notification
! outputs:  The control context menu notification event is set
! returns:
! =======================================================================================
CSciControl.SetContextMenuEvent PROCEDURE(LONG lNotifyEvent) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.lContextMenuEvent = lNotifyEvent
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.TakeOpenWindow
! purpose:  Handle the OpenWindow Event
! inputs :
! outputs:
! returns:  BYTE
!           Level:Benign to indicate processing of this event should continue normally
!           Level:Notify to indicate processing is completed for this event
!                                and the ACCEPT loop should CYCLE
!           Level:Fatal to indicate the event could not be processed
!                                and the ACCEPT loop should BREAK
! =======================================================================================
CSciControl.TakeOpenWindow PROCEDURE !,BYTE,VIRTUAL

ReturnValue BYTE,AUTO
hwndParent  UNSIGNED,AUTO
hInstance   UNSIGNED,AUTO
szClassName CSTRING('Scintilla')

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ! Get window size
     SELF.GetWindowRect()

     ! Create the control window
     hWndParent = SELF.W{PROP:ClientHandle}
     hInstance = SYSTEM{PROP:AppInstance}
     IF SELF.bThemed
        SELF.hWnd = CreateWindowEx(WS_EX_WINDOWEDGE,           |
                                 szClassName,                  |
                                 ,                             |
                                 WS_BORDER + WS_CHILD + WS_VISIBLE,        |
                                 SELF.rc.left,                 |
                                 SELF.rc.top,                  |
                                 SELF.rc.left + SELF.rc.Right, |
                                 SELF.rc.top + SELF.rc.Bottom, |
                                 hWndParent,                   |
                                 SELF.id,                      |   !Id to use in notification messages
                                 hInstance,                    |
                                 0)
     ELSE
        SELF.hWnd = CreateWindowEx(WS_EX_CLIENTEDGE,           |
                                 szClassName,                  |
                                 ,                             |
                                 WS_CHILD + WS_VISIBLE,        |
                                 SELF.rc.left,                 |
                                 SELF.rc.top,                  |
                                 SELF.rc.left + SELF.rc.Right, |
                                 SELF.rc.top + SELF.rc.Bottom, |
                                 hWndParent,                   |
                                 SELF.id,                      |   !Id to use in notification messages
                                 hInstance,                    |
                                 0)
     END

     ! Check for success
     IF SELF.hwnd
        SetWindowPos(SELF.hWnd,HWND_TOP,0,0,0,0,BOR(SWP_NOMOVE,SWP_NOSIZE))
        IF SELF.bThemed
           ! Subclass the WndProc
           SubClassed.hWnd = SELF.hwnd
           SubClassed.WndProc = GetWindowLong(SELF.hwnd,GWL_WNDPROC)
           ADD(SubClassed,+SubClassed.hWnd)
           SetWindowLong(SELF.hwnd,GWL_WNDPROC,ADDRESS(WindowProc))
        END

        ! Setup for Direct Call to Scintilla message processing function
        SELF.SciMsgProc = SendMessage(SELF.hWnd, SCI_GETDIRECTFUNCTION, 0, 0)
        SELF.SciMsgPtr  = SendMessage(SELF.hWnd, SCI_GETDIRECTPOINTER, 0, 0)

        ! Store a pointer to ourself in the window USERDATA
        SetWindowLong(SELF.hwnd, GWL_USERDATA, ADDRESS(SELF))

        ! We're good to go
        ReturnValue = Level:Benign
     ELSE
        ! Bummer
        ReturnValue = Level:Fatal
     END

  ELSE  ! Not initialised
     ReturnValue = Level:Fatal
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.TakeGainFocus
! purpose:  Handle the GainFocus Event
! inputs :
! outputs:
! returns:  BYTE
!           Level:Benign to indicate processing of this event should continue normally
!           Level:Notify to indicate processing is completed for this event
!                                and the ACCEPT loop should CYCLE
!           Level:Fatal to indicate the event could not be processed
!                                and the ACCEPT loop should BREAK
! =======================================================================================
CSciControl.TakeGainFocus   PROCEDURE !,BYTE,VIRTUAL

ReturnValue BYTE,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.Reset(FALSE)
     ReturnValue = Level:Benign
  ELSE
     ReturnValue = Level:Fatal
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.TakeSized
! purpose:  Handle the Sized Event
! inputs :
! outputs:
! returns:  BYTE
!           Level:Benign to indicate processing of this event should continue normally
!           Level:Notify to indicate processing is completed for this event
!                                and the ACCEPT loop should CYCLE
!           Level:Fatal to indicate the event could not be processed
!                                and the ACCEPT loop should BREAK
! =======================================================================================
CSciControl.TakeSized   PROCEDURE !,BYTE,VIRTUAL

ReturnValue BYTE,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.Reset(FALSE)
     ReturnValue = Level:Benign
  ELSE  ! Not initialised
     ReturnValue = Level:Fatal
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.TakeAlertKey
! purpose:  Handle the AlertKey Event
! inputs :
! outputs:
! returns:  BYTE
!           Level:Benign to indicate processing of this event should continue normally
!           Level:Notify to indicate processing is completed for this event
!                                and the ACCEPT loop should CYCLE
!           Level:Fatal to indicate the event could not be processed
!                                and the ACCEPT loop should BREAK
! =======================================================================================
CSciControl.TakeAlertKey    PROCEDURE !,BYTE,VIRTUAL

ReturnValue BYTE,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = Level:Benign
  ELSE
     ReturnValue = Level:Fatal
  END

  RETURN(ReturnValue)                       ! Exit Procedure


!================================================================
!  Methods generated from Scintilla.iFace
!================================================================

! =======================================================================================
! CSciControl.AddText
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:
! =======================================================================================
CSciControl.AddText PROCEDURE(LONG lLength, *CSTRING szText) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ADDTEXT, lLength, ADDRESS(szText))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AddStyledText
! purpose:
! inputs :  lLength
!           szC
! outputs:
! returns:
! =======================================================================================
CSciControl.AddStyledText   PROCEDURE(LONG lLength, *CSTRING szC) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ADDSTYLEDTEXT, lLength, ADDRESS(szC))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.InsertText
! purpose:
! inputs :  lPos
!           szText
! outputs:
! returns:
! =======================================================================================
CSciControl.InsertText  PROCEDURE(LONG lPos, *CSTRING szText) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_INSERTTEXT, lPos, ADDRESS(szText))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ClearAll
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ClearAll    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CLEARALL, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ClearDocumentStyle
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ClearDocumentStyle  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CLEARDOCUMENTSTYLE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetLength
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLength   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLENGTH, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetCharAt
! purpose:
! inputs :  lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCharAt   PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCHARAT, lPos, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetCurrentPos
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCurrentPos   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCURRENTPOS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetAnchor
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetAnchor   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETANCHOR, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetStyleAt
! purpose:
! inputs :  lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetStyleAt  PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSTYLEAT, lPos, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.Redo
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Redo    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_REDO, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetUndoCollection
! purpose:
! inputs :  bCollectUndo
! outputs:
! returns:
! =======================================================================================
CSciControl.SetUndoCollection   PROCEDURE(BOOL bCollectUndo) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETUNDOCOLLECTION, bCollectUndo, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SelectAll
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.SelectAll   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SELECTALL, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetSavePoint
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSavePoint    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSAVEPOINT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetStyledText
! purpose:
! inputs :
!           tr
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetStyledText   PROCEDURE(*Sci_TextRange tr) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSTYLEDTEXT, 0, ADDRESS(tr))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.CanRedo
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.CanRedo PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CANREDO, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerLineFromHandle
! purpose:
! inputs :  lHandle
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.MarkerLineFromHandle    PROCEDURE(LONG lHandle) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MARKERLINEFROMHANDLE, lHandle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerDeleteHandle
! purpose:
! inputs :  lHandle
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerDeleteHandle  PROCEDURE(LONG lHandle) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERDELETEHANDLE, lHandle, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetUndoCollection
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetUndoCollection   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETUNDOCOLLECTION, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetViewWS
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetViewWS   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETVIEWWS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetViewWS
! purpose:
! inputs :  lViewWS
! outputs:
! returns:
! =======================================================================================
CSciControl.SetViewWS   PROCEDURE(LONG lViewWS) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETVIEWWS, lViewWS, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.PositionFromPoint
! purpose:
! inputs :  lX
!           lY
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.PositionFromPoint   PROCEDURE(LONG lX, LONG lY) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_POSITIONFROMPOINT, lX, lY)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.PositionFromPointClose
! purpose:
! inputs :  lX
!           lY
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.PositionFromPointClose  PROCEDURE(LONG lX, LONG lY) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_POSITIONFROMPOINTCLOSE, lX, lY)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GotoLine
! purpose:
! inputs :  lLine
! outputs:
! returns:
! =======================================================================================
CSciControl.GotoLine    PROCEDURE(LONG lLine) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_GOTOLINE, lLine, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GotoPos
! purpose:
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.GotoPos PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_GOTOPOS, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetAnchor
! purpose:
! inputs :  lPosAnchor
! outputs:
! returns:
! =======================================================================================
CSciControl.SetAnchor   PROCEDURE(LONG lPosAnchor) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETANCHOR, lPosAnchor, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetCurLine
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCurLine  PROCEDURE(LONG lLength, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCURLINE, lLength, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetEndStyled
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetEndStyled    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETENDSTYLED, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.ConvertEOLs
! purpose:
! inputs :  lEolMode
! outputs:
! returns:
! =======================================================================================
CSciControl.ConvertEOLs PROCEDURE(LONG lEolMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CONVERTEOLS, lEolMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetEOLMode
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetEOLMode  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETEOLMODE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetEOLMode
! purpose:
! inputs :  lEolMode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetEOLMode  PROCEDURE(LONG lEolMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETEOLMODE, lEolMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StartStyling
! purpose:
! inputs :  lPos
!           lMask
! outputs:
! returns:
! =======================================================================================
CSciControl.StartStyling    PROCEDURE(LONG lPos, LONG lMask) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STARTSTYLING, lPos, lMask)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetStyling
! purpose:
! inputs :  lLength
!           lStyle
! outputs:
! returns:
! =======================================================================================
CSciControl.SetStyling  PROCEDURE(LONG lLength, LONG lStyle) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSTYLING, lLength, lStyle)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetBufferedDraw
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetBufferedDraw PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETBUFFEREDDRAW, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetBufferedDraw
! purpose:
! inputs :  bBuffered
! outputs:
! returns:
! =======================================================================================
CSciControl.SetBufferedDraw PROCEDURE(BOOL bBuffered) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETBUFFEREDDRAW, bBuffered, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetTabWidth
! purpose:
! inputs :  lTabWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.SetTabWidth PROCEDURE(LONG lTabWidth) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETTABWIDTH, lTabWidth, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetTabWidth
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetTabWidth PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTABWIDTH, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetCodePage
! purpose:
! inputs :  lCodePage
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCodePage PROCEDURE(LONG lCodePage) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCODEPAGE, lCodePage, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetUsePalette
! purpose:
! inputs :  bUsePalette
! outputs:
! returns:
! =======================================================================================
CSciControl.SetUsePalette   PROCEDURE(BOOL bUsePalette) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETUSEPALETTE, bUsePalette, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerDefine
! purpose:
! inputs :  lMarkerNumber
!           lMarkerSymbol
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerDefine    PROCEDURE(LONG lMarkerNumber, LONG lMarkerSymbol) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERDEFINE, lMarkerNumber, lMarkerSymbol)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerSetFore
! purpose:
! inputs :  lMarkerNumber
!           lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerSetFore   PROCEDURE(LONG lMarkerNumber, LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERSETFORE, lMarkerNumber, lFore)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerSetBack
! purpose:
! inputs :  lMarkerNumber
!           lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerSetBack   PROCEDURE(LONG lMarkerNumber, LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERSETBACK, lMarkerNumber, lBack)
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.MarkerSetBackSelected
! purpose:
! inputs :  lMarkerNumber
!           lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerSetBackSelected   PROCEDURE(LONG lMarkerNumber, LONG lBack)  !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERSETBACKSELECTED, lMarkerNumber, lBack)
  END

  RETURN

! =======================================================================================
! CSciControl.MarkerEnableHighlight
! purpose:
! inputs :  enabled
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerEnableHighlight   PROCEDURE(BOOL enabled) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERENABLEHIGHLIGHT, enabled, 0)
  END

  RETURN


! =======================================================================================
! CSciControl.MarkerAdd
! purpose:
! inputs :  lLine
!           lMarkerNumber
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.MarkerAdd   PROCEDURE(LONG lLine, LONG lMarkerNumber) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MARKERADD, lLine, lMarkerNumber)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerDelete
! purpose:
! inputs :  lLine
!           lMarkerNumber
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerDelete    PROCEDURE(LONG lLine, LONG lMarkerNumber) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERDELETE, lLine, lMarkerNumber)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerDeleteAll
! purpose:
! inputs :  lMarkerNumber
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerDeleteAll PROCEDURE(LONG lMarkerNumber) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERDELETEALL, lMarkerNumber, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerGet
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.MarkerGet   PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MARKERGET, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerNext
! purpose:
! inputs :  lLineStart
!           lMarkerMask
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.MarkerNext  PROCEDURE(LONG lLineStart, LONG lMarkerMask) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MARKERNEXT, lLineStart, lMarkerMask)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerPrevious
! purpose:
! inputs :  lLineStart
!           lMarkerMask
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.MarkerPrevious  PROCEDURE(LONG lLineStart, LONG lMarkerMask) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MARKERPREVIOUS, lLineStart, lMarkerMask)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerDefinePixmap
! purpose:
! inputs :  lMarkerNumber
!           lpXPMData
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerDefinePixmap  PROCEDURE(LONG lMarkerNumber, LONG lpXPMData) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERDEFINEPIXMAP, lMarkerNumber, lpXPMData)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.MarkerAddSet
! purpose:
! inputs :  lLine
!           lSet
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerAddSet   PROCEDURE(LONG lLine, LONG lSet) !,VIRTUAL
  CODE
  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERADDSET, lLine, lSet)
  END

  RETURN


! =======================================================================================
! CSciControl.MarkerSetAlpha
! purpose:
! inputs :  lMarkerNumber
!           lAlpha
! outputs:
! returns:
! =======================================================================================
CSciControl.MarkerSetAlpha PROCEDURE(LONG lMarkerNumber, LONG lAlpha) !,VIRTUAL
  CODE
  IF SELF.bInitialised
     SELF.SendMessage(SCI_MARKERSETALPHA, lMarkerNumber, lAlpha)
  END

  RETURN


! =======================================================================================
! CSciControl.SetMarginTypeN
! purpose:
! inputs :  lMargin
!           lMarginType
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMarginTypeN  PROCEDURE(LONG lMargin, LONG lMarginType) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMARGINTYPEN, lMargin, lMarginType)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMarginTypeN
! purpose:
! inputs :  lMargin
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetMarginTypeN  PROCEDURE(LONG lMargin) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMARGINTYPEN, lMargin, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMarginWidthN
! purpose:
! inputs :  lMargin
!           lPixelWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMarginWidthN PROCEDURE(LONG lMargin, LONG lPixelWidth) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMARGINWIDTHN, lMargin, lPixelWidth)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMarginWidthN
! purpose:
! inputs :  lMargin
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetMarginWidthN PROCEDURE(LONG lMargin) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMARGINWIDTHN, lMargin, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMarginMaskN
! purpose:
! inputs :  lMargin
!           lMask
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMarginMaskN  PROCEDURE(LONG lMargin, LONG lMask) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMARGINMASKN, lMargin, lMask)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMarginMaskN
! purpose:
! inputs :  lMargin
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetMarginMaskN  PROCEDURE(LONG lMargin) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMARGINMASKN, lMargin, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMarginSensitiveN
! purpose:
! inputs :  lMargin
!           bSensitive
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMarginSensitiveN PROCEDURE(LONG lMargin, BOOL bSensitive) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMARGINSENSITIVEN, lMargin, bSensitive)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMarginSensitiveN
! purpose:
! inputs :  lMargin
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetMarginSensitiveN PROCEDURE(LONG lMargin) !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMARGINSENSITIVEN, lMargin, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMarginCursorN
! purpose:
! inputs : lMargin
!          lCursor
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMarginCursorN  PROCEDURE(LONG lMargin, LONG lCursor) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMARGINCURSORN, lMargin, lCursor)
  END

  RETURN

! =======================================================================================
! CSciControl.GetMarginCursorN
! purpose:
! inputs : lMargin
! outputs:
! returns: MarginCursorN
! =======================================================================================
CSciControl.GetMarginCursorN  PROCEDURE(LONG lMargin) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMARGINCURSORN, lMargin, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                       ! Exit Procedure

! =======================================================================================
! CSciControl.StyleClearAll
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleClearAll   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLECLEARALL, 0, 0)
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.StyleSetFore
! purpose:
! inputs :  lStyle
!           lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetFore    PROCEDURE(LONG lStyle, LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETFORE, lStyle, lFore)
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.StyleGetFore
! purpose:
! inputs :  lStyle
! outputs:  lFore
! returns:
! =======================================================================================
CSciControl.StyleGetFore    PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETFORE, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure

! =======================================================================================
! CSciControl.StyleSetBack
! purpose:
! inputs :  lStyle
!           lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetBack    PROCEDURE(LONG lStyle, LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETBACK, lStyle, lBack)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetBack
! purpose:
! inputs :  lStyle
! outputs:  lBack
! returns:
! =======================================================================================
CSciControl.StyleGetBack    PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETBACK, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetBold
! purpose:
! inputs :  lStyle
!           bBold
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetBold    PROCEDURE(LONG lStyle, BOOL bBold) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETBOLD, lStyle, bBold)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetBold
! purpose:
! inputs :  lStyle
! outputs:  bold
! returns:
! =======================================================================================
CSciControl.StyleGetBold    PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    BOOL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETBOLD, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetItalic
! purpose:
! inputs :  lStyle
!           bItalic
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetItalic  PROCEDURE(LONG lStyle, BOOL bItalic) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETITALIC, lStyle, bItalic)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetItalic
! purpose:
! inputs :  lStyle
! outputs:  italic
! returns:
! =======================================================================================
CSciControl.StyleGetItalic  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    BOOL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETITALIC, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetSize
! purpose:
! inputs :  lStyle
!           lSizePoints
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetSize    PROCEDURE(LONG lStyle, LONG lSizePoints) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETSIZE, lStyle, lSizePoints)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetSize
! purpose:
! inputs :  lStyle
! outputs:  Font Size
! returns:
! =======================================================================================
CSciControl.StyleGetSize    PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETSIZE, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetFont
! purpose:
! inputs :  lStyle
!           szFontName
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetFont    PROCEDURE(LONG lStyle, *CSTRING szFontName) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETFONT, lStyle, ADDRESS(szFontName))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetFont
! purpose:
! inputs :  lStyle
!           szFontName
! outputs:  szFontName
! returns:
! =======================================================================================
CSciControl.StyleGetFont    PROCEDURE(LONG lStyle, *CSTRING szFontName) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLEGETFONT, lStyle, ADDRESS(szFontName))
  ELSE
     szFontName = ''
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.StyleSetEOLFilled
! purpose:
! inputs :  lStyle
!           bFilled
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetEOLFilled   PROCEDURE(LONG lStyle, BOOL bFilled) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETEOLFILLED, lStyle, bFilled)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetEOLFilled
! purpose:
! inputs :  lStyle
! outputs:  EOL Filled
! returns:
! =======================================================================================
CSciControl.StyleGetEOLFilled  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    BOOL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETEOLFILLED, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleResetDefault
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleResetDefault   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLERESETDEFAULT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetUnderline
! purpose:
! inputs :  lStyle
!           bUnderline
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetUnderline   PROCEDURE(LONG lStyle, BOOL bUnderline) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETUNDERLINE, lStyle, bUnderline)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetUnderline
! purpose:
! inputs :  lStyle
! outputs:  Underline
! returns:
! =======================================================================================
CSciControl.StyleGetUnderline  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    BOOL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETUNDERLINE, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetCase
! purpose:
! inputs :  lStyle
!           lCaseForce
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetCase    PROCEDURE(LONG lStyle, LONG lCaseForce) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETCASE, lStyle, lCaseForce)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetCase
! purpose:
! inputs :  lStyle
! outputs:  Case
! returns:
! =======================================================================================
CSciControl.StyleGetCase  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETCASE, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetSizeFractional
! purpose:  Set the size of characters of a style. Size is in points multiplied by 100.
! inputs :  lStyle
!           lSize
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetSizeFractional  PROCEDURE(LONG lStyle, LONG lSize) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETSIZEFRACTIONAL, lStyle, lSize)
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.StyleGetSizeFractional
! purpose:  Get the size of characters of a style in points multiplied by 100
! inputs :  lStyle
! outputs:
! returns:  lSize
! =======================================================================================
CSciControl.StyleGetSizeFractional  PROCEDURE(LONG lStyle) !,LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETCASE, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetWeight
! purpose:
! inputs :  lStyle
!           lWeight
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetWeight PROCEDURE(LONG lStyle, LONG lWeight) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETWEIGHT, lStyle, lWeight)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetWeight
! purpose:
! inputs :  lStyle
! outputs:
! returns:  weight
! =======================================================================================
CSciControl.StyleGetWeight PROCEDURE(LONG lStyle) !,LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETWEIGHT, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetCharacterSet
! purpose:
! inputs :  lStyle
!           lCharacterSet
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetCharacterSet    PROCEDURE(LONG lStyle, LONG lCharacterSet) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETCHARACTERSET, lStyle, lCharacterSet)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetCharacterSet
! purpose:
! inputs :  lStyle
! outputs:  CharacterSet
! returns:
! =======================================================================================
CSciControl.StyleGetCharacterSet  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETCHARACTERSET, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetHotSpot
! purpose:
! inputs :  lStyle
!           bHotspot
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetHotSpot PROCEDURE(LONG lStyle, BOOL bHotspot) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETHOTSPOT, lStyle, bHotspot)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetHotSpot
! purpose:
! inputs :  lStyle
! outputs:  HotSpot
! returns:
! =======================================================================================
CSciControl.StyleGetHotSpot  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    BOOL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETHOTSPOT, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetSelFore
! purpose:
! inputs :  bUseSetting
!           lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSelFore  PROCEDURE(BOOL bUseSetting, LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSELFORE, bUseSetting, lFore)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetSelBack
! purpose:
! inputs :  bUseSetting
!           lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSelBack  PROCEDURE(BOOL bUseSetting, LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSELBACK, bUseSetting, lBack)
  END

  RETURN                                    ! Exit Procedure




! =======================================================================================
! CSciControl.GetSelAlpha
! purpose:  Get the alpha of the selection.
! inputs :
! outputs:
! returns:  lAlpha
! =======================================================================================
CSciControl.GetSelAlpha PROCEDURE() !,LONG,VIRTUAL
ReturnValue    LONG

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSELALPHA, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetSelAlpha
! purpose:  Set the alpha of the selection.
! inputs :  lAlpha
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSelAlpha PROCEDURE(LONG lAlpha) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSELALPHA, lAlpha, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetSelEOLFilled
! purpose:  Is the selection end of line filled?
! inputs :
! outputs:
! returns:  bFilled
! =======================================================================================
CSciControl.GetSelEOLFilled   PROCEDURE()  !,BOOL,VIRTUAL

ReturnValue    BOOL(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSELEOLFILLED, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetSelEOLFilled
! purpose:  Set the selection to have its end of line filled or not.
! inputs :  bFilled
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSelEOLFilled   PROCEDURE(BOOL bFilled) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSELEOLFILLED, bFilled, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetCaretFore
! purpose:
! inputs :  lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCaretFore    PROCEDURE(LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCARETFORE, lFore, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AssignCmdKey
! purpose:
! inputs :  lKm
!           lMsg
! outputs:
! returns:
! =======================================================================================
CSciControl.AssignCmdKey    PROCEDURE(LONG lKm, LONG lMsg) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ASSIGNCMDKEY, lKm, lMsg)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ClearCmdKey
! purpose:
! inputs :  lKm
! outputs:
! returns:
! =======================================================================================
CSciControl.ClearCmdKey PROCEDURE(LONG lKm) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CLEARCMDKEY, lKm, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ClearAllCmdKeys
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ClearAllCmdKeys PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CLEARALLCMDKEYS, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetStylingEx
! purpose:
! inputs :  lLength
!           szStyles
! outputs:
! returns:
! =======================================================================================
CSciControl.SetStylingEx    PROCEDURE(LONG lLength, *CSTRING szStyles) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSTYLINGEX, lLength, ADDRESS(szStyles))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetVisible
! purpose:
! inputs :  lStyle
!           bVisible
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetVisible PROCEDURE(LONG lStyle, BOOL bVisible) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETVISIBLE, lStyle, bVisible)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleGetVisible
! purpose:
! inputs :  lStyle
! outputs:
! returns:  Visible
! =======================================================================================
CSciControl.StyleGetVisible  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    BOOL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETVISIBLE, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.GetCaretPeriod
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCaretPeriod  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCARETPERIOD, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetCaretPeriod
! purpose:
! inputs :  lPeriodMilliseconds
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCaretPeriod  PROCEDURE(LONG lPeriodMilliseconds) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCARETPERIOD, lPeriodMilliseconds, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetWordChars
! purpose:
! inputs :
!           szCharacters
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWordChars    PROCEDURE(*CSTRING szCharacters) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWORDCHARS, 0, ADDRESS(szCharacters))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.BeginUndoAction
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.BeginUndoAction PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_BEGINUNDOACTION, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.EndUndoAction
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.EndUndoAction   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ENDUNDOACTION, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.IndicSetStyle
! purpose:
! inputs :  lIndic
!           lStyle
! outputs:
! returns:
! =======================================================================================
CSciControl.IndicSetStyle   PROCEDURE(LONG lIndic, LONG lStyle) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_INDICSETSTYLE, lIndic, lStyle)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.IndicGetStyle
! purpose:
! inputs :  lIndic
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.IndicGetStyle   PROCEDURE(LONG lIndic) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_INDICGETSTYLE, lIndic, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.IndicSetFore
! purpose:
! inputs :  lIndic
!           lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.IndicSetFore    PROCEDURE(LONG lIndic, LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_INDICSETFORE, lIndic, lFore)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.IndicGetFore
! purpose:
! inputs :  lIndic
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.IndicGetFore    PROCEDURE(LONG lIndic) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_INDICGETFORE, lIndic, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.IndicSetUnder
! purpose:  Set an indicator to draw under text or over(default).
! inputs :  lIndic
!           bUnder
! outputs:
! returns:
! =======================================================================================
CSciControl.IndicSetUnder  PROCEDURE(LONG lIndic, BOOL bUnder) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_INDICSETUNDER, lIndic, bUnder)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.IndicGetUnder
! purpose:  Retrieve whether indicator drawn under or over text.
! inputs :  lIndic
! outputs:
! returns:  bUnder
! =======================================================================================
CSciControl.IndicGetUnder  PROCEDURE(LONG lIndic)  !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_INDICGETUNDER, lIndic, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetWhitespaceFore
! purpose:
! inputs :  bUseSetting
!           lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWhitespaceFore   PROCEDURE(BOOL bUseSetting, LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWHITESPACEFORE, bUseSetting, lFore)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetWhitespaceBack
! purpose:
! inputs :  bUseSetting
!           lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWhitespaceBack   PROCEDURE(BOOL bUseSetting, LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWHITESPACEBACK, bUseSetting, lBack)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetWhitespaceSize
! purpose:  Set the size of the dots used to mark space characters.
! inputs :  lSize
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWhitespaceSize PROCEDURE(LONG lSize)   !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWHITESPACESIZE, lSize, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetWhitespaceSize
! purpose:  Get the size of the dots used to mark space characters.
! inputs :
! outputs:
! returns:  WhitespaceSize
! =======================================================================================
CSciControl.GetWhitespaceSize PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETWHITESPACESIZE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetStyleBits
! purpose:
! inputs :  lBits
! outputs:
! returns:
! =======================================================================================
CSciControl.SetStyleBits    PROCEDURE(LONG lBits) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSTYLEBITS, lBits, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetStyleBits
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetStyleBits    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSTYLEBITS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetLineState
! purpose:
! inputs :  lLine
!           lState
! outputs:
! returns:
! =======================================================================================
CSciControl.SetLineState    PROCEDURE(LONG lLine, LONG lState) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETLINESTATE, lLine, lState)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetLineState
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLineState    PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLINESTATE, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetMaxLineState
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetMaxLineState PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMAXLINESTATE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetCaretLineVisible
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetCaretLineVisible PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCARETLINEVISIBLE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetCaretLineVisible
! purpose:
! inputs :  bShow
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCaretLineVisible PROCEDURE(BOOL bShow) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCARETLINEVISIBLE, bShow, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetCaretLineBack
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCaretLineBack    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCARETLINEBACK, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetCaretLineBack
! purpose:
! inputs :  lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCaretLineBack    PROCEDURE(LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCARETLINEBACK, lBack, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StyleSetChangeable
! purpose:
! inputs :  lStyle
!           bChangeable
! outputs:
! returns:
! =======================================================================================
CSciControl.StyleSetChangeable  PROCEDURE(LONG lStyle, BOOL bChangeable) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STYLESETCHANGEABLE, lStyle, bChangeable)
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.StyleGetChangeable
! purpose:  Get is a style changeable or not (read only).
!           Experimental feature, currently buggy.
! inputs :  lStyle
! outputs:
! returns:  Changeable
! =======================================================================================
CSciControl.StyleGetChangeable  PROCEDURE(LONG lStyle) !LONG,VIRTUAL
ReturnValue    BOOL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_STYLEGETCHANGEABLE, lStyle, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN  ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCShow
! purpose:
! inputs :  lLenEntered
!           szItemList
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCShow   PROCEDURE(LONG lLenEntered, *CSTRING szItemList) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSHOW, lLenEntered, ADDRESS(szItemList))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCCancel
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCCancel PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCCANCEL, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCActive
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.AutoCActive PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCACTIVE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCPosStart
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.AutoCPosStart   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCPOSSTART, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCComplete
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCComplete   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCCOMPLETE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCStops
! purpose:
! inputs :
!           szCharacterSet
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCStops  PROCEDURE(*CSTRING szCharacterSet) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSTOPS, 0, ADDRESS(szCharacterSet))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetSeparator
! purpose:
! inputs :  lSeparatorCharacter
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetSeparator   PROCEDURE(LONG lSeparatorCharacter) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETSEPARATOR, lSeparatorCharacter, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetSeparator
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.AutoCGetSeparator   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETSEPARATOR, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSelect
! purpose:
! inputs :
!           szText
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSelect PROCEDURE(*CSTRING szText) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSELECT, 0, ADDRESS(szText))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetCancelAtStart
! purpose:
! inputs :  bCancel
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetCancelAtStart   PROCEDURE(BOOL bCancel) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETCANCELATSTART, bCancel, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetCancelAtStart
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.AutoCGetCancelAtStart   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETCANCELATSTART, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetFillUps
! purpose:
! inputs :
!           szCharacterSet
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetFillUps PROCEDURE(*CSTRING szCharacterSet) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETFILLUPS, 0, ADDRESS(szCharacterSet))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetChooseSingle
! purpose:
! inputs :  bChooseSingle
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetChooseSingle    PROCEDURE(BOOL bChooseSingle) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETCHOOSESINGLE, bChooseSingle, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetChooseSingle
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.AutoCGetChooseSingle    PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETCHOOSESINGLE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetIgnoreCase
! purpose:
! inputs :  bIgnoreCase
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetIgnoreCase  PROCEDURE(BOOL bIgnoreCase) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETIGNORECASE, bIgnoreCase, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetIgnoreCase
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.AutoCGetIgnoreCase  PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETIGNORECASE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.UserListShow
! purpose:
! inputs :  lListType
!           szItemList
! outputs:
! returns:
! =======================================================================================
CSciControl.UserListShow    PROCEDURE(LONG lListType, *CSTRING szItemList) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_USERLISTSHOW, lListType, ADDRESS(szItemList))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetAutoHide
! purpose:
! inputs :  bAutoHide
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetAutoHide    PROCEDURE(BOOL bAutoHide) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETAUTOHIDE, bAutoHide, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetAutoHide
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.AutoCGetAutoHide    PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETAUTOHIDE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetDropRestOfWord
! purpose:
! inputs :  bDropRestOfWord
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetDropRestOfWord  PROCEDURE(BOOL bDropRestOfWord) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETDROPRESTOFWORD, bDropRestOfWord, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetDropRestOfWord
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.AutoCGetDropRestOfWord  PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETDROPRESTOFWORD, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.RegisterImage
! purpose:
! inputs :  lType
!           szXpmData
! outputs:
! returns:
! =======================================================================================
CSciControl.RegisterImage   PROCEDURE(LONG lType, LONG lpXpmData) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_REGISTERIMAGE, lType, lpXpmData)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ClearRegisteredImages
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ClearRegisteredImages   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CLEARREGISTEREDIMAGES, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetTypeSeparator
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.AutoCGetTypeSeparator   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETTYPESEPARATOR, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetTypeSeparator
! purpose:
! inputs :  lSeparatorCharacter
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetTypeSeparator   PROCEDURE(LONG lSeparatorCharacter) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETTYPESEPARATOR, lSeparatorCharacter, 0)
  END

  RETURN                                    ! Exit Procedure






! =======================================================================================
! CSciControl.AutoCSetMaxWidth
! purpose:  Set the maximum width, in characters, of auto-completion and user lists.
!           Set to 0 to autosize to fit longest item, which is the default.
! inputs :  lCharacterCount
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetMaxWidth  PROCEDURE(LONG lCharacterCount) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETMAXWIDTH, lCharacterCount, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetMaxWidth
! purpose:  Get the maximum width, in characters, of auto-completion and user lists.
! inputs :
! outputs:
! returns:  lCharacterCount
! =======================================================================================
CSciControl.AutoCGetMaxWidth  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETMAXWIDTH, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCSetMaxHeight
! purpose:  Set the maximum height, in rows, of auto-completion and user lists.
!           The default is 5 rows.
! inputs :  lRowCount
! outputs:
! returns:
! =======================================================================================
CSciControl.AutoCSetMaxHeight PROCEDURE(LONG lRowCount) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AUTOCSETMAXHEIGHT, lRowCount, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.AutoCGetMaxHeight
! purpose:  Get the maximum height, in rows, of auto-completion and user lists.
! inputs :
! outputs:
! returns:  lRowCount
! =======================================================================================
CSciControl.AutoCGetMaxHeight PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AUTOCGETMAXHEIGHT, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetIndent
! purpose:
! inputs :  lIndentSize
! outputs:
! returns:
! =======================================================================================
CSciControl.SetIndent   PROCEDURE(LONG lIndentSize) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETINDENT, lIndentSize, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetIndent
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetIndent   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETINDENT, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetUseTabs
! purpose:
! inputs :  bUseTabs
! outputs:
! returns:
! =======================================================================================
CSciControl.SetUseTabs  PROCEDURE(BOOL bUseTabs) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETUSETABS, bUseTabs, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetUseTabs
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetUseTabs  PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETUSETABS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetLineIndentation
! purpose:
! inputs :  lLine
!           lIndentSize
! outputs:
! returns:
! =======================================================================================
CSciControl.SetLineIndentation  PROCEDURE(LONG lLine, LONG lIndentSize) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETLINEINDENTATION, lLine, lIndentSize)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetLineIndentation
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLineIndentation  PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLINEINDENTATION, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetLineIndentPosition
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLineIndentPosition   PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLINEINDENTPOSITION, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetColumn
! purpose:
! inputs :  lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetColumn   PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCOLUMN, lPos, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetHScrollBar
! purpose:
! inputs :  bShow
! outputs:
! returns:
! =======================================================================================
CSciControl.SetHScrollBar   PROCEDURE(BOOL bShow) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETHSCROLLBAR, bShow, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetHScrollBar
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetHScrollBar   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETHSCROLLBAR, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetIndentationGuides
! purpose:
! inputs :  bShow
! outputs:
! returns:
! =======================================================================================
CSciControl.SetIndentationGuides    PROCEDURE(BOOL bShow) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETINDENTATIONGUIDES, bShow, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetIndentationGuides
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetIndentationGuides    PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETINDENTATIONGUIDES, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetHighlightGuide
! purpose:
! inputs :  lColumn
! outputs:
! returns:
! =======================================================================================
CSciControl.SetHighlightGuide   PROCEDURE(LONG lColumn) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETHIGHLIGHTGUIDE, lColumn, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetHighlightGuide
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetHighlightGuide   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETHIGHLIGHTGUIDE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetLineEndPosition
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLineEndPosition  PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLINEENDPOSITION, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetCodePage
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCodePage PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCODEPAGE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetCaretFore
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCaretFore    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCARETFORE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetUsePalette
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetUsePalette   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETUSEPALETTE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetReadOnly
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetReadOnly PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETREADONLY, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetCurrentPos
! purpose:
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCurrentPos   PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCURRENTPOS, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetSelectionStart
! purpose:
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSelectionStart   PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSELECTIONSTART, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetSelectionStart
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetSelectionStart   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSELECTIONSTART, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetSelectionEnd
! purpose:
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSelectionEnd PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSELECTIONEND, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetSelectionEnd
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetSelectionEnd PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSELECTIONEND, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetEmptySelection
! purpose:  Set caret to a position, while removing any existing selection.
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.SetEmptySelection PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETEMPTYSELECTION, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetPrintMagnification
! purpose:
! inputs :  lMagnification
! outputs:
! returns:
! =======================================================================================
CSciControl.SetPrintMagnification   PROCEDURE(LONG lMagnification) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETPRINTMAGNIFICATION, lMagnification, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetPrintMagnification
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetPrintMagnification   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETPRINTMAGNIFICATION, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetPrintColourMode
! purpose:
! inputs :  lMode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetPrintColourMode  PROCEDURE(LONG lMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETPRINTCOLOURMODE, lMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetPrintColourMode
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetPrintColourMode  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETPRINTCOLOURMODE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.FindText
! purpose:
! inputs :  lFlags
!           ft
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.FindText    PROCEDURE(LONG lFlags, *findtext ft) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_FINDTEXT, lFlags, ADDRESS(ft))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.FormatRange
! purpose:
! inputs :  bDraw
!           fr
! outputs:
! returns:
! =======================================================================================
CSciControl.FormatRange PROCEDURE(BOOL bDraw, *Sci_RangeToFormat fr) !,LONG,PROC,VIRTUAL

ReturnValue LONG(0)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_FORMATRANGE, bDraw, ADDRESS(fr))
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.GetFirstVisibleLine
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetFirstVisibleLine PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETFIRSTVISIBLELINE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetLine
! purpose:
! inputs :  lLine
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLine PROCEDURE(LONG lLine, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLINE, lLine, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetLineCount
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLineCount    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLINECOUNT, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMarginLeft
! purpose:
! inputs :
!           lPixelWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMarginLeft   PROCEDURE(LONG lPixelWidth) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMARGINLEFT, 0, lPixelWidth)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMarginLeft
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetMarginLeft   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMARGINLEFT, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMarginRight
! purpose:
! inputs :
!           lPixelWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMarginRight  PROCEDURE(LONG lPixelWidth) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMARGINRIGHT, 0, lPixelWidth)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMarginRight
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetMarginRight  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMARGINRIGHT, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetModify
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetModify   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMODIFY, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetSel
! purpose:
! inputs :  lStart
!           lEnd
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSel  PROCEDURE(LONG lStart, LONG lEnd) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSEL, lStart, lEnd)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetSelText
! purpose:
! inputs :
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetSelText  PROCEDURE(*CSTRING szText) !,LONG,VIRTUAL,PROC

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSELTEXT, 0, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetTextRange
! purpose:
! inputs :
!           tr
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetTextRange    PROCEDURE(*Sci_TextRange tr) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTEXTRANGE, 0, ADDRESS(tr))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.HideSelection
! purpose:
! inputs :  bNormal
! outputs:
! returns:
! =======================================================================================
CSciControl.HideSelection   PROCEDURE(BOOL bNormal) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HIDESELECTION, bNormal, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.PointXFromPosition
! purpose:
! inputs :
!           lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.PointXFromPosition  PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_POINTXFROMPOSITION, 0, lPos)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.PointYFromPosition
! purpose:
! inputs :
!           lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.PointYFromPosition  PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_POINTYFROMPOSITION, 0, lPos)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.LineFromPosition
! purpose:
! inputs :  lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.LineFromPosition    PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_LINEFROMPOSITION, lPos, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.PositionFromLine
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.PositionFromLine    PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_POSITIONFROMLINE, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.LineScroll
! purpose:
! inputs :  lColumns
!           lLines
! outputs:
! returns:
! =======================================================================================
CSciControl.LineScroll  PROCEDURE(LONG lColumns, LONG lLines) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINESCROLL, lColumns, lLines)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ScrollCaret
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ScrollCaret PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SCROLLCARET, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ReplaceSel
! purpose:
! inputs :
!           szText
! outputs:
! returns:
! =======================================================================================
CSciControl.ReplaceSel  PROCEDURE(*CSTRING szText) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_REPLACESEL, 0, ADDRESS(szText))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetReadOnly
! purpose:
! inputs :  bReadOnly
! outputs:
! returns:
! =======================================================================================
CSciControl.SetReadOnly PROCEDURE(BOOL bReadOnly) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETREADONLY, bReadOnly, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl._Null
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl._Null   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_NULL, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CanPaste
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.CanPaste    PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CANPASTE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.CanUndo
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.CanUndo PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CANUNDO, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.EmptyUndoBuffer
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.EmptyUndoBuffer PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_EMPTYUNDOBUFFER, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Undo
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Undo    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_UNDO, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Cut
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Cut PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CUT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Copy
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Copy    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_COPY, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Paste
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Paste   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PASTE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ClearClipboard
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ClearClipboard  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CLEAR, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetText
! purpose:
! inputs :
!           szText
! outputs:
! returns:
! =======================================================================================
CSciControl.SetText PROCEDURE(*CSTRING szText) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETTEXT, 0, ADDRESS(szText))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetText
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetText PROCEDURE(LONG lLength, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTEXT, lLength, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetTextLength
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetTextLength   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTEXTLENGTH, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetDirectFunction
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetDirectFunction   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETDIRECTFUNCTION, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetDirectPointer
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetDirectPointer    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETDIRECTPOINTER, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetOvertype
! purpose:
! inputs :  bOvertype
! outputs:
! returns:
! =======================================================================================
CSciControl.SetOvertype PROCEDURE(BOOL bOvertype) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETOVERTYPE, bOvertype, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetOvertype
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetOvertype PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETOVERTYPE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetCaretWidth
! purpose:
! inputs :  lPixelWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCaretWidth   PROCEDURE(LONG lPixelWidth) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCARETWIDTH, lPixelWidth, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetCaretWidth
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCaretWidth   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCARETWIDTH, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetTargetStart
! purpose:
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.SetTargetStart  PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETTARGETSTART, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetTargetStart
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetTargetStart  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTARGETSTART, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetTargetEnd
! purpose:
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.SetTargetEnd    PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETTARGETEND, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetTargetEnd
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetTargetEnd    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTARGETEND, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.ReplaceTarget
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.ReplaceTarget   PROCEDURE(LONG lLength, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_REPLACETARGET, lLength, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.ReplaceTargetRE
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.ReplaceTargetRE PROCEDURE(LONG lLength, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_REPLACETARGETRE, lLength, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SearchInTarget
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.SearchInTarget  PROCEDURE(LONG lLength, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_SEARCHINTARGET, lLength, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetSearchFlags
! purpose:
! inputs :  lFlags
! outputs:
! returns:
! =======================================================================================
CSciControl.SetSearchFlags  PROCEDURE(LONG lFlags) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSEARCHFLAGS, lFlags, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetSearchFlags
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetSearchFlags  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSEARCHFLAGS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipShow
! purpose:
! inputs :  lPos
!           szDefinition
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipShow PROCEDURE(LONG lPos, *CSTRING szDefinition) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPSHOW, lPos, ADDRESS(szDefinition))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipCancel
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipCancel   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPCANCEL, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipActive
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.CallTipActive   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CALLTIPACTIVE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipPosStart
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.CallTipPosStart PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CALLTIPPOSSTART, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipSetHlt
! purpose:
! inputs :  lStart
!           lEnd
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipSetHlt   PROCEDURE(LONG lStart, LONG lEnd) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPSETHLT, lStart, lEnd)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipSetBack
! purpose:
! inputs :  lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipSetBack  PROCEDURE(LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPSETBACK, lBack, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipSetFore
! purpose:
! inputs :  lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipSetFore  PROCEDURE(LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPSETFORE, lFore, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipSetForeHlt
! purpose:
! inputs :  lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipSetForeHlt   PROCEDURE(LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPSETFOREHLT, lFore, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipUseStyle
! purpose:  Enable use of STYLE_CALLTIP and set call tip tab size in pixels.
! inputs :  lTabSize
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipUseStyle   PROCEDURE(LONG lTabSize) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPUSESTYLE, lTabSize, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CallTipSetPosition
! purpose:  Set position of calltip, above or below text.
! inputs :  bAbove
! outputs:
! returns:
! =======================================================================================
CSciControl.CallTipSetPosition   PROCEDURE(BOOL bAbove) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CALLTIPSETPOSITION, bAbove, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.VisibleFromDocLine
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.VisibleFromDocLine  PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_VISIBLEFROMDOCLINE, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.DocLineFromVisible
! purpose:
! inputs :  lLineDisplay
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.DocLineFromVisible  PROCEDURE(LONG lLineDisplay) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_DOCLINEFROMVISIBLE, lLineDisplay, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.WrapCount
! purpose:  The number of display lines needed to wrap a document line
! inputs :  lLine
! outputs:
! returns:  WrapCount
! =======================================================================================
CSciControl.WrapCount   PROCEDURE(LONG lLine) !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_WRAPCOUNT, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetFoldLevel
! purpose:
! inputs :  lLine
!           lLevel
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFoldLevel    PROCEDURE(LONG lLine, LONG lLevel) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFOLDLEVEL, lLine, lLevel)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetFoldLevel
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetFoldLevel    PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETFOLDLEVEL, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetLastChild
! purpose:
! inputs :  lLine
!           lLevel
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLastChild    PROCEDURE(LONG lLine, LONG lLevel) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLASTCHILD, lLine, lLevel)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetFoldParent
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetFoldParent   PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETFOLDPARENT, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.ShowLines
! purpose:
! inputs :  lLineStart
!           lLineEnd
! outputs:
! returns:
! =======================================================================================
CSciControl.ShowLines   PROCEDURE(LONG lLineStart, LONG lLineEnd) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SHOWLINES, lLineStart, lLineEnd)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.HideLines
! purpose:
! inputs :  lLineStart
!           lLineEnd
! outputs:
! returns:
! =======================================================================================
CSciControl.HideLines   PROCEDURE(LONG lLineStart, LONG lLineEnd) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HIDELINES, lLineStart, lLineEnd)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetLineVisible
! purpose:  Is a line visible?
! inputs :  lLine
! outputs:
! returns:  True or False
! =======================================================================================
CSciControl.GetLineVisible  PROCEDURE(LONG lLine) !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLINEVISIBLE, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.GetAllLinesVisible
! purpose:  Are all lines visible?
! inputs :
! outputs:
! returns:  True or False
! =======================================================================================
CSciControl.GetAllLinesVisible  PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETALLLINESVISIBLE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetFoldExpanded
! purpose:
! inputs :  lLine
!           bExpanded
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFoldExpanded PROCEDURE(LONG lLine, BOOL bExpanded) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFOLDEXPANDED, lLine, bExpanded)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetFoldExpanded
! purpose:
! inputs :  lLine
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetFoldExpanded PROCEDURE(LONG lLine) !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETFOLDEXPANDED, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.ToggleFold
! purpose:
! inputs :  lLine
! outputs:
! returns:
! =======================================================================================
CSciControl.ToggleFold  PROCEDURE(LONG lLine) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_TOGGLEFOLD, lLine, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.EnsureVisible
! purpose:
! inputs :  lLine
! outputs:
! returns:
! =======================================================================================
CSciControl.EnsureVisible   PROCEDURE(LONG lLine) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ENSUREVISIBLE, lLine, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetFoldFlags
! purpose:
! inputs :  lFlags
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFoldFlags    PROCEDURE(LONG lFlags) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFOLDFLAGS, lFlags, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.EnsureVisibleEnforcePolicy
! purpose:
! inputs :  lLine
! outputs:
! returns:
! =======================================================================================
CSciControl.EnsureVisibleEnforcePolicy  PROCEDURE(LONG lLine) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ENSUREVISIBLEENFORCEPOLICY, lLine, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetTabIndents
! purpose:
! inputs :  bTabIndents
! outputs:
! returns:
! =======================================================================================
CSciControl.SetTabIndents   PROCEDURE(BOOL bTabIndents) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETTABINDENTS, bTabIndents, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetTabIndents
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetTabIndents   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTABINDENTS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetBackSpaceUnIndents
! purpose:
! inputs :  bBsUnIndents
! outputs:
! returns:
! =======================================================================================
CSciControl.SetBackSpaceUnIndents   PROCEDURE(BOOL bBsUnIndents) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETBACKSPACEUNINDENTS, bBsUnIndents, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetBackSpaceUnIndents
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetBackSpaceUnIndents   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETBACKSPACEUNINDENTS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMouseDwellTime
! purpose:
! inputs :  lPeriodMilliseconds
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMouseDwellTime   PROCEDURE(LONG lPeriodMilliseconds) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMOUSEDWELLTIME, lPeriodMilliseconds, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMouseDwellTime
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetMouseDwellTime   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMOUSEDWELLTIME, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.WordStartPosition
! purpose:
! inputs :  lPos
!           bOnlyWordCharacters
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.WordStartPosition   PROCEDURE(LONG lPos, BOOL bOnlyWordCharacters) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_WORDSTARTPOSITION, lPos, bOnlyWordCharacters)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.WordEndPosition
! purpose:
! inputs :  lPos
!           bOnlyWordCharacters
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.WordEndPosition PROCEDURE(LONG lPos, BOOL bOnlyWordCharacters) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_WORDENDPOSITION, lPos, bOnlyWordCharacters)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetWrapMode
! purpose:
! inputs :  lMode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWrapMode PROCEDURE(LONG lMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWRAPMODE, lMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetWrapMode
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetWrapMode PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETWRAPMODE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetWrapVisualFlags
! purpose:  Set the display mode of visual flags for wrapped lines.
! inputs :  lWrapVisualFlags
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWrapVisualFlags   PROCEDURE(LONG lWrapVisualFlags) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWRAPVISUALFLAGS, lWrapVisualFlags, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetWrapVisualFlags
! purpose:  Retrive the display mode of visual flags for wrapped lines.
! inputs :
! outputs:
! returns:  lWrapVisualFlags
! =======================================================================================
CSciControl.GetWrapVisualFlags   PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETWRAPVISUALFLAGS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetWrapVisualFlagsLocation
! purpose:  Set the location of visual flags for wrapped lines.
! inputs :  lWrapVisualFlagsLocation
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWrapVisualFlagsLocation PROCEDURE(LONG lWrapVisualFlagsLocation) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWRAPVISUALFLAGSLOCATION, lWrapVisualFlagsLocation, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetWrapVisualFlagsLocation
! purpose:  Retrive the location of visual flags for wrapped lines.
! inputs :
! outputs:
! returns:  lWrapVisualFlagsLocation
! =======================================================================================
CSciControl.GetWrapVisualFlagsLocation PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETWRAPVISUALFLAGSLOCATION, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetWrapStartIndent
! purpose:  Set the start indent for wrapped lines.
! inputs :  lIndent
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWrapStartIndent   PROCEDURE(LONG lIndent) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWRAPSTARTINDENT, lIndent, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetWrapStartIndent
! purpose:  Retrive the start indent for wrapped lines.
! inputs :
! outputs:
! returns:  lIndent
! =======================================================================================
CSciControl.GetWrapStartIndent   PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETWRAPSTARTINDENT, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetWrapIndentMode
! purpose:  Sets how wrapped sublines are placed. Default is fixed.
! inputs :  lMode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetWrapIndentMode    PROCEDURE(LONG lMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETWRAPINDENTMODE, lMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetWrapIndentMode
! purpose:  Retrieve how wrapped sublines are placed. Default is fixed.
! inputs :
! outputs:
! returns:  lMode
! =======================================================================================
CSciControl.GetWrapIndentMode    PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETWRAPINDENTMODE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetLayoutCache
! purpose:
! inputs :  lMode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetLayoutCache PROCEDURE(LONG lMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETLAYOUTCACHE, lMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetLayoutCache
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLayoutCache  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLAYOUTCACHE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetScrollWidth
! purpose:
! inputs :  lPixelWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.SetScrollWidth  PROCEDURE(LONG lPixelWidth) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSCROLLWIDTH, lPixelWidth, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetScrollWidth
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetScrollWidth  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSCROLLWIDTH, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetScrollWidthTracking
! purpose:
! inputs :  lPixelWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.SetScrollWidthTracking  PROCEDURE(BOOL tracking) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSCROLLWIDTHTRACKING, tracking, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetScrollWidthTracking
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetScrollWidthTracking  PROCEDURE() !,LONG,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSCROLLWIDTHTRACKING, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.TextWidth
! purpose:
! inputs :  lStyle
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.TextWidth   PROCEDURE(LONG lStyle, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_TEXTWIDTH, lStyle, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetEndAtLastLine
! purpose:
! inputs :  bEndAtLastLine
! outputs:
! returns:
! =======================================================================================
CSciControl.SetEndAtLastLine    PROCEDURE(BOOL bEndAtLastLine) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETENDATLASTLINE, bEndAtLastLine, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetEndAtLastLine
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetEndAtLastLine    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETENDATLASTLINE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.TextHeight
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.TextHeight  PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_TEXTHEIGHT, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetVScrollBar
! purpose:
! inputs :  bShow
! outputs:
! returns:
! =======================================================================================
CSciControl.SetVScrollBar   PROCEDURE(BOOL bShow) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETVSCROLLBAR, bShow, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetVScrollBar
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetVScrollBar   PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETVSCROLLBAR, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AppendText
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:
! =======================================================================================
CSciControl.AppendText  PROCEDURE(LONG lLength, *CSTRING szText) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_APPENDTEXT, lLength, ADDRESS(szText))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetTwoPhaseDraw
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetTwoPhaseDraw PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTWOPHASEDRAW, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetTwoPhaseDraw
! purpose:
! inputs :  bTwoPhase
! outputs:
! returns:
! =======================================================================================
CSciControl.SetTwoPhaseDraw PROCEDURE(BOOL bTwoPhase) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETTWOPHASEDRAW, bTwoPhase, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetFontQuality
! purpose:  Choose the quality level for text from the FontQuality enumeration.
! inputs :  lFontQuality
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFontQuality PROCEDURE(LONG lFontQuality) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFONTQUALITY, lFontQuality, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetFontQuality
! purpose:  Retrieve the quality level for text.
! inputs :
! outputs:
! returns:  lFontQuality
! =======================================================================================
CSciControl.GetFontQuality PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETFONTQUALITY, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetFirstVisibleLine
! purpose:  Scroll so that a display line is at the top of the display.
! inputs :  lLineDisplay
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFirstVisibleLine  PROCEDURE(LONG lLineDisplay) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFIRSTVISIBLELINE, lLineDisplay, 0)
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.SetMultiPaste
! purpose:  Change the effect of pasting when there are multiple selections.
! inputs :  lMultiPaste
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMultiPaste  PROCEDURE(LONG lMultiPaste) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMULTIPASTE, lMultiPaste, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMultiPaste
! purpose:  Retrieve the effect of pasting when there are multiple selections.
! inputs :
! outputs:
! returns:  lMultiPaste
! =======================================================================================
CSciControl.GetMultiPaste  PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMULTIPASTE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.GetTag
! purpose:  Retrieve the value of a tag from a regular expression search.
! inputs :  lTagNumber
!           szTagValue
! outputs:
! returns:  ??
! =======================================================================================
CSciControl.GetTag   PROCEDURE(LONG lTagNumber, *CSTRING szTagValue) !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTAG, lTagNumber, ADDRESS(szTagValue))
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.TargetFromSelection
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.TargetFromSelection PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_TARGETFROMSELECTION, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LinesJoin
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LinesJoin   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINESJOIN, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LinesSplit
! purpose:
! inputs :  lPixelWidth
! outputs:
! returns:
! =======================================================================================
CSciControl.LinesSplit  PROCEDURE(LONG lPixelWidth) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINESSPLIT, lPixelWidth, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetFoldMarginColour
! purpose:
! inputs :  bUseSetting
!           lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFoldMarginColour PROCEDURE(BOOL bUseSetting, LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFOLDMARGINCOLOUR, bUseSetting, lBack)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetFoldMarginHiColour
! purpose:
! inputs :  bUseSetting
!           lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFoldMarginHiColour   PROCEDURE(BOOL bUseSetting, LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFOLDMARGINHICOLOUR, bUseSetting, lFore)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineDown
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineDown    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEDOWN, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineDownExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineDownExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEDOWNEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineUp
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineUp  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEUP, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineUpExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineUpExtend    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEUPEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CharLeft
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.CharLeft    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CHARLEFT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CharLeftExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.CharLeftExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CHARLEFTEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CharRight
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.CharRight   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CHARRIGHT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CharRightExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.CharRightExtend PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CHARRIGHTEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WordLeft
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordLeft    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDLEFT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WordLeftExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordLeftExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDLEFTEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WordRight
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordRight   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDRIGHT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WordRightExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordRightExtend PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDRIGHTEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Home
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Home    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HOME, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.HomeExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.HomeExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HOMEEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineEnd
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineEnd PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineEndExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineEndExtend   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEENDEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DocumentStart
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DocumentStart   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DOCUMENTSTART, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DocumentStartExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DocumentStartExtend PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DOCUMENTSTARTEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DocumentEnd
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DocumentEnd PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DOCUMENTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DocumentEndExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DocumentEndExtend   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DOCUMENTENDEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.PageUp
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.PageUp  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PAGEUP, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.PageUpExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.PageUpExtend    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PAGEUPEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.PageDown
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.PageDown    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PAGEDOWN, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.PageDownExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.PageDownExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PAGEDOWNEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.EditToggleOvertype
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.EditToggleOvertype  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_EDITTOGGLEOVERTYPE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Cancel
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Cancel  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CANCEL, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DeleteBack
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DeleteBack  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DELETEBACK, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Tab
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Tab PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_TAB, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.BackTab
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.BackTab PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_BACKTAB, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.NewLine
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.NewLine PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_NEWLINE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.FormFeed
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.FormFeed    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_FORMFEED, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.VCHome
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.VCHome  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_VCHOME, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.VCHomeExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.VCHomeExtend    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_VCHOMEEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ZoomIn
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ZoomIn  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ZOOMIN, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ZoomOut
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ZoomOut PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ZOOMOUT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DelWordLeft
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DelWordLeft PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DELWORDLEFT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DelWordRight
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DelWordRight    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DELWORDRIGHT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DelWordRightEnd
! purpose:  Delete the word to the right of the caret, but not the trailing non-word characters.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DelWordRightEnd                 PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DELWORDRIGHTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineCut
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineCut PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINECUT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineDelete
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineDelete  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEDELETE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineTranspose
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineTranspose   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINETRANSPOSE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineDuplicate
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineDuplicate   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEDUPLICATE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LowerCase
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LowerCase   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LOWERCASE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.UpperCase
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.UpperCase   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_UPPERCASE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineScrollDown
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineScrollDown  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     !SELF.SendMessage(SCI_LINESCROLLDOWN, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineScrollUp
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineScrollUp    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     !SELF.SendMessage(SCI_LINESCROLLUP, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DeleteBackNotLine
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DeleteBackNotLine   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DELETEBACKNOTLINE, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.HomeDisplay
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.HomeDisplay PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HOMEDISPLAY, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.HomeDisplayExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.HomeDisplayExtend   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HOMEDISPLAYEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineEndDisplay
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineEndDisplay  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEENDDISPLAY, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineEndDisplayExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineEndDisplayExtend    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEENDDISPLAYEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.HomeWrap
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.HomeWrap    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HOMEWRAP, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.HomeWrapExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.HomeWrapExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HOMEWRAPEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineEndWrap
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineEndWrap PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEENDWRAP, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineEndWrapExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineEndWrapExtend   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINEENDWRAPEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.VCHomeWrap
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.VCHomeWrap  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_VCHOMEWRAP, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.VCHomeWrapExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.VCHomeWrapExtend    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_VCHOMEWRAPEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineCopy
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.LineCopy    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LINECOPY, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.MoveCaretInsideView
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.MoveCaretInsideView PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MOVECARETINSIDEVIEW, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LineLength
! purpose:
! inputs :  lLine
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.LineLength  PROCEDURE(LONG lLine) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_LINELENGTH, lLine, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.BraceHighlight
! purpose:
! inputs :  lPos1
!           lPos2
! outputs:
! returns:
! =======================================================================================
CSciControl.BraceHighlight  PROCEDURE(LONG lPos1, LONG lPos2) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_BRACEHIGHLIGHT, lPos1, lPos2)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.BraceHighlightIndicator
! purpose:  Use specified indicator to highlight matching braces instead of changing their style.
! inputs :  bUseBraceHighlightIndicator
!           lIndicator
! outputs:
! returns:
! =======================================================================================
CSciControl.BraceHighlightIndicator PROCEDURE(BOOL bUseBraceHighlightIndicator, LONG lIndicator) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_BRACEHIGHLIGHTINDICATOR, bUseBraceHighlightIndicator, lIndicator)
  END

  RETURN


! =======================================================================================
! CSciControl.BraceBadLight
! purpose:
! inputs :  lPos
! outputs:
! returns:
! =======================================================================================
CSciControl.BraceBadLight   PROCEDURE(LONG lPos) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_BRACEBADLIGHT, lPos, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.BraceBadLightIndicator
! purpose:  Use specified indicator to highlight non matching brace instead of changing its style.
! inputs :  bUseBraceBadLightIndicator
!           lIndicator
! outputs:
! returns:
! =======================================================================================
CSciControl.BraceBadLightIndicator  PROCEDURE(BOOL bUseBraceBadLightIndicator, LONG lIndicator) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_BRACEBADLIGHTINDICATOR, bUseBraceBadLightIndicator, lIndicator)
  END

  RETURN


! =======================================================================================
! CSciControl.BraceMatch
! purpose:
! inputs :  lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.BraceMatch  PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_BRACEMATCH, lPos, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.GetViewEOL
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetViewEOL  PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETVIEWEOL, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetViewEOL
! purpose:
! inputs :  bVisible
! outputs:
! returns:
! =======================================================================================
CSciControl.SetViewEOL  PROCEDURE(BOOL bVisible) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETVIEWEOL, bVisible, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetDocPointer
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetDocPointer   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETDOCPOINTER, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetDocPointer
! purpose:
! inputs :
!           lPointer
! outputs:
! returns:
! =======================================================================================
CSciControl.SetDocPointer   PROCEDURE(LONG lPointer) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETDOCPOINTER, 0, lPointer)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetModEventMask
! purpose:
! inputs :  lMask
! outputs:
! returns:
! =======================================================================================
CSciControl.SetModEventMask PROCEDURE(LONG lMask) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMODEVENTMASK, lMask, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetEdgeColumn
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetEdgeColumn   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETEDGECOLUMN, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetEdgeColumn
! purpose:
! inputs :  lColumn
! outputs:
! returns:
! =======================================================================================
CSciControl.SetEdgeColumn   PROCEDURE(LONG lColumn) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETEDGECOLUMN, lColumn, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetEdgeMode
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetEdgeMode PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETEDGEMODE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetEdgeMode
! purpose:
! inputs :  lMode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetEdgeMode PROCEDURE(LONG lMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETEDGEMODE, lMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetEdgeColour
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetEdgeColour   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETEDGECOLOUR, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetEdgeColour
! purpose:
! inputs :  lEdgeColour
! outputs:
! returns:
! =======================================================================================
CSciControl.SetEdgeColour   PROCEDURE(LONG lEdgeColour) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETEDGECOLOUR, lEdgeColour, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SearchAnchor
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.SearchAnchor    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SEARCHANCHOR, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SearchNext
! purpose:
! inputs :  lFlags
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.SearchNext  PROCEDURE(LONG lFlags, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_SEARCHNEXT, lFlags, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SearchPrev
! purpose:
! inputs :  lFlags
!           szText
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.SearchPrev  PROCEDURE(LONG lFlags, *CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_SEARCHPREV, lFlags, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.LinesOnScreen
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.LinesOnScreen   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_LINESONSCREEN, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.UsePopUp
! purpose:
! inputs :  bAllowPopUp
! outputs:
! returns:
! =======================================================================================
CSciControl.UsePopUp    PROCEDURE(BOOL bAllowPopUp) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_USEPOPUP, bAllowPopUp, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SelectionIsRectangle
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.SelectionIsRectangle    PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_SELECTIONISRECTANGLE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetZoom
! purpose:
! inputs :  lZoom
! outputs:
! returns:
! =======================================================================================
CSciControl.SetZoom PROCEDURE(LONG lZoom) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETZOOM, lZoom, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetZoom
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetZoom PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETZOOM, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.CreateDocument
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.CreateDocument  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CREATEDOCUMENT, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.AddRefDocument
! purpose:
! inputs :
!           lDoc
! outputs:
! returns:
! =======================================================================================
CSciControl.AddRefDocument  PROCEDURE(LONG lDoc) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ADDREFDOCUMENT, 0, lDoc)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ReleaseDocument
! purpose:
! inputs :
!           lDoc
! outputs:
! returns:
! =======================================================================================
CSciControl.ReleaseDocument PROCEDURE(LONG lDoc) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_RELEASEDOCUMENT, 0, lDoc)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetModEventMask
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetModEventMask PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMODEVENTMASK, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetFocus
! purpose:
! inputs :  bFocus
! outputs:
! returns:
! =======================================================================================
CSciControl.SetFocus    PROCEDURE(BOOL bFocus) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETFOCUS, bFocus, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetFocus
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetFocus    PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETFOCUS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetStatus
! purpose:
! inputs :  lStatusCode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetStatus   PROCEDURE(LONG lStatusCode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETSTATUS, lStatusCode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetStatus
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetStatus   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSTATUS, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetMouseDownCaptures
! purpose:
! inputs :  bCaptures
! outputs:
! returns:
! =======================================================================================
CSciControl.SetMouseDownCaptures    PROCEDURE(BOOL bCaptures) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETMOUSEDOWNCAPTURES, bCaptures, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetMouseDownCaptures
! purpose:
! inputs :
! outputs:
! returns:  BOOL
! =======================================================================================
CSciControl.GetMouseDownCaptures    PROCEDURE() !,BOOL,VIRTUAL

ReturnValue BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETMOUSEDOWNCAPTURES, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetCursor
! purpose:
! inputs :  lCursorType
! outputs:
! returns:
! =======================================================================================
CSciControl.SetCursor   PROCEDURE(LONG lCursorType) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCURSOR, lCursorType, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetCursor
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetCursor   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCURSOR, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetControlCharSymbol
! purpose:
! inputs :  lSymbol
! outputs:
! returns:
! =======================================================================================
CSciControl.SetControlCharSymbol    PROCEDURE(LONG lSymbol) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETCONTROLCHARSYMBOL, lSymbol, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetControlCharSymbol
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetControlCharSymbol    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETCONTROLCHARSYMBOL, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.WordPartLeft
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordPartLeft    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDPARTLEFT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WordPartLeftExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordPartLeftExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDPARTLEFTEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WordPartRight
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordPartRight   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDPARTRIGHT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WordPartRightExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WordPartRightExtend PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WORDPARTRIGHTEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetVisiblePolicy
! purpose:
! inputs :  lVisiblePolicy
!           lVisibleSlop
! outputs:
! returns:
! =======================================================================================
CSciControl.SetVisiblePolicy    PROCEDURE(LONG lVisiblePolicy, LONG lVisibleSlop) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETVISIBLEPOLICY, lVisiblePolicy, lVisibleSlop)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DelLineLeft
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DelLineLeft PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DELLINELEFT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.DelLineRight
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.DelLineRight    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_DELLINERIGHT, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetXOffset
! purpose:
! inputs :  lNewOffset
! outputs:
! returns:
! =======================================================================================
CSciControl.SetXOffset  PROCEDURE(LONG lNewOffset) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETXOFFSET, lNewOffset, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetXOffset
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetXOffset  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETXOFFSET, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.ChooseCaretX
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ChooseCaretX    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CHOOSECARETX, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GrabFocus
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.GrabFocus   PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_GRABFOCUS, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetXCaretPolicy
! purpose:
! inputs :  lCaretPolicy
!           lCaretSlop
! outputs:
! returns:
! =======================================================================================
CSciControl.SetXCaretPolicy PROCEDURE(LONG lCaretPolicy, LONG lCaretSlop) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETXCARETPOLICY, lCaretPolicy, lCaretSlop)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetYCaretPolicy
! purpose:
! inputs :  lCaretPolicy
!           lCaretSlop
! outputs:
! returns:
! =======================================================================================
CSciControl.SetYCaretPolicy PROCEDURE(LONG lCaretPolicy, LONG lCaretSlop) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETYCARETPOLICY, lCaretPolicy, lCaretSlop)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetPrintWrapMode
! purpose:
! inputs :  lMode
! outputs:
! returns:
! =======================================================================================
CSciControl.SetPrintWrapMode    PROCEDURE(LONG lMode) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETPRINTWRAPMODE, lMode, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetPrintWrapMode
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetPrintWrapMode    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETPRINTWRAPMODE, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.SetHotspotActiveFore
! purpose:
! inputs :  bUseSetting
!           lFore
! outputs:
! returns:
! =======================================================================================
CSciControl.SetHotspotActiveFore    PROCEDURE(BOOL bUseSetting, LONG lFore) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETHOTSPOTACTIVEFORE, bUseSetting, lFore)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetHotspotActiveFore
! purpose:
! inputs :
! outputs:
! returns:  lFore
! =======================================================================================
CSciControl.GetHotspotActiveFore    PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETHOTSPOTACTIVEFORE, 0, 0)
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetHotspotActiveBack
! purpose:
! inputs :  bUseSetting
!           lBack
! outputs:
! returns:
! =======================================================================================
CSciControl.SetHotspotActiveBack    PROCEDURE(BOOL bUseSetting, LONG lBack) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETHOTSPOTACTIVEBACK, bUseSetting, lBack)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetHotspotActiveBack
! purpose:
! inputs :
! outputs:
! returns:  lBack
! =======================================================================================
CSciControl.GetHotspotActiveBack    PROCEDURE() !,LONG,VIRTUAL
ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETHOTSPOTACTIVEBACK, 0, 0)
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetHotspotActiveUnderline
! purpose:
! inputs :  bUnderline
! outputs:
! returns:
! =======================================================================================
CSciControl.SetHotspotActiveUnderline   PROCEDURE(BOOL bUnderline) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETHOTSPOTACTIVEUNDERLINE, bUnderline, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetHotspotActiveUnderline
! purpose:
! inputs :
! outputs:
! returns:  bUnderline
! =======================================================================================
CSciControl.GetHotspotActiveUnderline   PROCEDURE() !,BOOL,VIRTUAL
ReturnValue    BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETHOTSPOTACTIVEUNDERLINE, 0, 0)
  END

  RETURN ReturnValue                        ! Exit Procedure



! =======================================================================================
! CSciControl.SetHotspotSingleLine
! purpose:  Limit hotspots to single line so hotspots on two lines don't merge.
! inputs :  bSingleLine
! outputs:
! returns:
! =======================================================================================
CSciControl.SetHotspotSingleLine            PROCEDURE(BOOL bSingleLine) !,VIRTUAL
  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETHOTSPOTSINGLELINE, bSingleLine, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetHotspotSingleLine
! purpose:  Get the HotspotSingleLine property
! inputs :
! outputs:
! returns:  bSingleLine
! =======================================================================================
CSciControl.GetHotspotSingleLine            PROCEDURE() !,BOOL,VIRTUAL
ReturnValue    BOOL,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETHOTSPOTSINGLELINE, 0, 0)
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.ParaDown
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ParaDown    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PARADOWN, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ParaDownExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ParaDownExtend  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PARADOWNEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ParaUp
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ParaUp  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PARAUP, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ParaUpExtend
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.ParaUpExtend    PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PARAUPEXTEND, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.PositionBefore
! purpose:
! inputs :  lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.PositionBefore  PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_POSITIONBEFORE, lPos, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.PositionAfter
! purpose:
! inputs :  lPos
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.PositionAfter   PROCEDURE(LONG lPos) !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_POSITIONAFTER, lPos, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.CopyRange
! purpose:
! inputs :  lStart
!           lEnd
! outputs:
! returns:
! =======================================================================================
CSciControl.CopyRange   PROCEDURE(LONG lStart, LONG lEnd) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_COPYRANGE, lStart, lEnd)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.CopyText
! purpose:
! inputs :  lLength
!           szText
! outputs:
! returns:
! =======================================================================================
CSciControl.CopyText    PROCEDURE(LONG lLength, *CSTRING szText) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_COPYTEXT, lLength, ADDRESS(szText))
  END

  RETURN                                    ! Exit Procedure



CSciControl.SetSelectionMode                PROCEDURE(LONG lMode) !,VIRTUAL
! Set the selection mode to stream (SC_SEL_STREAM) or rectangular (SC_SEL_RECTANGLE/SC_SEL_THIN) or by lines (SC_SEL_LINES).
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetSelectionMode, lMode, 0)
  END

  RETURN

CSciControl.GetSelectionMode                PROCEDURE() !,LONG,VIRTUAL
! Get the mode of the current selection.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelectionMode, 0, 0)
  END

  RETURN ReturnValue


CSciControl.GetLineSelStartPosition         PROCEDURE(LONG lLine) !,LONG,VIRTUAL
! Retrieve the position of the start of the selection at the given line (INVALID_POSITION if no selection on this line).
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetLineSelStartPosition, lLine, 0)
  END

  RETURN ReturnValue


CSciControl.GetLineSelEndPosition           PROCEDURE(LONG lLine) !,LONG,VIRTUAL
! Retrieve the position of the end of the selection at the given line (INVALID_POSITION if no selection on this line).
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetLineSelEndPosition, lLine, 0)
  END

  RETURN ReturnValue


CSciControl.LineDownRectExtend              PROCEDURE() !,VIRTUAL
! RectExtended rectangular selection moves
! Move caret down one line, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LineDownRectExtend, 0, 0)
  END

  RETURN


CSciControl.LineUpRectExtend                PROCEDURE() !,VIRTUAL
! Move caret up one line, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LineUpRectExtend, 0, 0)
  END

  RETURN


CSciControl.CharLeftRectExtend              PROCEDURE() !,VIRTUAL
! Move caret left one character, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CharLeftRectExtend, 0, 0)
  END

  RETURN


CSciControl.CharRightRectExtend             PROCEDURE() !,VIRTUAL
! Move caret right one character, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CharRightRectExtend, 0, 0)
  END

  RETURN


CSciControl.HomeRectExtend                  PROCEDURE() !,VIRTUAL
! Move caret to first position on line, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_HomeRectExtend, 0, 0)
  END

  RETURN


CSciControl.VCHomeRectExtend                PROCEDURE() !,VIRTUAL
! Move caret to before first visible character on line.
! If already there move to first character on line.
! In either case, extend rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_VCHomeRectExtend, 0, 0)
  END

  RETURN


CSciControl.LineEndRectExtend               PROCEDURE() !,VIRTUAL
! Move caret to last position on line, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LineEndRectExtend, 0, 0)
  END

  RETURN


CSciControl.PageUpRectExtend                PROCEDURE() !,VIRTUAL
! Move caret one page up, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PageUpRectExtend, 0, 0)
  END

  RETURN


CSciControl.PageDownRectExtend              PROCEDURE() !,VIRTUAL
! Move caret one page down, extending rectangular selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_PageDownRectExtend, 0, 0)
  END

  RETURN


CSciControl.StutteredPageUp                 PROCEDURE() !,VIRTUAL
! Move caret to top of page, or one page up if already at top of page.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_StutteredPageUp, 0, 0)
  END

  RETURN


CSciControl.StutteredPageUpExtend           PROCEDURE() !,VIRTUAL
! Move caret to top of page, or one page up if already at top of page, extending selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_StutteredPageUpExtend, 0, 0)
  END

  RETURN


CSciControl.StutteredPageDown               PROCEDURE() !,VIRTUAL
! Move caret to bottom of page, or one page down if already at bottom of page.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_StutteredPageDown, 0, 0)
  END

  RETURN


CSciControl.StutteredPageDownExtend         PROCEDURE() !,VIRTUAL
! Move caret to bottom of page, or one page down if already at bottom of page, extending selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_StutteredPageDownExtend, 0, 0)
  END

  RETURN


CSciControl.WordLeftEnd                     PROCEDURE() !,VIRTUAL
! Move caret left one word, position cursor at end of word.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WordLeftEnd, 0, 0)
  END

  RETURN


CSciControl.WordLeftEndExtend               PROCEDURE() !,VIRTUAL
! Move caret left one word, position cursor at end of word, extending selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WordLeftEndExtend, 0, 0)
  END

  RETURN


CSciControl.WordRightEnd                    PROCEDURE() !,VIRTUAL
! Move caret right one word, position cursor at end of word.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WordRightEnd, 0, 0)
  END

  RETURN


CSciControl.WordRightEndExtend              PROCEDURE() !,VIRTUAL
! Move caret right one word, position cursor at end of word, extending selection to new caret position.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_WordRightEndExtend, 0, 0)
  END

  RETURN


CSciControl.SetWhitespaceChars              PROCEDURE(*CSTRING szCharacters) !,VIRTUAL
! Set the set of characters making up whitespace for when moving or selecting by word.
! Should be called after SetWordChars.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetWhitespaceChars, 0, ADDRESS(szCharacters))
  END

  RETURN


CSciControl.SetCharsDefault                 PROCEDURE() !,VIRTUAL
! Reset the set of characters for whitespace and word characters to the defaults.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetCharsDefault, 0, 0)
  END

  RETURN


CSciControl.AutoCGetCurrent                 PROCEDURE() !,LONG,VIRTUAL
! Get currently selected item position in the auto-completion list
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AutoCGetCurrent, 0, 0)
  END

  RETURN ReturnValue


CSciControl.AutoCGetCurrentText             PROCEDURE(*CSTRING szCurrentText) !,LONG,VIRTUAL
! Get currently selected item text in the auto-completion list
! Returns the length of the item text
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AutoCGetCurrentText, 0, ADDRESS(szCurrentText))
  END

  RETURN ReturnValue


CSciControl.Allocate                        PROCEDURE(LONG lBytes) !,VIRTUAL
! Enlarge the document to a particular size of text bytes.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_Allocate, lBytes, 0)
  END

  RETURN


CSciControl.TargetAsUTF8                    PROCEDURE(*CSTRING szUTF8) !,LONG,VIRTUAL
! Returns the target converted to UTF8.
! Return the length in bytes.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_TargetAsUTF8, 0, ADDRESS(szUTF8))
  END

  RETURN ReturnValue


CSciControl.SetLengthForEncode              PROCEDURE(LONG lBytes) !,VIRTUAL
! Set the length of the utf8 argument for calling EncodedFromUTF8.
! Set to -1 and the string will be measured to the first nul.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetLengthForEncode, lBytes, 0)
  END

  RETURN


CSciControl.EncodedFromUTF8                 PROCEDURE(*CSTRING szUtf8, *CSTRING szEncoded) !,LONG,VIRTUAL
! Translates a UTF8 string into the document encoding.
! Return the length of the result in bytes.
! On error return 0.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_EncodedFromUTF8, ADDRESS(szUtf8), ADDRESS(szEncoded))
  END

  RETURN ReturnValue


CSciControl.FindColumn                      PROCEDURE(LONG lLine, LONG lColumn) !,LONG,VIRTUAL
! Find the position of a column on a line taking into account tabs and
! multi-byte characters. If beyond end of line, return line end position.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_FindColumn, lLine, lColumn)
  END

  RETURN ReturnValue


CSciControl.GetCaretSticky                  PROCEDURE() !,LONG,VIRTUAL
! Can the caret preferred x position only be changed by explicit movement commands?
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetCaretSticky, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetCaretSticky                  PROCEDURE(LONG lUseCaretStickyBehaviour) !,VIRTUAL
! Stop the caret preferred x position changing when the user types.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetCaretSticky, lUseCaretStickyBehaviour, 0)
  END

  RETURN


CSciControl.ToggleCaretSticky               PROCEDURE() !,VIRTUAL
! Switch between sticky and non-sticky: meant to be bound to a key.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ToggleCaretSticky, 0, 0)
  END

  RETURN


CSciControl.SetPasteConvertEndings          PROCEDURE(BOOL bConvert) !,VIRTUAL
! Enable/Disable convert-on-paste for line endings
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetPasteConvertEndings, bConvert, 0)
  END

  RETURN


CSciControl.GetPasteConvertEndings          PROCEDURE() !,BOOL,VIRTUAL
! Get convert-on-paste setting
ReturnValue    BOOL(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetPasteConvertEndings, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SelectionDuplicate              PROCEDURE() !,VIRTUAL
! Duplicate the selection. If selection empty duplicate the line containing the caret.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SelectionDuplicate, 0, 0)
  END

  RETURN


CSciControl.SetCaretLineBackAlpha           PROCEDURE(LONG lAlpha) !,VIRTUAL
! Set background alpha of the caret line.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetCaretLineBackAlpha, lAlpha, 0)
  END

  RETURN


CSciControl.GetCaretLineBackAlpha           PROCEDURE() !,LONG,VIRTUAL
! Get the background alpha of the caret line.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetCaretLineBackAlpha, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetCaretStyle                   PROCEDURE(LONG lCaretStyle) !,VIRTUAL
! Set the style of the caret to be drawn.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetCaretStyle, lCaretStyle, 0)
  END

  RETURN


CSciControl.GetCaretStyle                   PROCEDURE() !,LONG,VIRTUAL
! Returns the current style of the caret.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetCaretStyle, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetIndicatorCurrent             PROCEDURE(LONG lIndicator) !,VIRTUAL
! Set the indicator used for IndicatorFillRange and IndicatorClearRange
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetIndicatorCurrent, lIndicator, 0)
  END

  RETURN


CSciControl.GetIndicatorCurrent             PROCEDURE() !,LONG,VIRTUAL
! Get the current indicator
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetIndicatorCurrent, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetIndicatorValue               PROCEDURE(LONG lValue) !,VIRTUAL
! Set the value used for IndicatorFillRange
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetIndicatorValue, lValue, 0)
  END

  RETURN


CSciControl.GetIndicatorValue               PROCEDURE() !,LONG,VIRTUAL
! Get the current indicator vaue
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetIndicatorValue, 0, 0)
  END

  RETURN ReturnValue


CSciControl.IndicatorFillRange              PROCEDURE(LONG lPosition, LONG lFillLength) !,VIRTUAL
! Turn a indicator on over a range.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_IndicatorFillRange, lPosition, lFillLength)
  END

  RETURN


CSciControl.IndicatorClearRange             PROCEDURE(LONG lPosition, LONG lClearLength) !,VIRTUAL
! Turn a indicator off over a range.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_IndicatorClearRange, lPosition, lClearLength)
  END

  RETURN


CSciControl.IndicatorAllOnFor               PROCEDURE(LONG lPosition) !,LONG,VIRTUAL
! Are any indicators present at position?
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_IndicatorAllOnFor, lPosition, 0)
  END

  RETURN ReturnValue


CSciControl.IndicatorValueAt                PROCEDURE(LONG lIndicator, LONG lPosition) !,LONG,VIRTUAL
! What value does a particular indicator have at at a position?
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_IndicatorValueAt, lIndicator, lPosition)
  END

  RETURN ReturnValue


CSciControl.IndicatorStart                  PROCEDURE(LONG lIndicator, LONG lPosition) !,LONG,VIRTUAL
! Where does a particular indicator start?
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_IndicatorStart, lIndicator, lPosition)
  END

  RETURN ReturnValue


CSciControl.IndicatorEnd                    PROCEDURE(LONG lIndicator, LONG lPosition) !,LONG,VIRTUAL
! Where does a particular indicator end?
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_IndicatorEnd, lIndicator, lPosition)
  END

  RETURN ReturnValue


CSciControl.SetPositionCache                PROCEDURE(LONG lSize) !,VIRTUAL
! Set number of entries in position cache
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetPositionCache, lSize, 0)
  END

  RETURN


CSciControl.GetPositionCache                PROCEDURE() !,LONG,VIRTUAL
! How many entries are allocated to the position cache?
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetPositionCache, 0, 0)
  END

  RETURN ReturnValue


CSciControl.CopyAllowLine                   PROCEDURE() !,VIRTUAL
! Copy the selection, if selection empty copy the line with the caret
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_CopyAllowLine, 0, 0)
  END

  RETURN


CSciControl.GetCharacterPointer             PROCEDURE() !,LONG,VIRTUAL
! Compact the document buffer and return a read-only pointer to the
! characters in the document.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetCharacterPointer, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetKeysUnicode                  PROCEDURE(BOOL bKeysUnicode) !,VIRTUAL
! Always interpret keyboard input as Unicode
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetKeysUnicode, bKeysUnicode, 0)
  END

  RETURN


CSciControl.GetKeysUnicode                  PROCEDURE() !,BOOL,VIRTUAL
! Are keys always interpreted as Unicode?
ReturnValue    BOOL(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetKeysUnicode, 0, 0)
  END

  RETURN ReturnValue


CSciControl.IndicSetAlpha                   PROCEDURE(LONG lIndicator, LONG lAlpha) !,VIRTUAL
! Set the alpha fill colour of the given indicator.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_IndicSetAlpha, lIndicator, lAlpha)
  END

  RETURN


CSciControl.IndicGetAlpha                   PROCEDURE(LONG lIndicator) !,LONG,VIRTUAL
! Get the alpha fill colour of the given indicator.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_IndicGetAlpha, lIndicator, 0)
  END

  RETURN ReturnValue


CSciControl.IndicSetOutlineAlpha            PROCEDURE(LONG lIndicator, LONG lAlpha) !,VIRTUAL
! Set the alpha outline colour of the given indicator.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_IndicSetOutlineAlpha, lIndicator, lAlpha)
  END

  RETURN


CSciControl.IndicGetOutlineAlpha            PROCEDURE(LONG lIndicator) !,LONG,VIRTUAL
! Get the alpha outline colour of the given indicator.
ReturnValue    LONG(FALSE)

  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_IndicGetOutlineAlpha, lIndicator, 0)
  END

  RETURN ReturnValue


CSciControl.SetExtraAscent                  PROCEDURE(LONG lExtraAscent) !,VIRTUAL
! Set extra ascent for each line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetExtraAscent, lExtraAscent, 0)
  END

  RETURN


CSciControl.GetExtraAscent                  PROCEDURE() !,LONG,VIRTUAL
! Get extra ascent for each line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetExtraAscent, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetExtraDescent                 PROCEDURE(LONG lExtraDescent) !,VIRTUAL
! Set extra descent for each line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetExtraDescent, lExtraDescent, 0)
  END

  RETURN


CSciControl.GetExtraDescent                 PROCEDURE() !,LONG,VIRTUAL
! Get extra descent for each line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetExtraDescent, 0, 0)
  END

  RETURN ReturnValue


CSciControl.MarkerSymbolDefined             PROCEDURE(LONG lMarkerNumber) !,LONG,VIRTUAL
! Which symbol was defined for markerNumber with MarkerDefine
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MarkerSymbolDefined, lMarkerNumber,0)
  END

  RETURN ReturnValue


CSciControl.MarginSetText                   PROCEDURE(LONG lLine, *CSTRING szText) !,VIRTUAL
! Set the text in the text margin for a line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MarginSetText, lLine, ADDRESS(szText))
  END

  RETURN


CSciControl.MarginGetText                   PROCEDURE(LONG lLine, *CSTRING szText) !,LONG,VIRTUAL
! Get the text in the text margin for a line
ReturnValue    LONG(FALSE)

  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MarginGetText, lLine, ADDRESS(szText))
  END

  RETURN ReturnValue


CSciControl.MarginSetStyle                  PROCEDURE(LONG lLine, LONG lstyle) !,VIRTUAL
! Set the style number for the text margin for a line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MarginSetStyle, lLine, lstyle)
  END

  RETURN


CSciControl.MarginGetStyle                  PROCEDURE(LONG lLine) !,LONG,VIRTUAL
! Get the style number for the text margin for a line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MarginGetStyle, lLine, 0)
  END

  RETURN ReturnValue


CSciControl.MarginSetStyles                 PROCEDURE(LONG lLine, *CSTRING szStyles) !,VIRTUAL
! Set the style in the text margin for a line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MarginSetStyles, lLine, ADDRESS(szStyles))
  END

  RETURN


CSciControl.MarginGetStyles                 PROCEDURE(LONG lLine, *CSTRING szStyles) !,LONG,VIRTUAL
! Get the styles in the text margin for a line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MarginGetStyles, lLine, ADDRESS(szStyles))
  END

  RETURN ReturnValue


CSciControl.MarginTextClearAll              PROCEDURE() !,VIRTUAL
! Clear the margin text on all lines
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MarginTextClearAll, 0, 0)
  END

  RETURN


CSciControl.MarginSetStyleOffset            PROCEDURE(LONG lStyle) !,VIRTUAL
! Get the start of the range of style numbers used for margin text
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MarginSetStyleOffset, lStyle, 0)
  END

  RETURN


CSciControl.MarginGetStyleOffset            PROCEDURE() !,LONG,VIRTUAL
! Get the start of the range of style numbers used for margin text
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_MarginGetStyleOffset, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetMarginOptions                PROCEDURE(LONG lMarginOptions) !,VIRTUAL
! Set the margin options.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetMarginOptions, lMarginOptions, 0)
  END

  RETURN


CSciControl.GetMarginOptions                PROCEDURE() !,LONG,VIRTUAL
! Get the margin options.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetMarginOptions, 0, 0)
  END

  RETURN ReturnValue


CSciControl.AnnotationSetText               PROCEDURE(LONG lLine, *CSTRING szText) !,VIRTUAL
! Set the annotation text for a line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AnnotationSetText, lLine, ADDRESS(szText))
  END

  RETURN


CSciControl.AnnotationGetText               PROCEDURE(LONG lLine, *CSTRING szText) !,LONG,VIRTUAL
! Get the annotation text for a line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AnnotationGetText, lLine, ADDRESS(szText))
  END

  RETURN ReturnValue


CSciControl.AnnotationSetStyle              PROCEDURE(LONG lLine, LONG lStyle) !,VIRTUAL
! Set the style number for the annotations for a line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AnnotationSetStyle, lLine, lStyle)
  END

  RETURN


CSciControl.AnnotationGetStyle              PROCEDURE(LONG lLine) !,LONG,VIRTUAL
! Get the style number for the annotations for a line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AnnotationGetStyle, lLine, 0)
  END

  RETURN ReturnValue


CSciControl.AnnotationSetStyles             PROCEDURE(LONG lLine, *CSTRING szStyles) !,VIRTUAL
! Set the annotation styles for a line
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AnnotationSetStyles, lLine, ADDRESS(szStyles))
  END

  RETURN


CSciControl.AnnotationGetStyles             PROCEDURE(LONG lLine, *CSTRING szStyles) !,LONG,VIRTUAL
! Get the annotation styles for a line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AnnotationGetStyles, lLine, ADDRESS(szStyles))
  END

  RETURN ReturnValue


CSciControl.AnnotationGetLines              PROCEDURE(LONG lLine) !,LONG,VIRTUAL
! Get the number of annotation lines for a line
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AnnotationGetLines, lLine, 0)
  END

  RETURN ReturnValue


CSciControl.AnnotationClearAll              PROCEDURE() !,VIRTUAL
! Clear the annotations from all lines
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AnnotationClearAll, 0, 0)
  END

  RETURN


CSciControl.AnnotationSetVisible            PROCEDURE(LONG lVisible) !,VIRTUAL
! Set the visibility for the annotations for a view
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AnnotationSetVisible, lVisible, 0)
  END

  RETURN


CSciControl.AnnotationGetVisible            PROCEDURE() !,LONG,VIRTUAL
! Get the visibility for the annotations for a view
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AnnotationGetVisible, 0, 0)
  END

  RETURN ReturnValue


CSciControl.AnnotationSetStyleOffset        PROCEDURE(LONG lStyle) !,VIRTUAL
! Get the start of the range of style numbers used for annotations
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AnnotationSetStyleOffset,lStyle , 0)
  END

  RETURN


CSciControl.AnnotationGetStyleOffset        PROCEDURE() !,LONG,VIRTUAL
! Get the start of the range of style numbers used for annotations
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AnnotationGetStyleOffset, 0, 0)
  END

  RETURN ReturnValue


CSciControl.AddUndoAction                   PROCEDURE(LONG lToken, LONG lFlags) !,VIRTUAL
! Add a container action to the undo stack
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_AddUndoAction, lToken, lFlags)
  END

  RETURN


CSciControl.CharPositionFromPoint           PROCEDURE(LONG X, LONG Y) !,LONG,VIRTUAL
! Find the position of a character from a point within the window.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CharPositionFromPoint, X, Y)
  END

  RETURN ReturnValue


CSciControl.CharPositionFromPointClose      PROCEDURE(LONG X, LONG Y) !,LONG,VIRTUAL
! Find the position of a character from a point within the window.
! Return INVALID_POSITION if not close to text.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CharPositionFromPointClose, X, Y)
  END

  RETURN ReturnValue


CSciControl.SetMultipleSelection            PROCEDURE(BOOL bMultipleSelection) !,VIRTUAL
! Set whether multiple selections can be made
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetMultipleSelection, bMultipleSelection, 0)
  END

  RETURN


CSciControl.GetMultipleSelection            PROCEDURE() !,BOOL,VIRTUAL
! Whether multiple selections can be made
ReturnValue    BOOL(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetMultipleSelection, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetAdditionalSelectionTyping    PROCEDURE(BOOL bAdditionalSelectionTyping) !,VIRTUAL
! Set whether typing can be performed into multiple selections
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetAdditionalSelectionTyping, bAdditionalSelectionTyping, 0)
  END

  RETURN


CSciControl.GetAdditionalSelectionTyping    PROCEDURE() !,BOOL,VIRTUAL
! Whether typing can be performed into multiple selections
ReturnValue    BOOL(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetAdditionalSelectionTyping, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetAdditionalCaretsBlink        PROCEDURE(BOOL bAdditionalCaretsBlink) !,VIRTUAL
! Set whether additional carets will blink
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetAdditionalCaretsBlink, bAdditionalCaretsBlink, 0)
  END

  RETURN


CSciControl.GetAdditionalCaretsBlink        PROCEDURE() !,BOOL,VIRTUAL
! Whether additional carets will blink
ReturnValue    BOOL(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetAdditionalCaretsBlink, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetAdditionalCaretsVisible      PROCEDURE(BOOL bAdditionalCaretsVisible) !,VIRTUAL
! Set whether additional carets are visible
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetAdditionalCaretsVisible, bAdditionalCaretsVisible, 0)
  END

  RETURN


CSciControl.GetAdditionalCaretsVisible      PROCEDURE() !,BOOL,VIRTUAL
! Whether additional carets are visible
ReturnValue    BOOL(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetAdditionalCaretsVisible, 0, 0)
  END

  RETURN ReturnValue


CSciControl.GetSelections                   PROCEDURE() !,LONG,VIRTUAL
! How many selections are there?
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelections, 0, 0)
  END

  RETURN ReturnValue


CSciControl.ClearSelections                 PROCEDURE() !,VIRTUAL
! Clear selections to a single empty stream selection
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ClearSelections, 0, 0)
  END

  RETURN


CSciControl.SetSelection                    PROCEDURE(LONG lCaret,LONG lAnchor) !,LONG,VIRTUAL
! Set a simple selection
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_SetSelection, lCaret, lAnchor)
  END

  RETURN ReturnValue


CSciControl.AddSelection                    PROCEDURE(LONG lCaret,LONG lAnchor) !,LONG,VIRTUAL
! Add a selection
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_AddSelection, lCaret, lAnchor)
  END

  RETURN ReturnValue


CSciControl.SetMainSelection                PROCEDURE(LONG lSelection) !,VIRTUAL
! Set the main selection
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetMainSelection, lSelection, 0)
  END

  RETURN


CSciControl.GetMainSelection                PROCEDURE() !,LONG,VIRTUAL
! Which selection is the main selection
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetMainSelection, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetSelectionNCaret              PROCEDURE(LONG lSelection, LONG lPos) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetSelectionNCaret, lSelection, lPos)
  END

  RETURN

CSciControl.GetSelectionNCaret              PROCEDURE(LONG lSelection) !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelectionNCaret, lSelection, 0)
  END

  RETURN ReturnValue

CSciControl.SetSelectionNAnchor             PROCEDURE(LONG lSelection, LONG lPosAnchor) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetSelectionNAnchor, lSelection, lPosAnchor)
  END

  RETURN

CSciControl.GetSelectionNAnchor             PROCEDURE(LONG lSelection) !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelectionNAnchor, lSelection, 0)
  END

  RETURN ReturnValue

CSciControl.SetSelectionNCaretVirtualSpace  PROCEDURE(LONG lSelection, LONG lSpace) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetSelectionNCaretVirtualSpace, lSelection, lSpace)
  END

  RETURN

CSciControl.GetSelectionNCaretVirtualSpace  PROCEDURE(LONG lSelection) !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelectionNCaretVirtualSpace, lSelection, 0)
  END

  RETURN ReturnValue

CSciControl.SetSelectionNAnchorVirtualSpace PROCEDURE(LONG lSelection, LONG lSpace) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetSelectionNAnchorVirtualSpace, lSelection, lSpace)
  END

  RETURN

CSciControl.GetSelectionNAnchorVirtualSpace PROCEDURE(LONG lSelection) !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelectionNAnchorVirtualSpace, lSelection, 0)
  END

  RETURN ReturnValue


CSciControl.SetSelectionNStart              PROCEDURE(LONG lselection, LONG lPos) !,VIRTUAL
! Sets the position that starts the selection - this becomes the anchor.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetSelectionNStart, lselection, lPos)
  END

  RETURN


CSciControl.GetSelectionNStart              PROCEDURE(LONG lSelection) !,LONG,VIRTUAL
! Returns the position at the start of the selection.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelectionNStart, lSelection, 0)
  END

  RETURN ReturnValue


CSciControl.SetSelectionNEnd                PROCEDURE(LONG lSelection, LONG lPos) !,VIRTUAL
! Sets the position that ends the selection - this becomes the currentPosition.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetSelectionNEnd, lSelection, lPos)
  END

  RETURN


CSciControl.GetSelectionNEnd                PROCEDURE(LONG lSelection) !,LONG,VIRTUAL
! Returns the position at the end of the selection.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetSelectionNEnd, lSelection, 0)
  END

  RETURN ReturnValue


CSciControl.SetRectangularSelectionCaret    PROCEDURE(LONG lPos) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetRectangularSelectionCaret, lPos, 0)
  END

  RETURN

CSciControl.GetRectangularSelectionCaret    PROCEDURE() !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetRectangularSelectionCaret, 0, 0)
  END

  RETURN ReturnValue

CSciControl.SetRectangularSelectionAnchor   PROCEDURE(LONG lPosAnchor) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetRectangularSelectionAnchor, lPosAnchor, 0)
  END

  RETURN

CSciControl.GetRectangularSelectionAnchor   PROCEDURE() !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetRectangularSelectionAnchor, 0, 0)
  END

  RETURN ReturnValue

CSciControl.SetRectangularSelectionCaretVirtualSpace  PROCEDURE(LONG lSpace) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetRectangularSelectionCaretVirtualSpace, lSpace, 0)
  END

  RETURN

CSciControl.GetRectangularSelectionCaretVirtualSpace  PROCEDURE() !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetRectangularSelectionCaretVirtualSpace, 0, 0)
  END

  RETURN ReturnValue

CSciControl.SetRectangularSelectionAnchorVirtualSpace PROCEDURE(LONG lSpace) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetRectangularSelectionAnchorVirtualSpace, lSpace, 0)
  END

  RETURN

CSciControl.GetRectangularSelectionAnchorVirtualSpace PROCEDURE() !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetRectangularSelectionAnchorVirtualSpace, 0, 0)
  END

  RETURN ReturnValue

CSciControl.SetVirtualSpaceOptions          PROCEDURE(LONG lVirtualSpaceOptions) !,VIRTUAL
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetVirtualSpaceOptions, lVirtualSpaceOptions, 0)
  END

  RETURN

CSciControl.GetVirtualSpaceOptions          PROCEDURE() !,LONG,VIRTUAL
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetVirtualSpaceOptions, 0, 0)
  END

  RETURN ReturnValue



CSciControl.SetRectangularSelectionModifier PROCEDURE(LONG lModifier) !,VIRTUAL
! On GTK+, allow selecting the modifier key to use for mouse-based
! rectangular selection. Often the window manager requires Alt+Mouse Drag
! for moving windows.
! Valid values are SCMOD_CTRL(default), SCMOD_ALT, or SCMOD_SUPER.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetRectangularSelectionModifier, lModifier, 0)
  END

  RETURN


CSciControl.GetRectangularSelectionModifier PROCEDURE() !,LONG,VIRTUAL
! Get the modifier key used for rectangular selection.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetRectangularSelectionModifier, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetAdditionalSelFore            PROCEDURE(LONG lFore) !,VIRTUAL
! Set the foreground colour of additional selections.
! Must have previously called SetSelFore with non-zero first argument for this to have an effect.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetAdditionalSelFore, lFore, 0)
  END

  RETURN


CSciControl.SetAdditionalSelBack            PROCEDURE(LONG lBack) !,VIRTUAL
! Set the background colour of additional selections.
! Must have previously called SetSelBack with non-zero first argument for this to have an effect.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetAdditionalSelBack, lBack, 0)
  END

  RETURN


CSciControl.SetAdditionalSelAlpha           PROCEDURE(LONG lAlpha) !,VIRTUAL
! Set the alpha of the selection.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetAdditionalSelAlpha, lAlpha, 0)
  END

  RETURN


CSciControl.GetAdditionalSelAlpha           PROCEDURE() !,LONG,VIRTUAL
! Get the alpha of the selection.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetAdditionalSelAlpha, 0, 0)
  END

  RETURN ReturnValue


CSciControl.SetAdditionalCaretFore          PROCEDURE(LONG lFore) !,VIRTUAL
! Set the foreground colour of additional carets.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetAdditionalCaretFore, lFore, 0)
  END

  RETURN


CSciControl.GetAdditionalCaretFore          PROCEDURE() !,LONG,VIRTUAL
! Get the foreground colour of additional carets.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetAdditionalCaretFore, 0, 0)
  END

  RETURN ReturnValue


CSciControl.RotateSelection                 PROCEDURE() !,VIRTUAL
! Set the main selection to the next selection.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_RotateSelection, 0, 0)
  END

  RETURN


CSciControl.SwapMainAnchorCaret             PROCEDURE() !,VIRTUAL
! Swap that caret and anchor of the main selection.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SwapMainAnchorCaret, 0, 0)
  END

  RETURN


CSciControl.ChangeLexerState                PROCEDURE(LONG lStart, LONG lEnd) !,LONG,VIRTUAL
! Indicate that the internal state of a lexer has changed over a range and therefore
! there may be a need to redraw.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ChangeLexerState, lStart, lEnd)
  END

  RETURN ReturnValue


CSciControl.ContractedFoldNext              PROCEDURE(LONG lLineStart) !,LONG,VIRTUAL
! Find the next line at or after lineStart that is a contracted fold header line.
! Return -1 when no more lines.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_ContractedFoldNext, lLineStart, 0)
  END

  RETURN ReturnValue


CSciControl.VerticalCentreCaret             PROCEDURE() !,VIRTUAL
! Centre current line in window.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_VerticalCentreCaret, 0, 0)
  END

  RETURN


CSciControl.MoveSelectedLinesUp             PROCEDURE() !,VIRTUAL
! Move the selected lines up one line, shifting the line above after the selection
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MoveSelectedLinesUp, 0, 0)
  END

  RETURN


CSciControl.MoveSelectedLinesDown           PROCEDURE() !,VIRTUAL
! Move the selected lines down one line, shifting the line below before the selection
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MoveSelectedLinesDown, 0, 0)
  END

  RETURN


CSciControl.SetIdentifier                   PROCEDURE(LONG lIdentifier) !,VIRTUAL
! Set the identifier reported as idFrom in notification messages.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SetIdentifier, lIdentifier, 0)
  END

  RETURN


CSciControl.GetIdentifier                   PROCEDURE() !,LONG,VIRTUAL
! Get the identifier.
ReturnValue    LONG(FALSE)
  CODE

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GetIdentifier, 0, 0)
  END

  RETURN ReturnValue


CSciControl.RGBAImageSetWidth               PROCEDURE(LONG lWidth) !,VIRTUAL
! Set the width for future RGBA image data.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_RGBAImageSetWidth, lWidth, 0)
  END

  RETURN


CSciControl.RGBAImageSetHeight              PROCEDURE(LONG lHeight) !,VIRTUAL
! Set the height for future RGBA image data.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_RGBAImageSetHeight, lHeight, 0)
  END

  RETURN


CSciControl.MarkerDefineRGBAImage           PROCEDURE(LONG lMarkerNumber, *STRING sPixels) !,VIRTUAL
! Define a marker from RGBA data.
! It has the width and height from RGBAImageSetWidth/Height
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_MarkerDefineRGBAImage, lMarkerNumber, ADDRESS(sPixels))
  END

  RETURN


CSciControl.RegisterRGBAImage               PROCEDURE(LONG lType, *STRING sPixels) !,VIRTUAL
! Register an RGBA image for use in autocompletion lists.
! It has the width and height from RGBAImageSetWidth/Height
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_RegisterRGBAImage, lType, ADDRESS(sPixels))
  END

  RETURN


CSciControl.ScrollToStart                   PROCEDURE() !,VIRTUAL
! Scroll to start of document.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ScrollToStart, 0, 0)
  END

  RETURN


CSciControl.ScrollToEnd                     PROCEDURE() !,VIRTUAL
! Scroll to end of document.
  CODE

  IF SELF.bInitialised
     SELF.SendMessage(SCI_ScrollToEnd, 0, 0)
  END

  RETURN






! =======================================================================================
! CSciControl.SetTechnology
! purpose:
! inputs :  technology
! outputs:
! returns:
! =======================================================================================
CSciControl.SetTechnology  PROCEDURE(LONG technology) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETTECHNOLOGY, technology, 0)
  END

  RETURN                                    ! Exit Procedure

! =======================================================================================
! CSciControl.GetTechnology
! purpose:
! inputs :
! outputs:  technology
! returns:
! =======================================================================================
CSciControl.GetTechnology  PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETTECHNOLOGY, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.CreateLoader
! purpose:
! inputs :  initial memory allocation in bytes
! outputs:  Document pointer
! returns:
! =======================================================================================
CSciControl.CreateLoader   PROCEDURE(LONG lBytes)  !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_CREATELOADER, lBytes, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure

! =======================================================================================
! CSciControl.StartRecord
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.StartRecord PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STARTRECORD, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.StopRecord
! purpose:
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.StopRecord  PROCEDURE() !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_STOPRECORD, 0, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetLexer
! purpose:
! inputs :  lLexer
! outputs:
! returns:
! =======================================================================================
CSciControl.SetLexer    PROCEDURE(LONG lLexer) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETLEXER, lLexer, 0)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetLexer
! purpose:
! inputs :
! outputs:
! returns:  LONG
! =======================================================================================
CSciControl.GetLexer    PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLEXER, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN(ReturnValue)                       ! Exit Procedure


! =======================================================================================
! CSciControl.Colourise
! purpose:
! inputs :  lStart
!           lEnd
! outputs:
! returns:
! =======================================================================================
CSciControl.Colourise   PROCEDURE(LONG lStart, LONG lEnd) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_COLOURISE, lStart, lEnd)
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetProperty
! purpose:
! inputs :  szKey
!           szValue
! outputs:
! returns:
! =======================================================================================
CSciControl.SetProperty PROCEDURE(*CSTRING szKey, *CSTRING szValue) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETPROPERTY, ADDRESS(szKey), ADDRESS(szValue))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetKeyWords
! purpose:
! inputs :  lKeywordSet
!           szKeyWords
! outputs:
! returns:
! =======================================================================================
CSciControl.SetKeyWords PROCEDURE(LONG lKeywordSet, *CSTRING szKeyWords) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETKEYWORDS, lKeywordSet, ADDRESS(szKeyWords))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetLexerLanguage
! purpose:
! inputs :
!           szLanguage
! outputs:
! returns:
! =======================================================================================
CSciControl.SetLexerLanguage    PROCEDURE(*CSTRING szLanguage) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_SETLEXERLANGUAGE, 0, ADDRESS(szLanguage))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.LoadLexerLibrary
! purpose:
! inputs :  szPath
! outputs:
! returns:
! =======================================================================================
CSciControl.LoadLexerLibrary    PROCEDURE(*CSTRING szPath) !,VIRTUAL

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.SendMessage(SCI_LOADLEXERLIBRARY, 0, ADDRESS(szPath))
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.GetProperty
! purpose:  Retrieve a "property" value previously set with SetProperty.
! inputs :  szKey
! outputs:  szBuf
! returns:
! =======================================================================================
CSciControl.GetProperty PROCEDURE(*CSTRING szKey, *CSTRING szBuf) !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETPROPERTY, ADDRESS(szKey), ADDRESS(szBuf))
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.GetPropertyExpanded
! purpose:  Retrieve a "property" value previously set with SetProperty,
!           with "$()" variable replacement on returned buffer.
! inputs :  szKey
! outputs:  szBuf
! returns:
! =======================================================================================
CSciControl.GetPropertyExpanded  PROCEDURE(*CSTRING szKey, *CSTRING szBuf) !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETPROPERTYEXPANDED, ADDRESS(szKey), ADDRESS(szBuf))
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure



! =======================================================================================
! CSciControl.GetPropertyInt
! purpose:  Retrieve a "property" value previously set with SetProperty,
!           interpreted as an int AFTER any "$()" variable replacement.
! inputs :  szKey
! outputs:
! returns:  PropertyInt
! =======================================================================================
CSciControl.GetPropertyInt PROCEDURE(*CSTRING szKey)  !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETPROPERTYINT, ADDRESS(szKey), 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.GetStyleBitsNeeded
! purpose:  Retrieve the number of bits the current lexer needs for styling.
! inputs :
! outputs:
! returns:  StyleBitsNeeded
! =======================================================================================
CSciControl.GetStyleBitsNeeded   PROCEDURE() !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETSTYLEBITSNEEDED, 0, 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.GetLexerLanguage
! purpose:  Retrieve the name of the lexer.
! inputs :
! outputs:  szText
! returns:  Return the length of the text.
! =======================================================================================
CSciControl.GetLexerLanguage  PROCEDURE(*CSTRING szText) !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_GETLEXERLANGUAGE, 0, ADDRESS(szText))
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure

! =======================================================================================
! CSciControl.PrivateLexerCall
! purpose:  For private communication between an application and a known lexer.
! inputs :  lOperation
!           lPointer
! outputs:
! returns:
! =======================================================================================
CSciControl.PrivateLexerCall  PROCEDURE(LONG lOperation, LONG lPointer) !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_PRIVATELEXERCALL, lOperation, lPointer)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.PropertyNames
! purpose:  Retrieve a '\n' separated list of properties understood by the current lexer.
! inputs :
! outputs:  szNames
! returns:
! =======================================================================================
CSciControl.PropertyNames  PROCEDURE(*CSTRING szNames)   !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_PROPERTYNAMES, 0, ADDRESS(szNames))
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.PropertyType
! purpose:  Retrieve the type of a property.
! inputs :  szName
! outputs:
! returns:  PropertyType
! =======================================================================================
CSciControl.PropertyType   PROCEDURE(*CSTRING szName) !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_PROPERTYTYPE, ADDRESS(szName), 0)
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.DescribeProperty
! purpose:  Describe a property.
! inputs :  szName
! outputs:  szDescription
! returns:
! =======================================================================================
CSciControl.DescribeProperty  PROCEDURE(*CSTRING szName, *CSTRING szDescription) !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_DESCRIBEPROPERTY, ADDRESS(szName), ADDRESS(szDescription))
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.DescribeKeyWordSets
! purpose:  Retrieve a '\n' separated list of descriptions of the keyword sets understood by the current lexer.
! inputs :
! outputs:  szDescriptions
! returns:
! =======================================================================================
CSciControl.DescribeKeyWordSets  PROCEDURE(*CSTRING szDescriptions)  !,LONG,VIRTUAL

ReturnValue LONG(FALSE)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     ReturnValue = SELF.SendMessage(SCI_DESCRIBEKEYWORDSETS, 0, ADDRESS(szDescriptions))
  ELSE
     ReturnValue = 0
  END

  RETURN ReturnValue                        ! Exit Procedure


!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! PROTECTED methods of the class
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§


! =======================================================================================
! CSciControl.GetWindowRect
! purpose:  Get client coordinates for bounding region
! outputs:  SELF.rc is filled with proper client coordinates
! inputs :
! returns:
! =======================================================================================
CSciControl.GetWindowRect         PROCEDURE()   !,PROTECTED

pt  LIKE(POINT)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised = TRUE
     !get position of the region
     !====================================================================
     GetWindowRect(SELF.hWndRegion, SELF.rc)

     !convert upper left to client coordinates
     !====================================================================
     pt.x = SELF.rc.left
     pt.y = SELF.rc.top
     ScreenToClient(SELF.W{PROP:Handle},pt)
     SELF.rc.left = pt.x - 1
     SELF.rc.top = pt.y - 1

     !convert lower right to client coordinates
     !====================================================================
     pt.x = SELF.rc.right
     pt.y = SELF.rc.bottom
     ScreenToClient(SELF.W{PROP:Handle},pt)
     SELF.rc.right = pt.x + 1
     SELF.rc.bottom = pt.y + 1
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SetWindowPos
! purpose:  Position the control window
! outputs:
! inputs :
! returns:
! =======================================================================================
CSciControl.SetWindowPos          PROCEDURE()   !,PROTECTED

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised = TRUE
     !reposition the control
     !====================================================================
     IF SELF.hWnd <> 0
        InvalidateRect(SELF.hWnd,0,0)
        SetWindowPos(SELF.hWnd, 0,                  |
                     SELF.rc.left , SELF.rc.top,    |
                     SELF.rc.right - SELF.rc.left,  |
                     SELF.rc.bottom - SELF.rc.top, BOR(SWP_NOZORDER, CHOOSE(SELF.GetHide() = FALSE,SWP_SHOWWINDOW,SWP_HIDEWINDOW)))
     END
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.SendMessage
! purpose:  Provide access to scintilla control the fast way
! outputs:
! inputs :
! returns:
! =======================================================================================
CSciControl.SendMessage PROCEDURE(LONG uMsg, LONG wParam, LONG lParam)   !LONG,PROTECTED,PROC

ReturnValue LONG,AUTO

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised = TRUE
     ReturnValue = CallSciMsgProc(SELF.SciMsgProc, SELF.SciMsgPtr, uMsg, wParam, lParam)
  ELSE
     ReturnValue = FALSE
  END

  RETURN(ReturnValue)                       ! Exit Procedure


!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! WindowComponent Callback Methods
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

! =======================================================================================
! CSciControl.Kill
! purpose:  The Kill method releases any memory allocated during the life of the object
!           and performs any other required termination code.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Kill PROCEDURE

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised = TRUE

     ! Destroy the Window
?    ASSERT(SELF.hWnd <> 0)
     IF SELF.hWnd <> 0
        DestroyWindow(SELF.hWnd)
        SELF.hWnd = 0
     END

     ! Unsubclass the ClientWndProc
     IF OrigClientWndProc <> 0
        SELF.W{PROP:ClientWndProc} = OrigClientWndProc

        ! This next bit of code handles the LIFO queue
        ! of windows subclassed on this thread
        !---------------------------------------------
        ! Pop Top Entry off LIFO queue
        IF RECORDS(SubClassedClient)
           GET(SubClassedClient,1)
           DELETE(SubClassedClient)
        END
        ! Get Top Entry of LIFO queue
        IF RECORDS(SubClassedClient)
           GET(SubClassedClient,1)
        ELSE
           CLEAR(SubClassedClient)
        END

     END

     ! Not initialised any more
     SELF.bInitialised = FALSE
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.Reset
! purpose:  The Reset method resets the object's data if needed or if Force is TRUE.
! inputs :  (BYTE Force)
! outputs:
! returns:
! =======================================================================================
CSciControl.Reset PROCEDURE(BYTE Force)

  CODE                                      ! Enter Procedure

  IF SELF.bInitialised
     SELF.GetWindowRect()
     SELF.SetWindowPos()
  END

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.ResetRequired
! purpose:  The ResetRequired method determines whether the objects data needs to be
!           refreshed.  A TRUE return value indicates a refresh occurred and a screen
!           redraw is necessary.
! inputs :
! outputs:
! returns:  BYTE
! =======================================================================================
CSciControl.ResetRequired PROCEDURE!,BYTE

ReturnValue BYTE,AUTO

  CODE                                      ! Enter Procedure

  ReturnValue = FALSE

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.SetAlerts
! purpose:  The SetAlerts method alerts standard keystrokes for the control associated
!           with the window componet's object.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.SetAlerts PROCEDURE

  CODE                                      ! Enter Procedure

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.TakeEvent
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
CSciControl.TakeEvent PROCEDURE!,BYTE

ReturnValue BYTE,AUTO

  CODE                                      ! Enter the procedure

  ReturnValue = Level:Benign                ! initialise ReturnValue

  CASE EVENT()                              ! What event occurred?
  OF EVENT:OpenWindow                       !   OpenWindow
     ReturnValue = SELF.TakeOpenWindow()    !       call event handler
!     DO PaintBorder
  OF EVENT:GainFocus                        !   GainFocus
     ReturnValue = SELF.TakeGainFocus()     !       call event handler
!     DO PaintBorder
  OF EVENT:Sized                            !   Sized
     ReturnValue = SELF.TakeSized()         !       call event handler
!     DO PaintBorder
  OF EVENT:AlertKey                         !   AlertKey
     ReturnValue = SELF.TakeAlertKey()      !       call event handler
  END                                       ! End of Event Processor

  RETURN ReturnValue                        ! Exit the procedure

PaintBorder ROUTINE
  DATA
lb          LIKE(LOGBRUSH)
hOldBrush   UNSIGNED
hBrush      UNSIGNED
hDC         UNSIGNED
hCheckBox   UNSIGNED
rVal        UNSIGNED

  CODE
  lb.lbStyle = BS_SOLID
  lb.lbColor = 0B99D7FH
  lb.lbHatch = 0
  hBrush = CreateBrushIndirect(lb)
  hDC = getWindowDC(SELF.hwnd)
  hOldBrush = SelectObject(hDC,hBrush)
  ExtFloodFill(hDC,0,0,COLOR:BLACK,FLOODFILLSURFACE)
  SelectObject(hDC,hOldBrush)
  DeleteObject(hBrush)
  ReleaseDC(SELF.hwnd,hDC)
  EXIT

! =======================================================================================
! CSciControl.Update
! purpose:  Get VIEW data for the selected item.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.Update PROCEDURE

  CODE                                      ! Enter Procedure

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.UpdateWindow
! purpose:  The UpdateWindow method updates the controls on the window based upon
!           certain conditions set by the WindowComponent object.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.UpdateWindow PROCEDURE

  CODE                                      ! Enter Procedure

  RETURN                                    ! Exit Procedure


!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! WindowComponent Methods
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§


! =======================================================================================
! CSciControl.WindowComponent.Kill
! purpose:  The Kill method releases any memory allocated during the life of the object
!           and performs any other required termination code.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WindowComponent.Kill PROCEDURE

  CODE                                      ! Enter Procedure
     SELF.Kill()

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WindowComponent.Reset
! purpose:  The Reset method resets the object's data if needed or if Force is TRUE.
! inputs :  (BYTE Force)
! outputs:
! returns:
! =======================================================================================
CSciControl.WindowComponent.Reset PROCEDURE(BYTE Force)

  CODE                                      ! Enter Procedure

  SELF.Reset(Force)

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WindowComponent.ResetRequired
! purpose:  The ResetRequired method determines whether the objects data needs to be
!           refreshed.  A TRUE return value indicates a refresh occurred and a screen
!           redraw is necessary.
! inputs :
! outputs:
! returns:  BYTE
! =======================================================================================
CSciControl.WindowComponent.ResetRequired PROCEDURE!,BYTE

ReturnValue BYTE

  CODE                                      ! Enter Procedure

  ReturnValue = SELF.ResetRequired()

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.WindowComponent.SetAlerts
! purpose:  The SetAlerts method alerts standard keystrokes for the control associated
!           with the window componet's object.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WindowComponent.SetAlerts PROCEDURE

  CODE                                      ! Enter Procedure

  SELF.SetAlerts()

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WindowComponent.TakeEvent
! purpose:  Process the current ACCEPT loop event.
! inputs :
! outputs:
! returns:  BYTE
! =======================================================================================
CSciControl.WindowComponent.TakeEvent PROCEDURE!,BYTE

ReturnValue BYTE

  CODE                                      ! Enter Procedure

  ReturnValue = SELF.TakeEvent()

  RETURN ReturnValue                        ! Exit Procedure


! =======================================================================================
! CSciControl.WindowComponent.Update
! purpose:  Get VIEW data for the selected item.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WindowComponent.Update PROCEDURE

  CODE                                      ! Enter Procedure

  SELF.Update()

  RETURN                                    ! Exit Procedure


! =======================================================================================
! CSciControl.WindowComponent.UpdateWindow
! purpose:  The UpdateWindow method updates the controls on the window based upon
!           certain conditions set by the WindowComponent object.
! inputs :
! outputs:
! returns:
! =======================================================================================
CSciControl.WindowComponent.UpdateWindow PROCEDURE

  CODE                                      ! Enter Procedure

  SELF.UpdateWindow()

  RETURN                                    ! Exit Procedure


!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
! Module Procedures - implicitly PRIVATE
!§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

! =======================================================================================
! WindowProc
! purpose:  The Window Procedure Callback
! inputs :  UNSIGNED hWnd - The handle of the window receiving the message
!           UNSIGNED uMsg - The message
!           LONG wParam   - The wParam data of the message (depends on uMsg)
!           LONG lParam   - The lParam data of the message (depends on uMsg)
! outputs:
! returns:  LONG - The return value of the message processing
! =======================================================================================
WindowProc     PROCEDURE(UNSIGNED hWnd, |
                         UNSIGNED uMsg, |
                         LONG wParam,   |
                         LONG lParam)   !,LONG,PASCAL

lb          LIKE(LOGBRUSH)
hOldBrush   UNSIGNED
hBrush      UNSIGNED
hDC         UNSIGNED
hCheckBox   UNSIGNED
rVal        UNSIGNED
_hwnd       UNSIGNED
crColor     ULONG
crBrush     ULONG(0B99D7FH)
thisControl &CSciControl,AUTO

  CODE                                      ! Enter Procedure

  SubClassed.hWnd = hwnd
  GET(SubClassed,+SubClassed.hWnd)
  ASSERT(~ERRORCODE())
  OrigWindowProc &= SubClassed.WndProc

  CASE uMsg
  OF WM_PAINT
     rVal = CallWindowProc(OrigWindowProc, hWnd, uMsg, wParam, lParam )

     thisControl &= GetWindowLong(hwnd, GWL_USERDATA)
     IF ~thisControl &= NULL
        hDC = getWindowDC(hwnd)
        crColor = GetPixel(hDC,0,0)
        IF crColor <> crBrush
           lb.lbStyle = BS_SOLID
           lb.lbColor = crBrush
           lb.lbHatch = 0
           hBrush = CreateBrushIndirect(lb)
           hOldBrush = SelectObject(hDC,hBrush)
           ExtFloodFill(hDC,0,0,crColor,FLOODFILLSURFACE)
           SelectObject(hDC,hOldBrush)
           DeleteObject(hBrush)
        END
        ReleaseDC(hwnd,hDC)
     END

     RETURN(rVal)
  END

  RETURN CallWindowProc(OrigWindowProc, hWnd, uMsg, wParam, lParam )
                                            ! Exit Procedure

! =======================================================================================
! ClientWndProc
! purpose:  The ClientWindow Procedure Callback
! inputs :  UNSIGNED hWnd - The handle of the window receiving the message
!           UNSIGNED uMsg - The message
!           LONG wParam   - The wParam data of the message (depends on uMsg)
!           LONG lParam   - The lParam data of the message (depends on uMsg)
! outputs:
! returns:  LONG - The return value of the message processing
! =======================================================================================
ClientWndProc  PROCEDURE(UNSIGNED hWnd, |
                         UNSIGNED uMsg, |
                         LONG wParam,   |
                         LONG lParam)   !,LONG,PASCAL

NM          &NMHDR,AUTO
SCN         &SCNotification,AUTO
thisControl &CSciControl,AUTO
szClassName CSTRING(32),AUTO
hwndControl LONG,AUTO
  OMIT('End_Omit')
SCNotification          GROUP,TYPE
NotifyHeader              LIKE(NMHDR)
Position                  LONG          ! SCN_STYLENEEDED, SCN_MODIFIED, SCN_DWELLSTART,
                                        ! SCN_DWELLEND, SCN_CALLTIPCLICK,
                                        ! SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK
ch                        LONG          ! SCN_CHARADDED, SCN_KEY
modifiers                 LONG          ! SCN_KEY, SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK
modificationType          LONG          ! SCN_MODIFIED
pszText                   LONG          ! SCN_MODIFIED
length                    LONG          ! SCN_MODIFIED
linesAdded                LONG          ! SCN_MODIFIED
message                   LONG          ! SCN_MACRORECORD
wParam                    LONG          ! SCN_MACRORECORD
lParam                    LONG          ! SCN_MACRORECORD
line                      LONG          ! SCN_MODIFIED
foldLevelNow              LONG          ! SCN_MODIFIED
foldLevelPrev             LONG          ! SCN_MODIFIED
margin                    LONG          ! SCN_MARGINCLICK
listType                  LONG          ! SCN_USERLISTSELECTION
x                         LONG          ! SCN_DWELLSTART, SCN_DWELLEND
y                         LONG          ! SCN_DWELLSTART, SCN_DWELLEND
                        END
  !End_Omit

  CODE                                      ! Enter Procedure

  CASE uMsg
  OF WM_CONTEXTMENU
     IF hWnd = SubClassedClient.hWnd
        szClassName = ''
        IF GetClassName(wParam, szClassName, 32) <> 0
           IF szClassName = 'Scintilla'
              thisControl &= GetWindowLong(wParam, GWL_USERDATA)
              IF ~thisControl &= NULL
                 POST(thisControl.lContextMenuEvent,thisControl.feq)
              END
           END
        END
     END

  OF WM_COMMAND
     IF BSHIFT(BAND(wParam,0ffff0000h),-16) = SCEN_CHANGE
        hwndControl = GetDlgItem(hWnd, BAND(wParam,0ffffh))
        IF hwndControl <> 0
           szClassName = ''
           IF GetClassName(hwndControl, szClassName, 32) <> 0
              IF szClassName = 'Scintilla'
                 thisControl &= GetWindowLong(hwndControl, GWL_USERDATA)
                 IF ~thisControl &= NULL
                    POST(SCEN_CHANGE,thisControl.feq,thisControl.thread)
                 END
              END
           END
        END
     ELSIF BSHIFT(BAND(wParam,0ffff0000h),-16) = SCEN_SETFOCUS
        hwndControl = GetDlgItem(hWnd, BAND(wParam,0ffffh))
        IF hwndControl <> 0
           szClassName = ''
           IF GetClassName(hwndControl, szClassName, 32) <> 0
              IF szClassName = 'Scintilla'
                 thisControl &= GetWindowLong(hwndControl, GWL_USERDATA)
                 IF ~thisControl &= NULL
                    POST(SCEN_SETFOCUS,thisControl.feq,thisControl.thread)
                 END
              END
           END
        END
     ELSIF BSHIFT(BAND(wParam,0ffff0000h),-16) = SCEN_KILLFOCUS
        hwndControl = GetDlgItem(hWnd, BAND(wParam,0ffffh))
        IF hwndControl <> 0
           szClassName = ''
           IF GetClassName(hwndControl, szClassName, 32) <> 0
              IF szClassName = 'Scintilla'
                 thisControl &= GetWindowLong(hwndControl, GWL_USERDATA)
                 IF ~thisControl &= NULL
                    POST(SCEN_KILLFOCUS,thisControl.feq,thisControl.thread)
                 END
              END
           END
        END
     END

  OF WM_NOTIFY
     NM &= (lParam)
     !--------------------------------------------------------------
     ! When the Scintilla edit control was created, we gave it an id.
     ! This and the hWnd of the control can be used for identification
     ! of the notification message.  To get the hwnd of the control
     ! we placed the address of our class instance in the USERDATA of
     ! thecontrol window.
     ! Notify messages can come from anywhere and there may be other
     ! controls that generate notification messages that are using the
     ! USERDATA in their window for some purpose.
     ! Before assigning the class reference, we need to know that
     ! the window that sent the notification message is one of our
     ! scintilla control windows.  We check by looking at the window
     ! class of the notification window.
     ! If we find our class name, we know it is our control and that
     ! the USERDATA contains the address of the class. We then use it
     ! to reference our class.
     !--------------------------------------------------------------
     szClassName = ''
     IF GetClassName(NM.hwndFrom, szClassName, 32) <> 0
        IF szClassName = 'Scintilla'
           thisControl &= GetWindowLong(NM.hwndFrom, GWL_USERDATA)
           IF ~thisControl &= NULL
              CASE NM.Code
              OF    SCN_MARGINCLICK
                 SCN &= (lParam)
                 thisControl.MarginClickPosition = SCN.Position
                 IF NM.hwndFrom = thisControl.hWnd AND NM.idFrom = thisControl.id
                    POST(NM.Code,thisControl.feq,thisControl.thread)
                 END
              OF    SCN_HOTSPOTCLICK        |
              OROF  SCN_HOTSPOTDOUBLECLICK
                 SCN &= (lParam)
                 thisControl.HotSpotClickPosition = SCN.Position
                 IF NM.hwndFrom = thisControl.hWnd AND NM.idFrom = thisControl.id
                    POST(NM.Code,thisControl.feq,thisControl.thread)
                 END
              OF    SCN_UPDATEUI
                 SCN &= (lParam)
                 thisControl.Updated = SCN.updated
                 IF NM.hwndFrom = thisControl.hWnd AND NM.idFrom = thisControl.id
                    POST(NM.Code,thisControl.feq,thisControl.thread)
                 END
              OF    SCN_DOUBLECLICK
              !   SCN &= (lParam)
              !   thisControl.HotSpotClickPosition = SCN.Position
              !   IF NM.hwndFrom = thisControl.hWnd AND NM.idFrom = thisControl.id
              !      POST(NM.Code,thisControl.feq,thisControl.thread)
              !   END
              END
           END
        END
     END
  END

  RETURN CallWindowProc(OrigClientWndProc, hWnd, uMsg, wParam, lParam )
  ! Exit Procedure

