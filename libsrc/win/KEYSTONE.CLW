 SECTION('equates')
 include('svapi.inc'),ONCE
 include('wintypes.txt'),ONCE

 OMIT('_EndOfOmit_',_KeystoneEquatesPresent_)
_KeystoneEquatesPresent_    EQUATE(1)

!Class field offsets for GetClassLong()
GCL_MENUNAME            EQUATE(-8)
GCL_HBRBACKGROUND       EQUATE(-10)
GCL_HCURSOR             EQUATE(-12)
GCL_HICONx              EQUATE(-14)  ! Changed from GCL_HICON to avoid conflict with Clarion 7. KV 2010.10.21
GCL_HMODULE             EQUATE(-16)
GCL_CBWNDEXTRA          EQUATE(-18)
GCL_CBCLSEXTRA          EQUATE(-20)
GCL_WNDPROC             EQUATE(-24)
GCL_STYLE               EQUATE(-26)
GCW_ATOM                EQUATE(-32)
GCL_HICONSM             EQUATE(-34)

! _EndOfOmit_
!======================================================================================

! Color Types
CTLCOLOR_MSGBOX                 EQUATE(0)
CTLCOLOR_EDIT                   EQUATE(1)
CTLCOLOR_LISTBOX                EQUATE(2)
CTLCOLOR_BTN                    EQUATE(3)
CTLCOLOR_DLG                    EQUATE(4)
CTLCOLOR_SCROLLBAR              EQUATE(5)
CTLCOLOR_STATIC                 EQUATE(6)
CTLCOLOR_MAX                    EQUATE(7)

COLOR_HOTLIGHT                  EQUATE(26)
COLOR_GRADIENTACTIVECAPTION     EQUATE(27)
COLOR_GRADIENTINACTIVECAPTION   EQUATE(28)

! Brush Styles 
BS_SOLID                    EQUATE(0)
BS_NULL                     EQUATE(1)
BS_HOLLOW                   EQUATE(BS_NULL)
BS_HATCHED                  EQUATE(2)
BS_PATTERN                  EQUATE(3)
BS_INDEXED                  EQUATE(4)
BS_DIBPATTERN               EQUATE(5)
BS_DIBPATTERNPT             EQUATE(6)
BS_PATTERN8X8               EQUATE(7)
BS_DIBPATTERN8X8            EQUATE(8)
BS_MONOPATTERN              EQUATE(9)

AD_COUNTERCLOCKWISE         EQUATE(1)
AD_CLOCKWISE                EQUATE(2)

AT      GROUP,PRE(),TYPE
xpos     UNSIGNED
ypos     UNSIGNED
width    UNSIGNED
height   UNSIGNED
        END
        
!Segoe UI Font Weights
FONT:Light              EQUATE(200)        
FONT:SemiLight          EQUATE(300)        
FONT:SemiBold           EQUATE(600)

! OLE DOVERB equates
DOVERB:Primary          EQUATE(0)
DOVERB:Show             EQUATE(-1)
DOVERB:Open             EQUATE(-2)
DOVERB:Hide             EQUATE(-3)
DOVERB:UIActivate       EQUATE(-4)
DOVERB:InPlaceActivate  EQUATE(-7)
DOVERB:DiscardUndoState EQUATE(-6)
DOVERB:Properties       EQUATE(-7)

!Windows Message Equates
 OMIT('_EndOfInclude_', _DateTimePickClassPresent_)

_WM_USER_               EQUATE(1)
!_EndOfInclude_
 COMPILE('_EndOfInclude_', _DateTimePickClassPresent_)
_WM_USER_               EQUATE(1)
!_EndOfInclude_

! WM_ACTIVATE state values
WA_INACTIVE             EQUATE(0)
WA_ACTIVE               EQUATE(1)
WA_CLICKACTIVE          EQUATE(2)

WM_CHILDACTIVATE        EQUATE(00022h)

WM_SETHOTKEY            EQUATE(00032h)
WM_GETHOTKEY            EQUATE(00033h)

WM_GETOBJECT            EQUATE(0003Dh)

WM_COMMNOTIFY           EQUATE(00044h)  ! no longer suported

WM_POWER                EQUATE(00048h)
! wParam for WM_POWER window message and DRV_POWER driver notification
PWR_OK                  EQUATE(1)
PWR_FAIL                EQUATE(-1)
PWR_SUSPENDREQUEST      EQUATE(1)
PWR_SUSPENDRESUME       EQUATE(2)
PWR_CRITICALRESUME      EQUATE(3)

WM_COPYDATA             EQUATE(0004Ah)
WM_CANCELJOURNAL        EQUATE(0004Bh)

WM_SYNCPAINT            EQUATE(00088h)

WM_IME_STARTCOMPOSITION EQUATE(0010Dh)
WM_IME_ENDCOMPOSITION   EQUATE(0010Eh)
WM_IME_COMPOSITION      EQUATE(0010Fh)
WM_IME_KEYLAST          EQUATE(0010Fh)

WM_MENURBUTTONUP        EQUATE(00122h)
WM_MENUDRAG             EQUATE(00123h)
WM_MENUGETOBJECT        EQUATE(00124h)
WM_UNINITMENUPOPUP      EQUATE(00125h)
WM_MENUCOMMAND          EQUATE(00126h)

WM_CTLCOLORMSGBOX       EQUATE(00132h)
WM_CTLCOLOREDIT         EQUATE(00133h)
WM_CTLCOLORLISTBOX      EQUATE(00134h)
WM_CTLCOLORBTN          EQUATE(00135h)
WM_CTLCOLORDLG          EQUATE(00136h)
WM_CTLCOLORSCROLLBAR    EQUATE(00137h)
WM_CTLCOLORSTATIC       EQUATE(00138h)

WM_MOUSEWHEEL           EQUATE(0020Ah)

WHEEL_DELTA             EQUATE(120)         ! Value for rolling one detent

WM_ENTERMENULOOP        EQUATE(00211h)
WM_EXITMENULOOP         EQUATE(00212h)

WM_NEXTMENU             EQUATE(00213h)

WM_SIZING               EQUATE(00214h)
WM_CAPTURECHANGED       EQUATE(00215h)
WM_MOVING               EQUATE(00216h)

   OMIT('_EndOfInclude_',_SysShutdownPresent_)
WM_POWERBROADCAST       EQUATE(00218h)   !RR
   !_EndOfInclude_

PBT_APMQUERYSUSPEND     EQUATE(00000h)
PBT_APMQUERYSTANDBY     EQUATE(00001h)

PBT_APMQUERYSUSPENDFAILED EQUATE(00002h)
PBT_APMQUERYSTANDBYFAILED EQUATE(00003h)

PBT_APMSUSPEND          EQUATE(00004h)
PBT_APMSTANDBY          EQUATE(00005h)

PBT_APMRESUMECRITICAL   EQUATE(00006h)
PBT_APMRESUMESUSPEND    EQUATE(00007h)
PBT_APMRESUMESTANDBY    EQUATE(00008h)

PBTF_APMRESUMEFROMFAILURE EQUATE(000000001h)

PBT_APMBATTERYLOW       EQUATE(00009h)
PBT_APMPOWERSTATUSCHANGE  EQUATE(0000Ah)

PBT_APMOEMEVENT         EQUATE(0000Bh)
PBT_APMRESUMEAUTOMATIC  EQUATE(00012h)

   OMIT('_EndOfInclude_',_SysShutdownPresent_)
WM_DEVICECHANGE         EQUATE(00219h)
   !_EndOfInclude_
   
WM_ENTERSIZEMOVE        EQUATE(00231h)
WM_EXITSIZEMOVE         EQUATE(00232h)
WM_DROPFILES            EQUATE(00233h)
WM_MDIREFRESHMENU       EQUATE(00234h)

WM_IME_SETCONTEXT       EQUATE(00281h)
WM_IME_NOTIFY           EQUATE(00282h)
WM_IME_CONTROL          EQUATE(00283h)
WM_IME_COMPOSITIONFULL  EQUATE(00284h)
WM_IME_SELECT           EQUATE(00285h)
WM_IME_CHAR             EQUATE(00286h)

WM_IME_REQUEST          EQUATE(00288h)

WM_IME_KEYDOWN          EQUATE(00290h)
WM_IME_KEYUP            EQUATE(00291h)

WM_MOUSEHOVER           EQUATE(002A1h)
WM_MOUSELEAVE           EQUATE(002A3h)

WM_HOTKEY               EQUATE(00312h)

WM_PRINT                EQUATE(00317h)
WM_PRINTCLIENT          EQUATE(00318h)

WM_HANDHELDFIRST        EQUATE(00358h)
WM_HANDHELDLAST         EQUATE(0035Fh)

WM_AFXFIRST             EQUATE(00360h)
WM_AFXLAST              EQUATE(0037Fh)

WM_PENWINFIRST          EQUATE(00380h)
WM_PENWINLAST           EQUATE(0038Fh)

WM_APP                  EQUATE(08000h)
!
! NOTE: All Message Numbers below EQUATE(00400 are RESERVED.
!
! Private Window Messages Start Here:
!                 
  COMPILE('End_Compile',_WINDOWS_)
_WM_USER_               EQUATE(1)
  !End_Compile
  OMIT('End_Omit',_WM_USER_)
WM_USER                 EQUATE(00400h)
  !End_Omit

! wParam for WM_SIZING message
WMSZ_LEFT               EQUATE(1)
WMSZ_RIGHT              EQUATE(2)
WMSZ_TOP                EQUATE(3)
WMSZ_TOPLEFT            EQUATE(4)
WMSZ_TOPRIGHT           EQUATE(5)
WMSZ_BOTTOM             EQUATE(6)
WMSZ_BOTTOMLEFT         EQUATE(7)
WMSZ_BOTTOMRIGHT        EQUATE(8)

! WM_NCHITTEST and MOUSEHOOKSTRUCT Mouse Position Codes
HTERROR                 EQUATE(-2)
HTTRANSPARENT           EQUATE(-1)
HTNOWHERE               EQUATE(0)
HTCLIENT                EQUATE(1)
HTCAPTION               EQUATE(2)
HTSYSMENU               EQUATE(3)
HTGROWBOX               EQUATE(4)
HTSIZE                  EQUATE(HTGROWBOX)
HTMENU                  EQUATE(5)
HTHSCROLL               EQUATE(6)
HTVSCROLL               EQUATE(7)
HTMINBUTTON             EQUATE(8)
HTMAXBUTTON             EQUATE(9)
HTLEFT                  EQUATE(10)
HTRIGHT                 EQUATE(11)
HTTOP                   EQUATE(12)
HTTOPLEFT               EQUATE(13)
HTTOPRIGHT              EQUATE(14)
HTBOTTOM                EQUATE(15)
HTBOTTOMLEFT            EQUATE(16)
HTBOTTOMRIGHT           EQUATE(17)
HTBORDER                EQUATE(18)
HTREDUCE                EQUATE(HTMINBUTTON)
HTZOOM                  EQUATE(HTMAXBUTTON)
HTSIZEFIRST             EQUATE(HTLEFT)
HTSIZELAST              EQUATE(HTBOTTOMRIGHT)

HTOBJECT                EQUATE(19)
HTCLOSE                 EQUATE(20)
HTHELP                  EQUATE(21)

! SendMessageTimeout values
SMTO_NORMAL             EQUATE(00000h)
SMTO_BLOCK              EQUATE(00001h)
SMTO_ABORTIFHUNG        EQUATE(00002h)

SMTO_NOTIMEOUTIFNOTHUNG EQUATE(00008h)

CW_USEDEFAULT           EQUATE(080000000h)

! Map modes EQUATES
MM_TEXT             EQUATE(1)
MM_LOMETRIC         EQUATE(2)
MM_HIMETRIC         EQUATE(3)
MM_LOENGLISH        EQUATE(4)
MM_HIENGLISH        EQUATE(5)
MM_TWIPS            EQUATE(6)
MM_ISOTROPIC        EQUATE(7)
MM_ANISOTROPIC      EQUATE(8)

! PRNTDLG Flag Equates
PD_ALLPAGES                  EQUATE(000000000h)
PD_SELECTION                 EQUATE(000000001h)
PD_PAGENUMS                  EQUATE(000000002h)
PD_NOSELECTION               EQUATE(000000004h)
PD_NOPAGENUMS                EQUATE(000000008h)
PD_COLLATE                   EQUATE(000000010h)
PD_PRINTTOFILE               EQUATE(000000020h)
PD_PRINTSETUP                EQUATE(000000040h)
PD_NOWARNING                 EQUATE(000000080h)
PD_RETURNDC                  EQUATE(000000100h)
PD_RETURNIC                  EQUATE(000000200h)
PD_RETURNDEFAULT             EQUATE(000000400h)
PD_SHOWHELP                  EQUATE(000000800h)
PD_ENABLEPRINTHOOK           EQUATE(000001000h)
PD_ENABLESETUPHOOK           EQUATE(000002000h)
PD_ENABLEPRINTTEMPLATE       EQUATE(000004000h)
PD_ENABLESETUPTEMPLATE       EQUATE(000008000h)
PD_ENABLEPRINTTEMPLATEHANDLE EQUATE(000010000h)
PD_ENABLESETUPTEMPLATEHANDLE EQUATE(000020000h)
PD_USEDEVMODECOPIES          EQUATE(000040000h)
PD_DISABLEPRINTTOFILE        EQUATE(000080000h)
PD_HIDEPRINTTOFILE           EQUATE(000100000h)
PD_NONETWORKBUTTON           EQUATE(000200000h)

! ExtDeviceMode wMode Equates
DM_UPDATE           equate(1)
DM_COPY             equate(2)
DM_PROMPT           equate(4)
DM_MODIFY           equate(8)

DM_ORIENTATION      equate(0000001h)
DM_PAPERSIZE        equate(0000002h)
DM_PAPERLENGTH      equate(0000004h)
DM_PAPERWIDTH       equate(0000008h)
DM_SCALE            equate(0000010h)
DM_COPIES           equate(0000100h)
DM_DEFAULTSOURCE    equate(0000200h)
DM_PRINTQUALITY     equate(0000400h)
DM_COLOR            equate(0000800h)
DM_DUPLEX           equate(0001000h)
DM_YRESOLUTION      equate(0002000h)
DM_TTOPTION         equate(0004000h)

!* orientation selections *!
DMORIENT_PORTRAIT   equate(1)
DMORIENT_LANDSCAPE  equate(2)

!* paper selections *!
!*  Warning: The PostScript driver mistakingly uses DMPAPER_ values between *!
!*  50 and 56.  Don't use this range when defining new paper sizes.         *!

DMPAPER_LETTER              equate(1)  !* Letter 8 1/2 x 11 in               *!
DMPAPER_FIRST               equate(DMPAPER_LETTER)
DMPAPER_LETTERSMALL         equate(2)  !* Letter Small 8 1/2 x 11 in         *!
DMPAPER_TABLOID             equate(3)  !* Tabloid 11 x 17 in                 *!
DMPAPER_LEDGER              equate(4)  !* Ledger 17 x 11 in                  *!
DMPAPER_LEGAL               equate(5)  !* Legal 8 1/2 x 14 in                *!
DMPAPER_STATEMENT           equate(6)  !* Statement 5 1/2 x 8 1/2 in         *!
DMPAPER_EXECUTIVE           equate(7)  !* Executive 7 1/4 x 10 1/2 in        *!
DMPAPER_A3                  equate(8)  !* A3 297 x 420 mm                    *!
DMPAPER_A4                  equate(9)  !* A4 210 x 297 mm                    *!
DMPAPER_A4SMALL             equate(10) !* A4 Small 210 x 297 mm              *!
DMPAPER_A5                  equate(11) !* A5 148 x 210 mm                    *!
DMPAPER_B4                  equate(12) !* B4 250 x 354                       *!
DMPAPER_B5                  equate(13) !* B5 182 x 257 mm                    *!
DMPAPER_FOLIO               equate(14) !* Folio 8 1/2 x 13 in                *!
DMPAPER_QUARTO              equate(15) !* Quarto 215 x 275 mm                *!
DMPAPER_10X14               equate(16) !* 10X14 in                           *!
DMPAPER_11X17               equate(17) !* 11x17 in                           *!
DMPAPER_NOTE                equate(18) !* Note 8 1/2 x 11 in                 *!
DMPAPER_ENV_9               equate(19) !* Envelope #9 3 7/8 x 8 7/8          *!
DMPAPER_ENV_10              equate(20) !* Envelope #10 4 1/8 x 9 1/2         *!
DMPAPER_ENV_11              equate(21) !* Envelope #11 4 1/2 x 10 3/8        *!
DMPAPER_ENV_12              equate(22) !* Envelope #12 4 \276 x 11           *!
DMPAPER_ENV_14              equate(23) !* Envelope #14 5 x 11 1/2            *!
DMPAPER_CSHEET              equate(24) !* C size sheet                       *!
DMPAPER_DSHEET              equate(25) !* D size sheet                       *!
DMPAPER_ESHEET              equate(26) !* E size sheet                       *!
DMPAPER_ENV_DL              equate(27) !* Envelope DL 110 x 220mm            *!
DMPAPER_ENV_C5              equate(28) !* Envelope C5 162 x 229 mm           *!
DMPAPER_ENV_C3              equate(29) !* Envelope C3  324 x 458 mm          *!
DMPAPER_ENV_C4              equate(30) !* Envelope C4  229 x 324 mm          *!
DMPAPER_ENV_C6              equate(31) !* Envelope C6  114 x 162 mm          *!
DMPAPER_ENV_C65             equate(32) !* Envelope C65 114 x 229 mm          *!
DMPAPER_ENV_B4              equate(33) !* Envelope B4  250 x 353 mm          *!
DMPAPER_ENV_B5              equate(34) !* Envelope B5  176 x 250 mm          *!
DMPAPER_ENV_B6              equate(35) !* Envelope B6  176 x 125 mm          *!
DMPAPER_ENV_ITALY           equate(36) !* Envelope 110 x 230 mm              *!
DMPAPER_ENV_MONARCH         equate(37) !* Envelope Monarch 3.875 x 7.5 in    *!
DMPAPER_ENV_PERSONAL        equate(38) !* 6 3/4 Envelope 3 5/8 x 6 1/2 in    *!
DMPAPER_FANFOLD_US          equate(39) !* US Std Fanfold 14 7/8 x 11 in      *!
DMPAPER_FANFOLD_STD_GERMAN  equate(40) !* German Std Fanfold 8 1/2 x 12 in   *!
DMPAPER_FANFOLD_LGL_GERMAN  equate(41) !* German Legal Fanfold 8 1/2 x 13 in *!

DMPAPER_LAST        equate(DMPAPER_FANFOLD_LGL_GERMAN)

DMPAPER_USER        equate(256)

!* bin selections *!
DMBIN_UPPER         equate(1)
DMBIN_FIRST         equate(DMBIN_UPPER)
DMBIN_ONLYONE       equate(1)
DMBIN_LOWER         equate(2)
DMBIN_MIDDLE        equate(3)
DMBIN_MANUAL        equate(4)
DMBIN_ENVELOPE      equate(5)
DMBIN_ENVMANUAL     equate(6)
DMBIN_AUTO          equate(7)
DMBIN_TRACTOR       equate(8)
DMBIN_SMALLFMT      equate(9)
DMBIN_LARGEFMT      equate(10)
DMBIN_LARGECAPACITY equate(11)
DMBIN_CASSETTE      equate(14)
DMBIN_LAST          equate(DMBIN_CASSETTE)

DMBIN_USER          equate(256)    !* device specific bins start here *!

!* print qualities *!
DMRES_DRAFT         equate(-1)
DMRES_LOW           equate(-2)
DMRES_MEDIUM        equate(-3)
DMRES_HIGH          equate(-4)

!* color enable/disable for color printers *!
DMCOLOR_MONOCHROME  equate(1)
DMCOLOR_COLOR       equate(2)

!* duplex enable *!
DMDUP_SIMPLEX    equate(1)
DMDUP_VERTICAL   equate(2)
DMDUP_HORIZONTAL equate(3)

!* TrueType options *!
DMTT_BITMAP     equate(1)      !* print TT fonts as graphics *!
DMTT_DOWNLOAD   equate(2)      !* download TT fonts as soft fonts *!
DMTT_SUBDEV     equate(3)      !* substitute device fonts for TT fonts *!

! PRINTDLG type declaration
PRINTDLG        group,type
lStructSize         ulong
hwndOwner           unsigned
hDevMode            unsigned
hDevNames           unsigned
hDC                 unsigned
Flags               ulong
nFromPage           ushort
nToPage             ushort
nMinPage            ushort
nMaxPage            ushort
nCopies             ushort
hInstance           unsigned
lCustData           long
lpfnPrintHook       ulong
lpfnSetupHook       ulong
lpPrintTemplateName ulong
lpSetupTemplateName ulong
hPrintTemplate      unsigned
hSetupTemplate      unsigned
                end

! DEVNAMES type declaration
DEVNAMES        group,type
wDriverOffset     ushort
wDeviceOffset     ushort
wOutputOffset     ushort
wDefault          ushort
                end

! DEVMODE type declaration
tagDEVMODE      group,type
DeviceName        cstring(32)
SpecVersion       ushort !signed
DriverVersion     ushort !signed
dmSize            ushort !signed
DriverExtra       ushort !signed
Fields            ulong
Orientation       ushort
PaperSize         ushort
PaperLength       ushort
PaperWidth        ushort
Scale             ushort
Copies            ushort
DefaultSource     ushort
PrintQuality      ushort
Color             ushort
Duplex            ushort
YResolution       ushort
TTOption          ushort
Collate           ushort
FormName          cstring(32)
LogPixels         signed
BitsPerPel        ulong
PelsWidth         ulong
PelsHeight        ulong
DisplayFlags      ulong
DisplayFrequency  ulong
ICMMethod         ulong
ICMIntent         ulong
MediaType         ulong
DitherType        ulong
Reserved1         ulong
Reserved2         ulong
                 end

! Predefined Resource Types

RT_HTML             EQUATE(23)

! Code Page Default Values.

 COMPILE('End_Compile',_nettalk_=1)
_Registry_Equates_Present_      EQUATE(1)
!Security Access Mask values
KEY_QUERY_VALUE                 EQUATE(00001h)
KEY_SET_VALUE                   EQUATE(00002h)
KEY_CREATE_SUB_KEY              EQUATE(00004h)
KEY_ENUMERATE_SUB_KEYS          EQUATE(00008h)
KEY_NOTIFY                      EQUATE(00010h)
KEY_CREATE_LINK                 EQUATE(00020h)

! The following are masks for the predefined standard access types
DELETE                          EQUATE(000010000h)
READ_CONTROL                    EQUATE(000020000h)
WRITE_DAC                       EQUATE(000040000h)
WRITE_OWNER                     EQUATE(000080000h)
SYNCHRONIZE                     EQUATE(000100000h)

STANDARD_RIGHTS_REQUIRED        EQUATE(0000F0000h)

STANDARD_RIGHTS_READ            EQUATE(READ_CONTROL)
STANDARD_RIGHTS_WRITE           EQUATE(READ_CONTROL)
STANDARD_RIGHTS_EXECUTE         EQUATE(READ_CONTROL)

STANDARD_RIGHTS_ALL             EQUATE(0001F0000h)

SPECIFIC_RIGHTS_ALL             EQUATE(00000FFFFh)

KEY_READ                        EQUATE(000020019h)
KEY_WRITE                       EQUATE(000020006h)
KEY_EXECUTE                     EQUATE(KEY_READ)
KEY_ALL_ACCESS                  EQUATE(0001F003Fh)

REG_OPTION_NON_VOLATILE         EQUATE(00000000h)
REG_CREATED_NEW_KEY             EQUATE(00000001h)   ! New Registry Key created
REG_OPENED_EXISTING_KEY         EQUATE(00000002h)   ! Existing Key Opened

!some error codes
!ERROR_SUCCESS                   EQUATE(0)
ERROR_MORE_ITEMS                EQUATE(234)
!ERROR_NO_MORE_ITEMS             EQUATE(259)
!End_Compile

!Well known registry keys
HKEY_CLASSES_ROOT               EQUATE(080000000h)
HKEY_CURRENT_USER               EQUATE(080000001h)
HKEY_LOCAL_MACHINE              EQUATE(080000002h)
HKEY_USERS                      EQUATE(080000003h)
HKEY_PERFORMANCE_DATA           EQUATE(080000004h)
HKEY_CURRENT_CONFIG             EQUATE(080000005h)
HKEY_DYN_DATA                   EQUATE(080000006h)

 OMIT('End_Omit',_Registry_Equates_Present_=1)
_Registry_Equates_Present_      EQUATE(1)

!Registry Data Types
!==========================================================
! 2003.08.26 RR - commented out Registry Data Type equates
!                 as they are in C6.0 equates.clw
!==========================================================
!REG_NONE                        EQUATE(0)    !No value type
!REG_SZ                          EQUATE(1)    !Unicode nul terminated string
!REG_EXPAND_SZ                   EQUATE(2)    !Unicode nul terminated string
!                                             !(with environment variable references)
!REG_BINARY                      EQUATE(3)    !Free form binary
!REG_DWORD                       EQUATE(4)    !32-bit number
!REG_DWORD_LITTLE_ENDIAN         EQUATE(4)    !32-bit number (same as REG_DWORD)
!REG_DWORD_BIG_ENDIAN            EQUATE(5)    !32-bit number
!REG_LINK                        EQUATE(6)    !Symbolic Link (unicode)
!REG_MULTI_SZ                    EQUATE(7)    !Multiple Unicode strings
!REG_RESOURCE_LIST               EQUATE(8)    !Resource list in the resource map
!REG_FULL_RESOURCE_DESCRIPTOR    EQUATE(9)    !Resource list in the hardware description
!REG_RESOURCE_REQUIREMENTS_LIST  EQUATE(10)
!==========================================================

!Security Access Mask values
KEY_QUERY_VALUE                 EQUATE(00001h)
KEY_SET_VALUE                   EQUATE(00002h)
KEY_CREATE_SUB_KEY              EQUATE(00004h)
KEY_ENUMERATE_SUB_KEYS          EQUATE(00008h)
KEY_NOTIFY                      EQUATE(00010h)
KEY_CREATE_LINK                 EQUATE(00020h)

! The following are masks for the predefined standard access types
DELETE                          EQUATE(000010000h)
READ_CONTROL                    EQUATE(000020000h)
WRITE_DAC                       EQUATE(000040000h)
WRITE_OWNER                     EQUATE(000080000h)
SYNCHRONIZE                     EQUATE(000100000h)

STANDARD_RIGHTS_REQUIRED        EQUATE(0000F0000h)

STANDARD_RIGHTS_READ            EQUATE(READ_CONTROL)
STANDARD_RIGHTS_WRITE           EQUATE(READ_CONTROL)
STANDARD_RIGHTS_EXECUTE         EQUATE(READ_CONTROL)

STANDARD_RIGHTS_ALL             EQUATE(0001F0000h)

SPECIFIC_RIGHTS_ALL             EQUATE(00000FFFFh)

KEY_READ                        EQUATE(000020019h)
KEY_WRITE                       EQUATE(000020006h)
KEY_EXECUTE                     EQUATE(KEY_READ)
KEY_ALL_ACCESS                  EQUATE(0001F003Fh)

REG_OPTION_NON_VOLATILE         EQUATE(00000000h)
REG_CREATED_NEW_KEY             EQUATE(00000001h)   ! New Registry Key created
REG_OPENED_EXISTING_KEY         EQUATE(00000002h)   ! Existing Key Opened

!some error codes

ERROR_MORE_ITEMS                EQUATE(234)

!End_Omit

! File creation flags must start at the high end since they
! are combined with the attributes

FILE_FLAG_OPEN_REPARSE_POINT    EQUATE(000200000h)
FILE_FLAG_OPEN_NO_RECALL        EQUATE(000100000h)

!  These are the generic rights.

!
! Define access rights to files and directories
!
! The FILE_READ_DATA and FILE_WRITE_DATA constants are also defined in
! devioctl.h as FILE_READ_ACCESS and FILE_WRITE_ACCESS. The values for these
! constants *MUST* always be in sync.
! The values are redefined in devioctl.h because they must be available to
! both DOS and NT.
FILE_READ_DATA            EQUATE(00001h)    ! file & pipe
FILE_LIST_DIRECTORY       EQUATE(00001h)    ! directory

FILE_WRITE_DATA           EQUATE(00002h)    ! file & pipe
FILE_ADD_FILE             EQUATE(00002h)    ! directory

FILE_APPEND_DATA          EQUATE(00004h)    ! file
FILE_ADD_SUBDIRECTORY     EQUATE(00004h)    ! directory
FILE_CREATE_PIPE_INSTANCE EQUATE(00004h)    ! named pipe

FILE_READ_EA              EQUATE(00008h)    ! file & directory

FILE_WRITE_EA             EQUATE(00010h)    ! file & directory

FILE_EXECUTE              EQUATE(00020h)    ! file
FILE_TRAVERSE             EQUATE(00020h)    ! directory

FILE_DELETE_CHILD         EQUATE(00040h)    ! directory

FILE_READ_ATTRIBUTES      EQUATE(00080h)    ! all

FILE_WRITE_ATTRIBUTES     EQUATE(00100h)    ! all

FILE_ALL_ACCESS           EQUATE(STANDARD_RIGHTS_REQUIRED + SYNCHRONIZE + 03FFh)

FILE_GENERIC_READ         EQUATE(STANDARD_RIGHTS_READ + FILE_READ_DATA + FILE_READ_ATTRIBUTES + FILE_READ_EA + SYNCHRONIZE)

FILE_GENERIC_WRITE        EQUATE(STANDARD_RIGHTS_WRITE + FILE_WRITE_DATA + FILE_WRITE_ATTRIBUTES + FILE_WRITE_EA + FILE_APPEND_DATA + SYNCHRONIZE)

FILE_GENERIC_EXECUTE      EQUATE(STANDARD_RIGHTS_EXECUTE + FILE_READ_ATTRIBUTES + FILE_EXECUTE + SYNCHRONIZE)

FILE_ATTRIBUTE_ENCRYPTED            EQUATE(000000040h)

FILE_ATTRIBUTE_SPARSE_FILE          EQUATE(000000200h)
FILE_ATTRIBUTE_REPARSE_POINT        EQUATE(000000400h)

FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  EQUATE(000002000h)

MAILSLOT_NO_MESSAGE                 EQUATE(-1)
MAILSLOT_WAIT_FOREVER               EQUATE(-1)

FILE_VOLUME_QUOTAS                  EQUATE(000000020h)
FILE_SUPPORTS_SPARSE_FILES          EQUATE(000000040h)
FILE_SUPPORTS_REPARSE_POINTS        EQUATE(000000080h)
FILE_SUPPORTS_REMOTE_STORAGE        EQUATE(000000100h)

FILE_SUPPORTS_OBJECT_IDS            EQUATE(000010000h)
FILE_SUPPORTS_ENCRYPTION            EQUATE(000020000h)

!WM_PRINT flags
PRF_CHECKVISIBLE    EQUATE(000000001h)
PRF_NONCLIENT       EQUATE(000000002h)
PRF_CLIENT          EQUATE(000000004h)
PRF_ERASEBKGND      EQUATE(000000008h)
PRF_CHILDREN        EQUATE(000000010h)
PRF_OWNED           EQUATE(000000020h)

! Binary raster ops
R2_BLACK            EQUATE(1)   !  0       
R2_NOTMERGEPEN      EQUATE(2)   ! DPon     
R2_MASKNOTPEN       EQUATE(3)   ! DPna     
R2_NOTCOPYPEN       EQUATE(4)   ! PN       
R2_MASKPENNOT       EQUATE(5)   ! PDna     
R2_NOT              EQUATE(6)   ! Dn       
R2_XORPEN           EQUATE(7)   ! DPx      
R2_NOTMASKPEN       EQUATE(8)   ! DPan     
R2_MASKPEN          EQUATE(9)   ! DPa      
R2_NOTXORPEN        EQUATE(10)  ! DPxn     
R2_NOP              EQUATE(11)  ! D        
R2_MERGENOTPEN      EQUATE(12)  ! DPno     
R2_COPYPEN          EQUATE(13)  ! P        
R2_MERGEPENNOT      EQUATE(14)  ! PDno     
R2_MERGEPEN         EQUATE(15)  ! DPo      
R2_WHITE            EQUATE(16)  !  1       
R2_LAST             EQUATE(16)

! Ternary raster operations

  OMIT('End_Omit',_WINDOWS_)

  !End_Omit

! Device Parameters for GetDeviceCaps() 
DRIVERVERSION       EQUATE(0)           ! Device driver version
TECHNOLOGY          EQUATE(2)           ! Device classification
HORZSIZE            EQUATE(4)           ! Horizontal size in millimeters
VERTSIZE            EQUATE(6)           ! Vertical size in millimeters
HORZRES             EQUATE(8)           ! Horizontal width in pixels
VERTRES             EQUATE(10)          ! Vertical height in pixels
BITSPIXEL           EQUATE(12)          ! Number of bits per pixel
PLANES              EQUATE(14)          ! Number of planes
NUMBRUSHES          EQUATE(16)          ! Number of brushes the device has
NUMPENS             EQUATE(18)          ! Number of pens the device has
NUMMARKERS          EQUATE(20)          ! Number of markers the device has
NUMFONTS            EQUATE(22)          ! Number of fonts the device has
NUMCOLORS           EQUATE(24)          ! Number of colors the device supports
PDEVICESIZE         EQUATE(26)          ! Size required for device descriptor
CURVECAPS           EQUATE(28)          ! Curve capabilities
LINECAPS            EQUATE(30)          ! Line capabilities
POLYGONALCAPS       EQUATE(32)          ! Polygonal capabilities
TEXTCAPS            EQUATE(34)          ! Text capabilities
CLIPCAPS            EQUATE(36)          ! Clipping capabilities
RASTERCAPS          EQUATE(38)          ! Bitblt capabilities
ASPECTX             EQUATE(40)          ! Length of the X leg
ASPECTY             EQUATE(42)          ! Length of the Y leg
ASPECTXY            EQUATE(44)          ! Length of the hypotenuse

SHADEBLENDCAPS      EQUATE(45)          ! Shading and blending caps

LOGPIXELSX          EQUATE(88)          ! Logical pixels/inch in X
LOGPIXELSY          EQUATE(90)          ! Logical pixels/inch in Y

SIZEPALETTE         EQUATE(104)         ! Number of entries in physical palette
NUMRESERVED         EQUATE(106)         ! Number of reserved entries in palette
COLORRES            EQUATE(108)         ! Actual color resolution

! Printing related DeviceCaps. These replace the appropriate Escapes
PHYSICALWIDTH       EQUATE(110)         ! Physical Width in device units
PHYSICALHEIGHT      EQUATE(111)         ! Physical Height in device units
PHYSICALOFFSETX     EQUATE(112)         ! Physical Printable Area x margin
PHYSICALOFFSETY     EQUATE(113)         ! Physical Printable Area y margin
SCALINGFACTORX      EQUATE(114)         ! Scaling factor x
SCALINGFACTORY      EQUATE(115)         ! Scaling factor y

! Display driver specific
VREFRESH            EQUATE(116)         ! Current vertical refresh rate of the
                                        ! display device (for displays only) in Hz
DESKTOPVERTRES      EQUATE(117)         ! Horizontal width of entire desktop in
                                        ! pixels
DESKTOPHORZRES      EQUATE(118)         ! Vertical height of entire desktop in
                                        ! pixels
BLTALIGNMENT        EQUATE(119)         ! Preferred blt alignment

! Device Capability Masks: 
! Device Technologies 
DT_PLOTTER          EQUATE(0)           !Vector plotter
DT_RASDISPLAY       EQUATE(1)           ! Raster display
DT_RASPRINTER       EQUATE(2)           ! Raster printer
DT_RASCAMERA        EQUATE(3)           ! Raster camera
DT_CHARSTREAM       EQUATE(4)           ! Character-stream, PLP
DT_METAFILE         EQUATE(5)           ! Metafile, VDM
DT_DISPFILE         EQUATE(6)           ! Display-file

! Curve Capabilities 
CC_NONE             EQUATE(0)           ! Curves not supported
CC_CIRCLES          EQUATE(1)           ! Can do circles
CC_PIE              EQUATE(2)           ! Can do pie wedges
CC_CHORD            EQUATE(4)           ! Can do chord arcs
CC_ELLIPSES         EQUATE(8)           ! Can do ellipese
CC_WIDE             EQUATE(16)          ! Can do wide lines
CC_STYLED           EQUATE(32)          ! Can do styled lines
CC_WIDESTYLED       EQUATE(64)          ! Can do wide styled lines
CC_INTERIORS        EQUATE(128)         ! Can do interiors
CC_ROUNDRECT        EQUATE(256)         !

! Line Capabilities
LC_NONE             EQUATE(0)           ! Lines not supported
LC_POLYLINE         EQUATE(2)           ! Can do polylines
LC_MARKER           EQUATE(4)           ! Can do markers
LC_POLYMARKER       EQUATE(8)           ! Can do polymarkers
LC_WIDE             EQUATE(16)          ! Can do wide lines
LC_STYLED           EQUATE(32)          ! Can do styled lines
LC_WIDESTYLED       EQUATE(64)          ! Can do wide styled lines
LC_INTERIORS        EQUATE(128)         ! Can do interiors

! Polygonal Capabilities
PC_NONE             EQUATE(0)           ! Polygonals not supported
PC_POLYGON          EQUATE(1)           ! Can do polygons
PC_RECTANGLE        EQUATE(2)           ! Can do rectangles
PC_WINDPOLYGON      EQUATE(4)           ! Can do winding polygons
PC_TRAPEZOID        EQUATE(4)           ! Can do trapezoids
PC_SCANLINE         EQUATE(8)           ! Can do scanlines
PC_WIDE             EQUATE(16)          ! Can do wide borders
PC_STYLED           EQUATE(32)          ! Can do styled borders
PC_WIDESTYLED       EQUATE(64)          ! Can do wide styled borders
PC_INTERIORS        EQUATE(128)         ! Can do interiors
PC_POLYPOLYGON      EQUATE(256)         ! Can do polypolygons
PC_PATHS            EQUATE(512)         ! Can do paths

! Clipping Capabilities 
CP_NONE             EQUATE(0)           ! No clipping of output
CP_RECTANGLE        EQUATE(1)           ! Output clipped to rects
CP_REGION           EQUATE(2)           ! obsolete

! Text Capabilities 
TC_OP_CHARACTER     EQUATE(000000001h)  ! Can do OutputPrecision   CHARACTER
TC_OP_STROKE        EQUATE(000000002h)  ! Can do OutputPrecision   STROKE
TC_CP_STROKE        EQUATE(000000004h)  ! Can do ClipPrecision     STROKE
TC_CR_90            EQUATE(000000008h)  ! Can do CharRotAbility    90
TC_CR_ANY           EQUATE(000000010h)  ! Can do CharRotAbility    ANY
TC_SF_X_YINDEP      EQUATE(000000020h)  ! Can do ScaleFreedom      X_YINDEPENDENT
TC_SA_DOUBLE        EQUATE(000000040h)  ! Can do ScaleAbility      DOUBLE
TC_SA_INTEGER       EQUATE(000000080h)  ! Can do ScaleAbility      INTEGER
TC_SA_CONTIN        EQUATE(000000100h)  ! Can do ScaleAbility      CONTINUOUS
TC_EA_DOUBLE        EQUATE(000000200h)  ! Can do EmboldenAbility   DOUBLE
TC_IA_ABLE          EQUATE(000000400h)  ! Can do ItalisizeAbility  ABLE
TC_UA_ABLE          EQUATE(000000800h)  ! Can do UnderlineAbility  ABLE
TC_SO_ABLE          EQUATE(000001000h)  ! Can do StrikeOutAbility  ABLE
TC_RA_ABLE          EQUATE(000002000h)  ! Can do RasterFontAble    ABLE
TC_VA_ABLE          EQUATE(000004000h)  ! Can do VectorFontAble    ABLE
TC_RESERVED         EQUATE(000008000h)
TC_SCROLLBLT        EQUATE(000010000h)  ! Don't do text scroll with blt

! Raster Capabilities 
RC_NONE             EQUATE(0)
RC_BITBLT           EQUATE(1)           ! Can do standard BLT.
RC_BANDING          EQUATE(2)           ! Device requires banding support
RC_SCALING          EQUATE(4)           ! Device requires scaling support
RC_BITMAP64         EQUATE(8)           ! Device can support >64K bitmap
RC_GDI20_OUTPUT     EQUATE(00010h)      ! has 2.0 output calls
RC_GDI20_STATE      EQUATE(00020h)
RC_SAVEBITMAP       EQUATE(00040h)
RC_DI_BITMAP        EQUATE(00080h)      ! supports DIB to memory
RC_PALETTE          EQUATE(00100h)      ! supports a palette
RC_DIBTODEV         EQUATE(00200h)      ! supports DIBitsToDevice
RC_BIGFONT          EQUATE(00400h)      ! supports >64K fonts
RC_STRETCHBLT       EQUATE(00800h)      ! supports StretchBlt
RC_FLOODFILL        EQUATE(01000h)      ! supports FloodFill
RC_STRETCHDIB       EQUATE(02000h)      ! supports StretchDIBits
RC_OP_DX_OUTPUT     EQUATE(04000h)
RC_DEVBITS          EQUATE(08000h)

! Shading and blending caps                
SB_NONE             EQUATE(000000000h)
SB_CONST_ALPHA      EQUATE(000000001h)
SB_PIXEL_ALPHA      EQUATE(000000002h)
SB_PREMULT_ALPHA    EQUATE(000000004h)

SB_GRAD_RECT        EQUATE(000000010h)
SB_GRAD_TRI         EQUATE(000000020h)

! StretchBlt() Modes
BLACKONWHITE        EQUATE(1)
WHITEONBLACK        EQUATE(2)
COLORONCOLOR        EQUATE(3)
HALFTONE            EQUATE(4)
MAXSTRETCHBLTMODE   EQUATE(4)

! New StretchBlt() Modes
STRETCH_ANDSCANS    EQUATE(BLACKONWHITE)
STRETCH_ORSCANS     EQUATE(WHITEONBLACK)
STRETCH_DELETESCANS EQUATE(COLORONCOLOR)
STRETCH_HALFTONE    EQUATE(HALFTONE)

! Stock Logical Objects
BLACK_PEN           EQUATE(7)

DC_BRUSH            EQUATE(18)
DC_PEN              EQUATE(19)
STOCK_LAST          EQUATE(19)

! Spooler Error Codes
SP_NOTREPORTED      EQUATE(04000h)
SP_ERROR            EQUATE(-1)
SP_APPABORT         EQUATE(-2)
SP_USERABORT        EQUATE(-3)
SP_OUTOFDISK        EQUATE(-4)
SP_OUTOFMEMORY      EQUATE(-5)

! GDI Escapes
NEWFRAME                    EQUATE(1)
ABORTDOC                    EQUATE(2)
NEXTBAND                    EQUATE(3)
SETCOLORTABLE               EQUATE(4)
GETCOLORTABLE               EQUATE(5)
FLUSHOUTPUT                 EQUATE(6)
DRAFTMODE                   EQUATE(7)
QUERYESCSUPPORT             EQUATE(8)
SETABORTPROC                EQUATE(9)
STARTDOC                    EQUATE(10)
ENDDOC                      EQUATE(11)
GETPHYSPAGESIZE             EQUATE(12)
GETPRINTINGOFFSET           EQUATE(13)
GETSCALINGFACTOR            EQUATE(14)
MFCOMMENT                   EQUATE(15)
GETPENWIDTH                 EQUATE(16)
SETCOPYCOUNT                EQUATE(17)
SELECTPAPERSOURCE           EQUATE(18)
DEVICEDATA                  EQUATE(19)
PASSTHROUGH                 EQUATE(19)
GETTECHNOLGY                EQUATE(20)
GETTECHNOLOGY               EQUATE(20)
SETLINECAP                  EQUATE(21)
SETLINEJOIN                 EQUATE(22)
SETMITERLIMIT               EQUATE(23)
BANDINFO                    EQUATE(24)
DRAWPATTERNRECT             EQUATE(25)
GETVECTORPENSIZE            EQUATE(26)
GETVECTORBRUSHSIZE          EQUATE(27)
ENABLEDUPLEX                EQUATE(28)
GETSETPAPERBINS             EQUATE(29)
GETSETPRINTORIENT           EQUATE(30)
ENUMPAPERBINS               EQUATE(31)
SETDIBSCALING               EQUATE(32)
EPSPRINTING                 EQUATE(33)
ENUMPAPERMETRICS            EQUATE(34)
GETSETPAPERMETRICS          EQUATE(35)
POSTSCRIPT_DATA             EQUATE(37)
POSTSCRIPT_IGNORE           EQUATE(38)
MOUSETRAILS                 EQUATE(39)
GETDEVICEUNITS              EQUATE(42)

GETEXTENDEDTEXTMETRICS      EQUATE(256)
GETEXTENTTABLE              EQUATE(257)
GETPAIRKERNTABLE            EQUATE(258)
GETTRACKKERNTABLE           EQUATE(259)
EXTTEXTOUT                  EQUATE(512)
GETFACENAME                 EQUATE(513)
DOWNLOADFACE                EQUATE(514)
ENABLERELATIVEWIDTHS        EQUATE(768)
ENABLEPAIRKERNING           EQUATE(769)
SETKERNTRACK                EQUATE(770)
SETALLJUSTVALUES            EQUATE(771)
SETCHARSET                  EQUATE(772)

STRETCHBLT                  EQUATE(2048)
GETSETSCREENPARAMS          EQUATE(3072)
QUERYDIBSUPPORT             EQUATE(3073)
BEGIN_PATH                  EQUATE(4096)
CLIP_TO_PATH                EQUATE(4097)
END_PATH                    EQUATE(4098)
EXT_DEVICE_CAPS             EQUATE(4099)
RESTORE_CTM                 EQUATE(4100)
SAVE_CTM                    EQUATE(4101)
SET_ARC_DIRECTION           EQUATE(4102)
SET_BACKGROUND_COLOR        EQUATE(4103)
SET_POLY_MODE               EQUATE(4104)
SET_SCREEN_ANGLE            EQUATE(4105)
SET_SPREAD                  EQUATE(4106)
TRANSFORM_CTM               EQUATE(4107)
SET_CLIP_BOX                EQUATE(4108)
SET_BOUNDS                  EQUATE(4109)
SET_MIRROR_MODE             EQUATE(4110)
OPENCHANNEL                 EQUATE(4110)
DOWNLOADHEADER              EQUATE(4111)
CLOSECHANNEL                EQUATE(4112)
POSTSCRIPT_PASSTHROUGH      EQUATE(4115)
ENCAPSULATED_POSTSCRIPT     EQUATE(4116)

POSTSCRIPT_IDENTIFY         EQUATE(4117)   ! new escape for NT5 pscript driver
POSTSCRIPT_INJECTION        EQUATE(4118)   ! new escape for NT5 pscript driver

! Parameters for POSTSCRIPT_IDENTIFY escape
PSIDENT_GDICENTRIC          EQUATE(0)
PSIDENT_PSCENTRIC           EQUATE(1)

! Header structure for the input buffer to POSTSCRIPT_INJECTION escape
_PSINJECTDATA   GROUP,TYPE()
DataBytes         DWORD         ! number of raw data bytes
InjectionPoint    DWORD         ! injection point
Flags             DWORD         ! flags
Reserved          DWORD         ! reserved field - must be 0
                END
! Followed by raw data to be injected 

! Constants for PSINJECTDATA.Flags field
PSINJECT_APPEND             EQUATE(0)
PSINJECT_REPLACE            EQUATE(1)

! Constants for PSINJECTDATA.InjectionPoint field
! The data injected at these points coexist with the output emitted
! by the driver for the same points.
PSINJECT_BEGINSTREAM        EQUATE(0)
PSINJECT_PSADOBE            EQUATE(1)
PSINJECT_COMMENTS           EQUATE(2)
PSINJECT_BEGINDEFAULTS      EQUATE(3)
PSINJECT_ENDDEFAULTS        EQUATE(4)
PSINJECT_BEGINPROLOG        EQUATE(5)
PSINJECT_ENDPROLOG          EQUATE(6)
PSINJECT_BEGINSETUP         EQUATE(7)
PSINJECT_ENDSETUP           EQUATE(8)
PSINJECT_ENDPAGECOMMENTS    EQUATE(9)
PSINJECT_BEGINPAGESETUP     EQUATE(10)
PSINJECT_ENDPAGESETUP       EQUATE(11)
PSINJECT_SHOWPAGE           EQUATE(12)
PSINJECT_PAGETRAILER        EQUATE(13)
PSINJECT_TRAILER            EQUATE(14)
PSINJECT_EOF                EQUATE(15)
PSINJECT_ENDSTREAM          EQUATE(16)
PSINJECT_VMSAVE             EQUATE(17)
PSINJECT_VMRESTORE          EQUATE(18)

! The data injected at these points are appended to the output
! emitted by the driver for the same points. It will go into
! the document trailer section. They must be in the form of:
!     %%+ resource-type resource-names
PSINJECT_DOCNEEDEDRES       EQUATE(19)
PSINJECT_DOCSUPPLIEDRES     EQUATE(20)

! The data injected at these points replaces the output emitted
! by the driver for the same points.
PSINJECT_PAGES              EQUATE(21)
PSINJECT_PAGEORDER          EQUATE(22)
PSINJECT_ORIENTATION        EQUATE(23)
PSINJECT_BOUNDINGBOX        EQUATE(24)
PSINJECT_PAGENUMBER         EQUATE(25)
PSINJECT_PAGEBBOX           EQUATE(26)
PSINJECT_MAX                EQUATE(27)

DEFAULT_PITCH               EQUATE(0)
FIXED_PITCH                 EQUATE(1)
VARIABLE_PITCH              EQUATE(2)
MONO_FONT                   EQUATE(8)

HANGUL_CHARSET              EQUATE(129)

! Static Control Mesages
STM_SETICON                 EQUATE(00170h)
STM_GETICON                 EQUATE(00171h)
STM_SETIMAGE                EQUATE(00172h)
STM_GETIMAGE                EQUATE(00173h)
STM_MSGMAX                  EQUATE(00174h)
STN_CLICKED                 EQUATE(0)
STN_DBLCLK                  EQUATE(1)
STN_ENABLE                  EQUATE(2)
STN_DISABLE                 EQUATE(3)

! ChooseColor Flag Equates
CC_RGBINIT              EQUATE(000000001h)
CC_FULLOPEN             EQUATE(000000002h)
CC_PREVENTFULLOPEN      EQUATE(000000004h)
CC_SHOWHELP             EQUATE(000000008h)
CC_ENABLEHOOK           EQUATE(000000010h)
CC_ENABLETEMPLATE       EQUATE(000000020h)
CC_ENABLETEMPLATEHANDLE EQUATE(000000040h)
CC_SOLIDCOLOR           EQUATE(000000080h)
CC_ANYCOLOR             EQUATE(000000100h)

!==============================================================================
 OMIT('_EndOfInclude_',_VariantClPresent_)
_VariantClPresent_ EQUATE(1)
! Variant Types
VT_EMPTY                    EQUATE(0)
VT_NULL                     EQUATE(1)
VT_I2                       EQUATE(2)
VT_I4                       EQUATE(3)
VT_R4                       EQUATE(4)
VT_R8                       EQUATE(5)
VT_CY                       EQUATE(6)
VT_DATE                     EQUATE(7)
VT_BSTR                     EQUATE(8)
VT_DISPATCH                 EQUATE(9)
VT_ERROR                    EQUATE(10)
VT_BOOL                     EQUATE(11)
VT_VARIANT                  EQUATE(12)
VT_UNKNOWN                  EQUATE(13)
VT_DECIMAL                  EQUATE(14)
VT_I1                       EQUATE(16)
VT_UI1                      EQUATE(17)
VT_UI2                      EQUATE(18)
VT_UI4                      EQUATE(19)
VT_I8                       EQUATE(20)
VT_UI8                      EQUATE(21)
VT_INT                      EQUATE(22)
VT_UINT                     EQUATE(23)
VT_VOID                     EQUATE(24)
VT_HRESULT                  EQUATE(25)
VT_PTR                      EQUATE(26)
VT_SAFEARRAY                EQUATE(27)
VT_CARRAY                   EQUATE(28)
VT_USERDEFINED              EQUATE(29)
VT_LPSTR                    EQUATE(30)
VT_LPWSTR                   EQUATE(31)
VT_FILETIME                 EQUATE(64)
VT_BLOB                     EQUATE(65)
VT_STREAM                   EQUATE(66)
VT_STORAGE                  EQUATE(67)
VT_STREAMED_OBJECT          EQUATE(68)
VT_STORED_OBJECT            EQUATE(69)
VT_BLOB_OBJECT              EQUATE(70)
VT_CF                       EQUATE(71)
VT_CLSID                    EQUATE(72)
VT_VECTOR                   EQUATE(01000h)
VT_ARRAY                    EQUATE(02000h)
VT_BYREF                    EQUATE(04000h)
VT_RESERVED                 EQUATE(08000h)
VT_ILLEGAL                  EQUATE(0FFFFh)
VT_ILLEGALMASKED            EQUATE(0FFFh)
VT_TYPEMASK                 EQUATE(0FFFh)
!_EndOfInclude_
!=====================================================================================
VARTYPE EQUATE(USHORT)
VARIANT     GROUP,TYPE
vt            USHORT
wReserved1    USHORT
wReserved2    USHORT
wReserved3    USHORT
union         REAL
lVal            LONG,OVER(union)        ! VT_I4                 ByVal Long
bVal            BYTE,OVER(union)        ! VT_UI1                ByVal Byte
iVal            SIGNED,OVER(union)      ! VT_I2                 ByVal Integer
fltVal          SREAL,OVER(union)       ! VT_R4                 ByVal Single
dblVal          REAL,OVER(union)        ! VT_R8                 ByVal Double
boolVal         BOOL,OVER(union)        ! VT_BOOL               ByVal Boolean
!scode           SCODE,OVER(union)       ! VT_ERROR
cyVal           REAL,OVER(union)        ! VT_CY                 ByVal Currency
date            REAL,OVER(union)        ! VT_DATE               ByVal Date
bstrVal         LONG,OVER(union)        ! VT_BSTR               ByVal String
punkVal         LONG,OVER(union)        ! VT_UNKNOWN
pdispVal        LONG,OVER(union)        ! VT_DISPATCH           ByVal Object
parray          LONG,OVER(union)        ! VT_ARRAY|*            ByVal array
! A bunch of other types that don't matter here...
pvarVal         LONG,OVER(union)        ! VT_BYREF|VT_VARIANT   ByRef Variant
byref           LONG,OVER(union)        ! Generic               ByRef
            END

 OMIT('_EndOfInclude_', _DateTimePickClassPresent_)
H_MAX                   EQUATE(0FFFFFFFFH + 1)
DTN_FIRST               EQUATE(H_MAX - 760)
DTN_LAST                EQUATE(H_MAX - 799)
DTN_DATETIMECHANGE      EQUATE(DTN_FIRST + 1)
!_EndOfInclude_

!  Default System and User IDs for language and locale.
LANG_SYSTEM_DEFAULT    EQUATE(2)
LANG_USER_DEFAULT      EQUATE(1)

LOCALE_SYSTEM_DEFAULT  EQUATE(LANG_SYSTEM_DEFAULT)
LOCALE_USER_DEFAULT    EQUATE(LANG_USER_DEFAULT)

!  Locale Types.
!  These types are used for the GetLocaleInfoA NLS API routine.

LOCALE_NOUSEROVERRIDE   EQUATE(080000000h)  ! OR in to avoid user override

LOCALE_ILANGUAGE            EQUATE(00001h)  ! language id 
LOCALE_SLANGUAGE            EQUATE(00002h)  ! localized name of language 
LOCALE_SENGLANGUAGE         EQUATE(01001h)  ! English name of language 
LOCALE_SABBREVLANGNAME      EQUATE(00003h)  ! abbreviated language name 
LOCALE_SNATIVELANGNAME      EQUATE(00004h)  ! native name of language 
LOCALE_ICOUNTRY             EQUATE(00005h)  ! country code 
LOCALE_SCOUNTRY             EQUATE(00006h)  ! localized name of country   
LOCALE_SENGCOUNTRY          EQUATE(01002h)  ! English name of country   
LOCALE_SABBREVCTRYNAME      EQUATE(00007h)  ! abbreviated country name 
LOCALE_SNATIVECTRYNAME      EQUATE(00008h)  ! native name of country   
LOCALE_IDEFAULTLANGUAGE     EQUATE(00009h)  ! default language id 
LOCALE_IDEFAULTCOUNTRY      EQUATE(0000Ah)  ! default country code 
LOCALE_IDEFAULTCODEPAGE     EQUATE(0000Bh)  ! default oem code page 
LOCALE_IDEFAULTANSICODEPAGE EQUATE(01004h)  ! default ansi code page 
                                            
LOCALE_SLIST                EQUATE(0000Ch)  ! list item separator 
LOCALE_IMEASURE             EQUATE(0000Dh)  ! 0 = metric, 1 = US 
                                            
LOCALE_SDECIMAL             EQUATE(0000Eh)  ! decimal separator 
LOCALE_STHOUSAND            EQUATE(0000Fh)  ! thousand separator 
LOCALE_SGROUPING            EQUATE(00010h)  ! digit grouping 
LOCALE_IDIGITS              EQUATE(00011h)  ! number of fractional digits 
LOCALE_ILZERO               EQUATE(00012h)  ! leading zeros for decimal 
LOCALE_INEGNUMBER           EQUATE(01010h)  ! negative number mode 
LOCALE_SNATIVEDIGITS        EQUATE(00013h)  ! native ascii 0-9 
                                            
LOCALE_SCURRENCY            EQUATE(00014h)  ! local monetary symbol 
LOCALE_SINTLSYMBOL          EQUATE(00015h)  ! intl monetary symbol 
LOCALE_SMONDECIMALSEP       EQUATE(00016h)  ! monetary decimal separator 
LOCALE_SMONTHOUSANDSEP      EQUATE(00017h)  ! monetary thousand separator 
LOCALE_SMONGROUPING         EQUATE(00018h)  ! monetary grouping 
LOCALE_ICURRDIGITS          EQUATE(00019h)  ! # local monetary digits 
LOCALE_IINTLCURRDIGITS      EQUATE(0001Ah)  ! # intl monetary digits 
LOCALE_ICURRENCY            EQUATE(0001Bh)  ! positive currency mode 
LOCALE_INEGCURR             EQUATE(0001Ch)  ! negative currency mode 
                                            
LOCALE_SDATE                EQUATE(0001Dh)  ! date separator 
LOCALE_STIME                EQUATE(0001Eh)  ! time separator 
LOCALE_SSHORTDATE           EQUATE(0001Fh)  ! short date-time separator 
LOCALE_SLONGDATE            EQUATE(00020h)  ! long date-time separator 
LOCALE_STIMEFORMAT          EQUATE(01003h)  ! time format string 
LOCALE_IDATE                EQUATE(00021h)  ! short date format ordering 
LOCALE_ILDATE               EQUATE(00022h)  ! long date format ordering 
LOCALE_ITIME                EQUATE(00023h)  ! time format specifier 
LOCALE_ITIMEMARKPOSN        EQUATE(01005h)  ! time marker position 
LOCALE_ICENTURY             EQUATE(00024h)  ! century format specifier 
LOCALE_ITLZERO              EQUATE(00025h)  ! leading zeros in time field 
LOCALE_IDAYLZERO            EQUATE(00026h)  ! leading zeros in day field 
LOCALE_IMONLZERO            EQUATE(00027h)  ! leading zeros in month field 
LOCALE_S1159                EQUATE(00028h)  ! AM designator 
LOCALE_S2359                EQUATE(00029h)  ! PM designator 

LOCALE_ICALENDARTYPE        EQUATE(01009h)  ! type of calendar specifier 
LOCALE_IOPTIONALCALENDAR    EQUATE(0100Bh)  ! additional calendar types specifier 

LOCALE_IFIRSTDAYOFWEEK      EQUATE(0100Ch)  ! first day of week specifier 
LOCALE_IFIRSTWEEKOFYEAR     EQUATE(0100Dh)  ! first week of year specifier 

                                            
LOCALE_SDAYNAME1            EQUATE(0002Ah)  ! long name for Monday 
LOCALE_SDAYNAME2            EQUATE(0002Bh)  ! long name for Tuesday 
LOCALE_SDAYNAME3            EQUATE(0002Ch)  ! long name for Wednesday 
LOCALE_SDAYNAME4            EQUATE(0002Dh)  ! long name for Thursday 
LOCALE_SDAYNAME5            EQUATE(0002Eh)  ! long name for Friday 
LOCALE_SDAYNAME6            EQUATE(0002Fh)  ! long name for Saturday 
LOCALE_SDAYNAME7            EQUATE(00030h)  ! long name for Sunday 
LOCALE_SABBREVDAYNAME1      EQUATE(00031h)  ! abbreviated name for Monday    
LOCALE_SABBREVDAYNAME2      EQUATE(00032h)  ! abbreviated name for Tuesday   
LOCALE_SABBREVDAYNAME3      EQUATE(00033h)  ! abbreviated name for Wednesday 
LOCALE_SABBREVDAYNAME4      EQUATE(00034h)  ! abbreviated name for Thursday  
LOCALE_SABBREVDAYNAME5      EQUATE(00035h)  ! abbreviated name for Friday    
LOCALE_SABBREVDAYNAME6      EQUATE(00036h)  ! abbreviated name for Saturday  
LOCALE_SABBREVDAYNAME7      EQUATE(00037h)  ! abbreviated name for Sunday    
LOCALE_SMONTHNAME1          EQUATE(00038h)  ! long name for January 
LOCALE_SMONTHNAME2          EQUATE(00039h)  ! long name for February 
LOCALE_SMONTHNAME3          EQUATE(0003Ah)  ! long name for March 
LOCALE_SMONTHNAME4          EQUATE(0003Bh)  ! long name for April 
LOCALE_SMONTHNAME5          EQUATE(0003Ch)  ! long name for May 
LOCALE_SMONTHNAME6          EQUATE(0003Dh)  ! long name for June 
LOCALE_SMONTHNAME7          EQUATE(0003Eh)  ! long name for July 
LOCALE_SMONTHNAME8          EQUATE(0003Fh)  ! long name for August 
LOCALE_SMONTHNAME9          EQUATE(00040h)  ! long name for September 
LOCALE_SMONTHNAME10         EQUATE(00041h)  ! long name for October 
LOCALE_SMONTHNAME11         EQUATE(00042h)  ! long name for November 
LOCALE_SMONTHNAME12         EQUATE(00043h)  ! long name for December 
LOCALE_SMONTHNAME13         EQUATE(0100Eh)  ! long name for 13th month (if exists) 
LOCALE_SABBREVMONTHNAME1    EQUATE(00044h)  ! abbreviated name for January 
LOCALE_SABBREVMONTHNAME2    EQUATE(00045h)  ! abbreviated name for February 
LOCALE_SABBREVMONTHNAME3    EQUATE(00046h)  ! abbreviated name for March 
LOCALE_SABBREVMONTHNAME4    EQUATE(00047h)  ! abbreviated name for April 
LOCALE_SABBREVMONTHNAME5    EQUATE(00048h)  ! abbreviated name for May 
LOCALE_SABBREVMONTHNAME6    EQUATE(00049h)  ! abbreviated name for June 
LOCALE_SABBREVMONTHNAME7    EQUATE(0004Ah)  ! abbreviated name for July 
LOCALE_SABBREVMONTHNAME8    EQUATE(0004Bh)  ! abbreviated name for August 
LOCALE_SABBREVMONTHNAME9    EQUATE(0004Ch)  ! abbreviated name for September 
LOCALE_SABBREVMONTHNAME10   EQUATE(0004Dh)  ! abbreviated name for October 
LOCALE_SABBREVMONTHNAME11   EQUATE(0004Eh)  ! abbreviated name for November 
LOCALE_SABBREVMONTHNAME12   EQUATE(0004Fh)  ! abbreviated name for December 
LOCALE_SABBREVMONTHNAME13   EQUATE(0100Fh)  ! abbreviated name for 13th month (if exists) 
                                            
LOCALE_SPOSITIVESIGN        EQUATE(00050h)  ! positive sign 
LOCALE_SNEGATIVESIGN        EQUATE(00051h)  ! negative sign 
LOCALE_IPOSSIGNPOSN         EQUATE(00052h)  ! positive sign position 
LOCALE_INEGSIGNPOSN         EQUATE(00053h)  ! negative sign position 
LOCALE_IPOSSYMPRECEDES      EQUATE(00054h)  ! mon sym precedes pos amt 
LOCALE_IPOSSEPBYSPACE       EQUATE(00055h)  ! mon sym sep by space from pos  
LOCALE_INEGSYMPRECEDES      EQUATE(00056h)  ! mon sym precedes neg amt 
LOCALE_INEGSEPBYSPACE       EQUATE(00057h)  ! mon sym sep by space from neg

!Registry Security Definitions

SECURITY_NULL_SID_AUTHORITY     GROUP
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                END
SECURITY_WORLD_SID_AUTHORITY    GROUP
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(1)
                                END
SECURITY_LOCAL_SID_AUTHORITY    GROUP
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(2)
                                END
SECURITY_CREATOR_SID_AUTHORITY  GROUP
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(3)
                                END
SECURITY_NON_UNIQUE_AUTHORITY   GROUP
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(4)
                                END
SECURITY_NT_AUTHORITY           GROUP
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(0)
                                  BYTE(5)
                                END

SECURITY_NULL_RID                   EQUATE(000000000h)
SECURITY_WORLD_RID                  EQUATE(000000000h)
SECURITY_LOCAL_RID                  EQUATE(000000000h)

SECURITY_CREATOR_OWNER_RID          EQUATE(000000000h)
SECURITY_CREATOR_GROUP_RID          EQUATE(000000001h)

SECURITY_CREATOR_OWNER_SERVER_RID   EQUATE(000000002h)
SECURITY_CREATOR_GROUP_SERVER_RID   EQUATE(000000003h)

SECURITY_DIALUP_RID                 EQUATE(000000001h)
SECURITY_NETWORK_RID                EQUATE(000000002h)
SECURITY_BATCH_RID                  EQUATE(000000003h)
SECURITY_INTERACTIVE_RID            EQUATE(000000004h)
SECURITY_SERVICE_RID                EQUATE(000000006h)
SECURITY_ANONYMOUS_LOGON_RID        EQUATE(000000007h)
SECURITY_PROXY_RID                  EQUATE(000000008h)
SECURITY_ENTERPRISE_CONTROLLERS_RID EQUATE(000000009h)
SECURITY_SERVER_LOGON_RID           EQUATE(SECURITY_ENTERPRISE_CONTROLLERS_RID)
SECURITY_PRINCIPAL_SELF_RID         EQUATE(00000000Ah)
SECURITY_AUTHENTICATED_USER_RID     EQUATE(00000000Bh)
SECURITY_RESTRICTED_CODE_RID        EQUATE(00000000Ch)

SECURITY_LOGON_IDS_RID              EQUATE(000000005h)
SECURITY_LOGON_IDS_RID_COUNT        EQUATE(000000003h)

SECURITY_LOCAL_SYSTEM_RID           EQUATE(000000012h)

SECURITY_NT_NON_UNIQUE              EQUATE(000000015h)

SECURITY_BUILTIN_DOMAIN_RID         EQUATE(000000020h)

! Well-known users ...
DOMAIN_USER_RID_ADMIN               EQUATE(0000001F4h)
DOMAIN_USER_RID_GUEST               EQUATE(0000001F5h)
DOMAIN_USER_RID_KRBTGT              EQUATE(0000001F6h)

!well-known groups ...
DOMAIN_GROUP_RID_ADMINS             EQUATE(000000200h)
DOMAIN_GROUP_RID_USERS              EQUATE(000000201h)
DOMAIN_GROUP_RID_GUESTS             EQUATE(000000202h)
DOMAIN_GROUP_RID_COMPUTERS          EQUATE(000000203h)
DOMAIN_GROUP_RID_CONTROLLERS        EQUATE(000000204h)
DOMAIN_GROUP_RID_CERT_ADMINS        EQUATE(000000205h)
DOMAIN_GROUP_RID_SCHEMA_ADMINS      EQUATE(000000206h)

! well-known aliases ...
DOMAIN_ALIAS_RID_ADMINS             EQUATE(000000220h)
DOMAIN_ALIAS_RID_USERS              EQUATE(000000221h)
DOMAIN_ALIAS_RID_GUESTS             EQUATE(000000222h)
DOMAIN_ALIAS_RID_POWER_USERS        EQUATE(000000223h)

DOMAIN_ALIAS_RID_ACCOUNT_OPS        EQUATE(000000224h)
DOMAIN_ALIAS_RID_SYSTEM_OPS         EQUATE(000000225h)
DOMAIN_ALIAS_RID_PRINT_OPS          EQUATE(000000226h)
DOMAIN_ALIAS_RID_BACKUP_OPS         EQUATE(000000227h)

DOMAIN_ALIAS_RID_REPLICATOR         EQUATE(000000228h)

tagSECURITY_ATTRIBUTES  GROUP,TYPE
nLength                   DWORD
lpSecurityDescriptor      LPVOID
bInheritHandle            BOOL
                        END

tagSID_IDENTIFIER_AUTHORITY GROUP,TYPE
Value                         BYTE,DIM(6)
                            END

tagSID              GROUP,TYPE
Revision              BYTE
SubAuthorityCount     BYTE
IdentifierAuthority   LIKE(SID_IDENTIFIER_AUTHORITY)
SubAuthority          ULONG,DIM(1)
                    END

! Definition: MULTIPLE_TRUSTEE_OPERATION
! If the trustee is a multiple trustee, this enumerated type specifies the type.
! TRUSTEE_IS_IMPERSONATE       The trustee is an impersonate trustee and the multiple
!                          trustee field in the trustee points to another trustee
!                          that is a trustee for the server that will be doing the
!                          impersonation.
!
MULTIPLE_TRUSTEE_OPERATION  ITEMIZE(0)
NO_MULTIPLE_TRUSTEE           EQUATE
TRUSTEE_IS_IMPERSONATE        EQUATE
                            END

! Definition: TRUSTEE_FORM
! This enumerated type specifies the form the trustee identifier is in for a
! particular trustee.
! TRUSTEE_IS_SID       The trustee is identified with a SID rather than with a name.
! TRUSTEE_IS_NAME      The trustee is identified with a name.
!
TRUSTEE_FORM    ITEMIZE(0)
TRUSTEE_IS_SID    EQUATE
TRUSTEE_IS_NAME   EQUATE
TRUSTEE_BAD_FORM  EQUATE
                END

! Definition: TRUSTEE_TYPE
! This enumerated type specifies the type of trustee account for the trustee
! returned by the API described in this document.
! TRUSTEE_IS_UNKNOWN - The trustee is an unknown, but not necessarily invalid
!                      type.  This field is not validated on input to the APIs
!                      that take Trustees.
! TRUSTEE_IS_USER      The trustee account is a user account.
! TRUSTEE_IS_GROUP     The trustee account is a group account.
!
TRUSTEE_TYPE                ITEMIZE(0)
TRUSTEE_IS_UNKNOWN            EQUATE
TRUSTEE_IS_USER               EQUATE
TRUSTEE_IS_GROUP              EQUATE
TRUSTEE_IS_DOMAIN             EQUATE
TRUSTEE_IS_ALIAS              EQUATE
TRUSTEE_IS_WELL_KNOWN_GROUP   EQUATE
TRUSTEE_IS_DELETED            EQUATE
TRUSTEE_IS_INVALID            EQUATE
                            END

tagTRUSTEE              GROUP,TYPE
pMultipleTrustee          ULONG
MultipleTrusteeOperation  LONG
TrusteeForm               LONG
TrusteeType               LONG
ptstrName                 ULONG
pSid                      ULONG,OVER(ptstrName)
                        END

NO_INHERITANCE      EQUATE(00h)

tagEXPLICIT_ACCESS  GROUP,TYPE
grfAccessPermissions  DWORD
grfAccessMode         LONG
grfInheritance        DWORD
Trustee               LIKE(tagTRUSTEE)
                    END

ACCESS_MODE         ITEMIZE(0)
NOT_USED_ACCESS       EQUATE
GRANT_ACCESS          EQUATE
SET_ACCESS            EQUATE
DENY_ACCESS           EQUATE
REVOKE_ACCESS         EQUATE
SET_AUDIT_SUCCESS     EQUATE
SET_AUDIT_FAILURE     EQUATE
                    END

tagACL      GROUP,TYPE
AclRevision   BYTE
Sbz1          BYTE
AclSize       WORD
AceCount      WORD
Sbz2          WORD
            END

! Current security descriptor revision value
!
SECURITY_DESCRIPTOR_REVISION    EQUATE(1)
SECURITY_DESCRIPTOR_REVISION1   EQUATE(1)

SECURITY_DESCRIPTOR_CONTROL EQUATE(WORD)
SE_OWNER_DEFAULTED          EQUATE(00001h)
SE_GROUP_DEFAULTED          EQUATE(00002h)
SE_DACL_PRESENT             EQUATE(00004h)
SE_DACL_DEFAULTED           EQUATE(00008h)
SE_SACL_PRESENT             EQUATE(00010h)
SE_SACL_DEFAULTED           EQUATE(00020h)
SE_DACL_AUTO_INHERIT_REQ    EQUATE(00100h)
SE_SACL_AUTO_INHERIT_REQ    EQUATE(00200h)
SE_DACL_AUTO_INHERITED      EQUATE(00400h)
SE_SACL_AUTO_INHERITED      EQUATE(00800h)
SE_DACL_PROTECTED           EQUATE(01000h)
SE_SACL_PROTECTED           EQUATE(02000h)
SE_SELF_RELATIVE            EQUATE(08000h)

tagSECURITY_DESCRIPTOR  GROUP,TYPE
Revision                  BYTE
Sbz1                      BYTE
Control                   SECURITY_DESCRIPTOR_CONTROL
Owner                     ULONG
Group                     ULONG
Sacl                      ULONG
Dacl                      ULONG
                        END

!  TYPEd Queue for GetHoliday Function in Cyber532
Holiday:QueueType QUEUE,TYPE
sName               STRING(60)
lDate               LONG
                   END
!==========================================================
! System Menu Command Values
! 2003.09.01 RR
!==========================================================
SC_SIZE         EQUATE(0F000h)
SC_MOVE         EQUATE(0F010h)
!SC_MINIMIZE     EQUATE(0F020h)  !RR 2013.07.17 removed for clarion9
SC_MAXIMIZE     EQUATE(0F030h)
SC_NEXTWINDOW   EQUATE(0F040h)
SC_PREVWINDOW   EQUATE(0F050h)
SC_CLOSE        EQUATE(0F060h)
SC_VSCROLL      EQUATE(0F070h)
SC_HSCROLL      EQUATE(0F080h)
SC_MOUSEMENU    EQUATE(0F090h)
SC_KEYMENU      EQUATE(0F100h)
SC_ARRANGE      EQUATE(0F110h)
!SC_RESTORE      EQUATE(0F120h)  !RR 2013.07.17 removed for clarion9
SC_TASKLIST     EQUATE(0F130h)
SC_SCREENSAVE   EQUATE(0F140h)
SC_HOTKEY       EQUATE(0F150h)
SC_DEFAULT      EQUATE(0F160h)
SC_MONITORPOWER EQUATE(0F170h)
SC_CONTEXTHELP  EQUATE(0F180h)
SC_SEPARATOR    EQUATE(0F00Fh)

 OMIT('_EndOfInclude_',_AppBarEquates_)
_AppBarEquates_ EQUATE(1)
!==========================================================
! AppBar Stuff
! 2004.04.02 RR
!==========================================================
ABM_NEW                 EQUATE(000000000h)
ABM_REMOVE              EQUATE(000000001h)
ABM_QUERYPOS            EQUATE(000000002h)
ABM_SETPOS              EQUATE(000000003h)
ABM_GETSTATE            EQUATE(000000004h)
ABM_GETTASKBARPOS       EQUATE(000000005h)
ABM_ACTIVATE            EQUATE(000000006h)  ! lParam == TRUE/FALSE means activate/deactivate
ABM_GETAUTOHIDEBAR      EQUATE(000000007h)
ABM_SETAUTOHIDEBAR      EQUATE(000000008h)  ! this can fail at any time.  MUST check the result
                                            ! lParam = TRUE/FALSE  Set/Unset
                                            ! uEdge = what edge
ABM_WINDOWPOSCHANGED    EQUATE(00000009h)
ABM_SETSTATE            EQUATE(00000000Ah)

! these are put in the wparam of callback messages
ABN_STATECHANGE         EQUATE(00000000h)
ABN_POSCHANGED          EQUATE(00000001h)
ABN_FULLSCREENAPP       EQUATE(00000002h)
ABN_WINDOWARRANGE       EQUATE(00000003h)   ! lParam == TRUE means hide

! flags for get state
ABS_AUTOHIDE            EQUATE(00000001h)
ABS_ALWAYSONTOP         EQUATE(00000002h)

ABE_UNKNOWN             EQUATE(-1)
ABE_LEFT                EQUATE(0)
ABE_TOP                 EQUATE(1)
ABE_RIGHT               EQUATE(2)
ABE_BOTTOM              EQUATE(3)

APPBARDATA      GROUP,TYPE
cbSize              LONG
hwnd                LONG
uCallbackMessage    LONG
uEdge               LONG
rc                  LIKE(RECT)
lParam              LONG                    !  message specific
                END
 !_EndOfInclude_
 
FLASHWINFO     GROUP,TYPE
cbSize            UNSIGNED
hwnd              HWND
dwFlags           DWORD
uCount            UNSIGNED
dwTimeout         DWORD 
               END
               
FLASHW_ALL        EQUATE(000000003h)   !  Flash both the window caption and taskbar button. This is equivalent to setting the FLASHW_CAPTION | FLASHW_TRAY flags.
FLASHW_CAPTION    EQUATE(000000001h)   !  Flash the window caption.
FLASHW_STOP       EQUATE(000000000h)   !  Stop flashing. The system restores the window to its original state.
FLASHW_TIMER      EQUATE(000000004h)   !  Flash continuously, until the FLASHW_STOP flag is set.
FLASHW_TIMERNOFG  EQUATE(00000000Ch)   !  Flash continuously until the window comes to the foreground.
FLASHW_TRAY       EQUATE(000000002h)   !  Flash the taskbar button.

!Devuna ComParams group used by Waste Management application
KCRCOMPARAM GROUP,TYPE,PRE(KcrComParam)
Manufacturer   CSTRING(21)
Port           ULONG
BaudRate       ULONG                     ! current baudrate
DataBits       ULONG                     ! current data bits
StopBits       ULONG                     ! current stop bits
Parity         ULONG                     ! current parity
            END            

 SECTION('map')
  MODULE('PROCTYPES')
    ENUMRESTYPEPROC(HMODULE hModule, ULONG lpType, LONG lParam),BOOL,PASCAL,TYPE
    ENUMRESNAMEPROC(HMODULE hModule, ULONG lpType, ULONG lpName, LONG lParam),BOOL,PASCAL,TYPE
    ENUMRESLANGPROC(HMODULE hModule, ULONG lpType, ULONG lpName, WORD wLanguage, LONG lParam),BOOL,PASCAL,TYPE
    WNDENUMPROC(HWND,LPARAM),BOOL,PASCAL,TYPE
    WNDPROC(HWND,UNSIGNED,WPARAM,LPARAM),LRESULT,PASCAL,TYPE
  END
  ! Some Windows API prototypes
  ! These are calls into Windows and Windows documentation should be
  ! referred to when using these functions
  MODULE('WINDOWS')
    AllocateAndInitializeSid (*tagSID_IDENTIFIER_AUTHORITY pIdentifierAuthority,   |
                              BYTE nSubAuthorityCount,                          |
                              DWORD nSubAuthority0,                             |
                              DWORD nSubAuthority1,                             |
                              DWORD nSubAuthority2,                             |
                              DWORD nSubAuthority3,                             |
                              DWORD nSubAuthority4,                             |
                              DWORD nSubAuthority5,                             |
                              DWORD nSubAuthority6,                             |
                              DWORD nSubAuthority7,                             |
                              *ULONG pSid),BOOL,PASCAL,RAW,PROC
    AppendMenu(HMENU,UNSIGNED,UNSIGNED,*LPCSTR),BOOL,PASCAL,RAW,NAME('AppendMenuA'),PROC
    BeginPaint(HWND,*PAINTSTRUCT),HDC,PASCAL,RAW,PROC
    BitBlt(HDC, SIGNED, SIGNED, SIGNED, SIGNED, HDC, SIGNED, SIGNED, DWORD),BOOL,PASCAL,PROC
    BringWindowToTop(HWND),BOOL,PASCAL,PROC
    CallWindowProc(ULONG,HWND,UINT,WPARAM,LPARAM),lONG,PASCAL,NAME('CallWindowProcA')
    ChooseColor(*? cc),BOOL,PASCAL,RAW,PROC,NAME('ChooseColorA')
    ClientToScreen(HWND, *POINT),PASCAL,RAW
    ClipCursor(*RECT),BOOL,PASCAL,RAW,PROC
    ClipCursor(ULONG),BOOL,PASCAL,RAW,PROC
    CloseClipboard(),BOOL,PASCAL,PROC
    CloseHandle(HANDLE hObject),BOOL,PASCAL,PROC
    CommDlgExtendedError(),ULONG,PASCAL
    CopyEnhMetaFile(HENHMETAFILE, *LPCSTR),HENHMETAFILE,PASCAL,RAW,NAME('CopyEnhMetaFileA'),PROC
    CopyImage(HANDLE,UNSIGNED,SIGNED,SIGNED,UNSIGNED),HANDLE,PASCAL
    CopyMetaFile(HMETAFILE, *LPCSTR),HMETAFILE,PASCAL,RAW,NAME('CopyMetaFileA'),PROC
    CopyRect(*RECT, *RECT),PASCAL,RAW
    CreateBitmap( SIGNED, SIGNED, UNSIGNED, UNSIGNED, LPVOID),HBITMAP,PASCAL
    CreateCompatibleBitmap(HDC, SIGNED, SIGNED),HBITMAP,PASCAL
    CreateCompatibleDC(HDC),HDC,PASCAL
    _CreateDC(*LPCSTR,*LPCSTR,*LPCSTR,*DEVMODE),HDC,PASCAL,RAW,NAME('CreateDCA')
    CreateDC(*LPCSTR,*LPCSTR,*LPCSTR,ULONG),HDC,PASCAL,RAW,NAME('CreateDCA')
    CreateDC(*LPCSTR,*LPCSTR,ULONG,ULONG),HDC,PASCAL,RAW,NAME('CreateDCA')
    CreateDC(*LPCSTR,ULONG,ULONG,ULONG),HDC,PASCAL,RAW,NAME('CreateDCA')
    CreateDIBitmap(HDC, *BITMAPINFOHEADER, DWORD, LPVOID, *BITMAPINFO, WORD),HBITMAP,PASCAL,RAW
    CreateDIBitmap(HDC, ULONG, DWORD, LPVOID, ULONG, WORD),HBITMAP,PASCAL,RAW
    CreateEvent(LONG lpSecurityAttribs, BOOL bManualReset,BOOL bInitialState, *CSTRING lpszEventName),HANDLE,PASCAL,RAW,PROC,NAME('CreateEventA')
    CreateFile(*CSTRING,DWORD,DWORD,ULONG,DWORD,DWORD,HANDLE),HANDLE,PASCAL,RAW,NAME('CreateFileA')
    CreateFont(SIGNED,SIGNED,SIGNED,SIGNED,SIGNED,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,DWORD,*LPCSTR),HFONT,PASCAL,RAW,NAME('CreateFontA')
    CreateHatchBrush( SIGNED, COLORREF),HBRUSH,PASCAL
    CreateMutex(*tagSECURITY_ATTRIBUTES lpSecurityAttribs, BOOL bInitialOwner, *CSTRING lpszMutexName),HANDLE,PASCAL,RAW,PROC,NAME('CreateMutexA')
    CreateMutex(LONG lpSecurityAttribs, BOOL bInitialOwner, *CSTRING lpszMutexName),HANDLE,PASCAL,RAW,PROC,NAME('CreateMutexA')
    CreatePalette(*LOGPALETTE),HPALETTE,PASCAL,RAW
    CreatePalette(ULONG),HPALETTE,PASCAL,RAW
    CreatePen( SIGNED, SIGNED, COLORREF),HPEN,PASCAL
    CreateProcess(*CSTRING,*CSTRING,LPVOID,LPVOID,BOOL,DWORD,LPVOID,*CSTRING,LPVOID,LPVOID),BOOL,RAW,PASCAL,NAME('CreateProcessA'),PROC
    CreateProcess(ULONG,*CSTRING,LPVOID,LPVOID,BOOL,DWORD,LPVOID,LPVOID,*STARTUPINFO,*PROCESS_INFORMATION),BOOL,RAW,PASCAL,NAME('CreateProcessA'),PROC
    CreateSolidBrush(COLORREF),HBRUSH,PASCAL
    CreateWindow(*LPCSTR,*LPCSTR, DWORD, SIGNED, SIGNED, SIGNED, SIGNED, HWND, HMENU, HINSTANCE, LPVOID),HWND,PASCAL,RAW
    CreateWindowEx(DWORD,*LPCSTR,ULONG,DWORD,SIGNED,SIGNED,SIGNED,SIGNED,HWND,HMENU,HINSTANCE,ULONG),HWND,PASCAL,RAW,NAME('CreateWindowExA')
    DefWindowProc(HWND,UNSIGNED,WPARAM,LPARAM),LRESULT,PASCAL,NAME('DefWindowProcA')
    DeleteDC(HDC),BOOL,PASCAL,PROC
    DeleteEnhMetaFile(HENHMETAFILE),BOOL,PASCAL,PROC
    DeleteFile(*LPCSTR),BOOL,PASCAL,RAW,NAME('DeleteFileA'),PROC
    DeleteMenu(HMENU, WORD, WORD),BOOL,PASCAL,PROC
    DeleteMetaFile(HMETAFILE),BOOL,PASCAL,PROC
    DeleteObject(HGDIOBJ),BOOL,PASCAL,PROC
    DestroyWindow(HWND),BOOL,PASCAL,PROC
    DispatchMessage(*MSGTYPE),LONG,PASCAL,RAW,NAME('DispatchMessageA'),PROC
    DPtoLP(HDC,*POINT,SIGNED),BOOL,PASCAL,RAW,PROC
    DrawIcon(HDC, SIGNED, SIGNED, HICON),BOOL,PASCAL,PROC
    DrawMenuBar(HWND),BOOL,PASCAL,PROC
    DrawText(HDC,*LPCSTR,SIGNED,*RECT,UNSIGNED),SIGNED,PASCAL,RAW,NAME('DrawTextA'),PROC
    _Ellipse(HDC, SIGNED, SIGNED, SIGNED, SIGNED),BOOL,PASCAL,PROC,NAME('Ellipse')
    EmptyClipboard(),BOOL,PASCAL,PROC
    EnableWindow(HWND,BOOL),BOOL,PASCAL,PROC
    EndDoc(HDC),SIGNED,PASCAL,PROC
    EndPaint(HWND,*PAINTSTRUCT),BOOL,PASCAL,RAW,PROC
    EnumChildWindows(HWND, WNDENUMPROC, ULONG),SHORT,PASCAL,PROC
    EnumClipboardFormats(UNSIGNED),UNSIGNED,PASCAL
    EnumResourceNames(HINSTANCE,*LPCSTR,ENUMRESNAMEPROC,LONG),BOOL,PASCAL,RAW,NAME('EnumResourceNamesA'),PROC
    EnumResourceNames(HINSTANCE,ULONG,ENUMRESNAMEPROC,LONG),BOOL,PASCAL,RAW,NAME('EnumResourceNamesA'),PROC
    EnumResourceTypes(HINSTANCE,ENUMRESTYPEPROC,LONG),BOOL,PASCAL,RAW,NAME('EnumResourceTypesA'),PROC
    EnumThreadWindows(DWORD,WNDENUMPROC,LPARAM),BOOL,PASCAL,PROC
    EnumWindows(WNDENUMPROC, ULONG),SIGNED,PASCAL,RAW,PROC
    Escape(HDC, SIGNED, SIGNED, ULONG, LPVOID),SIGNED,PASCAL,RAW,PROC
    ExtTextOut(HDC, SIGNED, SIGNED, UNSIGNED,*RECT ,*LPCSTR, UNSIGNED, *SIGNED),BOOL,PASCAL,RAW,NAME('ExtTextOutA'),PROC
    ExtTextOut(HDC,SIGNED,SIGNED,UNSIGNED,ULONG,*LPCSTR,UNSIGNED,ULONG),BOOL,PASCAL,RAW,NAME('ExtTextOutA'),PROC
    FileTimeToDosDateTime(*FILETIME,*WORD,*WORD),BOOL,PASCAL,RAW,PROC
    FileTimeToLocalFileTime(*FILETIME,*FILETIME),BOOL,PASCAL,RAW,PROC
    FillRect(HDC, *RECT, HBRUSH),SIGNED,PASCAL,RAW,PROC
    FindFirst(*CSTRING,*STRING,SHORT),SHORT,RAW,NAME('_findfirst')
    FindFirstFile(*CSTRING,*GROUP),HANDLE,RAW,PASCAL,NAME('FindFirstFileA')
    FindNext(*STRING),SHORT,RAW,NAME('_findnext')
    FindNextFile(HANDLE,*GROUP),BOOL,RAW,PASCAL,NAME('FindNextFileA')
    FindWindow(*CSTRING,*CSTRING),USHORT,RAW,PASCAL
    FindWindowEx(HWND,HWND,ULONG,*CSTRING),HWND,RAW,PASCAL,NAME('FindWindowExA')
!    FreeLibrary(HINSTANCE),BOOL,PASCAL,PROC
    FreeSid(ULONG pSid),RAW,PASCAL
    GetActiveWindow(),HWND,PASCAL
    GetBitmapBits(HBITMAP, LONG, LPVOID),LONG,PASCAL,RAW
    GetBkColor(HDC),COLORREF,PASCAL
    GetClassLong(HWND,SIGNED),DWORD,PASCAL,NAME('GetClassLongA')
    GetClassName(HWND,*LPSTR,SIGNED),SIGNED,PASCAL,RAW,PROC,NAME('GetClassNameA')
    GetClientRect(UNSIGNED,*RECT),RAW,PASCAL,PROC
    GetClipboardData(UNSIGNED),HANDLE,PASCAL
    GetComputerName(*CSTRING,*ULONG),LONG,RAW,PASCAL,NAME('GetComputerNameA')
    GetCurrentProcess(),HANDLE,PASCAL
    GetCursorPos(*POINT),BOOL,PASCAL,RAW
!    GetDC(UNSIGNED),UNSIGNED,PASCAL
    GetDesktopWindow(),HWND,PASCAL
!    GetDeviceCaps(HDC, SIGNED),SIGNED,PASCAL
    GetDIBits(HDC, HBITMAP, WORD, WORD, LPVOID, *BITMAPINFO, WORD),SIGNED,PASCAL,RAW,PROC
    GetDIBits(HDC, HBITMAP, WORD, WORD, LPVOID, ULONG, WORD),SIGNED,PASCAL,RAW,PROC
    GetDiskFreeSpace(*CSTRING DirectoryName, *LONG dwSectPerClust, *LONG dwBytesPerSect, *LONG dwFreeClusters, *LONG dwTotalClusters),BOOL,PASCAL,RAW,NAME('GetDiskFreeSpaceA')
    GetDiskFreeSpaceEx(*CSTRING DirectoryName, *GROUP FreeBytesAvail, *GROUP TotalNumberOfBytes, *GROUP TotalNumberOfFreeBytes),BOOL,PASCAL,RAW,NAME('GetDiskFreeSpaceExA')
    GetDriveType(*CSTRING),UNSIGNED,PASCAL,RAW,NAME('GetDriveTypeA')
    GetCurrentDriveType(ULONG=0),UNSIGNED,PASCAL,RAW,NAME('GetDriveTypeA')
    GetEnhMetaFileHeader(HENHMETAFILE, UNSIGNED, *ENHMETAHEADER ),UNSIGNED,PASCAL,RAW,PROC
    GetEnvironmentVariable(*CSTRING,*CSTRING,DWORD),DWORD,PASCAL,RAW,NAME('GetEnvironmentVariableA')
    GetExitCodeProcess(HANDLE,*DWORD),BOOL,PASCAL,PROC
    GetFileAttributes(*LPCSTR),DWORD,PASCAL,RAW,NAME('GetFileAttributesA')
    GetFileTime(HANDLE,ULONG,ULONG,ULONG),BOOL,PASCAL,PROC
    GetForegroundWindow(),HWND,PASCAL
    GetFullPathName(*CSTRING lpszFile, DWORD cchPath ,*CSTRING lpszPath, *LONG ppszFilePart),DWORD,PASCAL,RAW,NAME('GetFullPathNameA')
    _GetLastError(),DWORD,PASCAL,NAME('GetLastError')
    GetLocaleInfo(LCID,LCTYPE,*LPSTR,SIGNED),SIGNED,PASCAL,RAW,NAME('GetLocaleInfoA'),PROC
    GetMenu(UNSIGNED),UNSIGNED,RAW,PASCAL
    GetMenuItemID(UNSIGNED, SIGNED),USHORT,RAW,PASCAL
    GetMessage(*MSGTYPE,HWND,UNSIGNED,UNSIGNED),BOOL,PASCAL,RAW,NAME('GetMessageA')
    GetModuleHandle(*?),UNSIGNED,PASCAL,RAW,NAME('GetModuleHandleA')
    GetMetaFileBitsEx(HMETAFILE, UNSIGNED, LPVOID),UNSIGNED,PASCAL,PROC
    GetObject(HGDIOBJ, SIGNED, LPVOID),SIGNED,PASCAL,RAW,NAME('GetObjectA'),PROC
    GetOpenFileName(*?),bool,raw,pascal,name('GetOpenFileNameA')
    GetParent(HWND),HWND,PASCAL
    GetPixel(HDC, SIGNED, SIGNED),COLORREF,PASCAL
!    GetProcAddress(UNSIGNED,*CSTRING),LONG,PASCAL,RAW
    GetProfileString(*LPCSTR,*LPCSTR,*LPCSTR,*LPSTR,DWORD),DWORD,PASCAL,RAW,NAME('GetProfileStringA'),PROC
    GetScrollPos(HWND,SIGNED),SIGNED,PASCAL
    GetScrollRange(HWND, SIGNED, *SIGNED, *SIGNED),PASCAL
    GetStartupInfo(*STARTUPINFO),PASCAL,RAW,NAME('GetStartupInfoA')
    GetStockObject(SIGNED),HGDIOBJ,PASCAL
    GetStretchBltMode(HDC),SIGNED,PASCAL
    GetSysColor(SIGNED),COLORREF,PASCAL
    GetSystemDirectory(*LPSTR,UNSIGNED),UNSIGNED,PASCAL,RAW,NAME('GetSystemDirectoryA')
    GetSystemMenu(UNSIGNED, SIGNED),UNSIGNED,RAW,PASCAL
!    GetSystemMetrics(SIGNED),SIGNED,PASCAL
    GetSystemPaletteEntries(HDC, UNSIGNED, UNSIGNED, *PALETTEENTRY),UNSIGNED,PASCAL,RAW,PROC
    GetSystemPaletteEntries(HDC, UNSIGNED, UNSIGNED, ULONG),UNSIGNED,PASCAL,RAW,PROC
    GetTempFileName(*LPCSTR,STRING,UNSIGNED,*LPSTR),UNSIGNED,PASCAL,RAW,NAME('GetTempFileNameA'),PROC
    GetTempPath(DWORD,*LPSTR),DWORD,PASCAL,RAW,NAME('GetTempPathA'),PROC
    GetTextExtentPoint32(UNSIGNED,*CSTRING,UNSIGNED,*GROUP),BOOL,PASCAL,RAW,NAME('GetTextExtentPoint32A'),PROC
    GetTextExtentPoint32(UNSIGNED,*STRING,UNSIGNED,*GROUP),BOOL,PASCAL,RAW,NAME('GetTextExtentPoint32A'),PROC
    GetTopWindow(HWND),UNSIGNED,PASCAL
    GetUserDefaultLangID(),LANGID,PASCAL
    GetUserDefaultLCID(),LCID,PASCAL
    GetVersionEx(*OSVERSIONINFO),BOOL,PASCAL,RAW,NAME('GetVersionExA'),PROC
    GetViewportExtEx(HDC, *SIZE),BOOL,PASCAL,RAW
    GetWindow(USHORT, USHORT),RAW,USHORT,PASCAL
    GetWindowDC(UNSIGNED),UNSIGNED,PASCAL
    GetWindowExtEx(HDC, *SIZE),BOOL,PASCAL,RAW
    GetWindowLong(USHORT,SHORT),LONG,RAW,PASCAL,NAME('GetWindowLongA')
    GetWindowRect(USHORT,*RECT),RAW,PASCAL,PROC
    GetWindowsDirectory(*CSTRING,UNSIGNED),UNSIGNED,PASCAL,RAW,NAME('GetWindowsDirectoryA')
    GetWindowText(ULONG,*CSTRING,USHORT),USHORT,RAW,PASCAL,PROC,NAME('GetWindowTextA')
    GetWinMetaFileBits(HENHMETAFILE, UNSIGNED, ULONG, SIGNED, HDC),UNSIGNED,PASCAL,PROC
    GlobalAlloc(USHORT,ULONG),ULONG,PASCAL
    GlobalDosAlloc(ULONG),ULONG,PASCAL              !Return value has segment in
                                                    !high-order word and selector
                                                    !in low-order word
    GlobalDosFree(USHORT),USHORT,PASCAL
    _GlobalHandle(USHORT),ULONG,PASCAL,NAME('GlobalHandle')     !Return value has handle in
                                                    !low-order word and high-order
                                                    !word is segment or selector
    GlobalFree(HGLOBAL),HGLOBAL,PASCAL,PROC
    GlobalHandleToSel(ULONG),USHORT,PASCAL
    GlobalReAlloc(HGLOBAL, DWORD, WORD),HGLOBAL,PASCAL
    GlobalLock(UNSIGNED),ULONG,PASCAL,PROC
    GlobalUnlock(UNSIGNED),BOOL,PASCAL,PROC
    InitializeSecurityDescriptor(*tagSECURITY_DESCRIPTOR pSecurityDescriptor,  |
                                 DWORD dwRevision),BOOL,RAW,PASCAL,PROC
    IntersectRect(*RECT, *RECT, *RECT),BOOL,PASCAL,RAW,PROC
    IntersectRect(*RECT, ULONG, ULONG),BOOL,PASCAL,RAW,PROC
    InvalidateRect(HWND,ULONG,BOOL),BOOL,RAW,PASCAL,PROC
    IsClipboardFormatAvailable(WORD),BOOL,PASCAL
    IsRectEmpty(*RECT),BOOL,PASCAL,RAW
    IsWindowVisible(UNSIGNED),BOOL,RAW,PASCAL
    IsWindow(HWND hWnd),BOOL,RAW,PASCAL
    IsZoomed(HWND),BOOL,PASCAL
    LineTo(HDC, SIGNED, SIGNED),BOOL,PASCAL,PROC
    MoveToEx(HDC, SIGNED, SIGNED, *POINT),BOOL,PASCAL,RAW,PROC
    MoveToEx(HDC, SIGNED, SIGNED, ULONG),BOOL,PASCAL,PROC
    LoadBitmap(HINSTANCE,*LPCSTR),HBITMAP,PASCAL,RAW,NAME('LoadBitmapA')
    LoadCursor(HINSTANCE,*LPCSTR),HCURSOR,PASCAL,RAW,NAME('LoadCursorA')
    LoadCursor(HINSTANCE,ULONG),HCURSOR,PASCAL,RAW,NAME('LoadCursorA')
    LoadIcon(HINSTANCE,ULONG),HICON,PASCAL,RAW,NAME('LoadIconA')
    LoadIcon(HINSTANCE,*CSTRING),HICON,PASCAL,RAW,NAME('LoadIconA')
    LoadImage(HINSTANCE,*CSTRING,UNSIGNED,SIGNED,SIGNED,UNSIGNED),HANDLE,PASCAL,RAW,NAME('LoadImageA')
!    LoadLibrary(*LPCSTR),HINSTANCE,PASCAL,RAW,NAME('LoadLibraryA')
    LocalFree(HLOCAL),HLOCAL,PASCAL,PROC
    LPtoDP(HDC,*POINT,SIGNED),BOOL,PASCAL,RAW,PROC
    MCIGetErrorString(LONG,*CSTRING,USHORT),USHORT,RAW,PASCAL,PROC
    MCISendCommand(USHORT, USHORT, ULONG, ULONG), ULONG, RAW, PASCAL
    MCISendString(*CSTRING,*CSTRING,USHORT,USHORT),LONG,RAW,PASCAL,PROC,NAME('MciSendStringA')
    MemoryRead(USHORT, ULONG, *CSTRING, ULONG), ULONG, RAW, PASCAL
    MemoryWrite(USHORT, ULONG, *CSTRING, ULONG), ULONG, RAW, PASCAL
    ModifyMenu(UNSIGNED,UNSIGNED,UNSIGNED,UNSIGNED,*CSTRING),USHORT,PASCAL,RAW,NAME('ModifyMenuA')
    MoveWindow(HWND,SIGNED,SIGNED,SIGNED,SIGNED,BOOL),PASCAL
    MulDiv(SIGNED,SIGNED,SIGNED),SIGNED,PASCAL
    MultiByteToWideChar(UNSIGNED,DWORD,*LPCSTR,SIGNED,ULONG,SIGNED),SIGNED,PASCAL,RAW,PROC
    OpenClipboard(HWND),BOOL,PASCAL,PROC
!    PatBlt(HDC, SIGNED, SIGNED, SIGNED, SIGNED, DWORD),BOOL,PASCAL,PROC
    PeekMessage(*MSGTYPE, HWND, UNSIGNED, UNSIGNED, UNSIGNED),BOOL,PASCAL,RAW,NAME('PeekMessageA')
    PlayEnhMetaFile(HDC, HENHMETAFILE, *RECT ),BOOL,PASCAL,RAW,PROC
    PlayMetaFile(HDC, HMETAFILE),BOOL,PASCAL,PROC
    PostMessageA(HWND,UNSIGNED,WPARAM,LPARAM),BOOL,PASCAL,PROC
    PostQuitMessage(SIGNED),PASCAL
    PrintDlg(*?),BOOL,RAW,PASCAL,NAME('PrintDlgA')
    RealizePalette(HDC),UNSIGNED,PASCAL,PROC
    Rectangle(HDC, SIGNED, SIGNED, SIGNED, SIGNED),BOOL,PASCAL,PROC
!    ReleaseDC(UNSIGNED, UNSIGNED),SIGNED,PASCAL,PROC
    RedrawWindow(HWND,ULONG,UNSIGNED,UNSIGNED),BOOL,PASCAL
    RegisterWindowMessage(*CSTRING),UNSIGNED,PASCAL,RAW,NAME('RegisterWindowMessageA')
    RegCloseKey(UNSIGNED),LONG,RAW,PASCAL,NAME('RegCloseKey'),PROC
    RegCreateKeyEx(UNSIGNED,*CSTRING,ULONG,*CSTRING,ULONG,ULONG,ULONG,*ULONG,ULONG),LONG,RAW,PASCAL,NAME('RegCreateKeyExA')
    RegDeleteValue(UNSIGNED,*CSTRING),LONG,RAW,PASCAL,NAME('RegDeleteValueA'),PROC
    RegEnumKeyEx(UNSIGNED,ULONG,*CSTRING,*ULONG,ULONG,ULONG,ULONG,ULONG),LONG,RAW,PASCAL,NAME('RegEnumKeyExA'),PROC
    RegEnumKeyEx(UNSIGNED,ULONG,*CSTRING,*ULONG,ULONG,*CSTRING,*ULONG,ULONG),LONG,RAW,PASCAL,NAME('RegEnumKeyExA'),PROC
    RegEnumValue(UNSIGNED,ULONG,*CSTRING,*ULONG,ULONG,ULONG,*CSTRING,*ULONG),LONG,RAW,PASCAL,NAME('RegEnumValueA'),PROC
    RegFlushKey(UNSIGNED),LONG,RAW,PASCAL,NAME('RegFlushKey'),PROC
    RegOpenKeyEx(UNSIGNED,*CSTRING,ULONG,ULONG,*ULONG),LONG,RAW,PASCAL,NAME('RegOpenKeyExA'),PROC
    RegQueryValueEx(UNSIGNED,*CSTRING,ULONG,*ULONG,ULONG,*ULONG),LONG,RAW,PASCAL,NAME('RegQueryValueExA'),PROC
    RegSetValueEx(UNSIGNED,*CSTRING,ULONG,ULONG,*CSTRING,ULONG),LONG,RAW,PASCAL,NAME('RegSetValueExA'),PROC
    RegisterClass(*WNDCLASS),ATOM,PASCAL,RAW,NAME('RegisterClassA'),PROC
    RemoveDirectory(*LPCSTR),BOOL,PASCAL,RAW,NAME('RemoveDirectoryA'),PROC
    ScaleViewportExtEx(HDC, SIGNED, SIGNED, SIGNED, SIGNED, *SIZE),BOOL,PASCAL,RAW
    ScaleWindowExtEx(UNSIGNED, SIGNED, SIGNED, SIGNED, SIGNED, *SIZE),BOOL,PASCAL,RAW
    ScreenToClient(HWND, *POINT),PASCAL,RAW
    SelectObject(HDC, HGDIOBJ),HGDIOBJ,PASCAL,PROC
    SelectPalette(HDC, HPALETTE, BOOL),HPALETTE,PASCAL,PROC
    SendMessage(UNSIGNED,UNSIGNED,UNSIGNED,LONG),LONG,PASCAL,NAME('SendMessageA'),PROC
    SetActiveWindow(USHORT),USHORT,PASCAL,PROC
    SetBkColor(UNSIGNED, ULONG),ULONG,PASCAL,PROC
    SetBkMode(HDC, SIGNED),SIGNED,PASCAL,PROC
    SetCapture(HWND),HWND,PASCAL,PROC
    SetClassLong(HWND,SHORT,LONG),DWORD,RAW,PASCAL,NAME('SetClassLongA'),PROC
    SetClipboardData(UNSIGNED,HANDLE),HANDLE,PASCAL,PROC
    SetCursorPos(LONG,LONG),RAW,PASCAL
    SetDIBitsToDevice(HDC, SIGNED, SIGNED, DWORD, DWORD, SIGNED,SIGNED, UNSIGNED, UNSIGNED, LPVOID, *BITMAPINFO, UNSIGNED),SIGNED,PASCAL,RAW,PROC
    SetDIBitsToDevice(HDC, SIGNED, SIGNED, DWORD, DWORD, SIGNED,SIGNED, UNSIGNED, UNSIGNED, LPVOID, ULONG, UNSIGNED),SIGNED,PASCAL,RAW,PROC
    SetEntriesInAcl(ULONG cCountOfExplicitEntries,                 |
                    *tagEXPLICIT_ACCESS pListOfExplicitEntries,    |
                    *tagACL OldAcl,                                |
                    *ULONG NewAcl),DWORD,PASCAL,RAW,NAME('SetEntriesInAclA')
    SetEntriesInAcl(ULONG cCountOfExplicitEntries,                 |
                    ULONG pListOfExplicitEntries,                  |
                    ULONG OldAcl,                                  |
                    *ULONG NewAcl),DWORD,PASCAL,RAW,NAME('SetEntriesInAclA')
    SetEnvironmentVariable(*LPCSTR,*LPCSTR),BOOL,PASCAL,RAW,NAME('SetEnvironmentVariableA')
    SetFileAttributes(*LPCSTR,DWORD),BOOL,PASCAL,RAW,PROC,NAME('SetFileAttributesA')
    SetFocus(HWND),HWND,PASCAL
!    SetForegroundWindow(HWND),BOOL,PASCAL,PROC
    SetLocaleInfo(LCID,LCTYPE,*LPCSTR),BOOL,PASCAL,RAW,NAME('SetLocaleInfoA'),PROC
    SetMapMode(HDC, SIGNED),SIGNED,PASCAL
    SetMetaFileBitsEx(UNSIGNED,ULONG),HMETAFILE,PASCAL
    SetParent(HWND, HWND),HWND,PASCAL,PROC
    SetPriorityClass(LONG,LONG),BOOL,PASCAL,PROC
    SetRect(*RECT,SIGNED,SIGNED,SIGNED,SIGNED),BOOL,PASCAL,RAW,PROC
    SetRectEmpty(*RECT),BOOL,PASCAL,RAW,PROC
    SetROP2(HDC, SIGNED),SIGNED,PASCAL
    SetScrollPos(HWND, SIGNED, SIGNED, BOOL),SIGNED,PASCAL
    SetScrollRange(HWND, SIGNED, SIGNED, SIGNED, BOOL),PASCAL
    SetSecurityDescriptorDacl(*tagSECURITY_DESCRIPTOR pSecurityDescriptor, |
                              BOOL bDaclPresent,                           |
                              *tagACL pDacl,                               |
                              BOOL bDaclDefaulted),BOOL,PASCAL,RAW,PROC
    SetSecurityDescriptorDacl(*tagSECURITY_DESCRIPTOR pSecurityDescriptor, |
                              BOOL bDaclPresent,                           |
                              ULONG pDacl,                                 |
                              BOOL bDaclDefaulted),BOOL,PASCAL,RAW,PROC
    SetStretchBltMode(HDC, SIGNED),SIGNED,PASCAL,PROC
    SetSysModalWindow(HWND),HWND,PASCAL,PROC
    SetSysColors(SIGNED,*SIGNED,*COLORREF),PASCAL,RAW
    SetTextColor(HDC, COLORREF),COLORREF,PASCAL,PROC
    SetViewportExtEx(HDC, SIGNED, SIGNED, *SIZE),BOOL,PASCAL,RAW
    SetWindowExtEx(HDC, SIGNED, SIGNED, *SIZE),BOOL,PASCAL,RAW
!    SetWindowLong(HWND,SHORT,LONG),LONG,RAW,PASCAL,NAME('SetWindowLongA'),PROC
    SetWindowPos(UNSIGNED, UNSIGNED, SIGNED, SIGNED, SIGNED, SIGNED, SHORT),RAW,PASCAL
    SetWinMetaFileBits(UNSIGNED,*BYTE,HDC,*METAFILEPICT),HENHMETAFILE,PASCAL
    SetWinMetaFileBits(UNSIGNED,ULONG,HDC,ULONG),HENHMETAFILE,PASCAL
    ShellExecute(UNSIGNED,LONG,*CSTRING,LONG,*CSTRING,SIGNED),UNSIGNED,PASCAL,RAW,NAME('SHELLEXECUTEA'),PROC
    Shell_NotifyIcon(DWORD,*NOTIFYICONDATA),BOOL,RAW,PASCAL,PROC,NAME('Shell_NotifyIconA')
    Shell_NotifyIcon(ULONG,LONG),BOOL,PASCAL,NAME('Shell_NotifyIconA')
!    ShowWindow(USHORT,USHORT),BYTE,PASCAL,PROC
    Sleep(DWORD dwMilliseconds),PASCAL
    StartDoc(HDC,*DOCINFO),SIGNED,PASCAL,RAW,NAME('StartDocA'),PROC
    StretchBlt(HDC, SIGNED, SIGNED, SIGNED, SIGNED, HDC, SIGNED, SIGNED, SIGNED, SIGNED, DWORD),BOOL,PASCAL,PROC
    StretchDIBits(HDC, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, LPVOID, *BITMAPINFO, WORD, DWORD),SIGNED,PASCAL,RAW,PROC
    StretchDIBits(HDC, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, SIGNED, LPVOID, ULONG, WORD, DWORD),SIGNED,PASCAL,RAW,PROC
    SysAllocString(*STRING),ULONG,PASCAL,RAW
    !SysFreeString(ULONG),PASCAL
    SystemTimeToFileTime(*SYSTEMTIME,*FILETIME),BOOL,RAW,PASCAL,PROC
    TerminateProcess(HANDLE,UNSIGNED),BOOL,PASCAL,PROC
    TextOut(HDC,SIGNED,SIGNED,*LPCSTR,SIGNED),BOOL,PASCAL,RAW,NAME('TextOutA'),PROC
    TranslateMessage(*MSGTYPE),BOOL,PASCAL,RAW,PROC
    WaitMessage(),BOOL,PASCAL,PROC
    WindowFromPoint(POINT),HWND,PASCAL
    WinExec(*CSTRING,USHORT),RAW,USHORT,PASCAL ! For straightforward execute
                                               ! use can now use Clarion RUN
    UpdateWindow(HWND),BOOL,PASCAL,PROC
    WaitForInputIdle(HANDLE,DWORD),DWORD,PASCAL,PROC
!    WaitForSingleObject(HANDLE,DWORD),DWORD,PASCAL,PROC
  OMIT('End_Omit',_WINDOWS_)
    FreeLibrary(HINSTANCE),BOOL,PASCAL,PROC
    GetDC(UNSIGNED),UNSIGNED,PASCAL
    GetDeviceCaps(HDC, SIGNED),SIGNED,PASCAL
!    GetLastError(),DWORD,PASCAL
    GetProcAddress(UNSIGNED,*CSTRING),LONG,PASCAL,RAW
    GetSystemMetrics(SIGNED),SIGNED,PASCAL
    LoadLibrary(*LPCSTR),HINSTANCE,PASCAL,RAW,NAME('LoadLibraryA')
    PatBlt(HDC, SIGNED, SIGNED, SIGNED, SIGNED, DWORD),BOOL,PASCAL,PROC
    ReleaseDC(UNSIGNED, UNSIGNED),SIGNED,PASCAL,PROC
    SetForegroundWindow(HWND),BOOL,PASCAL,PROC
    SetWindowLong(HWND,SHORT,LONG),LONG,RAW,PASCAL,NAME('SetWindowLongA'),PROC
    SetWindowText(HWND hwnd, *CSTRING lpsz),BOOL,PROC,RAW,PASCAL,NAME('SetWindowTextA')
    ShowWindow(USHORT,USHORT),BYTE,PASCAL,PROC
    WaitForSingleObject(HANDLE,DWORD),DWORD,PASCAL,PROC
 !End_Omit
  END
  MODULE('WinNet')
    WNetGetUser(*CSTRING,*CSTRING,*ULONG),ULONG,RAW,PASCAL,NAME('WNetGetUserA'),PROC
  END
! These are functions that are not documented by Clarion but work fine
  MODULE('OTHER')
!*** Added By Fred
!    The prototypes are just like the ones in the TopSpeed C manual
      MKDIR(*CSTRING),SHORT,RAW,NAME('_MKDIR')
      _OSOPEN(*CSTRING,USHORT),SHORT,RAW,NAME('__OSOPEN')
      _OSREAD(SHORT,*?,USHORT),SHORT,RAW,NAME('__OSREAD')
      _OSWRITE(SHORT,*?,USHORT),SHORT,RAW,NAME('__OSWRITE')
      _OSCLOSE(SHORT),SHORT,RAW,NAME('__OSCLOSE')
      _OSCREATE(*CSTRING,SHORT),SHORT,RAW,NAME('__OSCREATE')
      _Remove(*CSTRING),SHORT,RAW,NAME('_REMOVE'),PROC
      _Access(*CSTRING,SHORT),SHORT,RAW,NAME('_ACCESS')
      _StrCpy( *CSTRING, *CSTRING ),RAW,NAME('_STRCPY')
      StrCat(ulong,ulong),name('_strcat')
      StrCpy(ulong,ulong),name('_strcpy')
      _MemCpy( ulong, ulong, unsigned ), NAME('_MEMCPY')
      MemCpy(ulong,ulong,unsigned),name('_memcpy')
      MemSet(ulong,ulong,unsigned),name('_memset')
      TMPNAM(*CSTRING),ULONG,RAW,NAME('_TMPNAM')
      _OSRename( *cstring, *cstring ), short, raw, name('__OSRENAME')
      chmod(*CSTRING,USHORT),SHORT,RAW,NAME('_CHMOD')
      LtoA(LONG,*CSTRING,SIGNED),ULONG,RAW,NAME('_ltoa'),PROC
      strtoul(*CSTRING s, *LONG endptr, LONG base),ULONG,RAW,NAME('_strtoul')
  END
  MODULE('WSLDEBUG')
      printdebughex(USHORT),NAME('WslDebug$PrintHex')
      printdebugstring(STRING),NAME('WslDebug$Print')
      printdebugline(),NAME('WslDebug$PrintLine')
      printdebugevent(),NAME('WslDebug$PrintEvent')
  END
  MODULE('BindTypes')
      BindFile(*CSTRING),TYPE
      UnBindFile(*CSTRING),TYPE
  END
