
!ABCIncludeFile(SCI)

!region Notices
! ================================================================================
! Notice : Copyright (C) 2017, Devuna
!          Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of Devuna-ClassViewer (https://github.com/Devuna/Devuna-ClassViewer)
!
!    Devuna-ClassViewer is free software: you can redistribute it and/or modify
!    it under the terms of the MIT License as published by
!    the Open Source Initiative.
!
!    Devuna-ClassViewer is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    MIT License for more details.
!
!    You should have received a copy of the MIT License
!    along with Devuna-ClassViewer.  If not, see <https://opensource.org/licenses/MIT>.
! ================================================================================
!endregion Notices

! ================================================================================
!Scintilla Viewer Class
!Author:        Randy Rogers <rrogers@devuna.com>
!Creation Date: 07/10/2003 at 3:04PM
!
!Revisions:
!==========
!2003.10.25 KCR modified to match Ron Schofields enhanced lexer for enhanced
!               reserved word processing
!2003.11.28 KCR added deprecated keyword lists for use with new
!               lexer dated 2003.11.27 or later
!2003.12.21 KCR added themed support and a default set of styles
!2004.12.09 KCR changed 'Family' from ABC to SCI
! ================================================================================


 OMIT('_EndOfInclude_',_CSciViewerClassPresent_)
_CSciViewerClassPresent_ EQUATE(1)

    INCLUDE('CSciControl.inc'),ONCE
    INCLUDE('ABERROR.INC'),ONCE
    INCLUDE('ABPOPUP.INC'),ONCE
    INCLUDE('cwHH.INC'),ONCE

ERROR_SUCCESS        EQUATE(0)
ERROR_NO_MORE_ITEMS  EQUATE(259)



markerBookmark              EQUATE(1)
!                     CSTRING('/* XPM */')
!                     STRING('<0,0>')
!                     STRING('<0,0,0>')
bookMarkBlueGem   GROUP,STATIC
                     CSTRING('    15    15      64            1')
                     CSTRING('  c none')
                     CSTRING('. c #0c0630')
                     CSTRING('# c #8c8a8c')
                     CSTRING('a c #244a84')
                     CSTRING('b c #545254')
                     CSTRING('c c #cccecc')
                     CSTRING('d c #949594')
                     CSTRING('e c #346ab4')
                     CSTRING('f c #242644')
                     CSTRING('g c #3c3e3c')
                     CSTRING('h c #6ca6fc')
                     CSTRING('i c #143789')
                     CSTRING('j c #204990')
                     CSTRING('k c #5c8dec')
                     CSTRING('l c #707070')
                     CSTRING('m c #3c82dc')
                     CSTRING('n c #345db4')
                     CSTRING('o c #619df7')
                     CSTRING('p c #acacac')
                     CSTRING('q c #346ad4')
                     CSTRING('r c #1c3264')
                     CSTRING('s c #174091')
                     CSTRING('t c #5482df')
                     CSTRING('u c #4470c4')
                     CSTRING('v c #2450a0')
                     CSTRING('w c #14162c')
                     CSTRING('x c #5c94f6')
                     CSTRING('y c #b7b8b7')
                     CSTRING('z c #646464')
                     CSTRING('A c #3c68b8')
                     CSTRING('B c #7cb8fc')
                     CSTRING('C c #7c7a7c')
                     CSTRING('D c #3462b9')
                     CSTRING('E c #7c7eac')
                     CSTRING('F c #44464c')
                     CSTRING('G c #a4a4a4')
                     CSTRING('H c #24224c')
                     CSTRING('I c #282668')
                     CSTRING('J c #5c5a8c')
                     CSTRING('K c #7c8ebc')
                     CSTRING('L c #dcd7e4')
                     CSTRING('M c #141244')
                     CSTRING('N c #1c2e5c')
                     CSTRING('O c #24327c')
                     CSTRING('P c #4472cc')
                     CSTRING('Q c #6ca2fc')
                     CSTRING('R c #74b2fc')
                     CSTRING('S c #24367c')
                     CSTRING('T c #b4b2c4')
                     CSTRING('U c #403e58')
                     CSTRING('V c #4c7fd6')
                     CSTRING('W c #24428c')
                     CSTRING('X c #747284')
                     CSTRING('Y c #142e7c')
                     CSTRING('Z c #64a2fc')
                     CSTRING('0 c #3c72dc')
                     CSTRING('1 c #bcbebc')
                     CSTRING('2 c #6c6a6c')
                     CSTRING('3 c #848284')
                     CSTRING('4 c #2c5098')
                     CSTRING('5 c #1c1a1c')
                     CSTRING('6 c #243250')
                     CSTRING('7 c #7cbefc')
                     CSTRING('8 c #d4d2d4')
                     CSTRING('    yCbgbCy    ')
                     CSTRING('   #zGGyGGz#   ')
                     CSTRING('  #zXTLLLTXz#  ')
                     CSTRING(' p5UJEKKKEJU5p ')
                     CSTRING(' lfISa444aSIfl ')
                     CSTRING(' wIYij444jsYIw ')
                     CSTRING(' .OsvnAAAnvsO. ')
                     CSTRING(' MWvDuVVVPDvWM ')
                     CSTRING(' HsDPVkxxtPDsH ')
                     CSTRING(' UiAtxohZxtuiU ')
                     CSTRING(' pNnkQRBRhkDNp ')
                     CSTRING(' 1FrqoR7Bo0rF1 ')
                     CSTRING(' 8GC6aemea6CG8 ')
                     CSTRING('  cG3l2z2l3Gc  ')
                     CSTRING('    1GdddG1    ')
                  END


MARGIN_SCRIPT_FOLD_INDEX    EQUATE(2)               !2004.12.18 KCR
szFoldProp                  CSTRING('fold')         !2004.12.18 KCR
szFoldCompactProp           CSTRING('fold.compact') !2004.12.18 KCR
szPropVal0                  CSTRING('0')            !2004.12.18 KCR
szPropVal1                  CSTRING('1')            !2004.12.18 KCR


TA_NOUPDATECP               EQUATE(0)
TA_UPDATECP                 EQUATE(1)

TA_LEFT                     EQUATE(0)
TA_RIGHT                    EQUATE(2)
TA_CENTER                   EQUATE(6)
TA_TOP                      EQUATE(0)
TA_BOTTOM                   EQUATE(8)
TA_BASELINE                 EQUATE(24)
TA_MASK                     EQUATE(BOR(BOR(TA_BASELINE,TA_CENTER),TA_UPDATECP))
TA_ALIGNMASK                EQUATE(BOR(TA_BASELINE,TA_CENTER))

!!  Default System and User IDs for language and locale.
!LANG_SYSTEM_DEFAULT    EQUATE(2)
!LANG_USER_DEFAULT      EQUATE(1)
!
!LOCALE_SYSTEM_DEFAULT  EQUATE(LANG_SYSTEM_DEFAULT)
!LOCALE_USER_DEFAULT    EQUATE(LANG_USER_DEFAULT)
!
!!  Locale Types.
!!  These types are used for the GetLocaleInfoA NLS API routine.
!
!LOCALE_NOUSEROVERRIDE   EQUATE(080000000h)  ! OR in to avoid user override
!
!LOCALE_ILANGUAGE            EQUATE(00001h)  ! language id
!LOCALE_SLANGUAGE            EQUATE(00002h)  ! localized name of language
!LOCALE_SENGLANGUAGE         EQUATE(01001h)  ! English name of language
!LOCALE_SABBREVLANGNAME      EQUATE(00003h)  ! abbreviated language name
!LOCALE_SNATIVELANGNAME      EQUATE(00004h)  ! native name of language
!LOCALE_ICOUNTRY             EQUATE(00005h)  ! country code
!LOCALE_SCOUNTRY             EQUATE(00006h)  ! localized name of country
!LOCALE_SENGCOUNTRY          EQUATE(01002h)  ! English name of country
!LOCALE_SABBREVCTRYNAME      EQUATE(00007h)  ! abbreviated country name
!LOCALE_SNATIVECTRYNAME      EQUATE(00008h)  ! native name of country
!LOCALE_IDEFAULTLANGUAGE     EQUATE(00009h)  ! default language id
!LOCALE_IDEFAULTCOUNTRY      EQUATE(0000Ah)  ! default country code
!LOCALE_IDEFAULTCODEPAGE     EQUATE(0000Bh)  ! default oem code page
!LOCALE_IDEFAULTANSICODEPAGE EQUATE(01004h)  ! default ansi code page
!
!LOCALE_SLIST                EQUATE(0000Ch)  ! list item separator
!LOCALE_IMEASURE             EQUATE(0000Dh)  ! 0 = metric, 1 = US
!
!LOCALE_SDECIMAL             EQUATE(0000Eh)  ! decimal separator
!LOCALE_STHOUSAND            EQUATE(0000Fh)  ! thousand separator
!LOCALE_SGROUPING            EQUATE(00010h)  ! digit grouping
!LOCALE_IDIGITS              EQUATE(00011h)  ! number of fractional digits
!LOCALE_ILZERO               EQUATE(00012h)  ! leading zeros for decimal
!LOCALE_INEGNUMBER           EQUATE(01010h)  ! negative number mode
!LOCALE_SNATIVEDIGITS        EQUATE(00013h)  ! native ascii 0-9
!
!LOCALE_SCURRENCY            EQUATE(00014h)  ! local monetary symbol
!LOCALE_SINTLSYMBOL          EQUATE(00015h)  ! intl monetary symbol
!LOCALE_SMONDECIMALSEP       EQUATE(00016h)  ! monetary decimal separator
!LOCALE_SMONTHOUSANDSEP      EQUATE(00017h)  ! monetary thousand separator
!LOCALE_SMONGROUPING         EQUATE(00018h)  ! monetary grouping
!LOCALE_ICURRDIGITS          EQUATE(00019h)  ! # local monetary digits
!LOCALE_IINTLCURRDIGITS      EQUATE(0001Ah)  ! # intl monetary digits
!LOCALE_ICURRENCY            EQUATE(0001Bh)  ! positive currency mode
!LOCALE_INEGCURR             EQUATE(0001Ch)  ! negative currency mode
!
!LOCALE_SDATE                EQUATE(0001Dh)  ! date separator
!LOCALE_STIME                EQUATE(0001Eh)  ! time separator
!LOCALE_SSHORTDATE           EQUATE(0001Fh)  ! short date-time separator
!LOCALE_SLONGDATE            EQUATE(00020h)  ! long date-time separator
!LOCALE_STIMEFORMAT          EQUATE(01003h)  ! time format string
!LOCALE_IDATE                EQUATE(00021h)  ! short date format ordering
!LOCALE_ILDATE               EQUATE(00022h)  ! long date format ordering
!LOCALE_ITIME                EQUATE(00023h)  ! time format specifier
!LOCALE_ITIMEMARKPOSN        EQUATE(01005h)  ! time marker position
!LOCALE_ICENTURY             EQUATE(00024h)  ! century format specifier
!LOCALE_ITLZERO              EQUATE(00025h)  ! leading zeros in time field
!LOCALE_IDAYLZERO            EQUATE(00026h)  ! leading zeros in day field
!LOCALE_IMONLZERO            EQUATE(00027h)  ! leading zeros in month field
!LOCALE_S1159                EQUATE(00028h)  ! AM designator
!LOCALE_S2359                EQUATE(00029h)  ! PM designator
!
!LOCALE_ICALENDARTYPE        EQUATE(01009h)  ! type of calendar specifier
!LOCALE_IOPTIONALCALENDAR    EQUATE(0100Bh)  ! additional calendar types specifier
!
!LOCALE_IFIRSTDAYOFWEEK      EQUATE(0100Ch)  ! first day of week specifier
!LOCALE_IFIRSTWEEKOFYEAR     EQUATE(0100Dh)  ! first week of year specifier
!
!LOCALE_SDAYNAME1            EQUATE(0002Ah)  ! long name for Monday
!LOCALE_SDAYNAME2            EQUATE(0002Bh)  ! long name for Tuesday
!LOCALE_SDAYNAME3            EQUATE(0002Ch)  ! long name for Wednesday
!LOCALE_SDAYNAME4            EQUATE(0002Dh)  ! long name for Thursday
!LOCALE_SDAYNAME5            EQUATE(0002Eh)  ! long name for Friday
!LOCALE_SDAYNAME6            EQUATE(0002Fh)  ! long name for Saturday
!LOCALE_SDAYNAME7            EQUATE(00030h)  ! long name for Sunday
!LOCALE_SABBREVDAYNAME1      EQUATE(00031h)  ! abbreviated name for Monday
!LOCALE_SABBREVDAYNAME2      EQUATE(00032h)  ! abbreviated name for Tuesday
!LOCALE_SABBREVDAYNAME3      EQUATE(00033h)  ! abbreviated name for Wednesday
!LOCALE_SABBREVDAYNAME4      EQUATE(00034h)  ! abbreviated name for Thursday
!LOCALE_SABBREVDAYNAME5      EQUATE(00035h)  ! abbreviated name for Friday
!LOCALE_SABBREVDAYNAME6      EQUATE(00036h)  ! abbreviated name for Saturday
!LOCALE_SABBREVDAYNAME7      EQUATE(00037h)  ! abbreviated name for Sunday
!LOCALE_SMONTHNAME1          EQUATE(00038h)  ! long name for January
!LOCALE_SMONTHNAME2          EQUATE(00039h)  ! long name for February
!LOCALE_SMONTHNAME3          EQUATE(0003Ah)  ! long name for March
!LOCALE_SMONTHNAME4          EQUATE(0003Bh)  ! long name for April
!LOCALE_SMONTHNAME5          EQUATE(0003Ch)  ! long name for May
!LOCALE_SMONTHNAME6          EQUATE(0003Dh)  ! long name for June
!LOCALE_SMONTHNAME7          EQUATE(0003Eh)  ! long name for July
!LOCALE_SMONTHNAME8          EQUATE(0003Fh)  ! long name for August
!LOCALE_SMONTHNAME9          EQUATE(00040h)  ! long name for September
!LOCALE_SMONTHNAME10         EQUATE(00041h)  ! long name for October
!LOCALE_SMONTHNAME11         EQUATE(00042h)  ! long name for November
!LOCALE_SMONTHNAME12         EQUATE(00043h)  ! long name for December
!LOCALE_SMONTHNAME13         EQUATE(0100Eh)  ! long name for 13th month (if exists)
!LOCALE_SABBREVMONTHNAME1    EQUATE(00044h)  ! abbreviated name for January
!LOCALE_SABBREVMONTHNAME2    EQUATE(00045h)  ! abbreviated name for February
!LOCALE_SABBREVMONTHNAME3    EQUATE(00046h)  ! abbreviated name for March
!LOCALE_SABBREVMONTHNAME4    EQUATE(00047h)  ! abbreviated name for April
!LOCALE_SABBREVMONTHNAME5    EQUATE(00048h)  ! abbreviated name for May
!LOCALE_SABBREVMONTHNAME6    EQUATE(00049h)  ! abbreviated name for June
!LOCALE_SABBREVMONTHNAME7    EQUATE(0004Ah)  ! abbreviated name for July
!LOCALE_SABBREVMONTHNAME8    EQUATE(0004Bh)  ! abbreviated name for August
!LOCALE_SABBREVMONTHNAME9    EQUATE(0004Ch)  ! abbreviated name for September
!LOCALE_SABBREVMONTHNAME10   EQUATE(0004Dh)  ! abbreviated name for October
!LOCALE_SABBREVMONTHNAME11   EQUATE(0004Eh)  ! abbreviated name for November
!LOCALE_SABBREVMONTHNAME12   EQUATE(0004Fh)  ! abbreviated name for December
!LOCALE_SABBREVMONTHNAME13   EQUATE(0100Fh)  ! abbreviated name for 13th month (if exists)
!
!LOCALE_SPOSITIVESIGN        EQUATE(00050h)  ! positive sign
!LOCALE_SNEGATIVESIGN        EQUATE(00051h)  ! negative sign
!LOCALE_IPOSSIGNPOSN         EQUATE(00052h)  ! positive sign position
!LOCALE_INEGSIGNPOSN         EQUATE(00053h)  ! negative sign position
!LOCALE_IPOSSYMPRECEDES      EQUATE(00054h)  ! mon sym precedes pos amt
!LOCALE_IPOSSEPBYSPACE       EQUATE(00055h)  ! mon sym sep by space from pos
!LOCALE_INEGSYMPRECEDES      EQUATE(00056h)  ! mon sym precedes neg amt
!LOCALE_INEGSEPBYSPACE       EQUATE(00057h)  ! mon sym sep by space from neg
!! Device Parameters for GetDeviceCaps()
!DRIVERVERSION              EQUATE(0)           ! Device driver version
!TECHNOLOGY                 EQUATE(2)           ! Device classification
!HORZSIZE                   EQUATE(4)           ! Horizontal size in millimeters
!VERTSIZE                   EQUATE(6)           ! Vertical size in millimeters
!HORZRES                    EQUATE(8)           ! Horizontal width in pixels
!VERTRES                    EQUATE(10)          ! Vertical height in pixels
!BITSPIXEL                  EQUATE(12)          ! Number of bits per pixel
!PLANES                     EQUATE(14)          ! Number of planes
!NUMBRUSHES                 EQUATE(16)          ! Number of brushes the device has
!NUMPENS                    EQUATE(18)          ! Number of pens the device has
!NUMMARKERS                 EQUATE(20)          ! Number of markers the device has
!NUMFONTS                   EQUATE(22)          ! Number of fonts the device has
!NUMCOLORS                  EQUATE(24)          ! Number of colors the device supports
!PDEVICESIZE                EQUATE(26)          ! Size required for device descriptor
!CURVECAPS                  EQUATE(28)          ! Curve capabilities
!LINECAPS                   EQUATE(30)          ! Line capabilities
!POLYGONALCAPS              EQUATE(32)          ! Polygonal capabilities
!TEXTCAPS                   EQUATE(34)          ! Text capabilities
!CLIPCAPS                   EQUATE(36)          ! Clipping capabilities
!RASTERCAPS                 EQUATE(38)          ! Bitblt capabilities
!ASPECTX                    EQUATE(40)          ! Length of the X leg
!ASPECTY                    EQUATE(42)          ! Length of the Y leg
!ASPECTXY                   EQUATE(44)          ! Length of the hypotenuse
!SHADEBLENDCAPS             EQUATE(45)          ! Shading and blending caps
!
!LOGPIXELSX                  EQUATE(88)          ! Logical pixels/inch in X
!LOGPIXELSY                  EQUATE(90)          ! Logical pixels/inch in Y
!
!! Printing related DeviceCaps. These replace the appropriate Escapes
!PHYSICALWIDTH               EQUATE(110)         ! Physical Width in device units
!PHYSICALHEIGHT              EQUATE(111)         ! Physical Height in device units
!PHYSICALOFFSETX             EQUATE(112)         ! Physical Printable Area x margin
!PHYSICALOFFSETY             EQUATE(113)         ! Physical Printable Area y margin
!SCALINGFACTORX              EQUATE(114)         ! Scaling factor x
!SCALINGFACTORY              EQUATE(115)         ! Scaling factor y
!
!! Map modes EQUATES
!MM_TEXT             EQUATE(1)
!MM_LOMETRIC         EQUATE(2)
!MM_HIMETRIC         EQUATE(3)
!MM_LOENGLISH        EQUATE(4)
!MM_HIENGLISH        EQUATE(5)
!MM_TWIPS            EQUATE(6)
!MM_ISOTROPIC        EQUATE(7)
!MM_ANISOTROPIC      EQUATE(8)
!
!! PRNTDLG Flag Equates
!PD_ALLPAGES                  EQUATE(000000000h)
!PD_SELECTION                 EQUATE(000000001h)
!PD_PAGENUMS                  EQUATE(000000002h)
!PD_NOSELECTION               EQUATE(000000004h)
!PD_NOPAGENUMS                EQUATE(000000008h)
!PD_COLLATE                   EQUATE(000000010h)
!PD_PRINTTOFILE               EQUATE(000000020h)
!PD_PRINTSETUP                EQUATE(000000040h)
!PD_NOWARNING                 EQUATE(000000080h)
!PD_RETURNDC                  EQUATE(000000100h)
!PD_RETURNIC                  EQUATE(000000200h)
!PD_RETURNDEFAULT             EQUATE(000000400h)
!PD_SHOWHELP                  EQUATE(000000800h)
!PD_ENABLEPRINTHOOK           EQUATE(000001000h)
!PD_ENABLESETUPHOOK           EQUATE(000002000h)
!PD_ENABLEPRINTTEMPLATE       EQUATE(000004000h)
!PD_ENABLESETUPTEMPLATE       EQUATE(000008000h)
!PD_ENABLEPRINTTEMPLATEHANDLE EQUATE(000010000h)
!PD_ENABLESETUPTEMPLATEHANDLE EQUATE(000020000h)
!PD_USEDEVMODECOPIES          EQUATE(000040000h)
!PD_DISABLEPRINTTOFILE        EQUATE(000080000h)
!PD_HIDEPRINTTOFILE           EQUATE(000100000h)
!PD_NONETWORKBUTTON           EQUATE(000200000h)
!
!! ExtDeviceMode wMode Equates
!DM_UPDATE           equate(1)
!DM_COPY             equate(2)
!DM_PROMPT           equate(4)
!DM_MODIFY           equate(8)
!
!DM_ORIENTATION      equate(0000001h)
!DM_PAPERSIZE        equate(0000002h)
!DM_PAPERLENGTH      equate(0000004h)
!DM_PAPERWIDTH       equate(0000008h)
!DM_SCALE            equate(0000010h)
!DM_COPIES           equate(0000100h)
!DM_DEFAULTSOURCE    equate(0000200h)
!DM_PRINTQUALITY     equate(0000400h)
!DM_COLOR            equate(0000800h)
!DM_DUPLEX           equate(0001000h)
!DM_YRESOLUTION      equate(0002000h)
!DM_TTOPTION         equate(0004000h)
!
!!* orientation selections *!
!DMORIENT_PORTRAIT   equate(1)
!DMORIENT_LANDSCAPE  equate(2)
!
!!* paper selections *!
!!*  Warning: The PostScript driver mistakingly uses DMPAPER_ values between *!
!!*  50 and 56.  Don't use this range when defining new paper sizes.         *!
!
!DMPAPER_LETTER              equate(1)  !* Letter 8 1/2 x 11 in               *!
!DMPAPER_FIRST               equate(DMPAPER_LETTER)
!DMPAPER_LETTERSMALL         equate(2)  !* Letter Small 8 1/2 x 11 in         *!
!DMPAPER_TABLOID             equate(3)  !* Tabloid 11 x 17 in                 *!
!DMPAPER_LEDGER              equate(4)  !* Ledger 17 x 11 in                  *!
!DMPAPER_LEGAL               equate(5)  !* Legal 8 1/2 x 14 in                *!
!DMPAPER_STATEMENT           equate(6)  !* Statement 5 1/2 x 8 1/2 in         *!
!DMPAPER_EXECUTIVE           equate(7)  !* Executive 7 1/4 x 10 1/2 in        *!
!DMPAPER_A3                  equate(8)  !* A3 297 x 420 mm                    *!
!DMPAPER_A4                  equate(9)  !* A4 210 x 297 mm                    *!
!DMPAPER_A4SMALL             equate(10) !* A4 Small 210 x 297 mm              *!
!DMPAPER_A5                  equate(11) !* A5 148 x 210 mm                    *!
!DMPAPER_B4                  equate(12) !* B4 250 x 354                       *!
!DMPAPER_B5                  equate(13) !* B5 182 x 257 mm                    *!
!DMPAPER_FOLIO               equate(14) !* Folio 8 1/2 x 13 in                *!
!DMPAPER_QUARTO              equate(15) !* Quarto 215 x 275 mm                *!
!DMPAPER_10X14               equate(16) !* 10X14 in                           *!
!DMPAPER_11X17               equate(17) !* 11x17 in                           *!
!DMPAPER_NOTE                equate(18) !* Note 8 1/2 x 11 in                 *!
!DMPAPER_ENV_9               equate(19) !* Envelope #9 3 7/8 x 8 7/8          *!
!DMPAPER_ENV_10              equate(20) !* Envelope #10 4 1/8 x 9 1/2         *!
!DMPAPER_ENV_11              equate(21) !* Envelope #11 4 1/2 x 10 3/8        *!
!DMPAPER_ENV_12              equate(22) !* Envelope #12 4 \276 x 11           *!
!DMPAPER_ENV_14              equate(23) !* Envelope #14 5 x 11 1/2            *!
!DMPAPER_CSHEET              equate(24) !* C size sheet                       *!
!DMPAPER_DSHEET              equate(25) !* D size sheet                       *!
!DMPAPER_ESHEET              equate(26) !* E size sheet                       *!
!DMPAPER_ENV_DL              equate(27) !* Envelope DL 110 x 220mm            *!
!DMPAPER_ENV_C5              equate(28) !* Envelope C5 162 x 229 mm           *!
!DMPAPER_ENV_C3              equate(29) !* Envelope C3  324 x 458 mm          *!
!DMPAPER_ENV_C4              equate(30) !* Envelope C4  229 x 324 mm          *!
!DMPAPER_ENV_C6              equate(31) !* Envelope C6  114 x 162 mm          *!
!DMPAPER_ENV_C65             equate(32) !* Envelope C65 114 x 229 mm          *!
!DMPAPER_ENV_B4              equate(33) !* Envelope B4  250 x 353 mm          *!
!DMPAPER_ENV_B5              equate(34) !* Envelope B5  176 x 250 mm          *!
!DMPAPER_ENV_B6              equate(35) !* Envelope B6  176 x 125 mm          *!
!DMPAPER_ENV_ITALY           equate(36) !* Envelope 110 x 230 mm              *!
!DMPAPER_ENV_MONARCH         equate(37) !* Envelope Monarch 3.875 x 7.5 in    *!
!DMPAPER_ENV_PERSONAL        equate(38) !* 6 3/4 Envelope 3 5/8 x 6 1/2 in    *!
!DMPAPER_FANFOLD_US          equate(39) !* US Std Fanfold 14 7/8 x 11 in      *!
!DMPAPER_FANFOLD_STD_GERMAN  equate(40) !* German Std Fanfold 8 1/2 x 12 in   *!
!DMPAPER_FANFOLD_LGL_GERMAN  equate(41) !* German Legal Fanfold 8 1/2 x 13 in *!
!
!DMPAPER_LAST        equate(DMPAPER_FANFOLD_LGL_GERMAN)
!
!DMPAPER_USER        equate(256)
!
!!* bin selections *!
!DMBIN_UPPER         equate(1)
!DMBIN_FIRST         equate(DMBIN_UPPER)
!DMBIN_ONLYONE       equate(1)
!DMBIN_LOWER         equate(2)
!DMBIN_MIDDLE        equate(3)
!DMBIN_MANUAL        equate(4)
!DMBIN_ENVELOPE      equate(5)
!DMBIN_ENVMANUAL     equate(6)
!DMBIN_AUTO          equate(7)
!DMBIN_TRACTOR       equate(8)
!DMBIN_SMALLFMT      equate(9)
!DMBIN_LARGEFMT      equate(10)
!DMBIN_LARGECAPACITY equate(11)
!DMBIN_CASSETTE      equate(14)
!DMBIN_LAST          equate(DMBIN_CASSETTE)
!
!DMBIN_USER          equate(256)    !* device specific bins start here *!
!
!!* print qualities *!
!DMRES_DRAFT         equate(-1)
!DMRES_LOW           equate(-2)
!DMRES_MEDIUM        equate(-3)
!DMRES_HIGH          equate(-4)
!
!!* color enable/disable for color printers *!
!DMCOLOR_MONOCHROME  equate(1)
!DMCOLOR_COLOR       equate(2)
!
!!* duplex enable *!
!DMDUP_SIMPLEX    equate(1)
!DMDUP_VERTICAL   equate(2)
!DMDUP_HORIZONTAL equate(3)
!
!!* TrueType options *!
!DMTT_BITMAP     equate(1)      !* print TT fonts as graphics *!
!DMTT_DOWNLOAD   equate(2)      !* download TT fonts as soft fonts *!
!DMTT_SUBDEV     equate(3)      !* substitute device fonts for TT fonts *!

! PRINTDLG type declaration
kcr_PRINTDLG      GROUP,TYPE
lStructSize          ULONG
hwndOwner            UNSIGNED
hDevMode             UNSIGNED
hDevNames            UNSIGNED
hDC                  UNSIGNED
Flags                ULONG
nFromPage            USHORT
nToPage              USHORT
nMinPage             USHORT
nMaxPage             USHORT
nCopies              USHORT
hInstance            UNSIGNED
lCustData            LONG
lpfnPrintHook        ULONG
lpfnSetupHook        ULONG
lpPrintTemplateName  ULONG
lpSetupTemplateName  ULONG
hPrintTemplate       UNSIGNED
hSetupTemplate       UNSIGNED
                  END

! DEVNAMES type declaration
kcr_DEVNAMES      GROUP,TYPE
wDriverOffset        USHORT
wDeviceOffset        USHORT
wOutputOffset        USHORT
wDefault             USHORT
                  END

! DEVMODE type declaration
kcr_DEVMODE       GROUP,TYPE
DeviceName           CSTRING(32)
SpecVersion          USHORT !signed
DriverVersion        USHORT !signed
dmSize               USHORT !signed
DriverExtra          USHORT !signed
Fields               ULONG
Orientation          USHORT
PaperSize            USHORT
PaperLength          USHORT
PaperWidth           USHORT
Scale                USHORT
Copies               USHORT
DefaultSource        USHORT
PrintQuality         USHORT
Color                USHORT
Duplex               USHORT
YResolution          USHORT
TTOption             USHORT
Collate              USHORT
FormName             CSTRING(32)
LogPixels            SIGNED
BitsPerPel           ULONG
PelsWidth            ULONG
PelsHeight           ULONG
DisplayFlags         ULONG
DisplayFrequency     ULONG
ICMMethod            ULONG
ICMIntent            ULONG
MediaType            ULONG
DitherType           ULONG
Reserved1            ULONG
Reserved2            ULONG
                  END

kcr_DOCINFO       GROUP,TYPE
cbSize               LONG
lpszDocName          LONG
lpszOutput           LONG
lpszDatatype         LONG
fwType               LONG
                  END

kcr_TEXTMETRIC    GROUP,TYPE
tmHeight             LONG
tmAscent             LONG
tmDescent            LONG
tmInternalLeading    LONG
tmExternalLeading    LONG
tmAveCharWidth       LONG
tmMaxCharWidth       LONG
tmWeight             LONG
tmOverhang           LONG
tmDigitizedAspectX   LONG
tmDigitizedAspectY   LONG
tmFirstChar          BYTE
tmLastChar           BYTE
tmDefaultChar        BYTE
tmBreakChar          BYTE
tmItalic             BYTE
tmUnderlined         BYTE
tmStruckOut          BYTE
tmPitchAndFamily     BYTE
tmCharSet            BYTE
                  END

kcr_PRINTER_DEFAULTS GROUP,TYPE
pDatatype               LONG
pDevMode                LONG
DesiredAccess           LONG
                     END

kcr_PRINTER_INFO_2   GROUP,TYPE
pServerName           ULONG
pPrinterName          ULONG
pShareName            ULONG
pPortName             ULONG
pDriverName           ULONG
pComment              ULONG
pLocation             ULONG
pDevMode              ULONG !LIKE(DEVMODE)
pSepFile              ULONG
pPrintProcessor       ULONG
pDatatype             ULONG
pParameters           ULONG
pSecurityDescriptor   ULONG
Attributes            LONG
Priority              LONG
DefaultPriority       LONG
StartTime             LONG
UntilTime             LONG
Status                LONG
cJobs                 LONG
AveragePPM            LONG
                    END

FindGrp     GROUP,TYPE
What          CSTRING(65)               !text to look for
Direction     STRING(4)                 !either 'Up  ' or 'Down'
MatchCase     BYTE                      !case sensitive?
WholeWord     BYTE                      !A match only occurs if the characters before and after are not word characters.
WordStart     BYTE                      !A match only occurs if the character before is not a word character.
RegExp        BYTE                      !The search string should be interpreted as a regular expression.
POSIX         BYTE                      !Treat regular expression in a more POSIX compatible manner by interpreting bare '
                                        !( and ) for tagged sections rather than \( and \).
bWordWrap     BYTE                      !
            END

STYLEGROUPTYPE  GROUP,TYPE
Font              CSTRING(65)
FontSize          LONG
FontStyle         LONG
Bold              BOOL
Italic            BOOL
Underline         BOOL
Fore              LONG
Back              LONG
EolFilled         BOOL
CaseOpt           LONG
Visible           BOOL
HotSpot           BOOL
                END

COLORGROUPTYPE  GROUP,TYPE
StyleGroup        LIKE(STYLEGROUPTYPE),DIM(SCE_CLW_LAST)
                END

!                ITEMIZE(1),PRE(CSciViewerMsg)
!OpenFailed              EQUATE
!BufferAllocationError   EQUATE
!SearchReachedBeginning  EQUATE
!SearchReachedEnd        EQUATE
!                END

CSciViewerMsg:OpenFailed              EQUATE(1)
CSciViewerMsg:BufferAllocationError   EQUATE(2)
CSciViewerMsg:SearchReachedBeginning  EQUATE(3)
CSciViewerMsg:SearchReachedEnd        EQUATE(4)
CSciViewerMsg:AccessDenied            EQUATE(5)

ClassErrors GROUP
Number        USHORT(5)
              USHORT(CSciViewerMsg:OpenFailed)
              BYTE(Level:Fatal)
              PSTRING('File Access Error')
              PSTRING('File %File could not be opened.  Error: %ErrorText.')
              USHORT(CSciViewerMsg:BufferAllocationError)
              BYTE(Level:Fatal)
              PSTRING('Buffer Allocation Error')
              PSTRING('A buffer of size [%Message] could not be allocated.')
              USHORT(CSciViewerMsg:SearchReachedBeginning)
              BYTE(Level:User)
              PSTRING('Beginning of File Error')
              PSTRING('The beginning of the %File file was encountered. Do you want to start again from the end of the file?')
              USHORT(CSciViewerMsg:SearchReachedEnd)
              BYTE(Level:User)
              PSTRING('End of File Error')
              PSTRING('The end of the file was encountered. Do you want to start again from the beginning?')
              USHORT(CSciViewerMsg:AccessDenied)
              BYTE(Level:User)
              PSTRING('Access Denied Error')
              PSTRING('File %File could not be opened.| |Do you want to save the file to a different location?')
            END

pagesetupMargin   LIKE(Sci_Rectangle)
hDevMode        HGLOBAL(0)
hDevNames       HGLOBAL(0)

!================================================================
!Scintilla Viewer Class Definition
!================================================================
CSciViewer          CLASS(CSciControl),TYPE,MODULE('CSciViewer.clw'),LINK('CSciViewer.clw',_SCILinkMode_),DLL(_SCIDllMode_)

! Protected Properties
szFileName            &CSTRING,PROTECTED                ! Name of file currently being viewed
szTextBuffer          &CSTRING,PROTECTED                ! text buffer for file - dynamically allocated

lTabWidth             LONG(4),PROTECTED                 ! Default tab width
bUseTabs              LONG(FALSE),PROTECTED             ! FALSE=Tabs are added as spaces
lIndent               LONG(4),PROTECTED                 ! Default indent width
bTabIndents           LONG(FALSE),PROTECTED
bBackSpaceUnindents   LONG(FALSE),PROTECTED
lWsMode               LONG(SCWS_INVISIBLE),PROTECTED    ! Invisible whitespace characters
bFoldMargin           LONG(TRUE),PROTECTED              ! Fold margin is initially visible

Popup                 &PopupClass,PROTECTED
ErrorMgr              &ErrorClass,PROTECTED
ErrorStatus           &ErrorStatusClass,PROTECTED
FindOptionsWindow     &Window,PROTECTED

Typeface              CSTRING(32)
Style                 GROUP(COLORGROUPTYPE),PRE(Style),PROTECTED
                      END

ClarionKeywords         &CSTRING,PROTECTED                ! Keyword Lists
CompilerDirectives      &CSTRING,PROTECTED                ! Keyword Lists
RuntimeExpressions      &CSTRING,PROTECTED                ! Keyword Lists
BuiltinProcsFuncs       &CSTRING,PROTECTED                ! Keyword Lists
StructDataTypes         &CSTRING,PROTECTED                ! Keyword Lists
Attributes              &CSTRING,PROTECTED                ! Keyword Lists
StandardEquates         &CSTRING,PROTECTED                ! Keyword Lists
ReservedWordsLabels     &CSTRING,PROTECTED                ! Keyword Lists !2003.10.25 KCR
ReservedWordsProcLabels &CSTRING,PROTECTED                ! Keyword Lists !2003.10.25 KCR

! CSciViewer Methods

Init                  PROCEDURE(*WINDOW W, LONG feq, UNSIGNED id, BOOL Themed = 0),BYTE,VIRTUAL   ! Initialise the class
OpenFile              PROCEDURE(*CSTRING szFileName),BOOL,PROC,VIRTUAL  !Open file and load buffer
ClearBuffer           PROCEDURE(),VIRTUAL                           ! Clear buffer
SetBuffer             PROCEDURE(),VIRTUAL                           ! Set buffer
SetLexerType          PROCEDURE(STRING szFileType),VIRTUAL          ! Set Scintilla Lexer
SetTextLexer          PROCEDURE(),VIRTUAL                           ! Set Scintilla Text Lexer
SetClarionLexer       PROCEDURE(),VIRTUAL                           ! Set Scintilla Clarion Lexer
SetDefaultMonoFont    PROCEDURE(),VIRTUAL                           ! Set Mono font
SetDefaultFont        PROCEDURE(*CSTRING szFontName, LONG lFontPoint),VIRTUAL   ! Set default font
SetDefaults           PROCEDURE(),VIRTUAL                           ! Set viewer defaults
AskGoToLine           PROCEDURE(),VIRTUAL                           ! GoToLine dialog
GotoLine              PROCEDURE(LONG lLine),VIRTUAL                 ! Override
TakeContextMenu       PROCEDURE(),BYTE,VIRTUAL                      ! Handle ContextMenu Event
SearchAsk             PROCEDURE(),VIRTUAL                           ! Search Dialog
SearchAsk             PROCEDURE(BOOL bShowWindow),VIRTUAL           ! Search Dialog
SearchAsk             PROCEDURE(BOOL bShowWindow,tagHTMLHelp HTMLHelp),VIRTUAL           ! Search Dialog
SearchNext            PROCEDURE(*FindGrp Find),LONG,VIRTUAL         ! Find Next/Previous
PrintAsk              PROCEDURE(),VIRTUAL                           ! Print the File
SetColors             PROCEDURE(*COLORGROUPTYPE color),VIRTUAL      ! Set Colors used by control
FoldMargin            PROCEDURE(),VIRTUAL                           ! Toggle Fold Margin on or off
FoldAll               PROCEDURE(),VIRTUAL                           ! Expand or Contract all folds
Expand                PROCEDURE(*LONG line, BOOL doExpand, BOOL force, LONG visLevels, LONG level)
GetFindGroup          PROCEDURE(),STRING,VIRTUAL                    ! Return FindGroup
SetFindGroup          PROCEDURE(*FindGrp pFindGroup),VIRTUAL        ! Set FindGroup
GetFindWhat           PROCEDURE(),STRING,VIRTUAL                    ! Return value in FindGroup.what
SetFindWhat           PROCEDURE(*CSTRING szWhat),VIRTUAL            ! Set the value in FindGroup.what
FindWindowTakeCloseWindow   PROCEDURE(),VIRTUAL                     ! Find Window CloseWindow Event Processing
FindWindowTakeOpenWindow    PROCEDURE(),VIRTUAL                     ! Find Window OpenWindow Event Processing
SetTypeface           PROCEDURE(*STRING sTypeface),VIRTUAL          ! Set Typeface for special windows
GetSelection          PROCEDURE(*Sci_CharacterRange crange),VIRTUAL
GetCurrentLineNumber  PROCEDURE(),LONG,VIRTUAL
BookmarkAdd           PROCEDURE(LONG lineno),VIRTUAL
BookmarkDelete        PROCEDURE(LONG lineno),VIRTUAL
BookmarkPresent       PROCEDURE(LONG lineno),LONG,VIRTUAL
BookmarkToggle        PROCEDURE(LONG lineno),VIRTUAL
BookmarkNext          PROCEDURE(BOOL forwardScan, BOOL select),VIRTUAL
OnPrint               PROCEDURE(BOOL bShowDialog),VIRTUAL



! WindowComponent Interface Callback Methods
Kill                  PROCEDURE(),VIRTUAL
Reset                 PROCEDURE(BYTE Force),VIRTUAL
ResetRequired         PROCEDURE(),BYTE,VIRTUAL
SetAlerts             PROCEDURE(),VIRTUAL
TakeEvent             PROCEDURE(),BYTE,VIRTUAL
Update                PROCEDURE(),VIRTUAL
UpdateWindow          PROCEDURE(),VIRTUAL
                    END

!_EndOfInclude_
