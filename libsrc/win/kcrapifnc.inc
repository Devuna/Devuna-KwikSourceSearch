! ================================================================================
! Notice : Copyright (C) 2017, Devuna
!          Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of Devuna-Common (https://github.com/Devuna/Devuna-Common)
!
!    Devuna-Common is free software: you can redistribute it and/or modify
!    it under the terms of the MIT License as published by
!    the Open Source Initiative.
!
!    Devuna-Common is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    MIT License for more details.
!
!    You should have received a copy of the MIT License
!    along with Devuna-Common.  If not, see <https://opensource.org/licenses/MIT>.
! ================================================================================

!Include Inside Global Map
 MODULE('kernel32')
    kcr_CallWindowProc(LONG lpPrevWndProc, HWND hwnd, UNSIGNED nMsg, UNSIGNED wParam, LONG lParam),LONG,PASCAL,NAME('CallWindowProcA')
    kcr_ChooseColor(*? cc),BOOL,PASCAL,RAW,PROC,NAME('ChooseColorA')
    kcr_ClientToScreen(HWND hwnd, *POINT pt),BOOL,PASCAL,RAW,PROC,NAME('ClientToScreen')
    kcr_CloseHandle(HANDLE hObject),BOOL,PASCAL,PROC,NAME('CloseHandle')
    kcr_ClosePrinter(HANDLE hPrinter),BOOL,PASCAL,PROC,NAME('ClosePrinter')
    kcr_CopyFile(*CSTRING szFileName, *cstring szNewFileName, bool bFailIfExists),BOOL,RAW,PASCAL,NAME('CopyFileA')
    kcr_CopyRect(*RECT rectDST, *RECT rectSRC),PASCAL,RAW,NAME('CopyRect')
    kcr_crc32(LONG lpBuffer, LONG nBufferLen, LONG CRC),LONG,PROC,NAME('Cla$crc32')
    kcr_CreateBrushIndirect(*LOGBRUSH),UNSIGNED,PASCAL,RAW,NAME('CreateBrushIndirect')
    kcr_CreateDC(*CSTRING szDriver, *CSTRING szDevice, LONG lpszOutput, LONG lpInitData),HDC,PASCAL,RAW,NAME('CreateDCA')
    kcr_CreateEvent(LONG lpSecurityAttribs, BOOL bManualReset,BOOL bInitialState, *CSTRING lpszEventName),HANDLE,PASCAL,RAW,PROC,NAME('CreateEventA')
    kcr_CreateEvent(LONG lpSecurityAttribs, BOOL bManualReset,BOOL bInitialState, LONG lpszEventName),HANDLE,PASCAL,RAW,PROC,NAME('CreateEventA')
    kcr_CreateFile(*CSTRING szFileName, LONG dwDesiredAccess, LONG dwShareMode, LONG lpSecurityAttributes, LONG dwCreationDisposition, LONG dwFlagsAndAttributes, HANDLE hTemplateFile),HANDLE,NAME('CreateFileA'),PASCAL,RAW
    kcr_CreateIconFromResourceEx(LONG pbIconBits, DWORD cbIconBits, BOOL fIcon, DWORD dwVersion, LONG cxDesired, LONG cyDesired, ULONG uFlags),HANDLE,PASCAL,NAME('CreateIconFromResourceEx')
    kcr_CreateMutex(*kcr_SECURITY_ATTRIBUTES lpSecurityAttribs, BOOL bInitialOwner, *CSTRING lpszMutexName),HANDLE,PASCAL,RAW,PROC,NAME('CreateMutexA')
    kcr_CreateMutex(LONG lpSecurityAttribs, BOOL bInitialOwner, *CSTRING lpszMutexName),HANDLE,PASCAL,RAW,PROC,NAME('CreateMutexA')
    kcr_CreatePipe(*HANDLE hReadPipe, *HANDLE hWritePipe, *SECURITY_ATTRIBUTES PipeAttributes, DWORD nSize),BOOL,PASCAL,RAW,PROC,NAME('CreatePipe')
    kcr_CreateProcess(LPCTSTR lpApplicationName, *CSTRING lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment,  LPCTSTR lpCurrentDirectory, *STARTUPINFOTYPE lpStartupInfo, *PROCESS_INFORMATION lpProcessInformation),BOOL,PASCAL,RAW,PROC,NAME('CreateProcessA')
    kcr_CreateRectRgn(LONG nLeft, LONG nTop, LONG nRight, LONG nBottom),LONG,PASCAL,NAME('CreateRectRgn')
    kcr_CreateSolidBrush(COLORREF),HBRUSH,PASCAL,NAME('CreateSolidBrush')
    kcr_DeleteDC(HDC hdc),BOOL,PASCAL,PROC,NAME('DeleteDC')
    kcr_DeleteFile(*CSTRING szFileName),BOOL,NAME('DeleteFileA'),RAW,PASCAL
    kcr_DeleteObject(UNSIGNED hGDIOBJ),BOOL,PASCAL,PROC,NAME('DeleteObject')
    kcr_DestroyIcon(HICON hIcon),BOOL,PASCAL,NAME('DestroyIcon')
    kcr_DeviceCapabilities(*CSTRING pDevice, *CSTRING pPort, WORD fwCapability, LONG pOutput, *tagDEVMODE pDevMode),DWORD,PASCAL,RAW,PROC,NAME('DeviceCapabilitiesA')
    kcr_DoDragDrop(LONG pDataObj, LONG pDropSource, LONG dwOKEffects, *DWORD dwEffect),LONG,PASCAL,RAW,NAME('DoDragDrop')
    kcr_DragDetect(HWND hWnd, LONG xPos, LONG yPos),BOOL,PASCAL,RAW,NAME('DragDetect')
    kcr_DragQueryFile(HDROP hDrop, UNSIGNED iFile, *CSTRING lpszFile,UNSIGNED cch),UNSIGNED,PASCAL,RAW,PROC,NAME('DragQueryFile')
    kcr_DrawIcon(HDC hDC, LONG x, LONG y, HANDLE hIcon),BOOL,PASCAL,PROC,NAME('DrawIcon')
    kcr_DrawMenuBar(HWND),BOOL,PASCAL,PROC,NAME('DrawMenuBar')
    kcr_DuplicateHandle(HANDLE hSourceProcessHandle, HANDLE hSourceHandle, HANDLE hTargetProcessHandle, *HANDLE lpTargetHandle, DWORD dwDesiredAccess, BOOL bInheritHandle, DWORD dwOptions),BOOL,PASCAL,RAW,PROC,NAME('DuplicateHandle')
    kcr_ExtFloodFill(UNSIGNED hwnd, SIGNED x, SIGNED y, ULONG color, UNSIGNED),BOOL,PASCAL,PROC,NAME('ExtFloodFill')
    kcr_FindResource(HMODULE hModule, LONG lpName,  LONG lpType),HANDLE,PASCAL,NAME('FindResourceA')
    kcr_FindWindowEx(HWND hwndParent, HWND hwndChildAfter, LONG lpszClass, *CSTRING szWindowCaption),HWND,PASCAL,RAW,NAME('FindWindowExA')
    kcr_FlashWindow(HWND hWnd, BOOL bInvert),BOOL,PASCAL,PROC,NAME('FlashWindow')
    kcr_FlashWindowEx(*FLASHWINFO fwi),BOOL,PASCAL,RAW,PROC,NAME('FlashWindowEx')
    kcr_FnSplit(*CSTRING szPath, *CSTRING szDrive, *CSTRING szDir, *CSTRING szFile, *CSTRING szExtension),SIGNED,RAW,NAME('_fnsplit')
    kcr_FormatMessage(LONG dwFlags, LONG lpSource, LONG dwMessageId, LONG dwLanguageId, *CSTRING lpBuffer, LONG nSize, LONG pArguments),LONG,RAW,PASCAL,NAME('FormatMessageA')
    kcr_FreeLibrary(HINSTANCE hInst),BOOL,PASCAL,PROC,NAME('FreeLibrary')
    kcr_GetCapture(),HWND,PASCAL,NAME('GetCapture')
    kcr_GetClientRect(UNSIGNED,*RECT),RAW,PASCAL,PROC,NAME('GetClientRect')
    kcr_GetCurrentProcess(),HANDLE,PASCAL,NAME('GetCurrentProcess')
    kcr_GetCurrentProcessId(),LONG,PASCAL,NAME('GetCurrentProcessId')
    kcr_GetCurrentThread(),LONG,PASCAL,NAME('GetCurrentThread')
    kcr_GetCurrentThreadId(),LONG,PASCAL,NAME('GetCurrentThreadId')
    kcr_GetCursorPos(*POINT lpPoint),BOOL,RAW,PASCAL,PROC,NAME('GetCursorPos')
    kcr_GetDC(HWND hWnd),UNSIGNED,PASCAL,NAME('GetDC')
    kcr_GetDefaultPrinter(*CSTRING szBuffer,*DWORD cchBuffer),BOOL,RAW,PASCAL,PROC,NAME('GetDefaultPrinterA')
    kcr_GetDesktopWindow(),HWND,PASCAL,NAME('GetDesktopWindow')
    kcr_GetDeviceCaps(HDC hdc, LONG nIndex),LONG,PASCAL,NAME('GetDeviceCaps')
    kcr_GetExitCodeProcess(HANDLE hProcess, *DWORD lpExitCode),BOOL,RAW,PASCAL,PROC,NAME('GetExitCodeProcess')
    kcr_GetFileSize(HANDLE hFile, *LONG FileSizeHigh),LONG,RAW,PASCAL,NAME('GetFileSize')
    kcr_GetFileTime(HANDLE hFile, LONG lpCreationTime, LONG lpLastAccessTime, LONG lpLastWriteTime),BOOL,RAW,PASCAL,PROC,NAME('GetFileTime')
    kcr_GetLastError(),LONG,PASCAL,NAME('GetLastError')
    kcr_GetMenu(UNSIGNED),UNSIGNED,RAW,PASCAL,NAME('GetMenu')
    kcr_GetMenuInfo(UNSIGNED hMenu, *MENUINFO lpmi),BOOL,RAW,PASCAL,PROC,NAME('GetMenuInfo')
    kcr_GetOverlappedResult(HANDLE hFile, LPOVERLAPPED lpOverlapped, LPDWORD lpNumberOfBytesTransferred, BOOL bWait),BOOL,PASCAL,RAW,PROC,NAME('GetOverlappedResult')
    kcr_GetParent(HWND),HWND,PASCAL,NAME('GetParent')
    kcr_GetPixel(UNSIGNED hdc, SIGNED x, SIGNED y),ULONG,PASCAL,NAME('GetPixel')
    kcr_GetPrinter(HANDLE hPrinter, DWORD Level, LONG lpPrinterInfo, DWORD cbBuf, *DWORD pcbNeeded),BOOL,RAW,PASCAL,PROC,NAME('GetPrinterA')
    kcr_GetProcAddress(UNSIGNED hModule,*CSTRING szProcName),LONG,PASCAL,RAW,NAME('GetProcAddress')
    kcr_GetProp(HWND hwnd, *CSTRING szPropertyName),HANDLE,RAW,PASCAL,NAME('GetPropA')
    kcr_GetProp(HWND hwnd, STRING szPropertyName),HANDLE,RAW,PASCAL,NAME('GetPropA')
    kcr_GetStdHandle(DWORD nStdHandle),HANDLE,RAW,PASCAL,NAME('GetStdHandle')    
    kcr_GetSysColor(LONG nIndex),LONG,PASCAL,NAME('GetSysColor')
    kcr_GetSystemMetrics(SIGNED),SIGNED,PASCAL,NAME('GetSystemMetrics')
    kcr_GetTempFileName(*CSTRING szPathName, *CSTRING szPrefixString, SHORT uUnique, *CSTRING szTempFileName),SHORT,PASCAL,RAW,PROC,NAME('GetTempFileNameA')
    kcr_GetTempPath(LONG nBufferLength, *CSTRING szBuffer),LONG,PASCAL,RAW,PROC,NAME('GetTempPathA')    
    kcr_GetUpdateRgn(HWND hWnd, HANDLE hRegion, BOOL bErase),LONG,PASCAL,NAME('GetUpdateRgn')
    kcr_GetVersionEx(*OSVERSIONINFO osvi),BOOL,PASCAL,RAW,NAME('GetVersionExA'),PROC
    kcr_GetWindowDC(UNSIGNED hwnd),UNSIGNED,PASCAL,NAME('GetWindowDC')
    kcr_GetWindowLong(USHORT,SHORT),LONG,RAW,PASCAL,NAME('GetWindowLongA')
    kcr_GetWindowRect(USHORT,*RECT),RAW,PASCAL,PROC,NAME('GetWindowRect')
    kcr_GetWindowThreadProcessId(HWND hWnd, *DWORD dwProcessId),DWORD,PASCAL,RAW,PROC,NAME('GetWindowThreadProcessId')
    kcr_GlobalAlloc(LONG uFlags, LONG dwBytes),LONG,PASCAL,NAME('GlobalAlloc')
    kcr_GlobalFree(HGLOBAL hMem),HGLOBAL,PASCAL,PROC,NAME('GlobalFree')
    kcr_GlobalLock(HGLOBAL hMem),LONG,PASCAL,PROC,NAME('GlobalLock')
    kcr_GlobalUnlock(HGLOBAL hMem),BOOL,PASCAL,PROC,NAME('GlobalUnlock')
    kcr_InvalidateRect(HWND hWnd,*RECT lpRect ,BOOL bErase),BOOL,RAW,PASCAL,PROC,NAME('InvalidateRect')
    kcr_LoadLibraryEx(*CSTRING szLibraryName, HANDLE hReserved, DWORD dwFlags),HANDLE,PASCAL,RAW,NAME('LoadLibraryExA')
    kcr_LoadResource(HMODULE hModule, HRSRC hResInfo),LONG,PASCAL,NAME('LoadResource')
    kcr_LockResource(HGLOBAL hResData),LONG,PASCAL,NAME('LockResource')
    kcr_LookupIconIdFromDirectoryEx(LONG presbits, BOOL bIcon, LONG cxDesired, LONG cyDesired, ULONG uFlags),LONG,PASCAL,NAME('LookupIconIdFromDirectoryEx')
    kcr_ltoa(LONG num,*CSTRING s,SIGNED radix),ULONG,RAW,NAME('_ltoa'),PROC
    kcr_MemCpy(ULONG pDest, ULONG pSrc, UNSIGNED nCount),name('_memcpy')
    kcr_MemSet(ULONG pDest, LONG ch, UNSIGNED nBytes),name('_memset')
    kcr_MoveWindow(HWND,SIGNED,SIGNED,SIGNED,SIGNED,BOOL),PASCAL,NAME('MoveWindow')
    kcr_OffsetRect(*RECT lpRC, LONG dx, LONG dy),BYTE,RAW,PASCAL,NAME('OffsetRect'),PROC
    kcr_OleFlushClipboard(),HRESULT,RAW,PASCAL,PROC,NAME('OleFlushClipboard')
    kcr_OleGetClipboard(*IDataObject ppDataObject),HRESULT,RAW,PASCAL,PROC,NAME('OleGetClipboard')
    kcr_OleInitialize(LONG pvReserved),LONG,PASCAL,PROC,NAME('OleInitialize')
    kcr_OleSetClipboard(*IDataObject ppDataObject),HRESULT,RAW,PASCAL,PROC,NAME('OleSetClipboard')
    kcr_OleSetClipboard(LONG ppDataObject),HRESULT,RAW,PASCAL,PROC,NAME('OleSetClipboard')
    kcr_OleUninitialize(),PASCAL,NAME('OleUninitialize')
    kcr_OpenPrinter(LONG lpPrinterName, *HANDLE hPrinter, LONG lpDefault),BOOL,RAW,PASCAL,PROC,NAME('OpenPrinterA')
    kcr_OpenPrinter(*CSTRING PrinterName, *HANDLE hPrinter, LONG lpDefault),BOOL,RAW,PASCAL,PROC,NAME('OpenPrinterA')
    kcr_OpenProcess(DWORD dwDesiredAccess, BOOL bInheritHandle, DWORD dwProcessId),HANDLE,RAW,PASCAL,PROC,NAME('OpenProcess')
    kcr_PeekNamedPipe(HANDLE hNamedPipe, LONG lpBuffer, DWORD nBufferSize, LONG lpBytesRead, LONG lpTotalBytesAvail, LONG lpBytesLeftThisMessage),BOOL,RAW,PASCAL,NAME('PeekNamedPipe')
    kcr_PostMessage(HWND hwnd, UNSIGNED wmag, UNSIGNED wparam, LONG lparam),BOOL,PASCAL,PROC,NAME('PostMessageA')
    kcr_QueryDosDevice(*CSTRING szDeviceName,*STRING strTargetPath,DWORD ucchMax),DWORD,PASCAL,PROC,NAME('QueryDosDeviceA')
    kcr_QueryDosDevice(LONG lpszDeviceName,*STRING strTargetPath,DWORD ucchMax),DWORD,PASCAL,PROC,NAME('QueryDosDeviceA')
    kcr_ReadFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, LPOVERLAPPED lpOverlapped),BOOL,PASCAL,RAW,NAME('ReadFile')
    kcr_ReadFileEx(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, LPOVERLAPPED lpOverlapped, LONG lpCompletionRoutine),BOOL,PASCAL,RAW,NAME('ReadFileEx')
    kcr_RedrawWindow(HWND hWnd, *RECT lpRect, HANDLE hRegion, ULONG uFlags),BOOL,PASCAL,RAW,PROC,NAME('RedrawWindow')
    kcr_RegCloseKey(UNSIGNED hKey),LONG,RAW,PASCAL,NAME('RegCloseKey'),PROC
    kcr_RegCreateKeyEx(UNSIGNED hKey,*CSTRING szSubKey,ULONG reserved,ULONG lpszClass,ULONG dwOptions,ULONG samDesired,ULONG lpSecurityAttributes,*ULONG hkResult,ULONG lpdwDisposition),LONG,RAW,PASCAL,NAME('RegCreateKeyExA')
    kcr_RegisterWindowMessage(*CSTRING lpString),LONG,RAW,PASCAL,NAME('RegisterWindowMessageA')
    kcr_RegEnumKeyEx(UNSIGNED hKey,ULONG dwIndex,*CSTRING lpName,*ULONG lpcName,ULONG lpReserved,ULONG lpClass,ULONG lpcClass,ULONG lpftLastWriteTime),LONG,RAW,PASCAL,NAME('RegEnumKeyExA'),PROC
    kcr_RegEnumValue(UNSIGNED hKey,ULONG dwIndex,*CSTRING valueName,*ULONG cchValueName, ULONG lpReserved, ULONG lpType, ULONG lpData, ULONG lpcbData),LONG,RAW,PASCAL,NAME('RegEnumValueA'),PROC
    kcr_RegOpenKeyEx(UNSIGNED hKey,*CSTRING szSubKey,ULONG ulOptions,ULONG samDesired,*ULONG hkResult),LONG,RAW,PASCAL,NAME('RegOpenKeyExA'),PROC
    kcr_RegQueryValueEx(UNSIGNED hKey,*CSTRING szValueName,ULONG reserved,*ULONG lType,ULONG lpData,*ULONG cbData),LONG,RAW,PASCAL,NAME('RegQueryValueExA'),PROC    
    kcr_RegSetValueEx(UNSIGNED hKey,*CSTRING szValueName,ULONG reserved,ULONG dwType,LONG lpData,ULONG cbData),LONG,RAW,PASCAL,NAME('RegSetValueExA'),PROC
    kcr_ReleaseCapture(),PASCAL,NAME('ReleaseCapture')
    kcr_ReleaseDC(HWND hWnd, HDC hDC),UNSIGNED,PASCAL,PROC,NAME('ReleaseDC')
    kcr_ReleaseStgMedium(*STGMEDIUM stgmed),RAW,PASCAL,NAME('ReleaseStgMedium')
    kcr_Remove(*CSTRING),SIGNED,RAW,PROC,NAME('_remove')
    kcr_RemoveProp(HWND hwnd, *CSTRING szPropertyName),HANDLE,RAW,PASCAL,PROC,NAME('RemovePropA')
    kcr_ReplaceFile(*CSTRING szFileName, *CSTRING szReplacementFileName, LONG lpBackupFileName, LONG dwReplaceFlags, LONG lpExclude = 0, LONG lpReserved = 0),BOOL,RAW,PASCAL,NAME('ReplaceFileA')
    kcr_ScreenToClient(HWND, *POINT),PASCAL,RAW,NAME('ScreenToClient')
    kcr_SelectObject(UNSIGNED hdc, UNSIGNED hGDIOBJ),UNSIGNED,PASCAL,PROC,NAME('SelectObject')
    kcr_SendMessage(UNSIGNED,UNSIGNED,UNSIGNED,LONG),LONG,PASCAL,NAME('SendMessageA'),PROC
    kcr_SetCapture(HWND hwnd),HWND,PASCAL,PROC,NAME('SetCapture')
    kcr_SetClassLong(HWND,SHORT,LONG),DWORD,RAW,PASCAL,NAME('SetClassLongA'),PROC
    kcr_SetCurrentDirectory(*CSTRING szCurDir),BOOL,RAW,PASCAL,NAME('SetCurrentDirectoryA'),PROC
    kcr_SetEndOfFile(HANDLE hFile),BOOL,PASCAL,PROC,NAME('SetEndOfFile')
    kcr_SetFilePointer(HANDLE hFile, LONG lDistanceToMove, *LONG lDistanceToMoveHigh, LONG dwMoveMethod),LONG,RAW,PASCAL,PROC,NAME('SetFilePointer')
    kcr_SetFocus(HWND hwnd),HWND,PASCAL,PROC,NAME('SetFocus')
    kcr_SetForegroundWindow(HWND hWnd),BOOL,PROC,PASCAL,NAME('SetForegroundWindow')
    kcr_SetHandleInformation(HANDLE hObject, DWORD dwMask, DWORD dwFlags),BOOL,RAW,PASCAL,NAME('SetHandleInformation'),PROC
    kcr_SetMenuInfo(UNSIGNED hMenu, *MENUINFO lpmi),BOOL,RAW,PASCAL,PROC,NAME('SetMenuInfo')
    kcr_SetProp(HWND hwnd, *CSTRING szPropertyName, HANDLE hData),BOOL,RAW,PASCAL,PROC,NAME('SetPropA')
    kcr_SetProp(HWND hwnd, STRING szPropertyName, HANDLE hData),BOOL,RAW,PASCAL,PROC,NAME('SetPropA')
    kcr_SetWindowLong(HWND hwnd,SHORT,LONG),LONG,RAW,PASCAL,NAME('SetWindowLongA'),PROC
    kcr_SetWindowPos(HWND hWnd, HWND hWndInsertAfter, UNSIGNED x, UNSIGNED y, UNSIGNED cx, UNSIGNED cy, UNSIGNED uFlags),BOOL,PASCAL,PROC,NAME('SetWindowPos')
    kcr_SetWindowText(HWND hwnd, *CSTRING lpsz),BOOL,PROC,RAW,PASCAL,NAME('SetWindowTextA')
    kcr_ShellExecute(HWND hwnd, *CSTRING lpOperation, *CSTRING lpFile, *CSTRING lpParameters, *CSTRING lpDirectory, SIGNED nShowCmd),UNSIGNED,PASCAL,RAW,NAME('ShellExecuteA'),PROC
    kcr_ShellExecute(HWND hwnd, LONG lpOperation, *CSTRING lpFile, LONG lpParameters, *CSTRING lpDirectory, SIGNED nShowCmd),UNSIGNED,PASCAL,RAW,NAME('ShellExecuteA'),PROC
    kcr_SHGetFolderPath(HWND hwnd, LONG csidl, HANDLE hToken, DWORD dwFlags, *CSTRING szPath),HRESULT,RAW,PASCAL,NAME('SHGetFolderPathA')
    kcr_ShowWindow(HWND hWnd, LONG nCmdShow),BOOL,PASCAL,PROC,NAME('ShowWindow')
    kcr_ShowWindowAsync(HWND hWnd, LONG nCmdShow),BOOL,PASCAL,PROC,NAME('ShowWindowAsync')
    kcr_SizeofResource(HMODULE hModule, HRSRC hResInfo),LONG,PASCAL,NAME('SizeofResource')
    kcr_SystemParametersInfo(UNSIGNED uAction, UNSIGNED uParam, LONG pvParam, UNSIGNED fUpdateProfile),BOOL,PROC,PASCAL,RAW,NAME('SystemParametersInfoA')
    kcr_TerminateProcess(HANDLE hProcess, DWORD uExitCode),BOOL,PROC,PASCAL,NAME('TerminateProcess')
    kcr_TerminateThread(HANDLE hThread, DWORD dwExitCode),BOOL,PROC,PASCAL,NAME('TerminateThread')
    kcr_UpdateWindow(HWND hWnd),BOOL,PASCAL,PROC,NAME('UpdateWindow')
    kcr_WaitForSingleObject(HANDLE hHandle, DWORD dwMilliseconds),LONG,PROC,PASCAL,RAW,NAME('WaitForSingleObject')
    kcr_WindowFromPoint(POINT pt),HWND,RAW,PASCAL,NAME('WindowFromPoint')
    kcr_WriteFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten, LPOVERLAPPED lpOverlapped),BOOL,PASCAL,RAW,NAME('WriteFile')    
    kcr_ZeroMemory(LONG DestinationPtr,LONG dwLength),RAW,PASCAL,NAME('RtlZeroMemory')
 END
 MODULE('clib')
    kcr_StrCpy(LONG pDestination, LONG pSource),LONG,RAW,PROC,NAME('_strcpy')
 END