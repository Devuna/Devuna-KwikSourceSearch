  !ABCIncludeFile(CTKEYCODENAME)

!Region Notices and Notes
! ================================================================================
! Notice : Copyright (C) 2005-2017, Monolith Custom Computing, Inc.
!          Distributed under MIT (https://opensource.org/licenses/MIT) 
! 
!    This file is part of Monolith-Common (https://github.com/MarkGoldberg/MonolithCC-Common) 
! 
!    MonolithCC-Common is free software: you can redistribute it and/or modify 
!    it under the terms of the MIT License as published by 
!    the Open Source Initiative. 
! 
!    MonolithCC-Common is distributed in the hope that it will be useful, 
!    but WITHOUT ANY WARRANTY; without even the implied warranty of 
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
!    MIT License for more details. 
! 
!    You should have received a copy of the MIT License 
!    along with MonolithCC-Common.  If not, see <https://opensource.org/licenses/MIT>. 
! ================================================================================
!
!  Description  : Class to describe KEYCODE()s 
!  Created      : Jan 20, 2005
!  Last Updated : July 18, 2017
!
!  Usage:
!      INCLUDE('ctQ_KeyCodes.inc'),ONCE
!   KeyCodes    ctQ_KeyCodes
!      CODE  
!      ...
!      ACCEPT
!        ...
!        MESSAGE('You Pressed ' & KeyCodes.ToName()) ) 
!        ...
!      END 
! ================================================================================
!EndRegion Notices and Notes

          INCLUDE('ctQ_IDName.inc'),ONCE
ctQ_KeyCodes CLASS(ctQ_IDName),TYPE,MODULE('ctQ_KeyCodes.clw'),LINK('ctQ_KeyCodes.clw') !,_CTKEYCODENAMELinkMode_),DLL(_CTKEYCODENAMEDLLMode_)
Construct       PROCEDURE
ToName          PROCEDURE(),STRING                   !Overload, which defaults to .GetName( KEYCODE() )
MissingCode     PROCEDURE(LONG ID),STRING,VIRTUAL
FillQ           PROCEDURE
FormatModifier  PROCEDURE(STRING modifier),STRING,VIRTUAL
             END 
