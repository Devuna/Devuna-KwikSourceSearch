  !ABCIncludeFile(ctQ_IDDescr)


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
!  Description  : Generic Class for Pairing an ID/Description
!  Description : Generic Class for Pairing an ID/Name
!  Created     : July 18, 2017 - rewritten from ctQName, as a ctQueue 
!  Last Updated : July 18, 2017
!
! ================================================================================
!EndRegion Notices and Notes


  INCLUDE('ctQueue.inc'),ONCE

gtIDName    GROUP,TYPE
ID             LONG
Name           &STRING
            END 

qtIDName    QUEUE(gtIDName),TYPE
            END

ctQ_IDName  CLASS(ctQueue),TYPE,MODULE('ctQ_IDName.clw'),LINK('ctQ_IDName.clw') !,_CTQ_Name_LinkMode_),DLL(_CTQ_Name_DLLMode_)
              !-----------------------------------------
Q            &qtIDName
MissingDescr  CSTRING(100)  !arbitrary length limitation
              !-----------------------------------------
Construct     PROCEDURE
Del           PROCEDURE,DERIVED

Add           PROCEDURE(LONG ID, STRING Name)
AddReplace    PROCEDURE(LONG ID, STRING Name)
Find          PROCEDURE(LONG ID),LONG            !returns ERRORCODE()
ToName        PROCEDURE(LONG ID),STRING
MissingCode   PROCEDURE(LONG ID),STRING,VIRTUAL
         END  

