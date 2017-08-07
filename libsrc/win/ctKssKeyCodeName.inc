  !ABCIncludeFile(CTKEYCODENAME)
  
!region Notices
! ================================================================================
! Notice : Copyright (C) 2017, Devuna
!          Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of Devuna-KwikSourceSearch (https://github.com/Devuna/Devuna-KwikSourceSearch)
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
! ================================================================================
!endregion Notices  

  INCLUDE('ctQ_KeyCodes.inc'),ONCE

ctKssKeyCodeName  CLASS(ctQ_KeyCodes),TYPE,MODULE('ctKssKeyCodeName.clw'),LINK('ctKssKeyCodeName.clw') !,_CTQ_Name_LinkMode_),DLL(_CTQ_Name_DLLMode_)
MissingCode          PROCEDURE(LONG _ID),STRING,VIRTUAL
_Fill                PROCEDURE
                  END !Class
