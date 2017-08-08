 MEMBER()

 !Region Notices
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
!EndRegion Notices

 MAP
 END
 INCLUDE('ctQ_IDName.inc'),ONCE
 INCLUDE('Errors.clw'),ONCE


!-------------------------------------------------------
ctQ_IDName.Construct     PROCEDURE
  CODE
  SELF.Q           &= NEW qtIDName; CLEAR(SELF.Q)
  SELF.BaseQ       &= SELF.Q 

  SELF.MissingDescr = 'Unknown ID'


!-------------------------------------------------------
ctQ_IDName.Del           PROCEDURE()!,DERIVED
   CODE
   DISPOSE(SELF.Q.Name)
   PARENT.DEL() 




!-------------------------------------------------------
ctQ_IDName.Add          PROCEDURE(LONG ID, STRING xName)
  CODE
  SELF.Q.ID           = ID
  SELF.Q.Name &= NEW STRING(SIZE(xName))
  SELF.Q.Name  =                 xName
  ADD(SELF.Q, SELF.Q.ID)


!-------------------------------------------------------
ctQ_IDName.AddReplace   PROCEDURE(LONG ID, STRING xName)
  CODE
  IF SELF.Find(ID) <> NoError
     SELF.Add(ID, xName)
  ELSE
     DISPOSE(SELF.Q.Name) ! do not use SELF.DEL here, as we are not deleting the row
     SELF.Q.Name &= NEW STRING(SIZE(xName))
     SELF.Q.Name =                  xName
     PUT(SELF.Q)
  END

!-------------------------------------------------------
ctQ_IDName.Find         PROCEDURE(LONG ID) !,Long
  CODE
  SELF.Q.ID   = ID
  GET(SELF.Q,SELF.Q.ID)
  RETURN ERRORCODE()


!-------------------------------------------------------
ctQ_IDName.ToName      PROCEDURE(LONG ID) !,string
  CODE
  IF SELF.Find(ID) <> NoError
     RETURN SELF.MissingCode(ID)
  ELSE
     RETURN SELF.Q.Name
  END


!-------------------------------------------------------
ctQ_IDName.MissingCode   PROCEDURE(LONG ID) !string
  CODE
  RETURN SELF.MissingDescr &'['& ID &']'



