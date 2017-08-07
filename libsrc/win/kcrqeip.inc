! ================================================================================
! Notice : Copyright (C) 2017, Devuna
!          Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of Devuna-Queue-Edit-In-Place (https://github.com/Devuna/Devuna-Queue-Edit-In-Place)
!
!    Devuna-Queue-Edit-In-Place is free software: you can redistribute it and/or modify
!    it under the terms of the MIT License as published by
!    the Open Source Initiative.
!
!    Devuna-Queue-Edit-In-Place is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    MIT License for more details.
!
!    You should have received a copy of the MIT License
!    along with Devuna-Queue-Edit-In-Place.  If not, see <https://opensource.org/licenses/MIT>.
! ================================================================================

OMIT('_EndOfInclude_',_QueueEIPPresent_)
_QueueEIPPresent_        EQUATE(1)
QEIP:MouseRightIndex     EQUATE(249)
QEIP:MouseLeft2Index     EQUATE(252)
  ITEMIZE(Msg:QBEColumnNotSupported+1),PRE(Msg)
QEIPRestrictDelete       EQUATE
  END
QEIP:Errors GROUP,PRE()
              USHORT(1)
              USHORT(Msg:QEIPRestrictDelete)
              BYTE(Level:Notify)
              PSTRING('Referential Integrity Delete Error')
              PSTRING('This record is referenced by the %Message. This record cannot be deleted while these references exist.')
            END
  _EndOfInclude_
