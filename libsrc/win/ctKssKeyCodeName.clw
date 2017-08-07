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

 MEMBER()
 MAP
 END
 INCLUDE('ctKssKeyCodeName.inc'),ONCE
 INCLUDE('keycodes.clw'),ONCE
NoError EQUATE(0)

!--------------------------------------------------
ctKssKeyCodeName.MissingCode  PROCEDURE(LONG _ID) !,string,virtual
SHIFT EQUATE(100H)
CTRL  EQUATE(200H)
ALT   EQUATE(400H)

  CODE
  IF _ID > SHIFT
     IF SELF.Find(_ID - BAND(_ID,CTRL + ALT)) = NoError    !There are some entries for SHIFT-ED _IDs
        RETURN CHOOSE(BAND(_ID,CTRL )=0,'','Ctrl')  & |
               CHOOSE(BAND(_ID,ALT  )=0,'','Alt')   & |
               SELF.Q.Name

     ELSIF SELF.Find(_ID - BAND(_ID,SHIFT + CTRL + ALT)) = NoError
        RETURN CHOOSE(BAND(_ID,CTRL )=0,'','Ctrl')  & |
               CHOOSE(BAND(_ID,ALT  )=0,'','Alt')   & |
               CHOOSE(BAND(_ID,SHIFT)=0,'','Shift') & |
               SELF.Q.Name
     END
  END
  RETURN PARENT.MissingCode(_ID)   
   
!--------------------------------------------------
ctKssKeyCodeName._Fill      PROCEDURE

  CODE
   SELF.AddReplace(Key0,'Key0')
   SELF.AddReplace(Key1,'Key1')
   SELF.AddReplace(Key2,'Key2')
   SELF.AddReplace(Key3,'Key3')
   SELF.AddReplace(Key4,'Key4')
   SELF.AddReplace(Key5,'Key5')
   SELF.AddReplace(Key6,'Key6')
   SELF.AddReplace(Key7,'Key7')
   SELF.AddReplace(Key8,'Key8')
   SELF.AddReplace(Key9,'Key9')
   SELF.AddReplace(AKey,'AKey')
   SELF.AddReplace(BKey,'BKey')
   SELF.AddReplace(CKey,'CKey')
   SELF.AddReplace(DKey,'DKey')
   SELF.AddReplace(EKey,'EKey')
   SELF.AddReplace(FKey,'FKey')
   SELF.AddReplace(GKey,'GKey')
   SELF.AddReplace(HKey,'HKey')
   SELF.AddReplace(IKey,'IKey')
   SELF.AddReplace(JKey,'JKey')
   SELF.AddReplace(KKey,'KKey')
   SELF.AddReplace(LKey,'LKey')
   SELF.AddReplace(MKey,'MKey')
   SELF.AddReplace(NKey,'NKey')
   SELF.AddReplace(OKey,'OKey')
   SELF.AddReplace(PKey,'PKey')
   SELF.AddReplace(QKey,'QKey')
   SELF.AddReplace(RKey,'RKey')
   SELF.AddReplace(SKey,'SKey')
   SELF.AddReplace(TKey,'TKey')
   SELF.AddReplace(UKey,'UKey')
   SELF.AddReplace(VKey,'VKey')
   SELF.AddReplace(WKey,'WKey')
   SELF.AddReplace(XKey,'XKey')
   SELF.AddReplace(YKey,'YKey')
   SELF.AddReplace(ZKey,'ZKey')
   SELF.AddReplace(F1Key,'F1Key')
   SELF.AddReplace(F2Key,'F2Key')
   SELF.AddReplace(F3Key,'F3Key')
   SELF.AddReplace(F4Key,'F4Key')
   SELF.AddReplace(F5Key,'F5Key')
   SELF.AddReplace(F6Key,'F6Key')
   SELF.AddReplace(F7Key,'F7Key')
   SELF.AddReplace(F8Key,'F8Key')
   SELF.AddReplace(F9Key,'F9Key')
   SELF.AddReplace(F10Key,'F10Key')
   SELF.AddReplace(F11Key,'F11Key')
   SELF.AddReplace(F12Key,'F12Key')
   SELF.AddReplace(AstKey,'AstKey')
   SELF.AddReplace(BSKey,'BSKey')
   SELF.AddReplace(CapsLockKey,'CapsLockKey')
   SELF.AddReplace(DecimalKey,'DecimalKey')
   SELF.AddReplace(DeleteKey,'DeleteKey')
   SELF.AddReplace(DownKey,'DownKey')
   SELF.AddReplace(EndKey,'EndKey')
   SELF.AddReplace(EnterKey,'EnterKey')
   SELF.AddReplace(EscKey,'EscKey')
   SELF.AddReplace(HomeKey,'HomeKey')
   SELF.AddReplace(InsertKey,'InsertKey')
   SELF.AddReplace(LeftKey,'LeftKey')
   SELF.AddReplace(MinusKey,'MinusKey')
   SELF.AddReplace(PauseKey,'PauseKey')
   SELF.AddReplace(PgDnKey,'PgDnKey')
   SELF.AddReplace(PgUpKey,'PgUpKey')
   SELF.AddReplace(PlusKey,'PlusKey')
   SELF.AddReplace(PrintKey,'PrintKey')
   SELF.AddReplace(RightKey,'RightKey')
   SELF.AddReplace(SlashKey,'SlashKey')
   SELF.AddReplace(SpaceKey,'SpaceKey')
   SELF.AddReplace(TabKey,'TabKey')
   SELF.AddReplace(UpKey,'UpKey')
   SELF.AddReplace(WinLeftKey,'WinLeftKey')
   SELF.AddReplace(WinRightKey,'WinRightKey')
   SELF.AddReplace(AppsKey,'AppsKey')
   SELF.AddReplace(KeyPad0,'KeyPad0')
   SELF.AddReplace(KeyPad1,'KeyPad1')
   SELF.AddReplace(KeyPad2,'KeyPad2')
   SELF.AddReplace(KeyPad3,'KeyPad3')
   SELF.AddReplace(KeyPad4,'KeyPad4')
   SELF.AddReplace(KeyPad5,'KeyPad5')
   SELF.AddReplace(KeyPad6,'KeyPad6')
   SELF.AddReplace(KeyPad7,'KeyPad7')
   SELF.AddReplace(KeyPad8,'KeyPad8')
   SELF.AddReplace(KeyPad9,'KeyPad9')
   SELF.AddReplace(MouseLeft,'MouseLeft')
   SELF.AddReplace(MouseRight,'MouseRight')
   SELF.AddReplace(MouseCenter,'MouseCenter')
   SELF.AddReplace(MouseLeft2,'MouseLeft2')
   SELF.AddReplace(MouseRight2,'MouseRight2')
   SELF.AddReplace(MouseCenter2,'MouseCenter2')
   SELF.AddReplace(MouseLeftUp,'MouseLeftUp')
   SELF.AddReplace(MouseRightUp,'MouseRightUp')
   SELF.AddReplace(MouseCenterUp,'MouseCenterUp')
