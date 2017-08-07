// ================================================================================
// Notice : Copyright (C) 2017, Devuna
//          Distributed under the MIT License (https://opensource.org/licenses/MIT)
//
//    This file is part of Devuna-Scintilla (https://github.com/Devuna/Devuna-Scintilla)
//
//    Devuna-Scintilla is free software: you can redistribute it and/or modify
//    it under the terms of the MIT License as published by
//    the Open Source Initiative.
//
//    Devuna-Scintilla is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    MIT License for more details.
//
//    You should have received a copy of the MIT License
//    along with Devuna-Scintilla.  If not, see <https://opensource.org/licenses/MIT>.
// ================================================================================
//
//********************************************************************************
// Source module name:  SciUtil.c
// Release Version:     Devuna Scintilla Control utility library
//********************************************************************************

//declarations
extern "C"  void * GetXPMData (long lImage);
long CallSciMsgProc(long (cdecl *fpSciMsgProc)(long, long, long, long), long ptr, long uMsg, long wParam, long lParam);


// data
/* XPM */
static char *bookmarkBluegem[] = {
/* width height num_colors chars_per_pixel */
"    15    15      64            1",
/* colors */
"  c none",
". c #0c0630",
"# c #8c8a8c",
"a c #244a84",
"b c #545254",
"c c #cccecc",
"d c #949594",
"e c #346ab4",
"f c #242644",
"g c #3c3e3c",
"h c #6ca6fc",
"i c #143789",
"j c #204990",
"k c #5c8dec",
"l c #707070",
"m c #3c82dc",
"n c #345db4",
"o c #619df7",
"p c #acacac",
"q c #346ad4",
"r c #1c3264",
"s c #174091",
"t c #5482df",
"u c #4470c4",
"v c #2450a0",
"w c #14162c",
"x c #5c94f6",
"y c #b7b8b7",
"z c #646464",
"A c #3c68b8",
"B c #7cb8fc",
"C c #7c7a7c",
"D c #3462b9",
"E c #7c7eac",
"F c #44464c",
"G c #a4a4a4",
"H c #24224c",
"I c #282668",
"J c #5c5a8c",
"K c #7c8ebc",
"L c #dcd7e4",
"M c #141244",
"N c #1c2e5c",
"O c #24327c",
"P c #4472cc",
"Q c #6ca2fc",
"R c #74b2fc",
"S c #24367c",
"T c #b4b2c4",
"U c #403e58",
"V c #4c7fd6",
"W c #24428c",
"X c #747284",
"Y c #142e7c",
"Z c #64a2fc",
"0 c #3c72dc",
"1 c #bcbebc",
"2 c #6c6a6c",
"3 c #848284",
"4 c #2c5098",
"5 c #1c1a1c",
"6 c #243250",
"7 c #7cbefc",
"8 c #d4d2d4",
/* pixels */
"    yCbgbCy    ",
"   #zGGyGGz#   ",
"  #zXTLLLTXz#  ",
" p5UJEKKKEJU5p ",
" lfISa444aSIfl ",
" wIYij444jsYIw ",
" .OsvnAAAnvsO. ",
" MWvDuVVVPDvWM ",
" HsDPVkxxtPDsH ",
" UiAtxohZxtuiU ",
" pNnkQRBRhkDNp ",
" 1FrqoR7Bo0rF1 ",
" 8GC6aemea6CG8 ",
"  cG3l2z2l3Gc  ",
"    1GdddG1    "
};

/* XPM */
static char *bookmarkRedgem[] = {
/* width height num_colors chars_per_pixel */
"    15    15      64            1",
/* colors */
"  c none",
". c #300c06",
"# c #8c8c8a",
"a c #84244a",
"b c #545452",
"c c #ccccce",
"d c #949495",
"e c #b4346a",
"f c #442426",
"g c #3c3c3e",
"h c #fc6ca6",
"i c #891437",
"j c #902049",
"k c #ec5c8d",
"l c #707070",
"m c #dc3c82",
"n c #b4345d",
"o c #f7619d",
"p c #acacac",
"q c #d4346a",
"r c #641c32",
"s c #911740",
"t c #df5482",
"u c #c44470",
"v c #a02450",
"w c #2c1416",
"x c #f65c94",
"y c #b7b7b8",
"z c #646464",
"A c #b83c68",
"B c #fc7cb8",
"C c #7c7c7a",
"D c #b93462",
"E c #ac7c7e",
"F c #4c4446",
"G c #a4a4a4",
"H c #4c2422",
"I c #682826",
"J c #8c5c5a",
"K c #bc7c8e",
"L c #e4dcd7",
"M c #441412",
"N c #5c1c2e",
"O c #7c2432",
"P c #cc4472",
"Q c #fc6ca2",
"R c #fc74b2",
"S c #7c2436",
"T c #c4b4b2",
"U c #58403e",
"V c #d64c7f",
"W c #8c2442",
"X c #847472",
"Y c #7c142e",
"Z c #fc64a2",
"0 c #dc3c72",
"1 c #bcbcbe",
"2 c #6c6c6a",
"3 c #848482",
"4 c #982c50",
"5 c #1c1c1a",
"6 c #502432",
"7 c #fc7cbe",
"8 c #d4d4d2",
/* pixels */
"    yCbgbCy    ",
"   #zGGyGGz#   ",
"  #zXTLLLTXz#  ",
" p5UJEKKKEJU5p ",
" lfISa444aSIfl ",
" wIYij444jsYIw ",
" .OsvnAAAnvsO. ",
" MWvDuVVVPDvWM ",
" HsDPVkxxtPDsH ",
" UiAtxohZxtuiU ",
" pNnkQRBRhkDNp ",
" 1FrqoR7Bo0rF1 ",
" 8GC6aemea6CG8 ",
"  cG3l2z2l3Gc  ",
"    1GdddG1    "
};

/* XPM */
static char * breakpointRoundedBlue[] = {
"16 16 18 1",
"       c None",
".      c #446D8C",
"+      c #6297BF",
"@      c #7CB7E2",
"#      c #78A1C2",
"$      c #4A7392",
"%      c #6399C2",
"&      c #7FBAE7",
"*      c #7FA6C5",
"=      c #8FC0E5",
"-      c #9AC0DF",
";      c #9BB8CF",
">      c #8FA5B7",
",      c #7E909E",
"'      c #66717B",
")      c #446984",
"!      c #5A656D",
"~      c #4C6A80",
"                ",
"                ",
"                ",
"    .........   ",
"   .+@@@@@@@#$  ",
"  .%&&&&&&&&&*. ",
"  .===========. ",
"  .-----------. ",
"  .;;;;;;;;;;;. ",
"  .>>>>>>>>>>>. ",
"  .,,,,,,,,,,,. ",
"  .'''''''''''. ",
"   )!!!!!!!!!~  ",
"    .........   ",
"                ",
"                "};


//implementation
long CallSciMsgProc(long (cdecl *fpSciMsgProc)(long, long, long, long), long ptr, long uMsg, long wParam, long lParam)
{
  return((*fpSciMsgProc)(ptr, uMsg, wParam, lParam));
}


extern "C"  void * GetXPMData (long lImage)
{
   void * retVal = 0;

   switch(lImage) {
     case 1:
         retVal = &bookmarkBluegem;
         break;
     case 2:
         retVal = &bookmarkRedgem;
         break;
     case 3:
         retVal = &breakpointRoundedBlue;
         break;
     default:
         retVal = &bookmarkBluegem;
   }
   return retVal;
}
