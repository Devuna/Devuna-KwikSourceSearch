/* Copyright 1998 ForeFront Inc.  All rights reserved.


This file contains all necessary JavaScript funtions to apply the following
Dynamic HTML effects:

	Blend Transitions
	Reveal Transitions
	Application of Visual Filters
	Application of Text Formatting
	Cursor Changes
	Toggle of Display and Visibility Attributes

This file is designed to be used with ForeHTML Pro 3.5. */


/////////////////////////////////////////////////////////////////////////////////////////////


/********************************************************************************************

	ChangeTextFormat (element id, new style values)

		- This function can alter the following style attributes:
			fontSize, fontFamily, fontWeight, fontVariant, fontStyle, textDecoration, & verticalAlign

		- format of style string:
			fontSize:#;fontFamily:$...

		- to leave a default value do not add the attribute to the list
		- to clear an attribute specify a * for example: textDecoration:*

*********************************************************************************************/


function ChangeTextFormat (eID, newStyle) {

	var		attr;
	var		value;
	var		index;

	if (! FullDHTML ())
		return;

	index = newStyle.indexOf (';');
	while (index != -1) {
		attr = newStyle.substring (0, index);	// this is the actual attribute
		newStyle = newStyle.substring (index + 1, newStyle.length);	// chop off the current attribute

		index = attr.indexOf (':');
		if (index != -1) { 	// we have a valid attribute
			value = attr.substring (index + 1, attr.length);
			attr = attr.substring (0, index);

			if (value == '*')
				value = "";

			eID.style[attr] = value;

			// go through sub P's, DIV's and SPANS's and set styles there too, they override
			for (i = 0; i < eID.all.tags("p").length; i++) {	
				sub = eID.all.tags("p")[i];
				sub.style[attr] = value;
				}

			for (i = 0; i < eID.all.tags("div").length; i++) {	
				sub = eID.all.tags("div")[i];
				sub.style[attr] = value;
				}

			for (i = 0; i < eID.all.tags("span").length; i++) {	
				sub = eID.all.tags("span")[i];
				sub.style[attr] = value;
				}
			}

		if (newStyle != "")	
			index = newStyle.indexOf (';');
		else
			index = -1;
		}


	if (newStyle != "") {	// we still have one attr left
		attr = newStyle;
		index = attr.indexOf (':');
		if (index != -1) { 	// we have a valid attribute
			value = attr.substring (index + 1, attr.length);
			attr = attr.substring (0, index);

			if (value == '*')
				value = "";

			eID.style[attr] = value;

			// go through sub P's, DIV's and SPANS's and set styles there too, they override
			for (i = 0; i < eID.all.tags("p").length; i++) {	
				sub = eID.all.tags("p")[i];
				sub.style[attr] = value;
				}

			for (i = 0; i < eID.all.tags("div").length; i++) {	
				sub = eID.all.tags("div")[i];
				sub.style[attr] = value;
				}

			for (i = 0; i < eID.all.tags("span").length; i++) {	
				sub = eID.all.tags("span")[i];
				sub.style[attr] = value;
				}
			}
		}
	}



/********************************************************************************************

	PlayTransition (element ID)

		- this function will play an assigned reveal or blend transition
		- the transition must be defined in the style of the object
		- the duration and type (if reveal) must also be defined
		- if the transition has already been played, it will not be played again
		- you must also set in the style: vivibility:hidden; width:auto

*********************************************************************************************/

function HideObj (eID) {

	eID.style.visibility = "hidden";
	}


function PlayTransition (eID) {

	if (! FullDHTML ())
		return;

	if (eID.filters.revealTrans) {
		if (eID.filters.revealTrans.status == 0) {
			HideObj (eID);
			eID.filters.revealTrans.apply();
			eID.style.visibility = ""; 
			eID.filters.revealTrans.play();  
			}
		}
	else if (eID.filters.blendTrans) {
		if (eID.filters.blendTrans.status == 0) {
			HideObj (eID);
			eID.filters.blendTrans.apply();
			eID.style.visibility = ""; 
			eID.filters.blendTrans.play();  
			}
		}
	}


/********************************************************************************************

	PlayAllTransitions ()

		- this function will be called duing onLoad, all DIV and SPAN objects will be checked
			for playTransOnLoad = 1 attribute
		- the transition must be defined in the style of the object
		- the duration and type (if reveal) must also be defined
		- if the transition has already been played, it will not be played again
		- you must also set in the style: vivibility:hidden; width:auto

*********************************************************************************************/

var	load_only = 1;

function PlayAllTransitions () {

	if (! FullDHTML ())
		return;

	var		eID;

	for (i = 0; i < document.all.tags("div").length; i++) {
		eID = document.all.tags("div")[i];
		if (! load_only || eID.getAttribute ("playTransOnLoad") == 1)
			PlayTransition (eID);
		}

	for (i = 0; i < document.all.tags("span").length; i++) {
		eID = document.all.tags("span")[i];
		if (! load_only || eID.getAttribute ("playTransOnLoad") == 1)
			PlayTransition (eID);
		}

	for (i = 0; i < document.all.tags("img").length; i++) {
		eID = document.all.tags("img")[i];
		if (! load_only || eID.getAttribute ("playTransOnLoad") == 1)
			PlayTransition (eID);
		}
	}

function ReplayAllTransitions () {
	load_only = 0;
	PlayAllTransitions ();
	}


/********************************************************************************************

	ToggleDisplay (element ID)

		- this function will toggle the display attribute of an object
		- when hidden, no space in the document will be reserved for this object

*********************************************************************************************/

function ToggleDisplay (eID) {

	if (! FullDHTML ())
		return;

	if (eID.style.display == "none")
		eID.style.display = "";
	else
		eID.style.display = "none";
	}


/********************************************************************************************

	ToggleVisibility (element ID)

		- this function will toggle the visibility attribute of an object
		- when hidden, space in the document will be reserved for this object

*********************************************************************************************/

function ToggleVisibility (eID) {

	if (! FullDHTML ())
		return;

	if (eID.style.visibility == "hidden")
		eID.style.visibility = "visible";
	else
		eID.style.visibility = "hidden";
	}


/********************************************************************************************

	ChangeCursor (element ID, cursor type)

		- this function will change the cursor when over the current object
		- to reset value, use cursor type 'auto'

*********************************************************************************************/

function ChangeCursor (eID, cursorType) {

	if (! FullDHTML ())
		return;
	if (eID == '')
		document.body.style.cursor = cursorType;
	else
		eID.style.cursor = cursorType;
	}


/********************************************************************************************

	ApplyFilter (element ID, filter string)

		- this function will apply a visible filter to the current object
		- you may only apply one filter at a time
		- to remove the filter set filter string to ""
		- you must set style="width:auto"
		- the syntax of the filter string is for example: ApplyFilter (eID, 'Glow(color=#FF0000,strength=2)')

*********************************************************************************************/

function ApplyFilter (eID, filterStr) {

	if (! FullDHTML ())
		return;

	eID.style.filter = filterStr;
	}


/********************************************************************************************

	ChangeImage (element ID, image)

		- this function will toggle between two images
		- the original image will be stored in a origImage attribute
		- the secondary image will be stored in secImage attribute
		- if image parameter is non-empty, the image will be set to that image

*********************************************************************************************/

function ChangeImage (eID, image) {

	if (!FullDHTML && ! PartialDHTML ())
		return;

	var			orig;

	if (eID.imgArray == null) {
		eID.imgArray = new Array (5);
		eID.imgCt = 0;
		}
	
	if (image == "" && eID.imgCt != 0) {
		image = eID.imgArray[eID.imgCt];
		eID.imgCt--;
		eID.src = image;
		}
	else if (image != "") {
		eID.imgCt++;
		eID.imgArray[eID.imgCt] = eID.src;
		eID.src = image;
		}
	}

/*****************************************************************************************

ChangeImageState (id, image id, visible image, hidden image)

	parameter list: 
		id is the ID of the object that, based on display state, will determine image to display
		id_img is the id of the image to change
		img_visible is the file name of the visible image
		img_hidden is the file name of the hidden image
	
	Usage: When using the ToggleDisplay function, this function can be used to change an image based on the state
	of the object being shown or hidden

*******************************************************************************************/

function ChangeImageState (id, img_id, img_visible, img_hidden) {

	if (id.style.display == "none")	// set image to img_hidden
		ChangeImage (img_id, img_hidden);		// ChangeImage defined in FH_DHTML.js
	else
		ChangeImage (img_id, img_visible);
	}


/******************************************************************************************

ToggleGroupDisplay (show object id, hide object id 1, hide object id 2...)

	Use this function to toggle the display attribute of a group of objects

*******************************************************************************************/

function ToggleGroupDisplay () {

	// show main object
	if (ToggleGroupDisplay.arguments[0].name != "") {
		if (ToggleGroupDisplay.arguments[0].style.display == "none") {
			ToggleGroupDisplay.arguments[0].style.display = "";
			}
		}

	// go through variable array and hide the rest
	for (var i = 1; i < ToggleGroupDisplay.arguments.length; i++) {
		if (ToggleGroupDisplay.arguments[i].style.display == "")
			ToggleGroupDisplay.arguments[i].style.display = "none";
		}
	}


/******************************************************************************************

ToggleGroupVisibility (show object id, hide object id 1, hide object id 2...)

	Use this function to toggle the visibility attribute of a group of objects

*******************************************************************************************/

function ToggleGroupVisibility () {

	// show main object
	if (ToggleGroupVisibility.arguments[0].style.visibility == "hidden")
		ToggleGroupVisibility.arguments[0].style.visibility = "visible";

	// go through variable array and hide the rest
	for (var i = 1; i < ToggleGroupVisibility.arguments.length; i++) {
		if (ToggleGroupVisibility.arguments[i].style.visibility == "visible")
			ToggleGroupVisibility.arguments[i].style.visibility = "hidden";
		}
	}
