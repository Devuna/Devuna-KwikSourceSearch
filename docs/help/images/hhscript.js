
function ShowPopup (file, flags, width, height) {

	var		path;
	var 	i;
	var		str1;
	var		str2;
	var		cc;

	i = file.indexOf (":");	// if file has a colon, just use it
	if (i == -1) {
		// make full path for string
		if (GetBrowser () == 1) {	// make full path
			path = GetPath (0, 0);

			cc = path.substring (path.length - 1, path.length);

			if (cc != '\\' && cc != '/')
				path += "/";

			path += file;
			}
		else
			path = file;
		}
	else
		path = file;

	PopObj.ShowPopup (path, flags, width, height);
	};


function LaunchApp (app, cmdline) {

	var		path;

	path = GetPath (1, 1);
	LaunchObj.LaunchApp2 (app, cmdline, path);
	};


function ShowHHSecWindow (topic, wndclass) {

	var			chm_path;

	if (FullDHTML () == 1)
		chm_path = document.location.href;
	else
		chm_path = document.location;

	if (chm_path.charAt(0)=="m" && chm_path.charAt(1)=="k" && chm_path.charAt(2)==":") {
		chm_path = FixSpaces (chm_path);
		PopObj.ShowSecondaryWindow (chm_path, topic, wndclass);
		}
	else
		alert ("You must have a compiled file to perform a secondary window jump");
	};


function GetPath (remove_file, path_only) {

	var		path;
	var 	i;
	var		str1;
	var		str2;
	var		cc;

	if (FullDHTML () == 1)
		str1 = document.location.href;
	else
		str1 = document.location;

	// check to see if mk:@MSIT...
	if (str1.charAt(0)=="m" && str1.charAt(1)=="k" && str1.charAt(2)==":") {

		str1 = FixSpaces (str1);

		// parse string and convert all \ to /
		i = 0;
		str2 = "";
		while (i < str1.length - 1) {
			cc = str1.charAt (i);
			if (cc == "\\") {
				if (str2 != "mk:")
					str2 += "/";
				}
			else {
				if (cc == "/") {
					if (str2 != "mk:")
						str2 += cc;
					}
				else
					str2 += cc;
				}

			i++;
			}

		i = str2.lastIndexOf ("::");
		if (i != -1)
			path = str2.substring (0, i+2);	// go beyond ::
		}
	else {
		i = str1.lastIndexOf ("\\");
		if (i == -1)	// try with /
			i = str1.lastIndexOf ("/");

		if (i != -1)
			path = str1.substring (0, i+1);
		}

	if (remove_file == 1) {	// take file:/// off front
		i = -1;
		i = path.indexOf ("file:///");
		if (i >= 0)
			path = path.substring (i + 8);
		}

	if (path_only == 1) {	// strip mk:@msitStore
		i = -1;
		i = path.indexOf ("mk:@MSITStore:");
		if (i >= 0) {
			path = path.substring (i + 14);

			// strip off .chm file name
			i = -1;
			i = path.lastIndexOf ("/");
			if (i >= 0)
				path = path.substring (0, i);
			}
		}

	return path;
	}


function FixSpaces (chm_path) {

	var			front, back;
	var			i;

	i = chm_path.indexOf ("%20");
	while (i != -1) {
		front = chm_path.substring (0, i);
		back = chm_path.substring (i + 3, chm_path.length);
		chm_path = front + " " + back;
		i = chm_path.indexOf ("%20");
		}

	return chm_path;
	}

