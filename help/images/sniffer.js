function SupportsDHTML () {

	var 	agent;
	var		version;

	agent = navigator.userAgent.toLowerCase ();
	version = parseInt (navigator.appVersion);

	if (agent.indexOf ("msie") != -1 || agent.indexOf ("mozilla") != -1 || 
		agent.indexOf ("spoofer") != -1 || agent.indexOf ("compatible") != -1) {

		if (version >= 4)
			return 1;
		}

	return 0;
	};


function FullDHTML () {

	var 	agent;
	var	version;

	agent = navigator.userAgent.toLowerCase ();
	version = parseInt (navigator.appVersion);

	if (agent.indexOf ("msie") != -1) {

		if (version >= 4)
			return 1;
		}

	return 0;
	};


function PartialDHTML () {

	var 	agent;
	var		version;

	agent = navigator.userAgent.toLowerCase ();
	version = parseInt (navigator.appVersion);

	if ((agent.indexOf ("mozilla") != -1 || agent.indexOf ("spoofer") != -1 || 
		agent.indexOf ("compatible") != -1) && agent.indexOf ("msie") == -1) {

		if (version >= 4)
			return 1;
		}

	return 0;
	};


function GetBrowser () {

	var 	agent;
	var		version;

	agent = navigator.userAgent.toLowerCase ();
	version = parseInt (navigator.appVersion);

	if (agent.indexOf ("msie") != -1)
		return 1;

	return 0;
	};


function IsOldBrowser () {

	var 	agent;
	var		version;

	version = parseInt (navigator.appVersion);

	if (version < 4)
		return 1;

	return 0;
	};


function IsIE () {

	var 	agent;
	var		version;

	agent = navigator.userAgent.toLowerCase ();
	version = parseInt (navigator.appVersion);

	if (agent.indexOf ("msie") != -1)
		return 1;

	return 0;
	}


function IsIE4 () {

	var 	agent;
	var		version;
	var		msie;

	agent = navigator.userAgent.toLowerCase ();
	msie = agent.indexOf ("msie ")

	if (msie != -1) {
		version = parseInt (agent.substring (msie+5, agent.indexOf (".", msie )))
		if (version == 4)
			return 1;
		}

	return 0;
	}


function IsNS4 () {

	var 	agent;
	var		version;

	agent = navigator.userAgent.toLowerCase ();
	version = parseInt (navigator.appVersion);

	if ((agent.indexOf ("mozilla") != -1 || agent.indexOf ("spoofer") != -1 || 
		agent.indexOf ("compatible") != -1) && agent.indexOf ("msie") == -1) {

		if (version >= 4)
			return 1;
		}

	return 0;
	}

function IsMac () {

	var 	agent;

	agent = navigator.userAgent.toLowerCase ();

	if (agent.indexOf ("mac") != -1)
		return 1;

	return 0;
	}
	
function IsWindows () {

	var 	agent;

	agent = navigator.userAgent.toLowerCase ();

	if (agent.indexOf ("windows") != -1)
		return 1;

	return 0;
	}
	
	
