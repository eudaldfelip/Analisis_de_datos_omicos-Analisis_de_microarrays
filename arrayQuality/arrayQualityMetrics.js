// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ true, false, true, true, false, true, true, true, true, true, true, true, true, true, true, true, true, true ];
var arrayMetadata    = [ [ "1", "GSM1134016", "GSM1134016_EA04058_25073_H133+_F15.CEL", "46XX", "10/25/05 17:03:52" ], [ "2", "GSM1134017", "GSM1134017_EA04058_25074_H133+_F16.CEL", "46XX", "10/27/05 17:24:11" ], [ "3", "GSM1134018", "GSM1134018_EA04058_25075_H133+_F20.CEL", "46XX", "10/25/05 17:16:27" ], [ "4", "GSM1134020", "GSM1134020_EA04058_25077_H133+_F34.CEL", "46XX", "10/25/05 19:49:42" ], [ "5", "GSM1134022", "GSM1134022_EA04058_25079_H133+_F42.CEL", "46XX", "10/25/05 18:35:24" ], [ "6", "GSM1134025", "GSM1134025_EA04058_25082_H133+_F52.CEL", "46XX", "10/25/05 19:13:56" ], [ "7", "GSM1134026", "GSM1134026_EA04058_28124_H133+_TS208.CEL", "45Xm", "08/18/06 18:56:47" ], [ "8", "GSM1134027", "GSM1134027_EA04058_28335_H133+_TS250.CEL", "45Xm", "08/18/06 17:59:22" ], [ "9", "GSM1134028", "GSM1134028_EA04058_30434_H133+_TS258.CEL", "45Xm", "11/30/06 10:40:09" ], [ "10", "GSM1134032", "GSM1134032_EA04058_52892_H133+_TS279.CEL", "45Xm", "08/05/08 11:32:23" ], [ "11", "GSM1134033", "GSM1134033_EA04058_52893_H133+_TS236.CEL", "45Xm", "08/05/08 11:27:33" ], [ "12", "GSM1134041", "GSM1134041_EA04058_53540_H133+_TS181.CEL", "45Xm", "08/05/08 11:52:03" ], [ "13", "GSM1134042", "GSM1134042_EA04058_30426_H133+_TS168.CEL", "45Xp", "11/30/06 09:34:38" ], [ "14", "GSM1134044", "GSM1134044_EA04058_52884_H133+_TS11.CEL", "45Xp", "08/05/08 11:09:44" ], [ "15", "GSM1134045", "GSM1134045_EA04058_52885_H133+_TS120.CEL", "45Xp", "08/05/08 11:04:50" ], [ "16", "GSM1134047", "GSM1134047_EA04058_52887_H133+_TS234.CEL", "45Xp", "08/05/08 11:07:29" ], [ "17", "GSM1134048", "GSM1134048_EA04058_52888_H133+_TS241.CEL", "45Xp", "08/05/08 11:20:56" ], [ "18", "GSM1134049", "GSM1134049_EA04058_52890_H133+_TS266.CEL", "45Xp", "08/05/08 11:17:35" ] ];
var svgObjectNames   = [ "pca", "dens", "dig" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
    for(i=0; i<ssrules.length; i++) {
        if (ssrules[i].selectorText == (".aqm" + reportObjId)) {
		ssrules[i].style.cssText = cssText[0+status];
		break;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}
