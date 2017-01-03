/* a privacy-respecting analytics server,
   see http://gasche.ulminfo.fr/neu-prl/statistics/about */
var analytics_server = "http://gasche.ulminfo.fr/neu-prl/statistics";
var http = new XMLHttpRequest();
var hostname = encodeURIComponent(window.location.hostname);
var url_suffix = window.location.pathname + window.location.search;
var url = analytics_server + "/visit/" + hostname  + "/" + url_suffix;
http.open("POST", url, true);
http.setRequestHeader("Content-length", 0);
http.send();
