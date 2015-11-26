"use strict";

function elm(name, attributes) {
  var newElement = document.createElement(name);
  for (var attr in attributes) {
    if (attributes.hasOwnProperty(attr)) {
      newElement.setAttribute(attr, attributes[attr]);
    }
  }
  for(var i = 2; i < arguments.length; i++) {
    var child = arguments[i];
    if (typeof child == "string")
      child = document.createTextNode(child);
    newElement.appendChild(child);
  }
  return newElement;
}


document.body.appendChild(elm("div", {},
			      elm("textarea", {rows: "20", cols: "100"}),
			      elm("div", {},
				  elm("button", {}, "Press Me"))));

function getURL(url, callback) {
  var req = new XMLHttpRequest();
  req.open("GET", url, true);
  req.addEventListener("load", function() {
    if (req.status < 400)
      callback(req.responseText);
    else
      callback(null, new Error("Request failed: " + req.statusText));
  });
  req.addEventListener("error", function() {
    callback(null, new Error("Network error"));
  });
  req.send(null);
}

var textarea = document.querySelectorAll("textarea")[0];
getURl("http://localhost:8000/main-script.js", function (text, error) {
  if (error)
    console.log("Error...");
  else
    textarea.textContent = text;
});

