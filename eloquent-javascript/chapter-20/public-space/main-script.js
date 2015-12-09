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

function request(method, url, callback, data) {
  var req = new XMLHttpRequest();
  req.open(method, url, true);
  req.addEventListener("load", function() {
    if (req.status < 400)
      callback(req.responseText);
    else
      callback(null, new Error("Request failed: " + req.statusText));
  });
  req.addEventListener("error", function() {
    callback(null, new Error("Network error"));
  });
  // method == "PUT" ? req.send(data) : req.send(null);
  req.send(data || null);
}

document.body.appendChild(elm("div", {},
			      elm("h2", {id: "file-name"}, "File name"),
			      elm("textarea", {rows: "20", cols: "100"}),
			      elm("div", {},
				  elm("button", {id: "save"}, "Save File"),
				  elm("button", {id: "open"}, "Open File"),
				  elm("button", {id: "close"}, "Delete File"),
				  elm("button", {id: "enter"}, "Enter Directory"),
				  elm("button", {id: "exit"}, "Exit Direcotyr"),
				  elm("span", {id: "error", style: "color: red"})),
			      elm("div", {id: "listing"})));

var directoryDepth = "";
var selected = null;
var fileName = document.querySelector("#file-name");
var textarea = document.querySelectorAll("textarea")[0];

request("GET", "http://localhost:8000/.", function(text, error) {
  if (error)
    console.log("Error on GET of main dir");
  else {
    var files = text.split("\n");
    var list = elm("ul", {});
    list.addEventListener("click", function(event) {
      Array.prototype.forEach.call(list.childNodes, function(item) {
	if (item == event.target) {
	  item.setAttribute("style", "background-color: violet");
	  selected = item.textContent;
	} else {
	  item.setAttribute("style", "background-color: none");
	}
      });
    });
    files.forEach(function(file) {
      var item = elm("li", {}, file);
      list.appendChild(item);
    });
    document.querySelector("#listing").appendChild(list);
  }
});


var openButton = document.querySelector("#open");
openButton.addEventListener("click", function() {
  request("GET", "http://localhost:8000/" + directoryDepth + selected, function (text, error) {
    if (error)
      console.log("Error...");
    else {
      textarea.value = text;
      fileName.textContent = "./" + selected;
    }
  });
});

var deleteButton = document.querySelector("#close");
deleteButton.addEventListener("click", function() {
  // request("DELETE", "http://localhost:8000/" + directoryDepth + selected, function(_, error) {
  //   if (error)
  //     console.log("Error deleting file(" + selected + ": " + String(error));
  // });
  console.log("DELETING FILE '" + selected + "'");
});

var saveButton = document.querySelector("#save");
saveButton.addEventListener("click", function() {
  request("PUT", "http://localhost:8000/" + directoryDepth + selected, function(_, error) {
    if (error)
      console.log("Error writing file (" + selected + "): " + String(error));
    else
      window.location.reload(true);
  }, textarea.value);
});

var openDirButton = document.querySelector("#enter");
openDirButton.addEventListener("click", function() {
  request("GET", "http://localhost:8000/" + directoryDepth + selected, function(text, error) {
    if (error)
      console.log("Error entering to directory (" + directoryDepth + selected + "):" + String(error));
    else {
      var listing = document.querySelector("#listing");
      listing.removeChild(listing.firstChild);
      
      var files = text.split("\n");
      var list = elm("ul", {});
      list.addEventListener("click", function(event) {
	Array.prototype.forEach.call(list.childNodes, function(item) {
	  if (item == event.target) {
	    item.setAttribute("style", "background-color: violet");
	    selected = item.textContent;
	  } else {
	    item.setAttribute("style", "background-color: none");
	  }
	});
      });
      files.forEach(function(file) {
	var item = elm("li", {}, file);
	list.appendChild(item);
      });
      document.querySelector("#listing").appendChild(list);
      directoryDepth += selected;
    }      
  });
});

var exitButton = document.querySelector("#exit");
exitButton.addEventListener("click", function() {
  console.log("Se quiere salir de un directorio");
  // hacer algo con las REGEXP para obtener la ultima carpeta y eliminarla del path
});


// TODO: meter todos los directorydepth + selected en una funcion que devuelva el path actual
