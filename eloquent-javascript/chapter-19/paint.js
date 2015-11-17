///////////////////////
// Building the DOM  //
///////////////////////

function elt(name, attributes) {
  var node = document.createElement(name);
  if (attributes)
    for (var attr in attributes)
      if (attributes.hasOwnProperty(attr))
	node.setAttribute(attr, attributes[attr]);

  for(var i = 2; i < arguments.length; i++) {
    var child = arguments[i];
    if (typeof child == "string")
      child = document.createTextNode(child);
    node.appendChild(child);
  }
  return node;
}

////////////////////
// The Foundation //
////////////////////

var controls = Object.create(null);

function createPaint(parent) {
  var canvas = elt("canvas", {width: 100, height: 80});
  var cx = canvas.getContext("2d");
  var toolbar = elt("div", {class: "toolbar"});
  for (var name in controls)
    toolbar.appendChild(controls[name](cx));

  var panel = elt("div", {class: "picturepanel"}, canvas);
  parent.appendChild(elt("div", null, panel, toolbar));
}

////////////////////
// Tool Selection //
////////////////////

var tools = Object.create(null);

controls.tool = function(cx) {
  var select = elt("select");
  for (var name in tools)
    select.appendChild(elt("option", null, name));

  cx.canvas.addEventListener("mousedown", function(event) {
    if (event.which == 1) {
      tools[select.value](event, cx)
      event.preventDefault();
    }
  });

  return elt("span", null, "Tool: ", select);
}


function relativePos(event, element) {
  var rect = element.getBoundingClientRect();
  return {x: Math.floor(event.clientX - rect.left),
	  y: Math.floor(event.clientY - rect.top)};
}

function trackDrag(onMove, onEnd) {
  function end(event) {
    removeEventListener("mousemove", onMove);
    removeEventListener("mouseup", end);
    if (onEnd)
      onEnd(event);
  }
  addEventListener("mousemove", onMove);
  addEventListener("mouseup", end);
}


tools.Line = function(event, cx, onEnd) {
  cx.lineCap = "round";

  var pos = relativePos(event, cx.canvas);
  trackDrag(function(event) {
    cx.beginPath();
    cx.moveTo(pos.x, pos.y);
    pos = relativePos(event, cx.canvas);
    cx.lineTo(pos.x, pos.y);
    cx.stroke();
  }, onEnd);
};

tools.Erase = function(event, cx) {
  cx.globalCompositeOperation = "destination-out";
  tools.Line(event, cx, function() {
    cx.globalCompositeOperation = "source-over";
  });
};

tools.Rectangle = function(event, cx) {
  var startPos = relativePos(event, cx.canvas);
  var rect = elt("div", {id: "guia-rect",
			 style: "position: absolute; background: lightgreen"});
  rect.style.left = event.clientX + "px";
  rect.style.top = event.clientY + "px";
  document.body.appendChild(rect);
  trackDrag(function(event) {
    var interPos = relativePos(event, cx.canvas);
    rect.style.width = JSON.stringify(interPos.x - startPos.x) + "px";
    rect.style.height = JSON.stringify(interPos.y - startPos.y) + "px";
  }, function(event) {
    var finalPos = relativePos(event, cx.canvas);
    console.log(startPos, finalPos);
    cx.strokeRect(startPos.x, startPos.y,
		  finalPos.x - startPos.x,
		  finalPos.y - startPos.y);
    document.body.removeChild(rect);
  });
};


//////////////////////////
// Color and Brush Size //
//////////////////////////

controls.color = function(cx) {
  var input = elt("input", {type: "color"});
  input.addEventListener("change", function() {
    cx.fillStyle = input.value;
    cx.strokeStyle = input.value;
  });
  return elt("span", null, "Color: ", input);
}

controls.brushSize = function(cx) {
  var select = elt("select");
  var sizes = [1, 2, 3, 5, 8, 12, 25, 35, 50, 75, 100];
  sizes.forEach(function(size) {
    select.appendChild(elt("option", {value: size}, size + "pixels"));
  });
  select.addEventListener("change", function() {
    cx.lineWidth = select.value;
  });
  return elt("span", null, "Brush size: ", select);
}


////////////
// Saving //
////////////

controls.save = function(cx) {
  var link = elt("a", {href: "/"}, "Save");
  function update() {
    try {
      link.href = cx.canvas.toDataURL();
    } catch (e) {
      if (e instanceof SecurityError)
	link.href = "javascript:alert(" + JSON.stringify("Can't save: " + e.toString()) + ")";
      else
	throw e;
    }
  }
  link.addEventListener("mouseover", update);
  link.addEventListener("focus", update);
  return link;
}


/////////////////////////
// Loading Image Files //
/////////////////////////

function loadImageURL(cx, url) {
  var image = elt("img");
  image.addEventListener("load", function(){
    var color = cx.fillStyle, size = cx.lineWidth;
    cx.canvas.width = image.width;
    cx.canvas.height = image.height;
    cx.drawImage(image, 0, 0);
    cx.fillStyle = color;
    cx.strokeStyle = color;
    cx.lineWidth = size;
  });
  image.src = url;
}

controls.openFile = function(cx) {
  var input = elt("input", {type: "file"});
  input.addEventListener("change", function(){
    if (input.files.length == 0) return;
    var reader = new FileReader();
    reader.addEventListener("load",function(){
      loadImageURL(cx, reader.result);
    });
    reader.readAsDataURL(input.files[0]);
  });
  return elt("div", null, "Open file: ", input);
};

controls.openURL = function(cx) {
  var input = elt("input", {type: "text"});
  var form = elt("form", null,
		 "Open URL: ", input,
		 elt("button", {type: "submit"}, "load"));
  form.addEventListener("submit",function(event){
    event.preventDefault();
    loadImageURL(cx, input.value);
  });
  return form;
};


//////////////////
// Finishing Up //
//////////////////

tools.Text = function(event, cx) {
  var text = prompt("Text:", "");
  if (text) {
    var pos = relativePos(event, cx.canvas);
    cx.font = Math.max(7, cx.lineWidth) + "px sans-serif";
    cx.fillText(text, pos.x, pos.y);
  }
};

tools.Spray = function(event, cx) {
  var radius = cx.lineWidth / 2;
  var area = radius * radius * Math.PI;
  var dotsPerTick = Math.ceil(area / 30);

  var currentPos = relativePos(event, cx.canvas);
  var spray = setInterval(function() {
    for(var i = 0; i < dotsPerTick; i++) {
      var offset = randomPointInRadius(radius);
      cx.fillRect(currentPos.x + offset.x,
		  currentPos.y + offset.y, 1, 1);
      
    }
  }, 25);
  trackDrag(function(event) {
    currentPos = relativePos(event, cx.canvas);
  }, function() {
    clearInterval(spray);
  });
};

function randomPointInRadius(radius){
  for (;;) {
    var x = Math.random() * 2 - 1;
    var y = Math.random() * 2 - 1;
    if (x * x + y * y <= 1)
      return {x: x * radius,
	      y: y * radius};
  }
}


////////////////
// Flood Fill //
////////////////

// function pixelAt(cx, x, y) {
//   return cx.getImageData(x, y, 1, 1).data;
// }

// function coordsToIndex(cx, x, y){
//   var width = cx.canvas.width, height = cx.canvas.height;
//   if (x < 0 || x >= width || y < 0 || y >= height) return null;
//   return (y * width + x);
// }

// function sameColor(a, b){
//   return Array.prototype.reduce.call(a, function(same, value, i) {
//     return same ? a[i] == b[i] : false;
//   }, true);
// }

// tools["Flood Fill"] = function(event, cx, onEnd) {
//   var pixels = [];

//   for(var y = 0; y < cx.canvas.height; y++)
//     for(var x = 0; x < cx.canvas.width; x++)
//       pixels.push({
// 	visited: false,
// 	data: pixelAt(cx, x, y)
//       });
//   var pos = relativePos(event, cx.canvas);
//   var startColor = pixelAt(cx, pos.x, pos.y);

//   function pixelFill(cx, x, y) {
//     cx.fillRect(x, y, 1, 1);
//     pixels[coordsToIndex(cx, x, y)].visited = true;

//     var up = pixels[coordsToIndex(cx, x, y-1)], down = pixels[coordsToIndex(cx, x, y+1)];
//     var left = pixels[coordsToIndex(cx, x-1, y)], right = pixels[coordsToIndex(cx, x+1, y)];
    
//     if (up && !up.visited && sameColor(up.data, startColor))
//       pixelFill(cx, x, y-1);
//     if (down && !down.visited && sameColor(down.data, startColor))
//       pixelFill(cx, x, y+1);
//     if (left && !left.visited && sameColor(left.data, startColor))
//       pixelFill(cx, x-1, y);
//     if (right && !right.visited && sameColor(right.data, startColor))
//       pixelFill(cx, x+1, y);
//   }

//   pixelFill(cx, pos.x, pos.y);
// }

function forAllNeighbors(point, fn) {
  fn({x: point.x, y: point.y + 1});
  fn({x: point.x, y: point.y - 1});
  fn({x: point.x + 1, y: point.y});
  fn({x: point.x - 1, y: point.y});
}

function isSameColor(data, pos1, pos2){
  var offset1 = (pos1.x + pos1.y * data.width) * 4;
  var offset2 = (pos2.x + pos2.y * data.width) * 4;
  for(var i = 0; i < 4; i++) {
    if (data.data[offset1 + i] != data.data[offset2 + i])
      return false;
  }
  return true;
}

tools["Flood Fill"] = function(event, cx) {
  var startPos = relativePos(event, cx.canvas);

  var data = cx.getImageData(0, 0, cx.canvas.width, cx.canvas.height);

  var alreadyFilled = new Array(data.width * data.height);

  var workList = [startPos];
  while (workList.length) {
    var pos = workList.pop();
    var offset = pos.x + pos.y * data.width;
    if (alreadyFilled[offset]) continue;

    cx.fillRect(pos.x, pos.y, 1, 1);
    alreadyFilled[offset] = true;

    forAllNeighbors(pos, function(neighbor) {
      if (neighbor.x >= 0 && neighbor.x < data.width &&
	  neighbor.y >= 0 && neighbor.y < data.height &&
	  isSameColor(data, startPos, neighbor))
	workList.push(neighbor);
    });
  }
};
