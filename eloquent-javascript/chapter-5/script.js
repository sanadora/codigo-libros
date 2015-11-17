function World (width, height){
    this.width = width, this.height = height;
    this.grid = new Array(this.width);
    for(var i = 0; i < this.width; i++)
	this.grid[i] = new Array(this.height);
    this.populate();
}
World.prototype.populate = function() {
    for(var y = 0; y < this.height; y++) {
	for(var x = 0; x < this.width; x++) {
	    this.grid[x][y] = new Cell(Math.random() > 0.5);
	}
    }
}
World.prototype.neighboursOf = function(xPos, yPos) {
    var neighbours = 0;
    var xFrom = Math.max(0, xPos-1), xTo = Math.min(this.width - 1, xPos + 1);
    var yFrom = Math.max(0, yPos-1), yTo = Math.min(this.height - 1, yPos + 1);
    for(var y = yFrom; y <= yTo; y++) {
	for(var x = xFrom; x <= xTo; x++) {
	    if (x == xPos && y == yPos)
		continue;
	    var cell = this.grid[x][y];
	    if (cell.alive) 
		neighbours += 1;
	}
    }
    return neighbours;
}
World.prototype.nextGeneration = function(){
    this.computeNeighbours();
    this.computeSurvivors();
}
World.prototype.computeNeighbours = function(){
    this.grid.forEach(function(row, x) {
	row.forEach(function(cell, y){
	    var neighbours = this.neighboursOf(x, y);
	    if (cell.alive && (neighbours == 2 || neighbours == 3)){
		// vive
		cell.willSurvive = true;
	    } else if (cell.alive && (neighbours > 3 || neighbours < 2)){
		// muere
		cell.willSurvive = false;
	    } else if (!cell.alive && neighbours == 3){
		// nace
		cell.willSurvive = true;
	    }
	}, this);
    }, this);
}
World.prototype.computeSurvivors = function() {
    this.grid.forEach(function(col) {
	col.forEach(function(cell) {
	    cell.alive = cell.willSurvive;
	});
    });
}

function Cell (alive){
    this.alive = alive;
    this.willSurvive = false;
}
Cell.prototype = Object.create(null);


function elm (name){
    var elm = document.createElement(name);
    for(var i = 1; i < arguments.length; i++) {
	var child = arguments[i];
	if (typeof child == "string")
	    child = document.createTextNode(child);
	elm.appendChild(child);
    }
    return elm;
}

function DOMDisplay(world, element){
    this.world = world;
    this.table = elm("table");
    this.world.grid.forEach(function(col, x) {
	var row = this.table.appendChild(elm("tr"));
	col.forEach(function(__, y) {
	    var cell = row.appendChild(elm("td"));
	    var checkbox = cell.appendChild(elm("input"));
	    checkbox.type = "checkbox";
	    checkbox.addEventListener("change", function(event) {
		var cell = world.grid[x][y];
		cell.alive = cell.willSurvive = checkbox.checked;
	    });
	}, this);
    }, this);
    element.appendChild(this.table);
}
DOMDisplay.prototype.update = function() {
    Array.prototype.forEach.call(this.table.rows, function(row, x) {
	Array.prototype.forEach.call(row.cells, function(cell, y) {
	    var checkbox = cell.childNodes[0];
	    checkbox.checked = this.world.grid[x][y].alive;
	});
    });
}

var world = new World(5,5);
var display = new DOMDisplay(world, document.body);
display.update();

function advance() {
    world.nextGeneration();
    display.update();
}


