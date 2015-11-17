var http = require("http");

var server = http.createServer(function(request, response) {
  console.log("Inside createServer...");
  response.writeHead(200, {"Content-Type": "text/plain"});
  request.on("data", function(chunk) {
    console.log("Data received.");
    response.write(chunk.toString().toUpperCase());
  });
  request.on("end", function(chunk) {
    console.log("Data transmission ended.");
    response.end();
  });
});

server.listen(8000);
console.log("Listening to port 8000...");


