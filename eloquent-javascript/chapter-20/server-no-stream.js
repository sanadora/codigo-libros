var http = require("http");

var server = http.createServer(function(request, response) {
  response.writeHead(200, {"Content-type": "text/html"});
  // response.write("<h1>Hello!</h1>" +
  // 		 "<p>You asked for <code>" +
  // 		 request.url + "</code></p>");
  // response.end();
  request.on("data", function(chunk) {
    response.write(chunk.toString().toUpperCase());
  });
  request.on("end", function() {
    response.end();
  });
});

server.listen(8000);


