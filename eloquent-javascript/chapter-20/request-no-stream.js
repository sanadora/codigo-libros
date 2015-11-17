var http = require("http");

var request = http.request({
  hostname: "localhost",
  port: 8000,
  method: "POST",
  headers: {Accept: "text/html"}
}, function(response) {
  response.on("data", function(chunk) {
    process.stdout.write(chunk.toString());
  });
  console.log("Server responded with status code",
	      response.statusCode);
});

setTimeout(function() {
  request.write("hola1");
}, 1000);
setTimeout(function() {
  request.write("hola2");
}, 2000);
setTimeout(function() {
  request.end();
}, 3000);

