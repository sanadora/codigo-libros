function random() {
    return new Promise(function(succeed, fail) {
	setTimeout(function() {
	    if (Math.random() > 0.5)
		succeed("Math.random() returned > 0.5");
	    else
		fail("No greater than 0.5");
	}, 1000);
    });
}

random().then(null).then(function(res){
    console.log(res);
});
