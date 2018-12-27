var sass = require('node-sass');
var path = require('path');
var fs = require("fs");

var sourceDir = path.join(__dirname, 'public', 'scss');
var destDir = path.join(__dirname, 'public', 'styles');

var scss_filename = sourceDir + "/main.scss";
var scss_dest = destDir + "/final.css";

sass.render({
  file: scss_filename,
  outFile: scss_dest
}, (error, result) => {
    if (!error) {
        fs.writeFile(scss_dest, result.css, (err) => {
            if(!err) {
                console.log("Success!");
            } else {
                console.log("fs error \n", err)
            }
        });
    } else {
        console.log("Node sass error\n", error);
    }
});
