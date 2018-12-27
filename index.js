const path = require("path");
const express = require("express");
const expressApp = express();
const http = require("http").Server(expressApp);


const INDEX_HTML_PATH = path.join(__dirname, "index.html");
const SCRIPTS_PATH = path.join(__dirname, "public", "scripts");
const STYLES_PATH = path.join(__dirname, "public", "styles");
const FONTAWESOME_BASE_PATH = path.join(__dirname, "public", "fonts", "fontawesome-pro-5.5.0-web");
const FONTAWESOME_CSS_PATH = path.join(FONTAWESOME_BASE_PATH, "css");
const FONTAWESOME_SVGS_PATH = path.join(FONTAWESOME_BASE_PATH, "svgs");


expressApp.use("/fontawesome", express.static(FONTAWESOME_CSS_PATH));
expressApp.use("/fontawesome-svg", express.static(FONTAWESOME_SVGS_PATH));
expressApp.use("/scripts", express.static(SCRIPTS_PATH));
expressApp.use("/styles", express.static(STYLES_PATH));

expressApp.get("/", (request, response) => {
    response.sendFile(INDEX_HTML_PATH);
});

http.listen(3004, function() {
    console.log("listening on port 3004");
});
