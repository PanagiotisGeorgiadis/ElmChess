// import './main.css';
// import '../dist/public/styles/styles.css'
// import "../public/styles/ChessBoard.css";
// import { Main } from './Main.elm';
//import registerServiceWorker from './registerServiceWorker';


let app = null;

let flags = Object.assign({
    playerColor: "White"
});

app = Elm.Main.fullscreen(flags);

// registerServiceWorker();
