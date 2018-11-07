import './main.css';
// import '../dist/public/styles/styles.css'
import "../public/styles/ChessBoard.css";
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';


let app = null;

let flags = Object.assign({
    playerType: "WhitePlayer"
});

// Main.embed(document.getElementById('root'));
app = Main.fullscreen(flags);

registerServiceWorker();
