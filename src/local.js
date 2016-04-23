import React from "react";
import ReactDOM from "react-dom";

import Program from "./Program";
import AudioControls from "./AudioControls";

const initialFsSource = `
void main() {
  gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
`;

const App = React.createClass({
    getInitialState: function() {
        return {
            fsSource: initialFsSource,
            showControls: false,
            bannerClass: ""
        };
    },
    componentWillMount: function() {
        this.connectToServer();
    },
    componentDidMount: function() {
        window.setTimeout(() => {
            this.setState({bannerClass:"hidden"});
        }, 3000);
    },
    connectToServer: function() {
        var wsConn = new WebSocket("ws://localhost:8080/");

        wsConn.onopen = function() {
            console.log("websocket opened!");
        };

        wsConn.onclose = () => {
            console.log('websocket closed');
            window.setTimeout(this.connectToServer, 1000);
        };

        wsConn.onmessage = (m) => {
            console.log(m.data);
            this.setState({
                fsSource: m.data
            });
        };
    },
    toggleShow() {
        this.setState((prev) => {return {showControls: !prev.showControls};});
    },
    render: function() {
        let buttonText = this.state.showControls ? "hide" : "show";
        let audioControlClassName = this.state.showControls ? "unhiddenFast" : "hiddenFast";
        return (
              <div id="entry">
                <div id="banner" className={this.state.bannerClass}>
                  <div id="title">Hylogen</div>
                </div>
                <button onClick={this.toggleShow} id="hideButton">
                {buttonText}
                </button>
                <div className={audioControlClassName}>
                <AudioControls initialState="usermedia"/>
                </div>
                <Program startAnimating={true}
                         fsSource={this.state.fsSource}/>
              </div>
        );
    }
});

ReactDOM.render(<App/>, document.getElementById("entry"));




// TODO: add hylogen landing screen
// TODO:
