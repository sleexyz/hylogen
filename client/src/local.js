import React from "react";
import ReactDOM from "react-dom";

import Program from "./Program";
import AudioControls from "./AudioControls";
import ErrorText from "./ErrorText";

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
      bannerClass: "",
      error: false,
      errorText: ""
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
    const wsConn = new WebSocket("ws://localhost:8080/");

    wsConn.onopen = function() {
      console.log("websocket opened!");
    };

    wsConn.onclose = () => {
      console.log('websocket closed');
      window.setTimeout(this.connectToServer, 1000);
    };

    wsConn.onmessage = (m) => {
      const obj =  JSON.parse(m.data);
      if (obj.tag === "Code") {
        this.setState({
          fsSource: obj.contents,
          error: false,
          errorText: ""
        });
      } else if (obj.tag === "Err") {
        console.error(obj.contents);
        this.setState({
          error: true,
          errorText: obj.contents,
        });
      } else {
        console.error("unexpected error!");
      }
    };
  },
  toggleShow() {
    this.setState((prev) => {return {showControls: !prev.showControls};});
  },
  render: function() {
    const buttonText = this.state.showControls ? "hide" : "show";
    const audioControlClassName = this.state.showControls ? "unhiddenFast" : "hiddenFast";

    const error = this.state.error
      ?(<div><ErrorText text={this.state.errorText}/></div>)
      :(<div></div>);

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
        {error}
        <Program startAnimating={true}
      fsSource={this.state.fsSource}
      withOSC={true}/>
        </div>
    );
  }
});

ReactDOM.render(<App/>, document.getElementById("entry"));
