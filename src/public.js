
import React from "react";
import ReactDOM from "react-dom";

import Program from "./Program";
import Audio from "./Audio";

const initialFsSource = `
void main() {
  gl_FragColor = vec4(audio.x, 1.0, 1.0, 1.0);
}
`;

const App = React.createClass({
  render: function() {
    return (
      <div id="entry">
        <div id="banner">
          <div id="title">Hylogen</div>
        </div>
        <div className="page grad"/>
        <div className="text">
          <p>
        <i className="cool">Hylogen</i> is a domain-specific language embedded in Haskell for writing fragment shaders, designed to be ergonomic for live coding.
        </p>

        <br/>
        <p className="right">
        [<a href="https://github.com/sleexyz/hylogen">github</a>]
        </p>
        <p className="right">
        [<a href="https://hackage.haskell.org/package/hylogen">hackage</a>]
      </p>
        </div>
        <div className="programContainer">
          <Program startAnimating={true}
                  fsSource={initialFsSource}/>
        </div>
      </div>
    );
  }
});

ReactDOM.render(<App/>, document.getElementById("entry"));
