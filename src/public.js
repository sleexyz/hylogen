
import React from "react";
import ReactDOM from "react-dom";

import Program from "./Program";
import Audio from "./Audio";

const initialFsSource = `
void main() {
  gl_FragColor = vec4(sin(time) * 0.5 + 0.5, 1.0, 1.0, 1.0);
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
            <a href="https://github.com/sleexyz/hylogen"><i className="cool">hylogen</i></a> is domain-specific language embedded in Haskell for writing fragment shaders.
          </p>
          <br/>
          <p>
            It is designed to be
          </p>
          <ul>
            <li>easy, concise, and ergonomic for live coding</li>
            <li>close to both GLSL and plain Haskell</li>
          </ul>
        </div>
        <div id="ProgramContainer">
          <Program startAnimating={true}
                  fsSource={initialFsSource}/>
        </div>
      </div>
    );
  }
});

ReactDOM.render(<App/>, document.getElementById("entry"));
