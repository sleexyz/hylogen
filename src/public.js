
import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

import Chooser from "./Chooser";
import Audio from "./Audio";
import AudioControls from "./AudioControls.jsx";

const sources=[`
void main() {
    gl_FragColor = ( (true && (abs((uvN.x + (-(1.0 / 100.0)))) < (1.0 / 100.0))) ? vec4(fract(((audio.z * uv().y) * 4.0)),fract(((audio.z * uv().y) * 4.0)),fract(((audio.z * uv().y) * 4.0)),1.0) : texture2D(backBuffer, ((vec2(((cos((audio.x * 2.0e-2)) * (vec2(vec2(abs(uvN.x),uvN.y).x,(vec2(abs(uvN.x),uvN.y).y * (0.8 * pow((1.1 / 0.8), ((audio.x + (-0.0)) / (1.0 + (-0.0))))))) + (-vec2(((1.0 / 100.0) * sign(uvN.x)),0.0))).x) + (sin((audio.x * 2.0e-2)) * (vec2(vec2(abs(uvN.x),uvN.y).x,(vec2(abs(uvN.x),uvN.y).y * (0.8 * pow((1.1 / 0.8), ((audio.x + (-0.0)) / (1.0 + (-0.0))))))) + (-vec2(((1.0 / 100.0) * sign(uvN.x)),0.0))).y)),((((-1.0) * sin((audio.x * 2.0e-2))) * (vec2(vec2(abs(uvN.x),uvN.y).x,(vec2(abs(uvN.x),uvN.y).y * (0.8 * pow((1.1 / 0.8), ((audio.x + (-0.0)) / (1.0 + (-0.0))))))) + (-vec2(((1.0 / 100.0) * sign(uvN.x)),0.0))).x) + (cos((audio.x * 2.0e-2)) * (vec2(vec2(abs(uvN.x),uvN.y).x,(vec2(abs(uvN.x),uvN.y).y * (0.8 * pow((1.1 / 0.8), ((audio.x + (-0.0)) / (1.0 + (-0.0))))))) + (-vec2(((1.0 / 100.0) * sign(uvN.x)),0.0))).y))) * vec2(0.5,0.5)) + vec2(0.5,0.5))));
}
`];



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
        <Chooser fsSources={sources}>
          <AudioControls/>
        </Chooser>
      </div>
    );
  }
});

ReactDOM.render(<App/>, document.getElementById("entry"));
