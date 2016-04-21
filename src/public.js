
import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

import Chooser from "./Chooser";
import Audio from "./Audio";
import AudioControls from "./AudioControls.jsx";

import sources from "./Sources";



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
