
import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

import Audio from "./Audio";
import AudioControls from "./AudioControls.jsx";

import sources from "./Sources";
import Program from "./Program";



const App = React.createClass({
  propTypes: {
    fsSources: PropTypes.arrayOf(PropTypes.string).isRequired
  },
  getInitialState: function() {
    return {
      i: 1
    };
  },
  next: function() {
    this.setState((prev) => {
      return {i: (prev.i + 1) % this.props.fsSources.length};
    });
  },
  render: function() {
    let source = this.props.fsSources[this.state.i];
    return (
      <div id="entry">
        <div id="banner">
          <div id="title">Hylogen</div>
        </div>
        <div className="page trans"
      style={{height:"66vh"}}/>
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
        <div className="page gradRev">
        </div>
        <div className="page trans"> </div>
        <div className="page trans"> </div>
        <button className="chooserNextButton"
              onClick={this.next}>[next]</button>
        <div className="programContainer">
        <div className="programContainerInner">
        <Program startAnimating={true}
      fsSource={source}/>
        </div>
        </div>
        <AudioControls/>
      </div>
    );
  }
});

ReactDOM.render(<App fsSources={sources}/>, document.getElementById("entry"));
