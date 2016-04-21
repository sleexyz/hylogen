import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

import Program from "./Program";


export default React.createClass({
  propTypes: {
    fsSources: PropTypes.arrayOf(PropTypes.string).isRequired
  },
  getInitialState: function() {
    return {
      i: 0
    };
  },
  next: function() {
    this.setState((prev) => {
      return {i: (prev.i + 1) % this.props.fsSources.length};
    });
  },
  prev: function() {
    this.setState((prev) => {
      return {i: (prev.i - 1) % this.props.fsSources.length};
    });
  },
  render: function() {
    let source = this.props.fsSources[this.state.i];

    return (
      <div className="programContainer">
        <div className="programContainerInner"
             onClick={this.next}>
          <Program startAnimating={true}
                   fsSource={source}/>
        </div>
        {this.props.children}
      </div>
    );
  }
});
