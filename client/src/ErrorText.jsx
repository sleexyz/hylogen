import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

// TODO: try out pure components?
export default React.createClass({
  propTypes: {
    text: PropTypes.string.isRequired
  },
  render() {
    return (<div className={"errorText"}>
            {this.props.text}
            </div>)
  }
});
