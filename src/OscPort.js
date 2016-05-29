require("expose?osc!osc/dist/osc-browser.min.js");

export default new function() {
  this.singleton = undefined;
  this.getInstance = function() {
    if (this.singleton === undefined ) {
      this.singleton = new osc.WebSocketPort({
        url: "ws://localhost:9091"
      });
      this.singleton.open();
    }
    return this.singleton;
  }.bind(this);
}
