require("expose?osc!osc/dist/osc-browser.min.js");

// singleton osc port

const oscPort = new osc.WebSocketPort({
  url: "ws://localhost:9091"
});

oscPort.open();

export default oscPort;
