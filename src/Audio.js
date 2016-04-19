let idCounter = 0;
let callbacks = [];



function onAccept (stream) {
  let audioCtx = new (window.AudioContext || window.webkitAudioContext)();
  let source = audioCtx.createMediaStreamSource(stream);
  let analyser = audioCtx.createAnalyser();
  source.connect(analyser);

  analyser.fftSize = 512;
  let bufferLength = analyser.frequencyBinCount;
  let dataArray = new Uint8Array(bufferLength);
  let audioAgain = null;
  let bands = {low: 0.0, mid: 0.0, upper: 0.0, high: 0.0};


  function toAudio() {
    audioAgain = requestAnimationFrame(toAudio);
    analyser.getByteFrequencyData(dataArray);

    // Taken from The_Force
    let k = 0, f = 0.0;
    let a = 5, b = 11, c = 24, d = bufferLength, i = 0;

    for (; i < a; i++) {
      f += dataArray[i];
    }

    f *= .2; // 1/(a-0)
    f *= .003921569; // 1/255
    bands.low = f;

    f = 0.0;
    for (; i < b; i++) {
      f += dataArray[i];
    }

    f *= .166666667; // 1/(b-a)
    f *= .003921569; // 1/255
    bands.mid = f;

    f = 0.0;
    for (; i < c; i++) {
      f += dataArray[i];
    }

    f *= .076923077; // 1/(c-b)
    f *= .003921569; // 1/255
    bands.upper = f;

    f = 0.0;
    for (; i < d; i++) {
      f += dataArray[i];
    }
    f *= .00204918; // 1/(d-c)
    f *= .003921569; // 1/255
    bands.high = f;

    for (let i = 0; i < callbacks.length; i++) {
      callbacks[i](bands);
    }
  };


  toAudio();
}


export default {
  initializeAudioUserMedia: function() {

    navigator.getUserMedia = navigator.getUserMedia
      || navigator.webkitGetUserMedia
      || navigator.mozGetUserMedia
      || navigator.msGetUserMedia;

    navigator.getUserMedia({audio: true}, onAccept, (e) => {console.error(e);});
  },
  initializeAudioSoundcloud: function() {
    //TODO: implement
  },
  addCallback: function(fn) {
    let i = idCounter;
    idCounter += 1;

    callbacks[i] = fn;
    return i;
  },
  removeCallback: function(i) {
    callbacks[i] = null;
  }
};



