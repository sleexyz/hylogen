var SoundCloudAudio = require("soundcloud-audio");

let idCounter = 0;
let callbacks = [];

let audioCtx = new (window.AudioContext || window.webkitAudioContext)();
let analyser = audioCtx.createAnalyser();
let source = null;

analyser.fftSize = 512;
let bufferLength = analyser.frequencyBinCount;
let dataArray = new Uint8Array(bufferLength);
let audioAgain = null;
let bands = {low: 0.0, mid: 0.0, upper: 0.0, high: 0.0};


let scPlayer = null;

let keepPlaying = true;
let mode = null;

let cleanup = function() {};

const video = document.createElement("video");
window.video = video;
video.muted = true;
video.playing = false;


function onAcceptAudio () {
  source.connect(analyser);

  function toAudio() {
    if (keepPlaying) {
      audioAgain = requestAnimationFrame(toAudio);
    }
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
    mode = "usermedia";
    cleanup();
    keepPlaying = true;

    navigator.getUserMedia = navigator.getUserMedia
      || navigator.webkitGetUserMedia
      || navigator.mozGetUserMedia
      || navigator.msGetUserMedia;

    navigator.getUserMedia({audio: true, video: {width : 1280, height: 720}}, function(stream) {

      video.src = window.URL.createObjectURL(stream);
      video.onloadedmetadata = function(e) {
        video.play();
        video.playing = true;
      };

      source = audioCtx.createMediaStreamSource(stream);
      onAcceptAudio();
    }, (e) => {console.error(e);});
    cleanup = function() {
      keepPlaying = false;
      source.disconnect(analyser);
    };
  },
  initializeAudioSoundCloud: async function(url, initPlaying) {
    mode = "sc";
    cleanup();
    keepPlaying = true;

    const {accessToken} = await(await fetch("https://sleexyz-sc_access_token.web.val.run/")).json();
    console.log(accessToken);


    let scPlayer = this.scPlayer = new SoundCloudAudio(accessToken);
    scPlayer.audio.crossOrigin = "anonymous";

    scPlayer.resolve(url, function(track) {
      if (mode !== "sc") {
        return;
      }
      scPlayer.audio.crossOrigin = "anonymous";

      // console.log(track);
      if (initPlaying) {
        scPlayer.play();
      }
    });

    source = audioCtx.createMediaElementSource(scPlayer.audio);
    source.connect(audioCtx.destination);
    onAcceptAudio();

    cleanup = function() {
      keepPlaying = false;
      scPlayer.stop();
      source.disconnect(analyser);
    };
  },
  addCallback: function(fn) {
    let i = idCounter;
    idCounter += 1;

    callbacks[i] = fn;
    return i;
  },
  removeCallback: function(i) {
    callbacks[i] = null;
  },
  video: video
};
