
var vsSource = [ "attribute vec3 aPosition;"
                 , "varying vec2 uvN;"
                 , "void main() {"
                 , "  gl_Position = vec4(aPosition, 1.0);"
                 , "  uvN = aPosition.xy;"
                 , "}"
               ].join("\n");

var fsHeader= [ "precision mediump float;"
                , "const float pi = 3.141592653589793238462643383;"
                , "uniform float time;"
                , "uniform vec2 mouse;"
                , "uniform vec2 resolution;"
                , "uniform vec4 audio;"
                , "uniform sampler2D backBuffer;"
                , "varying vec2 uvN;"
                , "vec2 uv() {"
                , "  return 0.5 * uvN  + 0.5;"
                , "}"
              ].join("\n");

var initialFsSource = [ "void main() {"
                        , "    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);"
                        , "}"
                      ].join("\n");


function loadProgram (gl, state, vsSource, fsSource) {

  // compileShader :: (gl, source, shaderType) -> Shader
  // throws Error on compilation error

  function compileShader (gl, source, shaderType) {
    //assert(shaderType === gl.FRAGMENT_SHADER || shaderType === g.VERTEXT_SHADER);

    var shader = gl.createShader(shaderType);

    gl.shaderSource(shader, source);
    gl.compileShader(shader);


    var success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
    if (!success) {
      console.log(source);
      throw "could not compile shader:" + gl.getShaderInfoLog(shader);
    }

    return shader;
  };



  var vs = compileShader(gl, vsSource, gl.VERTEX_SHADER);
  var fs = compileShader(gl, fsSource, gl.FRAGMENT_SHADER);


  var program = gl.createProgram();

  gl.attachShader(program, vs);
  gl.attachShader(program, fs);

  gl.linkProgram(program);

  var success = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (!success) {
    throw ("program failed to link:" + gl.getProgramInfoLog(program));
  }

  gl.useProgram(program);


  // Create a square as a strip of two triangles.
  gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([ -1,1,0, 1,1,0, -1,-1,0, 1,-1,0 ]), gl.STATIC_DRAW);


  // Assign attribute aPosition to each of the square's vertices.
  gl.aPosition = gl.getAttribLocation(program, "aPosition");
  gl.enableVertexAttribArray(gl.aPosition);
  gl.vertexAttribPointer(gl.aPosition, 3, gl.FLOAT, false, 0, 0);

  // backBuffer stuff
  function createTarget() {
    var target = {
      texture: gl.createTexture(),
      framebuffer: gl.createFramebuffer()
    };
    // set up framebuffer
    gl.bindTexture( gl.TEXTURE_2D, target.texture);
    gl.texImage2D( gl.TEXTURE_2D, 0,  gl.RGBA, WIDTH, HEIGHT, 0,  gl.RGBA,  gl.UNSIGNED_BYTE, null);

    gl.texParameteri( gl.TEXTURE_2D,  gl.TEXTURE_WRAP_S,  gl.CLAMP_TO_EDGE);
    gl.texParameteri( gl.TEXTURE_2D,  gl.TEXTURE_WRAP_T,  gl.CLAMP_TO_EDGE);

    gl.texParameteri( gl.TEXTURE_2D,  gl.TEXTURE_MAG_FILTER,  gl.NEAREST);
    gl.texParameteri( gl.TEXTURE_2D,  gl.TEXTURE_MIN_FILTER,  gl.NEAREST);

    gl.bindFramebuffer( gl.FRAMEBUFFER, target.framebuffer);
    gl.framebufferTexture2D( gl.FRAMEBUFFER,  gl.COLOR_ATTACHMENT0,  gl.TEXTURE_2D, target.texture, 0);

    // clean up
    gl.bindTexture( gl.TEXTURE_2D, null);
    gl.bindFramebuffer( gl.FRAMEBUFFER, null);

    return target;
  }
  state.fb[0] = createTarget();
  state.fb[1] = createTarget();

  // remember the address within the fragment shader of each of my uniforms variables
  gl.time = gl.getUniformLocation(program, "time");
  gl.mouse = gl.getUniformLocation(program, "mouse");
  gl.audio = gl.getUniformLocation(program, "audio");
  gl.resolution = gl.getUniformLocation(program, "resolution");
  gl.backBuffer = gl.getUniformLocation(program, "backBuffer");

  draw(gl, state);

  if (state.animationFrameRequest === null) {
    state.animationFrameRequest = requestAnimationFrame(function() {animate(gl, state);});
  }
}

function draw (gl, state) {
  gl.uniform1f(gl.time, (new Date().getTime() / 1000 - state.time0));
  gl.uniform2f(gl.mouse, state.mouse.x, state.mouse.y);
  gl.uniform2f(gl.resolution, WIDTH, HEIGHT);
  gl.uniform4f(gl.audio, state.audio.low, state.audio.mid, state.audio.upper, state.audio.high);

  gl.uniform1i(gl.backBuffer, 0); // Do I need to check for null?

  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, state.fb[state.bit].texture);
  state.bit = (state.bit + 1) % 2;
  gl.bindFramebuffer( gl.FRAMEBUFFER, state.fb[state.bit].framebuffer);
  gl.clear(gl.COLOR_BUFFER_BIT);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, state.fb[state.bit].texture);
  gl.bindFramebuffer( gl.FRAMEBUFFER, null);
  gl.clear(gl.COLOR_BUFFER_BIT);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

function animate (gl, state) {
  draw(gl, state);
  state.animationFrameRequest = requestAnimationFrame(function() { animate(gl, state);});
};



function setupState (state, canvas){
  state.animationFrameRequest = null;

  state.bit = 0
  state.fb = [null, null];

  state.time0 = new Date() / 1000;


  function setMouse (event) {
    var r = event.target.getBoundingClientRect();
    state.mouse.x = (event.clientX - r.left) / (r.right - r.left) * 2 -1;
    state.mouse.y = (event.clientY - r.bottom) / (r.top - r.bottom) * 2 - 1;
  };
  /* canvas.onmousedown = (event) => setMouse(event, 1); */
  /* canvas.onmouseup = (event) => setMouse(event, 0); */

  canvas.onmousemove = setMouse;
  state.mouse = {x: 0, y: 0};


  state.audio = {low: 0.0, mid: 0.0, upper: 0.0, high: 0.0};
  state.audioAgain = null;

  function onAccept (stream) {
    var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
    var source = audioCtx.createMediaStreamSource(stream);
    var analyser = audioCtx.createAnalyser();
    source.connect(analyser);

    analyser.fftSize = 512;
    var bufferLength = analyser.frequencyBinCount;
    var dataArray = new Uint8Array(bufferLength);


    function toAudio() {
      state.audioAgain = requestAnimationFrame(toAudio);
      analyser.getByteFrequencyData(dataArray);

      // Taken from The_Force
      var k = 0, f = 0.0;
      var a = 5, b = 11, c = 24, d = bufferLength, i = 0;

      for(; i < a; i++) {
        f += dataArray[i];
      }

      f *= .2; // 1/(a-0)
      f *= .003921569; // 1/255
      state.audio.low = f;

      f = 0.0;
      for(; i < b; i++) {
        f += dataArray[i];
      }

      f *= .166666667; // 1/(b-a)
      f *= .003921569; // 1/255
      state.audio.mid = f;

      f = 0.0;
      for(; i < c; i++) {
        f += dataArray[i];
      }

      f *= .076923077; // 1/(c-b)
      f *= .003921569; // 1/255
      state.audio.upper = f;

      f = 0.0;
      for(; i < d; i++) {
        f += dataArray[i];
      }
      f *= .00204918; // 1/(d-c)
      f *= .003921569; // 1/255
      state.audio.high = f;
    };

    toAudio();
  }
  function onFail(e) {
    console.error(e);
  }
  navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || navigator.msGetUserMedia;
  navigator.getUserMedia({audio: true}, onAccept, onFail);
}




// stateful variables

var canvas = document.getElementById("canvas");
var WIDTH = Math.max(window.innerHeight, window.innerWidth);
var HEIGHT =  Math.max(window.innerHeight, window.innerWidth);
canvas.width = WIDTH;
canvas.height = HEIGHT;

var gl = canvas.getContext("webgl");

var state = {};

setupState(state, canvas);


loadProgram (gl, state,  vsSource, fsHeader + "\n" + initialFsSource);


function fadeOut(elem) {
  elem.className = "removing";
  window.setTimeout(function() {
    elem.remove();
  }, 1000);
}

function connectToHylogen() {
  var wsConn = new WebSocket("ws://localhost:8080/");

  wsConn.onopen = function() {
    var landing = document.getElementById("landing");
    if (landing) {
      fadeOut(document.getElementById("bg"));
      window.setTimeout(function() {fadeOut(landing);}, 500);
    }
    console.log('websocket opened');

  };

  wsConn.onclose = function() {
    console.log('websocket closed');
    window.setTimeout(connectToHylogen, 100);
  };

  wsConn.onmessage = function (m) {
    console.log(m.data);
    loadProgram(gl, state,  vsSource, fsHeader + "\n" + m.data);
  };
}

connectToHylogen();
