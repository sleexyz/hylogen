
const vsSource = `
attribute vec3 aPosition;
varying vec3 uv;
void main() {
  gl_Position = vec4(aPosition, 1.0);
  uv = aPosition;
}
`;

const initialFsSource = `
precision mediump float;
uniform float time;
uniform vec3 mouse;
const float PI = 3.141592653589793238462643383;
varying vec3 uv;

void main() {
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
`;


function loadProgram (gl, state, vsSource, fsSource) {

  // compileShader :: (gl, source, shaderType) -> Shader
  // throws Error on compilation error

  function compileShader (gl, source, shaderType) {
    //assert(shaderType === gl.FRAGMENT_SHADER || shaderType === g.VERTEXT_SHADER);

    let shader = gl.createShader(shaderType);

    gl.shaderSource(shader, source);
    gl.compileShader(shader);


    let success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
    if (!success) {
      throw "could not compile shader:" + gl.getShaderInfoLog(shader);
    }

    return shader;
  };



  let vs = compileShader(gl, vsSource, gl.VERTEX_SHADER);
  let fs = compileShader(gl, fsSource, gl.FRAGMENT_SHADER);


  let program = gl.createProgram();

  gl.attachShader(program, vs);
  gl.attachShader(program, fs);

  gl.linkProgram(program);

  let success = gl.getProgramParameter(program, gl.LINK_STATUS);
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

  // remember the address within the fragment shader of each of my uniforms variables
  gl.time = gl.getUniformLocation(program, "time");
  gl.mouse = gl.getUniformLocation(program, "mouse");

  draw(gl, state);

  if (state.animationFrameRequest === null) {
    state.animationFrameRequest = requestAnimationFrame(() => animate(gl, state));
  }
}

function draw (gl, state) {

  gl.uniform1f(gl.time, (new Date().getTime() / 1000 - state.time0));
  gl.uniform3f(gl.mouse, state.mouse.x, state.mouse.y, state.mouse.z);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

function animate (gl, state) {
  draw(gl, state);
  state.animationFrameRequest = requestAnimationFrame(() => animate(gl, state));
};



function setupState (state, canvas){
  function setMouse (event, z) {
    let r = event.target.getBoundingClientRect();
    state.mouse.x = (event.clientX - r.left) / (r.right - r.left) * 2 -1;
    state.mouse.y = (event.clientY - r.bottom) / (r.top - r.bottom) * 2 - 1;

    if (z !== undefined) {
      state.mouse.z = z;
    }
  };

  state.animationFrameRequest = null;

  state.time0 = new Date() / 1000;


  canvas.onmousedown = (event) => setMouse(event, 1);
  canvas.onmousemove = (event) => setMouse(event);
  canvas.onmouseup = (event) => setMouse(event, 0);

  state.mouse = {x: 0, y: 0, z: 0};

  // TODO: implement Audio
}




// stateful variables

const canvas = document.getElementById("canvas");
canvas.width = Math.max(window.innerHeight, window.innerWidth);
canvas.height = Math.max(window.innerHeight, window.innerWidth);

const gl = canvas.getContext("webgl");

const state = {};

setupState(state, canvas);


loadProgram (gl, state,  vsSource, initialFsSource);


function fadeOut(elem) {
  elem.className = "removing";
  window.setTimeout(function() {
    elem.remove();
  }, 1000);
}

function connectToHylogen() {
  const wsConn = new WebSocket("ws://localhost:8080/");

  wsConn.onopen = function() {
    const landing = document.getElementById("landing");
    if (landing) {
      fadeOut(document.getElementById("bg"));
      window.setTimeout(() => {fadeOut(landing);}, 500);
    }
    console.log('websocket opened');

  };

  wsConn.onclose = function() {
    console.log('websocket closed');
    window.setTimeout(connectToHylogen, 100);
  };

  wsConn.onmessage = function (m) {
    console.log(m.data);
    loadProgram(gl, state,  vsSource, m.data);
  };
}

connectToHylogen();

