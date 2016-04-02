let animationFrameRequest = null;
let animating = true;

let time0 = require("./blah");

let canvas = document.getElementById("canvas");

canvas.width = Math.max(window.innerHeight, window.innerWidth);
canvas.height = Math.max(window.innerHeight, window.innerWidth);

let gl = canvas.getContext("webgl");


const setMouse = (event, z) => {
  let r = event.target.getBoundingClientRect();
  gl.cursor.x = (event.clientX - r.left) / (r.right - r.left) * 2 -1;
  gl.cursor.y = (event.clientY - r.bottom) / (r.top - r.bottom) * 2 - 1;

  if (z !== undefined) {
    gl.cursor.z = z;
  }
};

canvas.onmousedown = (event) => setMouse(event, 1);
canvas.onmousemove = (event) => setMouse(event);
canvas.onmouseup = (event) => setMouse(event, 0);
gl.cursor = {x: 0, y: 0, z: 0};

// compileShader :: (gl, source, shaderType) -> Shader
// @throws Error on compilation error
const compileShader = (gl, source, shaderType) => {
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

const createProgram = (gl, vs, fs) => {

  let program = gl.createProgram();

  gl.attachShader(program, vs);
  gl.attachShader(program, fs);

  gl.linkProgram(program);

  let success = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (!success) {
    throw ("program failed to link:" + gl.getProgramInfoLog(program));
  }
  return program;
};

const loadProgram = (gl, vsSource, fsSource) => {
  let vs = compileShader(gl, vsSource, gl.VERTEX_SHADER);
  let fs = compileShader(gl, fsSource, gl.FRAGMENT_SHADER);


  let program = createProgram(gl, vs, fs);
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

  draw(gl);

  if (animating) {
    // console.log(animationFrameRequest);
    if (animationFrameRequest === null) {
        //INVARIANT: afr is non-null if we are animating.
      animationFrameRequest = requestAnimationFrame(() => animate(gl));
    }
  }
}

const draw = (gl) => {
  gl.uniform1f(gl.time, (new Date().getTime() / 1000 - time0));
  gl.uniform3f(gl.mouse, gl.cursor.x, gl.cursor.y, gl.cursor.z);
  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

const animate = (gl) => {
  draw(gl);
  animationFrameRequest = requestAnimationFrame(() => animate(gl));
};


let vsSource = `
attribute vec3 aPosition;
varying vec3 uv;
void main() {
  gl_Position = vec4(aPosition, 1.0);
  uv = aPosition;
}
`;


let fsSource = require("./shader.js");

loadProgram (gl, vsSource, fsSource);


module.hot.accept((err) => {console.error(err);});
