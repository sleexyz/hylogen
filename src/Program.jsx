import React, {PropTypes} from "react";
import ReactDOM from "react-dom";
import Audio from "./Audio";


const vsSource = ` attribute vec3 aPosition;
varying vec2 uvN;
void main() {
  gl_Position = vec4(aPosition, 1.0);
  uvN = aPosition.xy;
}`;

const fsHeader = `precision mediump float;
const float pi = 3.141592653589793238462643383;
uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;
uniform vec4 audio;
uniform sampler2D backBuffer;
varying vec2 uvN;
vec2 uv() {
  return 0.5 * uvN  + 0.5;
}`;

export default React.createClass({
  propTypes: {
    vsSource: PropTypes.string,
    fsSource: PropTypes.string.isRequired,
    width: PropTypes.number,
    height: PropTypes.number,
    animation: PropTypes.bool
  },
  getDefaultProps() {
    return {
      width: Math.max(window.innerHeight, window.innerWidth),
      height: Math.max(window.innerHeight, window.innerWidth),
      vsSource: vsSource,
      animation: true
    };
  },
  componentDidMount() {
    let canvas = this.canvas;
    let gl = this.gl = canvas.getContext("webgl");

    let state = this.state_ = {};
    state.animationFrameRequest = null;
    state.bit = 0;
    state.fb = [null, null];
    state.time0 = new Date() / 1000;
    state.audioCallback = null;


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

    state.audioCallback = Audio.addCallback(function(bands) {
      state.audio.low = bands.low;
      state.audio.mid = bands.mid;
      state.audio.upper = bands.upper;
      state.audio.high = bands.high;
    });

    this.loadProgram();
  },
  loadProgram() {
    let gl = this.gl;
    let state = this.state_;
    let WIDTH = this.props.width;
    let HEIGHT = this.props.height;

    // compileShader :: (gl, source, shaderType) -> Shader
    // throws Error on compilation error

    function compileShader (gl, source, shaderType) {
      // assert(shaderType === gl.FRAGMENT_SHADER || shaderType === g.VERTEXT_SHADER);

      let shader = gl.createShader(shaderType);

      gl.shaderSource(shader, source);
      gl.compileShader(shader);


      let success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
      if (!success) {
        console.log(source);
        throw "could not compile shader:" + gl.getShaderInfoLog(shader);
      }

      return shader;
    };



    let vs = compileShader(gl, this.props.vsSource, gl.VERTEX_SHADER);
    let fs = compileShader(gl, fsHeader + "\n" + this.props.fsSource, gl.FRAGMENT_SHADER);

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

    // backBuffer stuff
    function createTarget() {
      let target = {
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


    this.draw();

    if (this.props.animation) {
      if (state.animationFrameRequest === null) {
        //INVARIANT: afr is non-null if we are animating.

        state.animationFrameRequest = requestAnimationFrame(this.animate);
      }
    }
  },
  draw() {
    let gl = this.gl;
    let state = this.state_;

    gl.uniform1f(gl.time, (new Date().getTime() / 1000 - state.time0));
    gl.uniform2f(gl.mouse, state.mouse.x, state.mouse.y);
    gl.uniform2f(gl.resolution, this.props.width, this.props.height);
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
  },
  animate() {
    this.draw();
    this.state_.animationFrameRequest = requestAnimationFrame(this.animate);
  },
  componentDidUpdate() {
    if (!this.props.animation) {
      cancelAnimationFrame(this.state_.animationFrameRequest);
      this.state_.animationFrameRequest = null;
    }

    this.loadProgram();
  },
  componentWillUnmount() {
    cancelAnimationFrame(this.state_.animationFrameRequest);
    Audio.removeCallback(this.state_.audioCallback);
  },
  render() {

    return (
      <canvas ref={(ref) => this.canvas = ref}
              className={"program"}
              width={this.props.width}
              height={this.props.height}/>
    );
  }
});

// TODO: fix animation stop
