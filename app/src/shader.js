module.exports = `
precision mediump float;

uniform float uTime;
uniform vec3 uCursor;
varying vec3 vPosition;

void main() {
    gl_FragColor = vec4(0.9, sin(1.0), 0.2, 1.0);
}
`;