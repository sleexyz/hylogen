precision mediump float;
uniform float time;
uniform vec3 mouse;
const float PI = 3.141592653589793238462643383; 
varying vec3 uv;

void main() {
    gl_FragColor = vec4(fract(((sqrt((pow(uv.x, 2.0) + pow(uv.y, 2.0))) * 10.0) + time)), (fract(((sqrt((pow(uv.x, 2.0) + pow(uv.y, 2.0))) * 10.0) + time)) * 0.2), (fract(((sqrt((pow(uv.x, 2.0) + pow(uv.y, 2.0))) * 10.0) + time)) * 0.5), 1.0);
}
`;