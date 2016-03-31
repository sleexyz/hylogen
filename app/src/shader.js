module.exports = `
precision mediump float;
uniform float time;
uniform vec3 mouse;
const float PI = 3.141592653589793238462643383; 
varying vec3 uv;

void main() {
    gl_FragColor = vec4((cos(((uv.x * pow(tan((time / 10.0)), 10.0)) * 100.0)) * sin(((uv.y * pow(tan((time / 10.0)), 10.0)) * 100.0))), (cos(((uv.x * pow(tan((time / 10.0)), 10.0)) * 100.0)) * sin(((uv.y * pow(tan((time / 10.0)), 10.0)) * 100.0))), (cos(((uv.x * pow(tan((time / 10.0)), 10.0)) * 100.0)) * sin(((uv.y * pow(tan((time / 10.0)), 10.0)) * 100.0))), 1.0);
}
`;