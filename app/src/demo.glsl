precision mediump float;
uniform float uTime;
uniform vec3 uCursor;
varying vec3 vPosition;
//computes a sphere of radius r
float computeZ(vec2 xy, float r) {
  // rr = xx + yy + zz
  float zz = r*r - xy.x * xy.x - xy.y * xy.y;
  if (zz < 0.) {
    return -1.;
  } else {
    return sqrt(zz);
  }
}
void main(void) {
  float x = vPosition.x;
  float y = vPosition.y;
  float z = computeZ(vPosition.xy, 1.0);
  float s = 0.0;
  if (z > 0.) {
    // start with dark shade
    s = 0.2;
    // add diffuse shading where surface faces the light
    float k =  0.25 * (sin(uTime) / 2. + 0.5) + 0.25;
    // dot the normal and light
    // lambertian shading from upper right
    vec3 n = vec3(x, y, z);
    vec3 l = vec3(uCursor.xy, 1.);
    s += k * max(0., dot(n, vec3(1., 1., 1.)));
    s += k * max(0., dot(n, l));
    float r = sqrt(abs(uCursor.x * uCursor.y));
    float dotprod = dot(n, l);
    // s *= sin(z * sin(z* sin(z * uTime) * uTime));
    s *= sin(sin(dotprod * (pow(2.0, 8.0  +  4.*(sin(uTime/1000.)))))* dotprod);
  }
  gl_FragColor = vec4(s * vec3(1.0, 1.0, 1.0), 1.0);
}