#pragma glslify: import("./lib/uniforms.glsl")

#ifndef SDF
#define SDF

float box(vec3 p, vec3 b){
  p = abs(p) - b;
  return min(0.,max(p.x, max(p.y, p.z))) + length(max(p, 0.));
}

float poly(vec3 p, int n, vec2 rh){
  mat2 rot = r2d(0.);
  float d = -1e5;
  for(int i=0; i<n; i++){
    rot *= r2d(TAU/float(n));
    d = max(d, dot(p.xz, rot*vec2(0., 1.)) - rh.x);
  }
  d = max(abs(p.y) - rh.y, d);
  return d;
}

float poly2D(vec2 xy, int n, float r){
  mat2 rot = r2d(0.);
  float d = -1e5;
  for(int i=0; i<n; i++){
    rot *= r2d(TAU/float(n));
    d = max(d, dot(xy, rot*vec2(0., 1.)) - r);
  }
  return d;
}

float torus(vec3 p, vec4 rr){
  float x = length(p.xz) - rr.x;
  float y = p.y;
  float th = atan(p.z, p.x);
  vec3 q = vec3(mod(th, .3) - .15, y, x);
  x = poly2D(r2d(th*.6)*q.yz, 5, rr.z);
  y = q.x;
  vec2 xy = vec2(x,y);
  return length(xy) - rr.w;
}

#endif
