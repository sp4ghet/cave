#pragma glslify: import("./lib/uniforms.glsl")

#ifndef UTIL
#define UTIL

float graph(float y, float f, float t){
  return smoothstep(f-t, f, y) - smoothstep(f, f+t, y);
}

#define lofi(p,m) floor(p*m)/m
#define r2d(t) mat2(cos(t),sin(t),-sin(t),cos(t))

void chmin(inout vec4 a, vec4 b){
  a = a.x < b.x ? a : b;
}

void chmax(inout vec4 a, vec4 b){
  a = a.x > b.x ? a : b;
}

float sq(float x){
  return x*x;
}

vec3 rotate(vec3 p, float angle, vec3 axis)
{
    vec3 a = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float r = 1.0 - c;
    mat3 m = mat3(
        a.x * a.x * r + c,
        a.y * a.x * r + a.z * s,
        a.z * a.x * r - a.y * s,
        a.x * a.y * r - a.z * s,
        a.y * a.y * r + c,
        a.z * a.y * r + a.x * s,
        a.x * a.z * r + a.y * s,
        a.y * a.z * r - a.x * s,
        a.z * a.z * r + c
    );
    return m * p;
}

vec3 grad(vec3 off, vec3 amp, vec3 fre, vec3 pha, float t){
  return off*.5 + 0.5 * amp * cos(TAU*(fre*t + pha));
}

vec2 uv2pt(vec2 uv){
  vec2 pt = (uv - .5) * 2.;
  pt.y *= resolution.y / resolution.x;
  return pt;
}

vec2 pt2uv(vec2 pt){
  pt.y *= resolution.x / resolution.y;
  pt += 1.;
  pt *= 0.5;
  return pt;
}

// x should be [0,1]
float getFFT(float x){
  x = x*x;
  float fft = texture(spectrum, vec2(x)).r;
  fft = .5 * log(fft+1.);
  return fft;
}

float gray(vec3 c){
  return c.x*0.299 + c.y*0.587 + c.z*0.114;
}

mat3 getOrthogonalBasis(vec3 lookAt){
    vec3 dir = normalize(lookAt);
    vec3 right = normalize(cross(vec3(0,1,0),dir));
    vec3 up = normalize(cross(dir, right));
    return mat3(right,up,dir);
}

float chi(vec3 a, vec3 b, float x){
  return max(x, dot(a,b));
}

vec2 kaleido(vec2 uv, int n){
  vec2 pt = uv2pt(uv);
  for(int i=0; i<n; i++){
    pt = abs(pt);
    pt *= r2d(PI/float(n));
    pt -= 0.03;
  }
  return pt2uv(pt);
}

#endif
