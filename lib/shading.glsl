#pragma glslify: import("./lib/uniforms.glsl")


#ifndef PBR
#define PBR
#pragma glslify: import("./lib/util.glsl")

// Eq 9.36 from Physically Based Rendering Vol 4.
vec3 diffuse(vec3 rho, vec3 f0, vec3 n, vec3 l, vec3 v){
  vec3 c = rho;

  float nl = max(0., dot(n,l)), nv = max(0., dot(n,v));

  c *= (21./20.)*(1. - pow(1.-nl,5.))*(1. - pow(1.-nv, 5.))*(1.- f0);
  return c;
}

float g1Smith(vec3 h, vec3 v, float roughness){
  float dp = dot(h,v);
  float d = step(0., dp);
  float tp = 1. / (dp*dp - 1.);
  return d / (.5 + .5 * sqrt(1. + sq(roughness) * sq(tp)));
}

float g2Naiive(vec3 l, vec3 v, vec3 h, float roughness){
  return g1Smith(h,l, roughness) * g1Smith(h, v, roughness);
}

// Eq 9.31 from Realtime Rendering
float g2smith(vec3 l, vec3 v, vec3 h, float roughness){
  float lh = dot(l,h), vh = dot(v,h);
  if(lh < 0. || vh < 0.) return 0.;

  float alpha = sq(roughness);
  float tanLH = 1. / (sq(lh) - 1.), tanVH = 1. / (sq(vh) - 1.);
  float lambdaL = sqrt(1. + alpha * sq(tanLH)), lambdaV = sqrt(1. + alpha * sq(tanVH));

  return 1. / (.5*lambdaL + .5*lambdaV);
}

float ggx(vec3 n, vec3 h, float roughness){
  float alpha = sq(roughness);
  float d = dot(n,h);
  float dp = step(0., d);
  float denom = PI * sq(1. + d*d * (alpha-1.));
  return d * alpha / denom;
}

vec3 schlick(vec3 h, vec3 l, vec3 f0){
  float d = max(0., dot(h,l));
  return f0 + (1. - f0) * pow(1. - d, 5.);
}

vec3 BRDF(vec3 v, vec3 l, vec3 n, float metalness, float roughness, vec3 rho){

  vec3 h = normalize(v + l);

  vec3 f0 = mix(vec3(0.02), rho, metalness);
  vec3 spec = schlick(h, l, f0);

  float mask = 1.;
  mask = g2smith(l,v,h,roughness);
  mask = g2Naiive(l,v,h,roughness);

  float ndf = ggx(n, h, roughness);

  vec3 fSpec = spec * mask * ndf;
  float kd = mix(1. - spec.r, 0., metalness);
  rho *= kd;

  vec3 diffuse = diffuse(rho, f0, n,  l,  v);
  float nlPlus = max(0., dot(n,l));
  return fSpec * nlPlus;
}

// Volumetric

float HenyeyGreenstein(float cosTheta, float g){
  float gsq = g*g;
  return (1. - gsq) / (4. * PI * pow(1. +  gsq  - 2.*g*cosTheta, 1.5));
}

#endif
