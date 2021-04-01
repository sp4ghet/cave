#pragma glslify: import("./lib/uniforms.glsl")

#ifndef POST
#define POST

#pragma glslify: import("./lib/util.glsl")

float ascii(vec2 uv, float scale, sampler2D buf){
  vec2 st = floor(uv*resolution/scale) * scale / resolution;
  float val = gray(texture(buf, st).rgb);
  val = clamp(val, 0., 1.) - 1e-5;
  vec2 tuv = mod(uv*resolution, scale) / scale;
  tuv.x *= 0.125;
  tuv.x += floor(val*8.)*0.125;
  float res = 1. - texture(texFont, tuv).r;
  return res;
}

const float vk[9] = float[](1.,0.,-1.,2.,0.,-2.,1.,0.,-1.);
const float hk[9] = float[](1.,2.,1.,0.,0.,0.,-1.,-2.,-1.);
float sobel(vec2 uv, float width, sampler2D buf){
  vec2 xy = uv*resolution;

  float vert = 0.,
        horz = 0.;

  for(int y = -1; y<=1 + stop_unroll; y++){
     for(int x=-1; x<=1 + stop_unroll; x++){
      if(x==0 && y==0){
        continue;
      }
      vec3 samp = texture(buf, (xy + vec2(x,y)*width) / resolution).rgb;
      float s = gray(samp);
      int yy = y + 1, xx = x+1;
      vert += vk[yy*3 + xx] * s;
      horz += hk[yy*3 + xx] * s;
    }
  }
  return length(vec2(vert,horz));
}

// https://www.shadertoy.com/view/wsVSDd
vec3 ACESFilm(vec3 x)
{
    float a = 2.51;
    float b = 0.03;
    float c = 2.43;
    float d = 0.59;
    float e = 0.14;
    return (x*(a*x+b))/(x*(c*x+d)+e);
}

// glitch1
float fractSin(vec2 p)
{
    float t = floor(fract(time) * 120.) / 10.;
    return fract(sin(dot(p, vec2(t * 12.9898, t * 78.233))) * 43758.5453);
}

float blockNoise(vec2 uv, float blockiness)
{
    vec2 lv = fract(uv);
    vec2 id = floor(uv);
    id = floor(id);

    float n1 = fractSin(id);
    float n2 = fractSin(id+vec2(1,0));
    float n3 = fractSin(id+vec2(0,1));
    float n4 = fractSin(id+vec2(1,1));

    vec2 u = smoothstep(0.0, 1.0 + blockiness, lv);

    return mix(mix(n1, n2, u.x), mix(n3, n4, u.x), u.y);
}

float fbm(vec2 uv, int count, float blockiness, float complexity)
{
    float val = 0.0;
    float amp = 0.5;

    for(int i=0; i<count; i++)
    {
    	val += amp * blockNoise(uv, blockiness);
      amp *= 0.5;
      uv *= complexity;
    }

    return val;
}

vec2 glitchShift(vec2 uv, float amp, float mini){
  vec2 a = vec2(uv.x * (resolution.x/resolution.y), uv.y);
  vec2 uv2 = vec2(a.x / resolution.x, exp(a.y));
  vec2 id = floor(uv * (7. + 1. + fractSin(vec2(fract(time)))));
  vec2 id2 = floor(uv * 3. + 1.);
  int loops = int(fractSin(id) * fractSin(id2) * 4. + 1.);

  const float glitchNarrowness = 30.0;
  const float glitchBlockiness = 2.0;

  float shift = amp * pow(fbm(uv2, loops, glitchBlockiness, glitchNarrowness), mini);
  shift = smoothstep(0.00001, 1., shift);
  vec2 uvShift = vec2(shift, 0.);
  return uvShift;
}

vec3 glitch(vec2 uv, float amp, float mini, sampler2D buf){
  vec2 uvShift = glitchShift(uv, amp, mini);
  vec3 c = vec3(0);
  c.r = texture(buf, fract(uv + uvShift)).r;
  c.g = texture(buf, fract(uv - uvShift)).g;
  c.b = texture(buf, fract(uv - uvShift)).b;

  return c;
}

#endif
