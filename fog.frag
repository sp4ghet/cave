#version 300 es
/*{
"PASSES": [{
        TARGET: "render",
        FLOAT: true,
    },
    {
      TARGET: "postrender",
      FLOAT: true
    },
    {
    }
    ]
}*/

#define ENTRY
#define ANGLE_loops 0

precision highp float;
out vec4 out_color;

uniform vec2 resolution;
uniform float time;
uniform float volume;

uniform sampler2D backbuffer;
uniform sampler2D samples;
uniform sampler2D spectrum;
uniform sampler2D midi;
uniform sampler2D note;
uniform sampler2D camera;

uniform sampler2D texNoise;
uniform sampler2D texDither;
uniform sampler2D texFont;

uniform sampler2D texJSP;
uniform sampler2D texDots;
uniform sampler2D noireW;
uniform sampler2D texCave;
uniform sampler2D texZenith;
uniform sampler2D spin;

uniform int	PASSINDEX;
uniform sampler2D render;
uniform sampler2D postrender;

uniform int stop_unroll;

const float PI = 3.14159265;
const float TAU = 2. * PI;

#pragma glslify: import("./lib/util.glsl")
#pragma glslify: import("./lib/sdf.glsl")
#pragma glslify: import("./lib/post.glsl")
#pragma glslify: import("./lib/midi.glsl")
#pragma glslify: import("./lib/rng.glsl")

vec2 seed;

float noise(vec3 p){
    float noise = 0.;

    vec3 seed = vec3(-4. ,-2.,0.5);

    float amp = 1.;
    float gain = 0.5;
    float lacunarity = 1.4;

    float warp = 1.3;
    float warpTrk = .7;
    float warpTrkGain = 1.5;

    mat3 rotMatrix = getOrthogonalBasis(seed);

    for(int i = 0; i < 4 + stop_unroll; i++){
        // Some domain warping. Similar to fbm.
        p += sin(p.zxy*warpTrk)*warp;
        // Calculate some noise value.
        noise += sin(dot(cos(p), sin(p.zxy )))*amp;

        p *= rotMatrix;
        p *= lacunarity;

        warpTrk *= warpTrkGain;
        amp *= gain;
    }

    return noise*.5;
}

const vec3 loopAxis = normalize(vec3(.3, .7, 0));

float ibox(vec3 p, vec3 b){
  p = abs(p) - b;
  float d = min(0., max(p.x, max(p.y,p.z))) + length(max(p, 0.));
  return -d;
}

vec4 map(vec3 q){
  vec3 p = q;
  vec4 d = vec4(1e7, 0., 0., 0.);

  // p=q;
  // p.xy *= r2d(time*.2);
  // p.yz *= r2d(-time*.3);
  // vec3 bdim = vec3(1.3);
  // float bx = box(p, bdim);
  // vec2 cdim = vec2(bdim.x*.85, bdim.x*2.);
  // float crs = box(p, cdim.xxy);
  // crs = min(crs, box(p, cdim.yxx));
  // crs = min(crs, box(p, cdim.xyx));
  // bx = max(bx, -crs);
  // chmin(d, vec4(bx, 0., 1., 0.));
  //
  // p = q;
  // bx = length(p) - .5;
  // chmin(d, vec4(bx, 1., 2., 0.));
  //
  // p = q;
  // p = rotate(p, time*.05, loopAxis);
  // p.xz *= r2d(time*.3);
  // float tr = torus(p, vec4(3., .3, .6, .03));
  // chmin(d, vec4(tr, 0., 1., 0.));
  //
  // p = q;
  // p.xz *= r2d(TAU*volume);
  // p.xy *= r2d(.5*TAU*volume);
  //
  // float trirad = .7;
  // float tri = poly(p, 3, vec2(trirad, 0.01));
  // tri = max(tri, -poly(p, 3, vec2(trirad*.9, .05)));
  // chmin(d, vec4(tri, 0., 100., 0.));
  //
  // d.x *= 0.9;

  // p = q;
  // p.xz *= r2d(PI* .1 * sin(.1*p.z - time));
  // p.xy = vec2(15.*atan(p.x, p.y)/PI, 5. - length(p.xy));
  // vec2 uv = mod(vec2(p.x, q.z - time*2.)/30., 1.) + .25;
  // float disp = texture(texNoise, fract(uv)).r;
  // disp += 0.1*texture(texNoise, fract(3.*uv + disp)).r;
  // p.y += disp;
  // float pl = dot(vec3(0.,1.,0.), p);
  // vec4 pld = vec4(pl, 2., 1., 0.);
  // chmin(d, pld);

  p = q;
  p -= vec3(0., 2., 0.);
  p += 0.05*noise(p);
  vec3 rdim = vec3(5., 5., 12.);
  float room = ibox(p, rdim) + 3.;
  chmin(d, vec4(room, 1., 1., 0.));

  return d;
}

float HenyeyGreenstein(float cosTheta, float g){
  float gsq = g*g;
  float x = 1. +  gsq  - 2.*g*cosTheta;
  return (1. - gsq) / (4. * PI * pow(abs(x), 1.5));
}

float mapVolume(vec3 p){
  float d = 0.;
  d = noise(p+vec3(0.,0.,-2.*time));

  return max(0.04, d*.25);
}

vec3 calcNormal(vec3 p, vec2 d){
  //
  vec4 n1 = vec4(0.0);
  for( int i=min(0,stop_unroll); i<4; i++ )
  {
      vec4 s = vec4(p, 0.0);
      s[i] += d.y;
      n1[i] = map(s.xyz).x;
      if( n1.x+n1.y+n1.z+n1.w>100.0 ) break;
  }
  vec3 n = normalize(n1.xyz-n1.w);
  return n;
}


vec4 post(vec2 uv){
  vec2 pt = uv2pt(uv);
  vec2 offset = 0.05 * pt * dot(pt,pt) * slider(2);
  float r = texture(render, uv - offset).r;
  float g = texture(render, uv ).g;
  float b = texture(render, uv + offset).b;
  vec3 c = vec3(r,g,b);

  float sob = sobel(uv, 2., render);
  c = mix(c, 10.*sob*c, slider(3));

  float scale = 256. * slider(5);
  if(scale > 1.){
      c *= ascii(uv, scale, render);
  }

  vec3 off = vec3(0.5,0.5,0.5),
  amp = vec3(0.5,0.5,0.5),
  fre = vec3(1.0,0.7,0.4),
  pha = vec3(0.0,0.15,0.20)+.6;
  c *= 0.5 + 0.5*grad(off, amp, fre, pha, .25 * (uv.x+uv.y));

  c = pow(abs(c), vec3(.4545));

  return vec4(c,1.);
}

vec2 jitterY(in vec2 uv, float amount){
  float y = lofi(uv.y, 400.) + fract(time);
  uv.x += amount*random(vec2(y));
  return uv;
}

vec2 scrollY(in vec2 uv, float mult, float speed){
  uv.y = fract(mult*uv.y + fract(mult*time*speed));
  return uv;
}

vec4 glitches(vec2 uv){
  uv = jitterY(uv, 0.05*slider(4)*stlap(8));
  uv = scrollY(uv, 1. + slider(7)*3., slider(6)*5.);
  vec3 c = glitch(uv, 0.5 * stlap(9), 7.*(1.1 - slider(1)), postrender);

  return vec4(c, 1.);
}

vec3 mask(vec2 uv, vec3 cur_col){
  vec2 pt = uv2pt(uv);
  vec3 c = vec3(0.);
  float noise = texture(texNoise, uv).r;
  noise = texture(texNoise, fract(uv + noise + 0.2*time)).r;
  noise = pow(noise, .4545);

  vec2 logoUV = r2d(-PI*.25) * pt + 1.;
  if(knob(2,4) > 0.1){
    float velC4 = texture(note, vec2(60./128., 0.)).x;
    float velD4 = texture(note, vec2(61./128., 0.)).x;
    logoUV *= 2.*resolution/vec2(1050.,146.);
    logoUV = pt2uv(logoUV);
    float cutoff = floor(logoUV.y);
    logoUV = mod(logoUV, vec2(1.25, 2.));
    float mask = texture(noireW, logoUV).r;
    float cutMask = mod(time*15., 29.);
    c += mask*step(cutoff, cutMask);
  }

  if(knob(2,5) > 0.1){
    logoUV = pt2uv(pt * 1.5 * resolution/vec2(1050., 146.));
    c += texture(noireW, logoUV).r;
  }

  if(knob(2,6) > 0.1){
    vec2 caveUV = pt2uv(pt * 2.5 * vec2(1., resolution.y/resolution.x));
    c += texture(texCave, caveUV).a;
  }

  if(knob(2,7) > 0.1){
    vec2 zenUV = pt2uv(pt*1.5*vec2(1., resolution.y/resolution.x));
    c += texture(texZenith, zenUV).a * texture(texZenith, zenUV).r;
  }

  if(knob(1,0) > 0.1){
    float samp = texture(samples, uv.xy).r - .5;
    c += step(.45, length(pt))* graph(samp*.5, (pt.y), 0.05);
  }

  if(knob(1,1) > 0.1){
    float samp = texture(samples, uv.xy).r - .5;
    c += graph(samp*.5, (pt.y), 0.05);
  }

  if(knob(1,2) > 0.1){
    float rad = .8*abs(atan(pt.x, -pt.y)) / PI;
    float spec = getFFT(rad);
    c += graph(0., length(pt)*10. - 4.5, 2.*spec);
  }

  if(knob(2,3) > 0.1){
    c += texture(texDots, fract(pt2uv(pt/(fract(time*.1)+1.))*resolution/vec2(1024.))).rgb;
  }

  // c *= noise;
  c = clamp(c, 0., 1.);
  float t = step(0.05,slap(0)+slap(1));
  c = mix(c, 1.-c, 1.-t);
  c *= cur_col;

  return c;
}

void main(){
  vec2 uv = gl_FragCoord.xy / resolution;
  vec2 pt = uv2pt(uv);
  seed = vec2(1., .3) * noise(vec3(uv*10., time));

  if(PASSINDEX == 1){
    out_color = post(uv);
    return;
  }
  if(PASSINDEX == 2){
    out_color = glitches(uv);
    return;
  }

  vec3 c = vec3(1.);

  float cameraShake = slider(0);
  vec2 uvJitter = (20.*slap(10) + cameraShake)*vec2(random_LCG(), random_LCG());
  uv += uvJitter*.01;
  uv += 0.3*stlap(11)*noise(vec3(uv*4.,time));
  for(int i=4; i<8; i++){
    if(stlap(i) > 0.1){
      uv = kaleido(uv, i);
    }
  }
  pt = uv2pt(uv);

  float ct = volume;
  ct = time*.01;
  vec3 jitter = .3 * vec3(cos(ct*30.), sin(70.*ct), cos(ct));
  vec3 ro = vec3(0);
  vec3 up = vec3(0,1,0);
  vec3 focus = vec3(0);

  ro += vec3(0., 3., 10.) + jitter;
  // ro += vec3(4., 0., 0.) + jitter.zxy;


  // ct = time*.5;
  // vec3 orbit = vec3(cos(ct), 0., sin(ct))*2.75;
  // vec3 vel = vec3(-sin(ct), 0., cos(ct));
  // vel = rotate(vel, -time*.05, loopAxis);
  // ro += rotate(orbit, -time*.05, loopAxis);
  // up = normalize(cross(ro, vel));
  // //
  // up = normalize(ro);
  // focus = ro + vel*.1;


  vec3 rov = normalize(focus - ro);
  vec3 cu = normalize(cross(rov, up));
  vec3 cv = cross(cu, rov);
  vec3 rd = mat3(cu,cv, rov) * normalize(vec3(pt*(1. + 50.*dot(pt,pt)*slider(2)), 1.));
  vec3 v = -rd;

  vec3 p = ro;
  float vt = 0.;
  vec4 d = vec4(0.);

  vec3 lpos = vec3(0., 0., -15.);
  lpos.x += 4.*sin(lpos.z*.1 - time);
  vec3 lc = vec3(5.) * (1. + volume);
  vec3 ld = normalize(vec3(-1.,2.,-1.));

  vec3 n;
  for(int bounce=0; bounce<2 + stop_unroll;bounce++){
    float t=0.;
    for(int i = 0; i < 75; i++){
      p = ro + rd*t;
      d = map(p);
      t += d.x*.9;
      if(abs(d.x) < 0.01 || t > 60.){
        break;
      }
    }

    if(bounce == 0){
      vt = min(t, 25.);
    }

    if(abs(d.x) > 0.01){
      c *= 0.;
      break;
    }
    n = calcNormal(p, vec2(0., .00768));
    vec3 l = normalize(lpos-p);
    float r = max(0.1, length(lpos - p) - .5);
    float intensity = 1. / (r*r);
    vec3 albedo = vec3(d.z);
    vec3 acc = vec3(0);
    acc += intensity * lc * albedo * chi(l,n,0.1);
    acc += 0.25*albedo * chi(ld,n,0.1);
    c *= acc;
    if(d.y == 1.){
      ro = p + .01*n;
      rd = reflect(rd,n);
    }else{
      break;
    }
  }

  float tVol = 0.;
  float st = vt/64.;
  vec3 volColor = vec3(0.);
  vec3 extinction = vec3(1.);
  float opticalDepth = 0.;
  for(int i=0; i < 64 + stop_unroll; i++){
    p = ro - v*tVol;
    bool isin = map(p).x < 0.;
    vec3 p2l = normalize(lpos - p);
    float r = max(0.01, length(lpos - p) - .5);
    float tScat = 0.;
    float rho = 1.*mapVolume(p);
    opticalDepth += rho*st;
    float cosTheta = dot(p2l, v);
    float p0 = HenyeyGreenstein(cosTheta, 0.6);
    float p1 = HenyeyGreenstein(cosTheta, 0.1);
    float phase = p0 + .9 * (p1 - p0);

    vec3 lscatin = PI * vec3(phase);
    vec3 transmission = exp(-opticalDepth*extinction);
    vec3 lint = lc * min(1., 1./(r*r));
    volColor += transmission * opticalDepth * lscatin * lint;
    tVol += st;
  }
  vec3 tr = exp(-opticalDepth*extinction);
  c = mix(volColor, c, tr);

  c = ACESFilm(c);
  if(knob(2,0)>0.1){
    vec3 meme = texture(spin, uv).rgb;
    meme = pow(meme, vec3(2.2));
    c = vec3(gray(meme));
  }
  uv = gl_FragCoord.xy / resolution;
  c = mask(uv, c*(1. + 1.*step(0.01,slider(5))));

  int flags = int(texture(render,uv).a);
  if(slap(11) > 0.1){
    flags ^= 2;
  }

  out_color = vec4(c,float(flags));
}
