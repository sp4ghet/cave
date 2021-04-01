#pragma glslify: import("./lib/uniforms.glsl")

#ifndef RNG
#define RNG

float random(vec2 x)
{
    int n = int(x.x * 40.0 + x.y * 6400.0);
    n = (n << 13) ^ n;
    return 1.0 - float( (n * (n * n * 15731 + 789221) + \
             1376312589) & 0x7fffffff) / 1073741824.0;
}

int gseed = 0;
float random_LCG()
{
    const int m = 2147483647;
    const int a = 89798713;
    const int c = 2123789138;
    gseed = int(time*1000.);
    gseed = a*gseed + c;
    gseed = gseed - m * int(gseed/m);
    int n = gseed;
    n = (n << 13) ^ n;
    return 1.0 - float( (n * (n * n * 15731 + 789221) + \
             1376312589) & 0x7fffffff) / 1073741824.0;
}

vec3 ortho(vec3 v) {
    //  See : http://lolengine.net/blog/2013/09/21/picking-orthogonal-vector-combing-coconuts
    return abs(v.x) > abs(v.z) ? vec3(-v.y, v.x, 0.0)  : vec3(0.0, -v.z, v.y);
}

vec3 getSampleBiased(vec3  dir, float power, vec2 seed) {
	dir = normalize(dir);
	vec3 o1 = normalize(ortho(dir));
	vec3 o2 = normalize(cross(dir, o1));
	vec2 r = vec2(random(seed), random(seed + .3));
	r.x=r.x*2.*PI;
	r.y=pow(r.y,1.0/(power+1.0));
	float oneminus = sqrt(1.0-r.y*r.y);
	return cos(r.x)*oneminus*o1+sin(r.x)*oneminus*o2+r.y*dir;
}

vec3 getSample(vec3 dir, vec2 seed) {
	return getSampleBiased(dir,0.0, seed); // <- unbiased!
}

vec3 getCosineWeightedSample(vec3 dir, vec2 seed) {
	return getSampleBiased(dir,1.0, seed);
}

vec3 getConeSample(vec3 dir, float extent, vec2 seed) {
  // Formula 34 in GI Compendium
	dir = normalize(dir);
	vec3 o1 = normalize(ortho(dir));
	vec3 o2 = normalize(cross(dir, o1));
	vec2 r =  vec2(random(seed), random(seed + .3));
	r.x=r.x*2.*PI;
	r.y=1.0-r.y*extent;
	float oneminus = sqrt(1.0-r.y*r.y);
	return cos(r.x)*oneminus*o1+sin(r.x)*oneminus*o2+r.y*dir;
}
#endif
