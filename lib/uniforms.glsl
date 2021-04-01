#ifndef ENTRY
#define ENTRY

precision highp float;

uniform vec2 resolution;
uniform float time;
uniform float volume;

uniform sampler2D backbuffer;
uniform sampler2D samples;
uniform sampler2D spectrum;
uniform sampler2D midi;
uniform sampler2D note;

uniform sampler2D texNoise;
uniform sampler2D texDither;
uniform sampler2D texFont;
uniform sampler2D noireW;

uniform int	PASSINDEX;
uniform sampler2D render;

uniform int stop_unroll;

const float PI = 3.14159265;
const float TAU = 2. * PI;

#endif
