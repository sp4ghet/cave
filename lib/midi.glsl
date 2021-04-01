#pragma glslify: import("./lib/uniforms.glsl")

#ifndef MIDI
#define MIDI

float slider(int idx){
  // user = 176.
  // factory = 184.
  // leftmost is 77~84
  return texture(midi, vec2(184./256., float(idx+77) / 128.)).x * 2.;
}

float knob(int row, int col){
  int rowLeft = 0;
  if(row == 0){
    rowLeft = 13;
  }else if(row == 1){
    rowLeft = 29;
  }else{
    rowLeft = 49;
  }
  return texture(midi, vec2(184./256., float(rowLeft+col) / 128.)).x * 2.;
}

float slap(int n){
  if(n > 7){
    n -= 8;
  }else{
    n += 8;
  }
  return texture(note, vec2(float(n+60) / 128.)).r;
}
float stlap(int n){
  return step(0.05, slap(n));
}

#endif
