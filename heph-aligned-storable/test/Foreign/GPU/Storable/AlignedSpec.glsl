// clang-format off
/*
 * This file contains types that are used to derive the oracles in AlignedSpec.
 * Eventually, I'd like to codegen AlignedSpec, but doing so will require SPIR-V
 * reflection that maps to Haskell types.
 *
 * To verify alignments, place the SUT in the UBO struct, then:
 *
 * glslangValidator --target-env vulkan1.3 -S vert -V AlignedSpec.glsl -o AlignedSpec.spv
 * spirv-dis AlignedSpec.spv > AlignedSpec.spv-dis
 * vi AlignedSpec.spv-dis
 */
// clang-format on
#version 450
#extension GL_EXT_scalar_block_layout : enable
#extension GL_EXT_shader_16bit_storage : enable
#extension GL_EXT_shader_explicit_arithmetic_types_int8 : enable
#extension GL_EXT_shader_explicit_arithmetic_types_int16 : enable
#extension GL_EXT_shader_explicit_arithmetic_types_int64 : enable
#extension GL_EXT_shader_explicit_arithmetic_types_float16 : enable

struct Simple {
  float simpleA;
  int simpleB;
};

struct MixedAlign {
  int16_t a;
  double b;
  int c;
};

struct BoolTest {
  bool isEnabled;
  float value;
};

struct PaddingTest {
  float f;
  vec4 v;
};

struct Std140Test {
  vec3 v3;
  float f1;
};

struct FinalPaddingTest {
  vec4 fptA;
  int fptB;
};

struct Uniforms {
  mat4 modelView;
  vec4 cameraPos;
};

struct Vertex {
  vec3 p;
  vec3 n;
  vec2 uv;
};

struct Nested {
  vec3 nestedV3;
  Simple nestedSimple;
  MixedAlign nestedMixed;
  BoolTest nestedBoolTest;
};

struct ComplexPadding {
  float a;
  vec3 b;
  mat3 c;
  float d;
  mat4 e;
  float f;
};

struct LayoutBug {
  float fieldA;
  float fieldB;
  double fieldC;
};

struct DoubleTrouble {
  double d;
  dmat3 m0;
  float f0;
  dvec3 v0;
  float f1;
  dvec4 v1;
  float f2;
  dmat4 m1;
};

struct IntMatrix {
  u16vec3 f1[3];
  uint f2;
};

struct LargePrimitives {
  double d;
  int64_t l;
};

struct SimpleArray {
  float f[3];
};

struct VectorStruct {
  float vsA;
  vec3 vsB[2];
  int vsC;
};

struct ComplexVectorStruct {
  float vsA;
  u16vec3 f1[3];
  dvec2 f2[2];
  float vsB;
  double vsC;
};

struct Insanity {
  float ina;
  ComplexVectorStruct inb[7];
  uint8_t inc;
  uint64_t ind;
  uint16_t ine;
  bvec3 inf;
  Nested ing[5];
  u16vec3 inh;
  Nested ini[5][2][4];
  bool inj;
  dvec3 ink;
};

struct Combined {
  float16_t x;
  uint8_t y;
};

struct Vec2 {
  mat2 v2;
};

struct KitchenSink {
  float16_t ksA;
  dvec3 ksB;
  float ksC;
  mat3 ksD;
  int ksE;
  mat3 ksF;
  uint8_t ksG;
  f16mat2x3 ksH;
  uint16_t ksI;
  f16mat3x2 ksJ;
  uint16_t ksK;
  mat3x4 ksL;
  int16_t ksM;
  dmat4x3 ksN;
  bool ksO;
  int8_t ksP;
  f16mat4 ksQ;
  int64_t ksR;
  dmat2x4 ksS;
  uint64_t ksT;
  f16mat4x2 ksU;
  float16_t ksV;
};

struct D2 {
  float16_t d2a;
  vec2 d2b;
  float16_t d2c;
  mat2 d2d;
  float16_t d2e;
};

struct TestNonSquare {
  f16mat2x3 matA;
  dmat3x4 matB;
};

struct UBO {
  // float f0;
  TestNonSquare sut;
  // float f;
};

layout(std140, ROW_MAJOR, binding = 0) buffer std140 { UBO ubo[]; }
ustd140;

layout(std430, ROW_MAJOR, binding = 1) buffer std430 { UBO ubo[]; }
ustd430;

layout(scalar, ROW_MAJOR, binding = 2) buffer scalar { UBO ubo[]; }
uscalar;

void main() {}
