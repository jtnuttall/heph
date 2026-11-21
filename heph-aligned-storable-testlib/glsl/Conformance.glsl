// clang-format off
/*
 * GLSL twins of the structs under test in heph-aligned-storable-testlib.
 *
 * Each struct below that has a Haskell twin of the same name (in
 * Foreign.GPU.Storable.Aligned.Test.Sut.* or .Marshal) has an oracle per layout,
 * the `Storable (Oracle l T)` instances, whose member offsets and sizeOf are the
 * numbers glslang gives here; their alignment follows from the layout rules (it
 * is not in the SPIR-V). Members are in Haskell field order; their names may
 * differ.
 *
 * Mapping to the Haskell types:
 *  - float16_t, float, double, bool, intN_t, uintN_t, int, uint are Half, Float,
 *    Double, Bool, IntN, WordN, Int32, Word32.
 *  - vecN, dvecN, ivecN, uvecN, u16vec3 and bvec3 are the shape-group types of
 *    Foreign.GPU.Storable.Aligned.Test.Vocab (Vec3 lib, DVec3 lib, ...).
 *  - Matrices are row_major. A row_major matCxR stores R rows of C components,
 *    which GlslMat calls MatVecs = R, MatVecDim = C, and linear calls M_RC
 *    (mat3x4 is Mat3x4 lib, M43 Float for linear).
 *  - T x[N] is a sized vector (Data.Vector.Sized, Data.Vector.Storable.Sized)
 *    or an AlignedArray.
 *  - The blocks at the bottom put every struct T in a runtime array under each
 *    layout L, in the block L_T. The array's ArrayStride is
 *    sizeOf (Strided L T), which the oracle's sizeOf equals.
 *
 * To check a struct:
 *
 * glslangValidator --target-env vulkan1.3 -S vert -V Conformance.glsl -o Conformance.spv
 * spirv-dis Conformance.spv > Conformance.spv-dis
 *
 * then read the OpMemberDecorate ... Offset and OpDecorate ... ArrayStride lines
 * of the struct type that L_T holds. The file as it was before it moved here is at
 * https://godbolt.org/z/njnvM8q4o.
 *
 * Verified: every member offset, inner ArrayStride and struct ArrayStride in each
 * layout equals the oracle's literals.
 *
 *   struct               glslang  date
 *   Simple               16.6.0   2026-11-21
 *   MixedAlign           16.6.0   2026-11-21
 *   BoolTest             16.6.0   2026-11-21
 *   LayoutBug            16.6.0   2026-11-21
 *   LargePrimitives      16.6.0   2026-11-21
 *   NestedPrim           16.6.0   2026-11-20, 2026-11-21
 *   ArrayStruct          16.6.0   2026-11-20, 2026-11-21
 *   DeepNest             16.6.0   2026-11-20, 2026-11-21
 *   StructWithArrayF     16.6.0   2026-11-20, 2026-11-21
 *   Std140Test           16.6.0   2026-11-21
 *   PaddingTest          16.6.0   2026-11-21
 *   FinalPaddingTest     16.6.0   2026-11-21
 *   Vertex               16.6.0   2026-11-21
 *   Nested               16.6.0   2026-11-21
 *   VectorStruct         16.6.0   2026-11-21
 *   TailVec3             16.6.0   2026-11-20, 2026-11-21
 *   Uniforms             16.6.0   2026-11-21
 *   DUniforms            16.6.0   2026-11-20, 2026-11-21
 *   ComplexPadding       16.6.0   2026-11-21
 *   D2                   16.6.0   2026-11-21
 *   DoubleTrouble        16.6.0   2026-11-21   std140 only (no std430/scalar oracle)
 *   TestNonSquare        16.6.0   2026-11-21
 *   KitchenSink          16.6.0   2026-11-21
 *   ComplexVectorStruct  16.6.0   2026-11-21
 *   Insanity             16.6.0   2026-11-21
 *   IntVecStruct         16.6.0   2026-11-20, 2026-11-21
 *   StructWithArrayV     16.6.0   2026-11-20, 2026-11-21   packed sizes only (no oracle)
 *
 * Under std140 the library gives int8/int16/uint8/uint16 4 bytes and the oracles
 * follow it, so they write u16vec3 components 4 bytes apart. glslang gives these
 * types their natural size in every layout (u16vec3 components 2 bytes apart).
 * No offset or stride above shows the difference: each such member is followed by
 * a more strictly aligned one.
 */
// clang-format on
#version 450
#extension GL_EXT_scalar_block_layout : enable
#extension GL_EXT_shader_16bit_storage : enable
#extension GL_EXT_shader_8bit_storage : enable
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

struct NestedPrim {
  float npHead;
  Simple npSimple;
  MixedAlign npMixed;
  BoolTest npBool;
};

struct ArrayStruct {
  float asA;
  uint16_t asB[3];
  double asC[2];
  float asD;
  double asE;
};

struct DeepNest {
  float dnA;
  NestedPrim dnB[5];
  uint8_t dnC;
  uint64_t dnD;
  NestedPrim dnE[5][2][4];
  bool dnF;
};

struct TailVec3 {
  float tvA;
  vec3 tvB;
};

struct IntVecStruct {
  float ivA;
  ivec3 ivB[2];
  uvec2 ivC;
  uvec3 ivD;
  int ivE;
};

struct DUniforms {
  dmat4 duM;
  dvec4 duV;
};

struct StructWithArrayF {
  float swaA;
  float swaB[4];
};

struct StructWithArrayV {
  float swaA;
  vec4 swaB[4];
};

#define BLOCKS(L, B) \
  layout(L, row_major, binding = B + 0) buffer L##_Simple { Simple x[]; } L##_simple; \
  layout(L, row_major, binding = B + 1) buffer L##_MixedAlign { MixedAlign x[]; } L##_mixedalign; \
  layout(L, row_major, binding = B + 2) buffer L##_BoolTest { BoolTest x[]; } L##_booltest; \
  layout(L, row_major, binding = B + 3) buffer L##_LayoutBug { LayoutBug x[]; } L##_layoutbug; \
  layout(L, row_major, binding = B + 4) buffer L##_LargePrimitives { LargePrimitives x[]; } L##_largeprimitives; \
  layout(L, row_major, binding = B + 5) buffer L##_NestedPrim { NestedPrim x[]; } L##_nestedprim; \
  layout(L, row_major, binding = B + 6) buffer L##_ArrayStruct { ArrayStruct x[]; } L##_arraystruct; \
  layout(L, row_major, binding = B + 7) buffer L##_DeepNest { DeepNest x[]; } L##_deepnest; \
  layout(L, row_major, binding = B + 8) buffer L##_StructWithArrayF { StructWithArrayF x[]; } L##_structwitharrayf; \
  layout(L, row_major, binding = B + 9) buffer L##_Std140Test { Std140Test x[]; } L##_std140test; \
  layout(L, row_major, binding = B + 10) buffer L##_PaddingTest { PaddingTest x[]; } L##_paddingtest; \
  layout(L, row_major, binding = B + 11) buffer L##_FinalPaddingTest { FinalPaddingTest x[]; } L##_finalpaddingtest; \
  layout(L, row_major, binding = B + 12) buffer L##_Vertex { Vertex x[]; } L##_vertex; \
  layout(L, row_major, binding = B + 13) buffer L##_Nested { Nested x[]; } L##_nested; \
  layout(L, row_major, binding = B + 14) buffer L##_VectorStruct { VectorStruct x[]; } L##_vectorstruct; \
  layout(L, row_major, binding = B + 15) buffer L##_TailVec3 { TailVec3 x[]; } L##_tailvec3; \
  layout(L, row_major, binding = B + 16) buffer L##_Uniforms { Uniforms x[]; } L##_uniforms; \
  layout(L, row_major, binding = B + 17) buffer L##_DUniforms { DUniforms x[]; } L##_duniforms; \
  layout(L, row_major, binding = B + 18) buffer L##_ComplexPadding { ComplexPadding x[]; } L##_complexpadding; \
  layout(L, row_major, binding = B + 19) buffer L##_D2 { D2 x[]; } L##_d2; \
  layout(L, row_major, binding = B + 20) buffer L##_DoubleTrouble { DoubleTrouble x[]; } L##_doubletrouble; \
  layout(L, row_major, binding = B + 21) buffer L##_TestNonSquare { TestNonSquare x[]; } L##_testnonsquare; \
  layout(L, row_major, binding = B + 22) buffer L##_KitchenSink { KitchenSink x[]; } L##_kitchensink; \
  layout(L, row_major, binding = B + 23) buffer L##_ComplexVectorStruct { ComplexVectorStruct x[]; } L##_complexvectorstruct; \
  layout(L, row_major, binding = B + 24) buffer L##_Insanity { Insanity x[]; } L##_insanity; \
  layout(L, row_major, binding = B + 25) buffer L##_IntVecStruct { IntVecStruct x[]; } L##_intvecstruct; \
  layout(L, row_major, binding = B + 26) buffer L##_StructWithArrayV { StructWithArrayV x[]; } L##_structwitharrayv;

BLOCKS(std140, 0)
BLOCKS(std430, 100)
BLOCKS(scalar, 200)

void main() {}
