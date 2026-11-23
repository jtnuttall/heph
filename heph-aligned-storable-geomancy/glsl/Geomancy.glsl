// clang-format off
/*
 * GLSL twins of the structs under test in test/GeomancySpec.hs.
 *
 * Each struct below has a Haskell twin of the same name with an oracle per
 * layout.
 *
 * To check a struct:
 *
 * glslangValidator --target-env vulkan1.3 -S vert -V Geomancy.glsl -o Geomancy.spv
 * spirv-dis Geomancy.spv > Geomancy.spv-dis
 *
 * then read the OpMemberDecorate ... Offset and OpDecorate ... ArrayStride lines
 * of the struct type that L_T holds.
 *
 * Last update date and glslange versions:
 *
 *   struct               glslang  date
 *   TransformTail        16.6.0   2026-11-21
 *   OffsetMat4           16.6.0   2026-11-21
 *   QuatStruct           16.6.0   2026-11-21
 *   Vec3Mix              16.6.0   2026-11-21
 *   DoubleMix            16.6.0   2026-11-21
 *   IntMix               16.6.0   2026-11-21
 */
// clang-format on
#version 450
#extension GL_EXT_scalar_block_layout : enable
#extension GL_EXT_shader_explicit_arithmetic_types_int64 : enable

struct TransformTail {
  mat4 ttM;
  float ttF;
};

struct OffsetMat4 {
  float omA;
  mat4 omM;
};

struct QuatStruct {
  float qsA;
  vec4 qsQ;
};

struct Vec3Mix {
  float vmA;
  vec3 vmP;
  float vmB;
  vec3 vmQ;
};

struct DoubleMix {
  float dmA;
  dmat4 dmT;
  dvec4 dmQ;
  dvec3 dmP;
  float dmB;
  dvec3 dmR;
};

struct IntMix {
  int imA;
  ivec3 imP;
  int imB;
  uvec3 imQ;
  uint imC;
  i64vec3 imR;
  uint imD;
  u64vec4 imS;
};

#define BLOCKS(L, B) \
  layout(L, binding = B + 0) buffer L##_TransformTail { TransformTail x[]; } L##_transformtail; \
  layout(L, binding = B + 1) buffer L##_OffsetMat4 { OffsetMat4 x[]; } L##_offsetmat4; \
  layout(L, binding = B + 2) buffer L##_QuatStruct { QuatStruct x[]; } L##_quatstruct; \
  layout(L, binding = B + 3) buffer L##_Vec3Mix { Vec3Mix x[]; } L##_vec3mix; \
  layout(L, binding = B + 4) buffer L##_DoubleMix { DoubleMix x[]; } L##_doublemix; \
  layout(L, binding = B + 5) buffer L##_IntMix { IntMix x[]; } L##_intmix;

BLOCKS(std140, 0)
BLOCKS(std430, 100)
BLOCKS(scalar, 200)

void main() {}
