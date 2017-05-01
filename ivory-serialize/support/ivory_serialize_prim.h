#ifndef __IVORY_SERIALIZE_PRIM_H__
#define __IVORY_SERIALIZE_PRIM_H__

#include <stdint.h>
#include <string.h>

#define IVORY_SERIALIZE_CAT(X,Y,Z) X##_##Y##_##Z
// Indirection in case X,Y defined
#define IVORY_SERIALIZE_C(X,Y,Z) IVORY_SERIALIZE_CAT(X,Y,Z)

// Implementation dependent sizes.
#ifndef CONFIG_IVORY_SIZEOF_FLOAT
#define CONFIG_IVORY_SIZEOF_FLOAT 4
#endif

#ifndef CONFIG_IVORY_FLOAT_AREA_TY
#define CONFIG_IVORY_FLOAT_AREA_TY uint32_t
#endif

#ifndef CONFIG_IVORY_SIZEOF_DOUBLE
#define CONFIG_IVORY_SIZEOF_DOUBLE 8
#endif

#ifndef CONFIG_IVORY_DOUBLE_AREA_TY
#define CONFIG_IVORY_DOUBLE_AREA_TY uint64_t
#endif

/* ENDIANNESS:
 * By default, this library supports a little-endian system.
 * To build for a big endian system, define CONFIG_IVORY_SERIALIZE_BIG_ENDIAN.
 */

/* Serializing can be done with type-aliasing in unions (since C99), memcpy, or
   turning off strict-aliasing (using compiler-specific pragmas). memcpy appears
   to be the most portable and produces the best assembly. (Direct casting is
   undefined.) See for details: http://blog.regehr.org/archives/959

   *** WARNING *** Depends on byte == 8bits (for sizeof and memcpy). This is
   most always the case.
*/

// Packing primitives:

static inline void ivory_serialize_pack_prim_1_le(uint8_t *dst, const uint8_t *src) {
	dst[0] = src[0];
}

static inline void ivory_serialize_pack_prim_1_be(uint8_t *dst, const uint8_t *src) {
	dst[0] = src[0];
}

static inline void ivory_serialize_pack_prim_2_le(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_BIG_ENDIAN
	dst[0] = src[0];
	dst[1] = src[1];
#else
	dst[0] = src[1];
	dst[1] = src[0];
#endif
}

static inline void ivory_serialize_pack_prim_2_be(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_BIG_ENDIAN
	dst[0] = src[1];
	dst[1] = src[0];
#else
	dst[0] = src[0];
	dst[1] = src[1];
#endif
}

static inline void ivory_serialize_pack_prim_4_le(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_BIG_ENDIAN
	dst[0] = src[0];
	dst[1] = src[1];
	dst[2] = src[2];
	dst[3] = src[3];
#else
	dst[0] = src[3];
	dst[1] = src[2];
	dst[2] = src[1];
	dst[3] = src[0];
#endif
}

static inline void ivory_serialize_pack_prim_4_be(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_BIG_ENDIAN
	dst[0] = src[3];
	dst[1] = src[2];
	dst[2] = src[1];
	dst[3] = src[0];
#else
	dst[0] = src[0];
	dst[1] = src[1];
	dst[2] = src[2];
	dst[3] = src[3];
#endif
}

static inline void ivory_serialize_pack_prim_8_le(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_BIG_ENDIAN
	dst[0] = src[0];
	dst[1] = src[1];
	dst[2] = src[2];
	dst[3] = src[3];
	dst[4] = src[4];
	dst[5] = src[5];
	dst[6] = src[6];
	dst[7] = src[7];
#else
	dst[0] = src[7];
	dst[1] = src[6];
	dst[2] = src[5];
	dst[3] = src[4];
	dst[4] = src[3];
	dst[5] = src[2];
	dst[6] = src[1];
	dst[7] = src[0];
#endif
}

static inline void ivory_serialize_pack_prim_8_be(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_BIG_ENDIAN
	dst[0] = src[7];
	dst[1] = src[6];
	dst[2] = src[5];
	dst[3] = src[4];
	dst[4] = src[3];
	dst[5] = src[2];
	dst[6] = src[1];
	dst[7] = src[0];
#else
	dst[0] = src[0];
	dst[1] = src[1];
	dst[2] = src[2];
	dst[3] = src[3];
	dst[4] = src[4];
	dst[5] = src[5];
	dst[6] = src[6];
	dst[7] = src[7];
#endif
}

// Functions to take care of offset and cast any source type to correct prim size:
static inline void ivory_serialize_pack_uint8_le(uint8_t *dst, uint32_t offs, const uint8_t *src) {
  uint8_t tmp[1] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_1_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_uint8_be(uint8_t *dst, uint32_t offs, const uint8_t *src) {
  uint8_t tmp[1] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_1_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int8_le(uint8_t *dst, uint32_t offs, const int8_t *src) {
  uint8_t tmp[1] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_1_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int8_be(uint8_t *dst, uint32_t offs, const int8_t *src) {
  uint8_t tmp[1] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_1_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_uint16_le(uint8_t *dst, uint32_t offs, const uint16_t *src) {
  uint8_t tmp[2] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_2_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_uint16_be(uint8_t *dst, uint32_t offs, const uint16_t *src) {
  uint8_t tmp[2] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_2_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int16_le(uint8_t *dst, uint32_t offs, const int16_t *src) {
  uint8_t tmp[2] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_2_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int16_be(uint8_t *dst, uint32_t offs, const int16_t *src) {
  uint8_t tmp[2] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_2_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_uint32_le(uint8_t *dst, uint32_t offs, const uint32_t *src) {
  uint8_t tmp[4] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_4_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_uint32_be(uint8_t *dst, uint32_t offs, const uint32_t *src) {
  uint8_t tmp[4] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_4_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int32_le(uint8_t *dst, uint32_t offs, const int32_t *src) {
  uint8_t tmp[4] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_4_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int32_be(uint8_t *dst, uint32_t offs, const int32_t *src) {
  uint8_t tmp[4] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_4_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_float_le(uint8_t *dst, uint32_t offs, const float *src) {
  uint8_t tmp[CONFIG_IVORY_SIZEOF_FLOAT] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	IVORY_SERIALIZE_C(ivory_serialize_pack_prim,CONFIG_IVORY_SIZEOF_FLOAT,le)(dst + offs, tmp);
}

static inline void ivory_serialize_pack_float_be(uint8_t *dst, uint32_t offs, const float *src) {
  uint8_t tmp[CONFIG_IVORY_SIZEOF_FLOAT] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	IVORY_SERIALIZE_C(ivory_serialize_pack_prim,CONFIG_IVORY_SIZEOF_FLOAT,be)(dst + offs, tmp);
}

static inline void ivory_serialize_pack_uint64_le(uint8_t *dst, uint32_t offs, const uint64_t *src) {
  uint8_t tmp[8] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_8_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_uint64_be(uint8_t *dst, uint32_t offs, const uint64_t *src) {
  uint8_t tmp[8] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_8_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int64_le(uint8_t *dst, uint32_t offs, const int64_t *src) {
  uint8_t tmp[8] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_8_le(dst + offs, tmp);
}

static inline void ivory_serialize_pack_int64_be(uint8_t *dst, uint32_t offs, const int64_t *src) {
  uint8_t tmp[8] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	ivory_serialize_pack_prim_8_be(dst + offs, tmp);
}

static inline void ivory_serialize_pack_double_le(uint8_t *dst, uint32_t offs, const double *src) {
  uint8_t tmp[CONFIG_IVORY_SIZEOF_DOUBLE] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	IVORY_SERIALIZE_C(ivory_serialize_pack_prim,CONFIG_IVORY_SIZEOF_DOUBLE,le)(dst + offs, tmp);
}

static inline void ivory_serialize_pack_double_be(uint8_t *dst, uint32_t offs, const double *src) {
  uint8_t tmp[CONFIG_IVORY_SIZEOF_DOUBLE] = {0};
  memcpy(&tmp, src, sizeof(tmp));
	IVORY_SERIALIZE_C(ivory_serialize_pack_prim,CONFIG_IVORY_SIZEOF_DOUBLE,be)(dst + offs, tmp);
}

// Unpacking primitives:

static inline uint8_t ivory_serialize_unpack_prim_1_le(const uint8_t *src){
	return ((uint8_t) src[0] << 0);
}

static inline uint8_t ivory_serialize_unpack_prim_1_be(const uint8_t *src){
	return ((uint8_t) src[0] << 0);
}

static inline uint16_t ivory_serialize_unpack_prim_2_le(const uint8_t *src){
	return (((uint16_t) src[0] << 0) |
			((uint16_t) src[1] << 8));
}

static inline uint16_t ivory_serialize_unpack_prim_2_be(const uint8_t *src){
	return (((uint16_t) src[1] << 0) |
			((uint16_t) src[0] << 8));
}

static inline uint32_t ivory_serialize_unpack_prim_4_le(const uint8_t *src){
	return (((uint32_t) src[0] << 0)  |
			((uint32_t) src[1] << 8)  |
			((uint32_t) src[2] << 16) |
			((uint32_t) src[3] << 24));
}

static inline uint32_t ivory_serialize_unpack_prim_4_be(const uint8_t *src){
	return (((uint32_t) src[3] << 0)  |
			((uint32_t) src[2] << 8)  |
			((uint32_t) src[1] << 16) |
			((uint32_t) src[0] << 24));
}

static inline uint64_t ivory_serialize_unpack_prim_8_le(const uint8_t *src){
	return (((uint64_t) src[0] << 0)  |
			((uint64_t) src[1] << 8)  |
			((uint64_t) src[2] << 16) |
			((uint64_t) src[3] << 24) |
			((uint64_t) src[4] << 32) |
			((uint64_t) src[5] << 40) |
			((uint64_t) src[6] << 48) |
			((uint64_t) src[7] << 56));
}

static inline uint64_t ivory_serialize_unpack_prim_8_be(const uint8_t *src){
	return (((uint64_t) src[7] << 0)  |
			((uint64_t) src[6] << 8)  |
			((uint64_t) src[5] << 16) |
			((uint64_t) src[4] << 24) |
			((uint64_t) src[3] << 32) |
			((uint64_t) src[2] << 40) |
			((uint64_t) src[1] << 48) |
			((uint64_t) src[0] << 56));
}

// Functions to cast unpacked result to specific destination types:
static inline void ivory_serialize_unpack_uint8_le(const uint8_t *src, uint32_t offs, uint8_t *dst) {
	uint8_t tmp = ivory_serialize_unpack_prim_1_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_uint8_be(const uint8_t *src, uint32_t offs, uint8_t *dst) {
	uint8_t tmp = ivory_serialize_unpack_prim_1_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int8_le(const uint8_t *src, uint32_t offs, int8_t *dst) {
	uint8_t tmp = ivory_serialize_unpack_prim_1_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int8_be(const uint8_t *src, uint32_t offs, int8_t *dst) {
	uint8_t tmp = ivory_serialize_unpack_prim_1_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_uint16_le(const uint8_t *src, uint32_t offs, uint16_t *dst) {
	uint16_t tmp = ivory_serialize_unpack_prim_2_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_uint16_be(const uint8_t *src, uint32_t offs, uint16_t *dst) {
	uint16_t tmp = ivory_serialize_unpack_prim_2_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int16_le(const uint8_t *src, uint32_t offs, int16_t *dst) {
	uint16_t tmp = ivory_serialize_unpack_prim_2_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int16_be(const uint8_t *src, uint32_t offs, int16_t *dst) {
	uint16_t tmp = ivory_serialize_unpack_prim_2_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_uint32_le(const uint8_t *src, uint32_t offs, uint32_t *dst) {
	uint32_t tmp = ivory_serialize_unpack_prim_4_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_uint32_be(const uint8_t *src, uint32_t offs, uint32_t *dst) {
	uint32_t tmp = ivory_serialize_unpack_prim_4_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int32_le(const uint8_t *src, uint32_t offs, int32_t *dst) {
	uint32_t tmp = ivory_serialize_unpack_prim_4_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int32_be(const uint8_t *src, uint32_t offs, int32_t *dst) {
	uint32_t tmp = ivory_serialize_unpack_prim_4_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_float_le(const uint8_t *src, uint32_t offs, float *dst) {
	CONFIG_IVORY_FLOAT_AREA_TY tmp = IVORY_SERIALIZE_C(ivory_serialize_unpack_prim,CONFIG_IVORY_SIZEOF_FLOAT,le)(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_float_be(const uint8_t *src, uint32_t offs, float *dst) {
	CONFIG_IVORY_FLOAT_AREA_TY tmp = IVORY_SERIALIZE_C(ivory_serialize_unpack_prim,CONFIG_IVORY_SIZEOF_FLOAT,be)(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_uint64_le(const uint8_t *src, uint32_t offs, uint64_t *dst) {
	uint64_t tmp = ivory_serialize_unpack_prim_8_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_uint64_be(const uint8_t *src, uint32_t offs, uint64_t *dst) {
	uint64_t tmp = ivory_serialize_unpack_prim_8_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int64_le(const uint8_t *src, uint32_t offs, int64_t *dst) {
	uint64_t tmp = ivory_serialize_unpack_prim_8_le(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_int64_be(const uint8_t *src, uint32_t offs, int64_t *dst) {
	uint64_t tmp = ivory_serialize_unpack_prim_8_be(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_double_le(const uint8_t *src, uint32_t offs, double *dst) {
	CONFIG_IVORY_DOUBLE_AREA_TY tmp = IVORY_SERIALIZE_C(ivory_serialize_unpack_prim,CONFIG_IVORY_SIZEOF_DOUBLE,le)(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

static inline void ivory_serialize_unpack_double_be(const uint8_t *src, uint32_t offs, double *dst) {
	CONFIG_IVORY_DOUBLE_AREA_TY tmp = IVORY_SERIALIZE_C(ivory_serialize_unpack_prim,CONFIG_IVORY_SIZEOF_DOUBLE,be)(src+offs);
	memcpy(dst, &tmp, sizeof(*dst));
}

#endif // __IVORY_SERIALIZE_PRIM_H__

