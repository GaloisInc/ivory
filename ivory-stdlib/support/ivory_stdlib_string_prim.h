/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * ivory_string_stdlib_prim.h --- C string primitives.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 */

#ifndef __IVORY_STDLIB_STRING_PRIM_H__
#define __IVORY_STDLIB_STRING_PRIM_H__

#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

uint32_t ivory_stdlib_strlcpy(char *dest, const char *src,  uint32_t size);

void ivory_stdlib_strncpy_uint8( uint8_t *dest, const char *src, uint32_t size);

/* Copy at most 'min(dest_len, src_len)' bytes from 'src' to
 * dest, stopping early if a null terminator is encountered.
 *
 * Returns the number of bytes written to 'dest' (which is
 * never null-terminated. */
int32_t ivory_stdlib_string_copy_z(
  uint8_t *dest, int32_t dest_len,
  const uint8_t *src, int32_t src_len);

/* Copy at most 'min(dest_len, src_len)' bytes from 'src' to
 * dest.  Returns the number of bytes written to 'dest', which
 * is not null terminated. */
int32_t ivory_stdlib_string_copy(
    uint8_t *dest, int32_t dest_len,
    const uint8_t *src, int32_t src_len);

#ifdef __cplusplus
}
#endif

#endif  /* !defined __IVORY_STDLIB_STRING_PRIM_H__ */
