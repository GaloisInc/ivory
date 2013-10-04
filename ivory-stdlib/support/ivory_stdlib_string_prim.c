
#include "ivory_stdlib_string_prim.h"
#include <string.h>

void ivory_stdlib_strncpy_uint8(uint8_t *dest, const char *src,  uint32_t size) {
    strncpy((char*)dest, src, size);
}

/*
 * Copy src to string dst of size siz.  At most siz-1 characters
 * will be copied.  Always NUL terminates (unless siz == 0).
 * Returns strlen(src); if retval >= siz, truncation occurred.
 *
 * Copyright (c) 1998 Todd C. Miller <Todd.Miller@courtesan.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
uint32_t ivory_stdlib_strlcpy(char *dest, const char *src, uint32_t size)
{
    char *d = dest;
    const char *s = src;
    uint32_t n = size;

    /* Copy as many bytes as will fit */
    if (n != 0 && --n != 0) {
        do {
            if ((*d++ = *s++) == 0)
                break;
        } while (--n != 0);
    }

    /* Not enough room in dest, add NUL and traverse rest of src */
    if (n == 0) {
        if (size != 0)
            *d = '\0';		/* NUL-terminate dest */
        while (*s++)
            ;
    }

    return (s - src - 1);	/* count does not include NUL */
}

/* Copy at most 'min(dest_len, src_len)' bytes from 'src' to
 * dest.  Returns the number of bytes written to 'dest', which
 * is not null terminated. */
int32_t ivory_stdlib_string_copy(
    uint8_t *dest, int32_t dest_len,
    const uint8_t *src, int32_t src_len)
{
    int32_t result = 0;
    int32_t n = dest_len < src_len ? dest_len : src_len;

    for (int32_t i = 0; i < n; ++i) {
        dest[i] = src[i];
        ++result;
    }

  return result;
}

/* Copy at most 'min(dest_len, src_len)' bytes from 'src' to
 * dest, stopping early if a null terminator is encountered.
 *
 * Returns the number of bytes written to 'dest' (which is
 * never null-terminated. */
int32_t ivory_stdlib_string_copy_z(
    uint8_t *dest, int32_t dest_len,
    const uint8_t *src, int32_t src_len)
{
    int32_t result = 0;
    int32_t n = dest_len < src_len ? dest_len : src_len;

    for (int32_t i = 0; i < n; ++i) {
        if (src[i] == 0)
            break;

        dest[i] = src[i];
        ++result;
    }

  return result;
}

