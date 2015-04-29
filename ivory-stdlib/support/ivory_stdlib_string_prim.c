
#include "ivory_stdlib_string_prim.h"
#include <string.h>

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

