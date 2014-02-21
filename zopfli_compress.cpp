#include <stdlib.h>
#include <string.h>

#include "zopfli/src/zopfli/zopfli.h"

extern "C" const unsigned char* zopfli_compress(
    size_t input_size,
    const unsigned char* input,
    size_t* output_size
) {
    ZopfliOptions options;
    ZopfliInitOptions(&options);

    unsigned char* out = 0;
    *output_size = 0;
    ZopfliCompress(&options, ZOPFLI_FORMAT_GZIP, input, input_size, &out, output_size);

    return out;
}
