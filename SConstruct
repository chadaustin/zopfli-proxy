import os

env = Environment(ENV=os.environ)
env.Append(CCFLAGS=['-g', '-O2', '-Wall', '-Werror'])

C_SOURCES = Split('''
    zopfli_compress.cpp

    zopfli/src/zopfli/blocksplitter.c
    zopfli/src/zopfli/cache.c
    zopfli/src/zopfli/deflate.c
    zopfli/src/zopfli/gzip_container.c
    zopfli/src/zopfli/hash.c
    zopfli/src/zopfli/katajainen.c
    zopfli/src/zopfli/lz77.c
    zopfli/src/zopfli/squeeze.c
    zopfli/src/zopfli/tree.c
    zopfli/src/zopfli/util.c
    zopfli/src/zopfli/zlib_container.c
    zopfli/src/zopfli/zopfli_lib.c
''')

lib = env.Library(
    'build/zopfli_compress',
    [ env.Object('build/' + os.path.splitext(os.path.basename(src))[0], src)
      for src in C_SOURCES ])

env.Command(
    'zopfli-proxy',
    ['main.hs', lib],
    'ghc -O2 -o $TARGET $SOURCES')
