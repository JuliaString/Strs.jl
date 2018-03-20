#-----------------------------------------------------------------------------
# MurmurHash3 was written by Austin Appleby, and is placed in the public
# domain. The author hereby disclaims copyright to this source code.

# This version was translated into Julia by Scott P. Jones

# Note - The x86 and x64 versions do _not_ produce the same results, as the
# algorithms are optimized for their respective platforms. You can still
# compile and run any of them on any platform, but your performance with the
# non-native version will be less than optimal.

rotl(x::Unsigned, r) = (x << r) | (x >>> (sizeof(typeof(x))*8 - r))

#-----------------------------------------------------------------------------
# Finalization mix - force all bits of a hash block to avalanche

function fmix(k::UInt64)
    k = xor(k, k >> 33)
    k *= 0xff51afd7ed558ccd
    k = xor(k, k >> 33)
    k *= 0xc4ceb9fe1a85ec53
    xor(k, k >> 33)
end

# Note: this is designed to work on the Str type, where I know in advance that
# the start of the strings are 8-byte aligned, and it is safe to access a full
# 8-byte chunk always at the end (simply masking off the remaining 1-7 bytes)

mask_load(pnt, left) = unsafe_load(pnt) & ((1%UInt64 << ((left & 7) << 3)) - 0x1)

const c1 = 0x87c37b91114253d5
const c2 = 0x4cf5ad432745937f

#-----------------------------------------------------------------------------

function murmurhash128(len, pnt, seed::UInt32)
    nblocks = len >>> 4

    h1 = seed%UInt64
    h2 = seed%UInt64

    #----------
    # body

    pnt8 = reinterpret(Ptr{UInt64}, pnt)
    for i = 1:nblocks
        h1 = (rotl(xor(h1, rotl(unsafe_load(pnt8) * c1, 31) * c2), 27) + h2) * 5
        h1 += 0x52dce729
        pnt8 += 8
        h2 = (rotl(xor(h2, rotl(unsafe_load(pnt8) * c2, 33) * c1), 31) + h1) * 5
        h2 += 0x38495ab5
        pnt8 += 8
    end

    #----------
    # tail

    if (left = len & 15) > 8
        h1 = xor(h1, rotl(unsafe_load(pnt8) * c1, 31) * c2)
        h2 = xor(h2, rotl(mask_load(pnt8 + 8, left) * c2, 33) * c1)
    elseif left > 0
        h1 = xor(h1, rotl(mask_load(pnt8, left) * c1, 31) * c2)
    end

    #----------
    # finalization

    h1 = xor(h1, len%UInt64)
    h2 = xor(h2, len%UInt64)

    h1 += h2
    h2 += h1

    h1 = fmix(h1)
    h2 = fmix(h2)

    h1 += h2
    h2 += h1

    h1, h2
end

murmurhash128(str, seed::UInt32) =
    @preserve str murmurhash128(sizeof(str), pointer(str), seed)
