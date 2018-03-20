#-----------------------------------------------------------------------------
# MurmurHash3 was written by Austin Appleby, and is placed in the public
# domain. The author hereby disclaims copyright to this source code.

# This version was translated into Julia by Scott P. Jones
# It is licensed under the MIT license

# Note - The x86 and x64 versions do _not_ produce the same results, as the
# algorithms are optimized for their respective platforms. You can still
# compile and run any of them on any platform, but your performance with the
# non-native version will be less than optimal.

@inline rotl(x::Unsigned, r) = (x << r) | (x >>> (sizeof(typeof(x))*8 - r))

@inline xor33(k::UInt64) = xor(k, k >>> 33)

#-----------------------------------------------------------------------------
# Finalization mix - force all bits of a hash block to avalanche

@inline fmix(k::UInt64) = xor33(xor33(xor33(k) * 0xff51afd7ed558ccd) * 0xc4ceb9fe1a85ec53)

# Note: this is designed to work on the Str/String types, where I know in advance that
# the start of the strings are 8-byte aligned, and it is safe to access a full
# 8-byte chunk always at the end (simply masking off the remaining 1-7 bytes)

@inline mask_load(pnt, left) = unsafe_load(pnt) & ((1%UInt64 << ((left & 7) << 3)) - 0x1)

const c1 = 0x87c37b91114253d5
const c2 = 0x4cf5ad432745937f

@inline function mhblock(h1, h2, k1, k2)
    h1 = (rotl(xor(h1, rotl(k1 * c1, 31) * c2), 27) + h2) * 5 + 0x52dce729
    h2 = (rotl(xor(h2, rotl(k2 * c2, 33) * c1), 31) + h1) * 5 + 0x38495ab5
    h1, h2
end

@inline function mhbody(nblocks, pnt, h1, h2)
    for i = 1:nblocks
        h1, h2 = mhblock(h1, h2, unsafe_load(pnt), unsafe_load(pnt + 8))
        pnt += 16
    end
    pnt, h1, h2
end

@inline mhtail1(h1, k1) = xor(h1, rotl(k1 * c1, 31) * c2)
@inline mhtail2(h2, k2) = xor(h2, rotl(k2 * c2, 33) * c1)

@inline function mhfin(len, h1, h2)
    h1 = xor(h1, len%UInt64)
    h2 = xor(h2, len%UInt64)

    h1 += h2
    h2 += h1

    h1 = fmix(h1)
    h2 = fmix(h2)

    h1 += h2
    h1, h1 + h2
end

#-----------------------------------------------------------------------------

function mmhash_128(len, pnt, seed::UInt32)
    pnt8, h1, h2 = mhbody(len >>> 4, reinterpret(Ptr{UInt64}, pnt), seed%UInt64, seed%UInt64)
    if (left = len & 15) > 0
        h1 = mhtail1(h1, left < 8 ? mask_load(pnt8, left) : unsafe_load(pnt8))
        left > 8 && (h2 = mhtail2(h2, mask_load(pnt8 + 8, left)))
    end
    mhfin(len, h1, h2)
end

mmhash_128(str, seed::UInt32) = @preserve str mmhash_128(sizeof(str), pointer(str), seed)

function mmhash_128(seed::UInt32)
    h1 = fmix((seed%UInt64)<<1)
    h2 = fmix(h1 + seed%UInt64)
    h1 += h2
    h1, h1 + h2
end
