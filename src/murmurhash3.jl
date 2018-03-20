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
        k1, k2 = unsafe_load(pnt), unsafe_load(pnt + 8)
        h1, h2 = mhblock(h1, h2, unsafe_load(pnt), unsafe_load(pnt + 8))
        println(" i=$i, h1=0x$(outhex(h1)), h2=0x$(outhex(h2))")
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

function mmhash_128(seed::UInt32)
    h1 = fmix((seed%UInt64)<<1)
    h2 = fmix(h1 + seed%UInt64)
    h1 += h2
    h1, h1 + h2
end

#---------------------------------------------------------------------------

# Optimized in-place conversion to UTF-8 for hashing compatibly with isequal / String
@inline shift_n(v, n) = v%UInt64 << ((n & 7)%UInt<<3)

# if cnt == 0 - 4, bytes must fit in k1
# cnt between 5 - 8, may overflow into k2
# if l == 8 - 12,  bytes must fit in k2
# cnt between 12 - 15, may overflow into k3

@inline function add_char(cnt, ch, k1)
    if ch < 0x80
        cnt + 1, k1 | shift_n(ch, cnt)
    elseif ch < 0xe0
        b1, b2 = get_utf8_2(ch)
        cnt + 2, k1 | shift_n(b1 | b2%UInt32<<8, cnt)
    elseif ch < 0xf0
        b1, b2, b3 = get_utf8_3(ch)
        cnt + 3, k1 | shift_n(b1 | b2%UInt32<<8 | b3%UInt32<<16, cnt)
    else
        b1, b2, b3, b4 = get_utf8_4(ch)
        cnt + 4, k1 | shift_n(b1 | b2%UInt32<<8 | b3%UInt32<<16 | b4%UInt32<<24, cnt)
    end
end

@inline function add_char_split(cnt, ch, k1)
    if ch < 0x80
        cnt + 1, k1 | shift_n(ch, cnt), 0%UInt64
    elseif ch < 0xe0
        b1, b2 = get_utf8_2(ch)
        if (cnt & 7) == 7
            cnt + 2, k1 | b1%UInt64<<56, b2%UInt64
        else
            cnt + 2, k1 | shift_n(b1 | b2%UInt32<<8, cnt), 0%UInt64
        end
    elseif ch < 0xf0
        b1, b2, b3 = get_utf8_3(ch)
        if (cnt & 7) == 5
            cnt + 3, k1 | b1%UInt64<<40 | b2%UInt64<<48 | b3%UInt64<<56, 0%UInt64
        elseif (cnt & 7) == 6
            cnt + 3, k1 | b1%UInt64<<48 | b2%UInt64<<56, b3%UInt64
        else
            cnt + 3, k1 | b1%UInt64<<56, b2 | b3%UInt64<<8
        end
    else
        # This will always go over, may be 1, 2, 3 bytes in second word
        b1, b2, b3, b4 = get_utf8_4(ch)
        if (cnt & 7) == 5
            k1 |= b1%UInt64<<40 | b2%UInt64<<48 | b3%UInt64<<56
            k2 =  b4%UInt64
        elseif (cnt & 7) == 6
            k1 |= b1%UInt64<<48 | b2%UInt64<<56
            k2 =  b3 | b4%UInt64<<8
        else
            k1 |= b1%UInt64<<56
            k2 =  b2 | b3%UInt64<<8 | b4%UInt64<<16
        end
        cnt + 4, k1, k2
    end
end

function _mmhash_128(str, seed::UInt32)
    println("_mmhash_128($(typeof(str))(\"$str\"), $seed)")
    k1 = k2 = 0%UInt64
    h1 = h2 = seed%UInt64
    cnt = len = 0
    @inbounds for ch in str
        println(" cnt=$cnt, k1=0x$(outhex(k1)), k2=0x$(outhex(k2))")
        if cnt < 5
            cnt, k1 = add_char(cnt, ch%UInt32, k1)
        elseif cnt < 8
            cnt, k1, k2 = add_char_split(cnt, ch%UInt32, k1)
        elseif cnt < 13
            cnt, k2 = add_char(cnt, ch%UInt32, k2)
        else
            cnt, k2, k3 = add_char_split(cnt, ch%UInt32, k2)
        println(" cnt=$cnt, k2=0x$(outhex(k1)), k3=0x$(outhex(k2))")
            # When k1 and k2 are full, then hash another block
            if cnt > 15
                h1, h2 = mhblock(h1, h2, k1, k2)
                k1 = k3
                k2 = 0%UInt64
                len += 16
                cnt &= 15
println(" len=$len, cnt=$cnt, k1=0x$(outhex(k1)), h1=0x$(outhex(h1)), h2=0x$(outhex(h2))")
            end
        end
    end
println(" => cnt=$cnt, k1=0x$(outhex(k1)), k2=0x$(outhex(k2)), h1=0x$(outhex(h1)), h2=0x$(outhex(h2))")
    # We should now have characters in k1 and k2, and total length in len
    if cnt != 0
        h1 = mhtail1(h1, k1)
        cnt > 8 && (h2 = mhtail2(h2, k2))
    end
    println(" len=$(len+cnt), h1=0x$(outhex(h1)), h2=0x$(outhex(h2))")
    mhfin(len + cnt, h1, h2)
end

mmhash_128(str::AbstractString, seed::UInt32) = _mmhash_128(str, seed)

#----------------------------------------------------------------------------

mmhash_128(str::Union{String, Str}, seed::UInt32) =
    @preserve str mmhash_128(sizeof(str), pointer(str), seed)

is_aligned(pnt::Ptr) = (reinterpret(UInt, pnt) & (sizeof(UInt) - 1)) == 0

# Check alignment of substrings first
function mmhash_128(str::SubString, seed::UInt32)
    @preserve str begin
        siz = sizeof(str)
        pnt = pointer(str)
        is_aligned(pnt) ? mmhash_128(siz, pnt, seed) : _mmhash_128(str, seed)
    end
end
