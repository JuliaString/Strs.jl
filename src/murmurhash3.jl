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

@inline rotl27(k) = rotl(k, 27)
@inline rotl31(k) = rotl(k, 31)
@inline rotl33(k) = rotl(k, 33)

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
    h1 = (rotl27(xor(h1, rotl31(k1 * c1) * c2)) + h2) * 5 + 0x52dce729
    h2 = (rotl31(xor(h2, rotl33(k2 * c2) * c1)) + h1) * 5 + 0x38495ab5
    h1, h2
end

@inline function mhbody(nblocks, pnt, h1, h2)
    for i = 1:nblocks
        h1, h2 = mhblock(h1, h2, unsafe_load(pnt), unsafe_load(pnt + 8))
        pnt += 16
    end
    pnt, h1, h2
end

@inline mhtail1(h1, k1) = xor(h1, rotl31(k1 * c1) * c2)
@inline mhtail2(h2, k2) = xor(h2, rotl33(k2 * c2) * c1)

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

function mmhash128_8(len, pnt, seed::UInt32)
    pnt8, h1, h2 = mhbody(len >>> 4, reinterpret(Ptr{UInt64}, pnt), seed%UInt64, seed%UInt64)
    if (left = len & 15) > 0
        h1 = mhtail1(h1, left < 8 ? mask_load(pnt8, left) : unsafe_load(pnt8))
        left > 8 && (h2 = mhtail2(h2, mask_load(pnt8 + 8, left)))
    end
    mhfin(len, h1, h2)
end

function mmhash128_8(seed)
    h1 = fmix(2*(seed%UInt64))
    h2 = fmix(3*(seed%UInt64))
    h1 + h2, h1 + 2*h2
end

#---------------------------------------------------------------------------

# Optimized in-place conversion to UTF-8 for hashing compatibly with isequal / String
@inline shift_n(v, n) = v%UInt64 << ((n & 7)%UInt<<3)

# if cnt == 0 - 4, bytes must fit in k1
# cnt between 5 - 8, may overflow into k2
# if l == 8 - 12,  bytes must fit in k2
# cnt between 12 - 15, may overflow into k3

@inline function add_utf8(cnt, ch, k1::UInt64)
    if ch <= 0x7f
        cnt + 1, k1 | shift_n(ch, cnt)
    elseif ch <= 0x7ff
        b1, b2 = get_utf8_2(ch)
        cnt + 2, k1 | shift_n(b1 | b2%UInt32<<8, cnt)
    elseif ch <= 0xffff
        b1, b2, b3 = get_utf8_3(ch)
        cnt + 3, k1 | shift_n(b1 | b2%UInt32<<8 | b3%UInt32<<16, cnt)
    else
        b1, b2, b3, b4 = get_utf8_4(ch)
        cnt + 4, k1 | shift_n(b1 | b2%UInt32<<8 | b3%UInt32<<16 | b4%UInt32<<24, cnt)
    end
end

@inline function add_utf8_split(cnt, ch, k1::UInt64)
    if ch <= 0x7f
        cnt + 1, k1 | shift_n(ch, cnt), 0%UInt64
    elseif ch <= 0x7ff
        b1, b2 = get_utf8_2(ch)
        if (cnt & 7) == 7
            cnt + 2, k1 | b1%UInt64<<56, b2%UInt64
        else
            cnt + 2, k1 | shift_n(b1 | b2%UInt32<<8, cnt), 0%UInt64
        end
    elseif ch <= 0xffff
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
            cnt + 4, k1 | b1%UInt64<<40 | b2%UInt64<<48 | b3%UInt64<<56, b4%UInt64
        elseif (cnt & 7) == 6
            cnt + 4, k1 | b1%UInt64<<48 | b2%UInt64<<56, b3 | b4%UInt64<<8
        else
            cnt + 4, k1 | b1%UInt64<<56, b2 | b3%UInt64<<8 | b4%UInt64<<16
        end
    end
end

# AbstractString MurmurHash3, converts to UTF-8 on the fly
function mmhash128_8_a(str, seed::UInt32)
    k1 = k2 = 0%UInt64
    h1 = h2 = seed%UInt64
    cnt = len = 0
    @inbounds for ch in str
        if cnt < 5
            cnt, k1 = add_utf8(cnt, ch%UInt32, k1)
        elseif cnt < 8
            cnt, k1, k2 = add_utf8_split(cnt, ch%UInt32, k1)
        elseif cnt < 13
            cnt, k2 = add_utf8(cnt, ch%UInt32, k2)
        else
            cnt, k2, k3 = add_utf8_split(cnt, ch%UInt32, k2)
            # When k1 and k2 are full, then hash another block
            if cnt > 15
                h1, h2 = mhblock(h1, h2, k1, k2)
                k1 = k3
                k2 = 0%UInt64
                len += 16
                cnt &= 15
            end
        end
    end
    # We should now have characters in k1 and k2, and total length in len
    if cnt != 0
        h1 = mhtail1(h1, k1)
        cnt > 8 && (h2 = mhtail2(h2, k2))
    end
    mhfin(len + cnt, h1, h2)
end

mmhash128_8(str::AbstractString, seed::UInt32) = mmhash128_8_a(str, seed)

#----------------------------------------------------------------------------

# Combine bits from k1, k2, k3
@inline shift_mix(shft, k1::T, k2::T, k3::T) where {T<:Union{UInt32,UInt64}} =
    k1 >>> shft | (k2 << (sizeof(T)*8 - shft)),
    k2 >>> shft | (k3 << (sizeof(T)*8 - shft)),
    k3 >>> shft

#----------------------------------------------------------------------------

function mmhash128_8_u(len, unaligned_pnt, seed::UInt32)
    # Should optimize handling of short (< 16 byte) unaligned strings
    ulp = reinterpret(UInt, unaligned_pnt)
    pnt = reinterpret(Ptr{UInt64}, ulp & ~(7%UInt64))
    fin = reinterpret(Ptr{UInt64}, (ulp + len + 0x7) & ~(7%UInt64)) - 8
    shft = (ulp & 7%UInt)<<3
    # println("_mmhash128_8_u($len, $unaligned_pnt, $seed) => $pnt, $fin")
    h1 = h2 = seed%UInt64
    k1 = unsafe_load(pnt) # Pick up first 1-7 bytes
    k2 = 0%UInt64
    while pnt < fin
        k1, k2, k3 = shift_mix(shft, k1, unsafe_load(pnt += 8), unsafe_load(pnt += 8))
        # print(" pnt=$pnt, k1=0x$(outhex(k1)), k2=0x$(outhex(k2))")
        h1, h2 = mhblock(h1, h2, k1, k2)
        # println(" => h1=0x$(outhex(h1)), h2=0x$(outhex(h2))")
        k1 = k3
    end
    # We should now have characters in k1 and k2, and total length in len
    if (len & 15) != 0
        h1 = mhtail1(h1, k1)
        (len & 15) > 8 && (h2 = mhtail2(h2, k2))
    end
    # println(" len=$len, h1=0x$(outhex(h1)), h2=0x$(outhex(h2))")
    mhfin(len, h1, h2)
end

const mmhash128   = @static sizeof(Int) == 8 ? mmhash128_8   : mmhash128_4
const mmhash128_a = @static sizeof(Int) == 8 ? mmhash128_8_a : mmhash128_4_a
const mmhash128_u = @static sizeof(Int) == 8 ? mmhash128_8_u : mmhash128_4_u

#----------------------------------------------------------------------------

mmhash128(str::Union{String, Str}, seed::UInt32) =
    @preserve str mmhash128(sizeof(str), pointer(str), seed)

is_aligned(pnt::Ptr) = (reinterpret(UInt, pnt) & (sizeof(UInt) - 1)%UInt) == 0

# Check alignment of substrings first
function mmhash128(str::SubString, seed::UInt32)
    @preserve str begin
        siz = sizeof(str)
        pnt = pointer(str)
        is_aligned(pnt) ? mmhash128(siz, pnt, seed) : mmhash128_u(siz, pnt, seed)
    end
end

#----------------------------------------------------------------------------

@inline xor16(k::UInt32) = xor(k, k >>> 16)
@inline xor13(k::UInt32) = xor(k, k >>> 13)

# I don't think these help the generated code anymore (but they make the code easier to read)
@inline rotl13(k) = rotl(k, 13)
@inline rotl15(k) = rotl(k, 15)
@inline rotl16(k) = rotl(k, 16)
@inline rotl17(k) = rotl(k, 17)
@inline rotl18(k) = rotl(k, 18)
@inline rotl19(k) = rotl(k, 19)

# Constants for mmhash_32
const d1 = 0xcc9e2d51
const d2 = 0x1b873593

@inline fmix(h::UInt32) = xor16(xor13(xor16(h) * 0x85ebca6b) * 0xc2b2ae35)

@inline mhblock(h1, k1) = rotl13(xor(h1, rotl15(k1 * d1) * d2))*5 + 0xe6546b64

@inline function mhbody(nblocks, pnt, h1)
    for i = 1:nblocks
        h1 = mhblock(h1, unsafe_load(pnt))
        pnt += 4
    end
    pnt, h1
end

function mmhash32(len, pnt, seed::UInt32)
    pnt, h1 = mhbody(len >>> 2, reinterpret(Ptr{UInt32}, pnt), seed)
    (len & 3) == 0 || (h1 = xor(h1, rotl15(unsafe_load(pnt)) * d1) * d2)
    fmix(xor(h1, len%UInt32))
end

@inline function mhfin(len, h1, h2, h3, h4)
    h1 = xor(h1, len%UInt32)
    h2 = xor(h2, len%UInt32)
    h3 = xor(h3, len%UInt32)
    h4 = xor(h4, len%UInt32)

    h1 += h2; h1 += h3; h1 += h4; h2 += h1; h3 += h1; h4 += h1

    h1 = fmix(h1)
    h2 = fmix(h2)
    h3 = fmix(h3)
    h4 = fmix(h4)

    h1 += h2; h1 += h3; h1 += h4; h2 += h1; h3 += h1; h4 += h1

    h2%UInt64 << 32 | h1, h4%UInt64 << 32 | h3
end

#-----------------------------------------------------------------------------

# Calculate MurmurHash for 32-bit platforms

# Constants for mmash128_4
const e1 = 0x239b961b
const e2 = 0xab0e9789
const e3 = 0x38b34ae5
const e4 = 0xa1e38b93

@inline function mhblock(h1, h2, h3, h4, k1, k2, k3, k4)
    h1 = (rotl19(xor(h1, rotl15(k1 * e1) * e2)) + h2)*5 + 0x561ccd1b
    h2 = (rotl17(xor(h2, rotl16(k2 * e2) * e3)) + h3)*5 + 0x0bcaa747
    h3 = (rotl15(xor(h3, rotl17(k3 * e3) * e4)) + h4)*5 + 0x96cd1c35
    h4 = (rotl13(xor(h4, rotl18(k4 * e4) * e1)) + h1)*5 + 0x32ac3b17
    h1, h2, h3, h4
end

@inline function mhbody(nblocks, pnt, h1, h2, h3, h4)
    for i = 1:nblocks
        h1, h2, h3, h4 =
            mhblock(h1, h2, h3, h4,
                    unsafe_load(pnt), unsafe_load(pnt+4), unsafe_load(pnt+8), unsafe_load(pnt+12))
        pnt += 16
    end
    pnt, h1, h2, h3, h4
end

function mmhash128_4(len, pnt, seed::UInt32)
    pnt, h1, h2, h3, h4 = mhbody(len >>> 4, pnt, seed, seed, seed, seed)
    if (left = len & 15) != 0
        h1  = xor(h1, rotl16(k1 * c1) * c2)
        if left > 4
            h2  = xor(h2, rotl16(k2 * c2) * c3)
            if left > 8
                h3  = xor(h3, rotl17(k3 * c3) * c4)
                left > 12 && (h4  = xor(h4, rotl18(k4 * c4) * c1))
            end
        end
    end
    mhfin(len, h1, h2, h3, h4)
end

@inline shift_n_32(v, n) = v%UInt32 << ((n & 7)%UInt<<3)

@inline function get_utf8(cnt, ch)
    if ch <= 0x7f
        cnt + 1, ch%UInt32
    elseif ch <= 0x7ff
        b1, b2 = get_utf8_2(ch)
        cnt + 2, b1 | b2%UInt32<<8
    elseif ch <= 0xffff
        b1, b2, b3 = get_utf8_3(ch)
        cnt + 3, b1 | b2%UInt32<<8 | b3%UInt32<<16
    else
        b1, b2, b3, b4 = get_utf8_4(ch)
        cnt + 4, b1 | b2%UInt32<<8 | b3%UInt32<<16 | b4%UInt32<<24
    end
end

@inline function add_utf8_split(cnt, ch, k1::UInt32)
    if ch <= 0x7f
        cnt + 1, k1 | shift_n_32(ch, cnt), 0%UInt32
    elseif ch <= 0x7ff
        b1, b2 = get_utf8_2(ch)
        if (cnt & 7) == 3
            cnt + 2, k1 | b1%UInt32<<24, b2%UInt32
        else
            cnt + 2, k1 | shift_n_32(b1 | b2%UInt32<<8, cnt), 0%UInt32
        end
    elseif ch <= 0xffff
        b1, b2, b3 = get_utf8_3(ch)
        if (cnt & 7) == 1
            cnt + 3, k1 | b1%UInt32<<8 | b2%UInt32<<16 | b3%UInt32<<24, 0%UInt32
        elseif (cnt & 7) == 2
            cnt + 3, k1 | b1%UInt32<<16 | b2%UInt32<<24, b3%UInt32
        else
            cnt + 3, k1 | b1%UInt32<<24, b2 | b3%UInt32<<8
        end
    else
        # This will always go over, may be 1, 2, 3 bytes in second word
        b1, b2, b3, b4 = get_utf8_4(ch)
        if (cnt & 7) == 1
            cnt + 4, k1 | b1%UInt32<<8 | b2%UInt32<<16 | b3%UInt32<<24, b4%UInt32
        elseif (cnt & 7) == 2
            cnt + 4, k1 | b1%UInt32<<16 | b2%UInt32<<24, b3 | b4%UInt32<<8
        else
            cnt + 4, k1 | b1%UInt32<<24, b2 | b3%UInt32<<8 | b4%UInt32<<16
        end
    end
end

# This are for debugging purposes, to compare against current C implementation, will be removed
_memhash(siz, ptr, seed) =
    ccall(Base.memhash, UInt, (Ptr{UInt8}, Csize_t, UInt32), ptr, siz, seed % UInt32)
