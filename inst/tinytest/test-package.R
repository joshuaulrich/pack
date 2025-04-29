
# target <- readBin(pipe("perl -e 'print pack(q{a8}, q{packrats})'", "rb"), raw(), n=8)
target_no_pad <- as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73))
target_null_pad <- as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73, 0, 0))
target_space_pad <- as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73, 0x20, 0x20))

## Null-padded string
msg <- "null padded string pattern - no pad"
a8 <- pack('a8', 'packrats')
expect_identical(a8, target_no_pad, info = msg)
expect_identical(unpack('a8', a8), list('packrats'), info = msg)

msg <- "null padded string pattern - w/pad"
a10 <- pack('a10', 'packrats')
expect_identical(a10, target_null_pad, info = msg)
expect_identical(unpack('a10', a10), list('packrats'), info = msg)

## Space-padded string
msg <- "space padded string pattern - no pad"
A8 <- pack('A8', 'packrats')
expect_identical(A8, target_no_pad, info = msg)
expect_identical(unpack('A8', A8), list('packrats'), info = msg)

msg <- "space padded string pattern - w/pad"
A10 <- pack('A10', 'packrats')
expect_identical(A10, target_space_pad, info = msg)
expect_identical(unpack('A10', A10), list('packrats'), info = msg)
# % perl -le '$x=pack "A10", "packrats"; $y=unpack "A10", $x; print "{$_}" for $x, $y'
# {packrats  }
# {packrats}

## Null-padded string with star
msg <- "null padded string* - no pad"
a_star <- pack('a*', 'packrats')
expect_identical(a_star, target_no_pad, info = msg)
expect_identical(unpack('a*', a_star), list('packrats'), info = msg)
# % perl -le '$x=pack "a*", "packrats"; $y=unpack "a*", $x; print "{$_}" for $x, $y'
# {packrats}
# {packrats}

msg <- "space padded string* - no pad"
A_star <- pack('A*', 'packrats')
expect_identical(A_star, target_no_pad, info = msg)
expect_identical(unpack('A*', A_star), list('packrats'), info = msg)
# % perl -le '$x=pack "A*", "packrats"; $y=unpack "A*", $x; print "{$_}" for $x, $y'
# {packrats}
# {packrats}


## TODO Space-padded string with stars


## Something to work with for the tests below
bvec <- c(1,1,0,1,0,0,1,0)

# Binary vector, little-endian
msg <- "little-endian binary vector"
x <- pack('b', as.raw(bvec))
expect_identical(x, as.raw(sum(bvec * 2^(0:7))), info = msg)  # 0x4b
expect_identical(unpack('b', x), list(as.raw(bvec)), info = msg)

## Binary vector, big-endian
msg <- "big-endian binary vector"
x <- pack('B', as.raw(bvec))
expect_identical(x, as.raw(sum(bvec * 2^(7:0))), info = msg)
expect_identical(unpack('B', x), list(as.raw(bvec)), info = msg)

# TODO 'b' and 'B' with counts


## Unsigned char (8 bits)
msg <- "8-bit unsigned char"
r <- as.raw(c(0x00, 0xff))
x <- pack('C C', 0L, 255L)  # min and max values
expect_identical(x, r, info = msg)
expect_identical(unpack('C C', r), list(0L, 255L), info = msg)

msg <- "8-bit unsigned char < 0"
expect_error(pack('C', -1L), info = msg)
# Unsure how to test unpack() here. You can always unpack a byte as an
# unsigned 8-bit int, even though it would be incorrect if that wasn't
# how it was packed.
#expect_error(unpack('C', c(0x00, 0xff)), list(-1L), info = msg)

msg <- "8-bit unsigned char > 255"
expect_error(pack('C', 256L), info = msg)
# Unsure how to test unpack() here. You can always unpack a byte as an
# unsigned 8-bit int, even though it would be incorrect if that wasn't
# how it was packed.
#expect_error(unpack('C', c(0x00, 0xff)), list(256L), info = msg)

# TODO 'C' with counts

## Unsigned 16-bit integer, little-endian
msg <- "16-bit unsigned integer, little-endian"
x <- pack('v', 12345)
# y <- readBin(pipe("perl -e 'print pack(q{v}, 12345)'", "rb"), raw(), n=2)
y <- as.raw(c(0x39, 0x30))
expect_identical(x, y, info = msg)
expect_identical(unpack('v', x), list(12345), info = msg)

## Unsigned 32-bit integer, little-endian
msg <- "32-bit unsigned integer, little-endian"
x <- pack('V', 1234567)
# y <- readBin(pipe("perl -e 'print pack(q{V}, 1234567)'", "rb"), raw(), n=4)
y <- as.raw(c(0x87, 0xd6, 0x12, 0))
expect_identical(x, y, info = msg)
expect_identical(unpack('V', x), list(1234567), info = msg)

## Null bytes
x <- pack('x', "foo")  # input data doesn't matter
expect_identical(x, as.raw(0), info = "null bytes")
expect_identical(unpack('x', x), list(), info = "null bytes")

# TODO 'x' with counts

# Native 4-byte float
msg <- "native 4-byte float"
x <- pack('f', 123.45)
# y <- readBin(pipe("perl -e 'print pack(q{f}, 123.45)'", "rb"), raw(), n=4)
y <- as.raw(c(0x66, 0xe6, 0xf6, 0x42))
if (.Platform$endian == "big") {
    # y was generated on little-endian machine
    y <- rev(y)
}
eps <- sqrt(2^-23)
expect_equal(x, y, tolerance = eps, info = msg)
expect_equal(unpack('f', x), list(123.45), tolerance = eps, info = msg)
x <- pack('f', '123.45')  # Should coerce input to number, the same way Perl does
expect_equal(x, y, tolerance = eps)

# Native 8-byte float
msg <- "native 8-byte float"
x <- pack('d', 123.45)
# y <- readBin(pipe("perl -e 'print pack(q{d}, 123.45)'", "rb"), raw(), n=8)
y <- as.raw(c(0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40))
if (.Platform$endian == "big") {
    # y was generated on little-endian machine
    y <- rev(y)
}
eps <- sqrt(2^-52)
expect_equal(x, y, tolerance = eps, info = msg)
expect_equal(unpack('d', x), list(123.45), tolerance = eps, info = msg)
x <- pack('d', '123.45')  # Should coerce input to number, the same way Perl does
expect_equal(x, y, tolerance = eps)

# Raw bytes
msg <- "raw bytes"
x <- as.raw(c(0xcd, 0x40)) # No corresponding 'H' pack() field
expect_identical(unpack('H',  x), list(x[1]), info = msg)
expect_identical(unpack('H1', x), list(x[1]), info = msg)
expect_identical(unpack('H2', x), list(x), info = msg)
expect_identical(unpack('H*', x), list(x), info = msg)

# TODO '/' count handling
