
## Null-padded string
test.null_padded_string_nopad <- function() {
  x <- pack('a8', 'packrats')
  # y <- readBin(pipe("perl -e 'print pack(q{a8}, q{packrats})'", "rb"), raw(), n=8)
  y <- as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73))
  checkIdentical(x, y)
  checkIdentical(unpack('a8', x), list('packrats'))
}

test.null_padded_string_pad <- function() {
  x <- pack('a10', 'packrats')
  # y <- readBin(pipe("perl -e 'print pack(q{a8}, q{packrats})'", "rb"), raw(), n=8)
  y <- as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73, 0, 0))
  checkIdentical(x, y)
  checkIdentical(unpack('a10', x), list('packrats'))
}

## Space-padded string
test.space_padded_string_nopad <- function() {
  x <- pack('A8', 'packrats')
  y <- as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73))
  checkIdentical(x, y)
  checkIdentical(unpack('A8', x), list('packrats'))
}

test.space_padded_string_pad <- function() {
  x <- pack('A10', 'packrats')
  y <- as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73, 0x20, 0x20))
  checkIdentical(x, y)
  checkIdentical(unpack('A10', x), list('packrats'))
  # % perl -le '$x=pack "A10", "packrats"; $y=unpack "A10", $x; print "{$_}" for $x, $y'
  # {packrats  }
  # {packrats}
}

## Null-padded string with star
test.null_padded_string_star <- function() {
  x <- pack('a*', 'packrats')
  checkIdentical(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73)))
  checkIdentical(unpack('a*', x), list('packrats'))
  # % perl -le '$x=pack "a*", "packrats"; $y=unpack "a*", $x; print "{$_}" for $x, $y'
  # {packrats}
  # {packrats}
}

## Space-padded string with star
test.space_padded_string_star <- function() {
  x <- pack('A*', 'packrats')
  checkIdentical(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73)))
  checkIdentical(unpack('A*', x), list('packrats'))
  # % perl -le '$x=pack "A*", "packrats"; $y=unpack "A*", $x; print "{$_}" for $x, $y'
  # {packrats}
  # {packrats}
}

## TODO Space-padded string with stars


## Something to work with for the tests below
bvec <- c(1,1,0,1,0,0,1,0)

## Binary vector, little-endian
test.binary_little_endian <- function() {
  x <- pack('b', as.raw(bvec))
  checkIdentical(x, as.raw(sum(bvec * 2^(0:7))))  # 0x4b
  checkIdentical(unpack('b', x), list(as.raw(bvec)))
}

## Binary vector, big-endian
test.binary_big_endian <- function() {
  x <- pack('B', as.raw(bvec))
  checkIdentical(x, as.raw(sum(bvec * 2^(7:0))))
  checkIdentical(unpack('B', x), list(as.raw(bvec)))
}

# TODO 'b' and 'B' with counts


## Unsigned char (8 bits)
test.uint8 <- function() {
  r <- as.raw(c(0x00, 0xff))
  x <- pack('C C', 0L, 255L)  # min and max values
  checkIdentical(x, r)
  checkIdentical(unpack('C C', r), list(0L, 255L))
}

test.uint8_fails_LT0 <- function() {
  checkException(pack('C', -1L))
  # Unsure how to test unpack() here. You can always unpack a byte as an
  # unsigned 8-bit int, even though it would be incorrect if that wasn't
  # how it was packed.
  #checkException(unpack('C', c(0x00, 0xff)), list(-1L))
}

test.uint8_fails_GT255 <- function() {
  checkException(pack('C', 256L))
  # Unsure how to test unpack() here. You can always unpack a byte as an
  # unsigned 8-bit int, even though it would be incorrect if that wasn't
  # how it was packed.
  #checkException(unpack('C', c(0x00, 0xff)), list(256L))
}

# TODO 'C' with counts

## Unsigned 16-bit integer, little-endian
test.uint16_little_endian <- function() {
  x <- pack('v', 12345)
  # y <- readBin(pipe("perl -e 'print pack(q{v}, 12345)'", "rb"), raw(), n=2)
  y <- as.raw(c(0x39, 0x30))
  checkIdentical(x, y)
  checkIdentical(unpack('v', x), list(12345))
}

## Unsigned 32-bit integer, little-endian
test.uint32_little_endian <- function() {
  x <- pack('V', 1234567)
  # y <- readBin(pipe("perl -e 'print pack(q{V}, 1234567)'", "rb"), raw(), n=4)
  y <- as.raw(c(0x87, 0xd6, 0x12, 0))
  checkIdentical(x, y)
  checkIdentical(unpack('V', x), list(1234567))
}

## Null bytes
test.null_bytes <- function() {
  x <- pack('x', "foo")  # input data doesn't matter
  checkIdentical(x, as.raw(0))
  checkIdentical(unpack('x', x), list())
}

# TODO 'x' with counts

# Native 4-byte float
test.native_single_float <- function() {
  x <- pack('f', 123.45)
  # y <- readBin(pipe("perl -e 'print pack(q{f}, 123.45)'", "rb"), raw(), n=4)
  y <- as.raw(c(0x66, 0xe6, 0xf6, 0x42))
  if(.Platform$endian=="big")  # y was generated on little-endian machine
    y <- rev(y)
  eps <- sqrt(2^-23)
  checkEqualsNumeric(x, y, tolerance=eps)
  checkEquals(unpack('f', x), list(123.45), tolerance=eps)

  # Should coerce input to number, the same way Perl does
  x <- pack('f', '123.45')
  checkEqualsNumeric(x, y, tolerance=eps)
}

# Native 8-byte float
test.native_double_float <- function() {
  x <- pack('d', 123.45)
  # y <- readBin(pipe("perl -e 'print pack(q{d}, 123.45)'", "rb"), raw(), n=8)
  y <- as.raw(c(0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40))
  if(.Platform$endian=="big")  # y was generated on little-endian machine
    y <- rev(y)
  eps <- sqrt(2^-52)
  checkEqualsNumeric(x, y, tolerance=eps)
  checkEquals(unpack('d', x), list(123.45), tolerance=eps)

  # Should coerce input to number, the same way Perl does
  x <- pack('d', '123.45')
  checkEqualsNumeric(x, y, tolerance=eps)
}

# Raw bytes
test.raw_bytes <- function() {
  x <- as.raw(c(0xcd, 0x40)) # No corresponding 'H' pack() field
  checkIdentical(unpack('H',  x), list(x[1]))
  checkIdentical(unpack('H1', x), list(x[1]))
  checkIdentical(unpack('H2', x), list(x))
  checkIdentical(unpack('H*', x), list(x))
}

# TODO '/' count handling
