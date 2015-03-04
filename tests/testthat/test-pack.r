context("pack")
test_that("pack works", {

  ## Null-padded string
  x <- pack('a8', 'packrats')
  expect_equal(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73)))
  expect_equal(unpack('a8', x), list('packrats'))

  ## Null-padded string
  x <- pack('a10', 'packrats')
  expect_equal(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73, 0, 0)))
  expect_equal(unpack('a10', x), list('packrats'))

  ## Space-padded string
  x <- pack('A8', 'packrats')
  expect_equal(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73)))
  expect_equal(unpack('A8', x), list('packrats'))

  ## Space-padded string
  x <- pack('A10', 'packrats')
  expect_equal(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73, 0x20, 0x20)))
  # expect_equal(unpack('A10', x), list('packrats'))  # TODO Doesn't work yet
  # % perl -le '$x=pack "A10", "packrats"; $y=unpack "A10", $x; print "{$_}" for $x, $y'
  # {packrats  }
  # {packrats}


  ## Null-padded string with stars
  # x <- pack('a*', 'packrats')  # TODO Doesn't work yet
  # expect_equal(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73)))
  # expect_equal(unpack('a*', x), list('packrats'))
  # % perl -le '$x=pack "a*", "packrats"; $y=unpack "a*", $x; print "{$_}" for $x, $y'
  # {packrats}
  # {packrats}

  ## TODO Space-padded string with stars


  ## Something to work with for the tests below
  bvec <- c(1,1,0,1,0,0,1,0)

  ## Binary vector, little-endian
  x <- pack('b', as.raw(bvec))
  expect_equal(x, as.raw(sum(bvec * 2^(0:7))))  # 0x4b
  expect_equal(unpack('b', x), list(as.raw(bvec)))

  ## Binary vector, big-endian
  x <- pack('B', as.raw(bvec))
  # expect_equal(x, as.raw(sum(bvec * 2^(7:0))))  # 0xd2 # TODO Doesn't work yet
  # expect_equal(unpack('B', x), list(as.raw(bvec))) # TODO Doesn't work yet

  # TODO 'b' and 'B' with counts

  ## Character vector (basically a no-op)
  x <- pack('C', 70)
  expect_equal(x, as.raw(0x46))  # 'F' character
  expect_equal(unpack('C', x), list(0x46))

  # TODO 'C' with counts

  ## Unsigned 16-bit integer, little-endian
  x <- pack('v', 12345)
  expect_equal(x, as.raw(c(0x39, 0x30)))
  expect_equal(unpack('v', x), list(12345))

  ## Unsigned 32-bit integer, little-endian
  x <- pack('V', 1234567)
  expect_equal(x, as.raw(c(0x87, 0xd6, 0x12, 0)))
  expect_equal(unpack('V', x), list(1234567))

  ## Null bytes
  x <- pack('x', "foo")
  expect_equal(x, as.raw(0))
  # expect_equal(unpack('x', x), list(NULL))  # TODO Doesn't work yet

  # TODO 'x' with counts

  # Native 4-byte float
  x <- as.raw(rev(c(0x42, 0xf6, 0xe6, 0x66))) # No corresponding 'f' pack() field
  # expect_equal(unpack('f', x), list(123.45))  # TODO Doesn't work yet

  # Native 8-byte float
  x <- as.raw(c(0xcd, 0xcc, 0xcc, 0xcc, 0xcc, 0xdc, 0x5e, 0x40)) # No corresponding 'd' pack() field
  expect_equal(unpack('d', x), list(123.45))

  # Raw bytes
  x <- as.raw(c(0xcd, 0x40)) # No corresponding 'H' pack() field
  # expect_equal(unpack('H', x), list(0xcd))  # TODO doesn't work yet
  expect_equal(unpack('H2', x), list(x))
  expect_equal(unpack('H*', x), list(x))

  # TODO '/' count handling
})
