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

  ## Null-padded string with stars
  # x <- pack('a*', 'packrats')  # TODO Doesn't work yet
  # expect_equal(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73)))
  # expect_equal(unpack('a*', x), list('packrats'))

  ## TODO Space-padded string with stars


  ## Binary vector, little-endian
  x <- pack('b', as.raw(c(1,1,0,1,0,0,1,0)))
  expect_equal(x, as.raw(0x4b))
  expect_equal(unpack('b', x), list(as.raw(c(1,1,0,1,0,0,1,0))))

  ## Binary vector, big-endian
  x <- pack('B', as.raw(c(1,1,0,1,0,0,1,0)))
  # expect_equal(x, as.raw(0xd2))  # TODO Doesn't work yet
  # expect_equal(unpack('B', x), list(as.raw(c(1,1,0,1,0,0,1,0)))) # TODO Doesn't work yet

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

  # TODO '/' count handling
})
