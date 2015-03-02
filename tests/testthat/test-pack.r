context("pack")
test_that("pack works", {
  
  ## Round-trip on a string
  x <- pack('a8', 'packrats')
  expect_equal(x, as.raw(c(0x70, 0x61, 0x63, 0x6b, 0x72, 0x61, 0x74, 0x73)))
  expect_equal(unpack('a8', x), list('packrats'))
})
