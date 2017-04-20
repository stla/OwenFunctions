context("OwenT")

test_that("Owen T is symmetric", {
  expect_true(OwenT(2,1) == OwenT(-2,1))
})

test_that("OwenT(0,a)", {
  a <- 1
  expect_equal(OwenT(0,a), atan(a)/(2*pi), tolerance=1e-14)
})

test_that("OwenT(h,1)", {
  h <- 1
  expect_equal(OwenT(h,1), pnorm(h)*(1-pnorm(h))/2, tolerance=1e-14)
})

test_that("Relation OwenT Cauchy", {
  h <- 2; a <- 2
  expect_equal(OwenT(h, a), 1/2*(pt(a, 1, h*sqrt(1+a^2)) - pnorm(-h)), tolerance=1e-12)
  expect_equal(OwenT(h, a),
               1/2*(pStudent(a, 1, h*sqrt(1+a^2)) - pnorm(-h)), tolerance=1e-17)
})

test_that("Comparison Mathematica", {
  expect_equal(OwenT(1,2), 0.078468186993084, tolerance=1e-14)
})
