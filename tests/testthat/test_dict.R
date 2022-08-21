
test_that("Dict (overwrite=TRUE) instance works", {

  d <- dict(
    a = "aaa", b = "bbb", c = "ccc",
    .class = "character", .overwrite = TRUE
  )

  ## basic
  expect_is(d, "Dict")
  expect_is(d, "R6")

  expect_equal(d$class, "character")
  expect_equal(d$overwrite, TRUE)
  expect_is(d$items, "tbl")
  expect_equal(d$keys, c("a", "b", "c"))
  expect_equal(d$values, list("aaa", "bbb", "ccc"))
  expect_equal(d$length, 3)

  ## get
  expect_equal(d$get("a"), "aaa")
  expect_equal(d$get(1), "aaa")
  expect_equal(d["a"], "aaa")
  expect_equal(d[1], "aaa")
  ## Suppress a following warning by {withr} when using expect_null, expect_true, expect_false
  ## "Changing language has no effect when envvar LANG='C'"
  suppressWarnings(expect_null(d$get("d")))

  ## has
  suppressWarnings({
    expect_true(d$has("a"))
    expect_false(d$has("d"))
  })
  expect_error(d$has(1), "key must be a character scalar.")
  expect_error(d$has(NULL), "key must be a character scalar.")

  ## add
  expect_is(d$add(d = "ddd"), "Dict")
  expect_is(d["e"] <- "eee", "character")
  expect_equal(d$values, list("aaa", "bbb", "ccc", "ddd", "eee"))
  expect_error(d$add(x = 1), "A value for key = \"x\" must be a character object.")
  expect_error(d[6] <- "xxx", "key must be a character scalar.")

  ## add (overwrite)
  d$add(c = "c", f = "fff")
  expect_equal(d$get("c"), "c")
  expect_equal(d$values, list("aaa", "bbb", "c", "ddd", "eee", "fff"))

  ## remove
  expect_is(d$remove("a"), "Dict")
  expect_equal(d$keys, c("b", "c", "d", "e", "f"))
  expect_error(d$remove(), "key must be a character scalar.")
  expect_error(d$remove(1), "key must be a character scalar.")
  expect_error(d$remove("a"), "key \"a\" is not found.")

  ## sort
  expect_is(d$sort(desc = TRUE), "Dict")
  expect_equal(d$keys, c("f", "e", "d", "c", "b"))

  ## clear
  expect_is(d$clear(), "Dict")
  expect_is(d$items, "tbl")
  expect_equal(dim(d$items), c(0, 2))

  ## constructor errors
  expect_error(dict(.class = 1), ".class must be a character scalar.")
  expect_error(dict(.overwrite = 1), ".overwrite must be a logical scalar.")

})

test_that("Dict (overwrite=FALSE) instance works", {

  d <- dict(
    a = "aaa", b = "bbb", c = "ccc",
    .class = "character", .overwrite = FALSE
  )

  ## add (overwrite)
  d$add(c = "c", e = "eee")
  expect_equal(d$get("c"), "ccc")
  expect_equal(d$values, list("aaa", "bbb", "ccc", "eee"))

})

test_that("Dict (class=data.frame) instance works", {

  d <- dict(
    iris = iris,
    .class = "data.frame", .overwrite = TRUE
  )

  ## get
  expect_is(d$get("iris"), "data.frame")

  ## add
  d$add(mtcars = mtcars)
  expect_is(d$values, "list")

  ## add (overwrite)
  d$add(mtcars = iris)
  suppressWarnings(expect_true(dplyr::setequal(d$get("mtcars"), iris)))

  ## remove
  d$remove("mtcars")
  expect_equal(d$keys, c("iris"))

})
