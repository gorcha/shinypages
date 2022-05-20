
test_that("router_ui", {
  expect_s3_class(router_ui(), "shiny.tag")
})

test_that("router_server", {
  session <- list(
    clientData = list(url_hash = "home"),
    userData = list()
  )

  rt <- router(router_page("", function(id) htmltools::tagList()))

  expect_s3_class(router_server(rt, list(), list(), session), "Observer.event")
})

test_that("router_link", {
  expect_equal(router_link(""), "./#!/")
  expect_equal(router_link("/"), "./#!/")
  expect_equal(router_link("blah"), "./#!/blah")
  expect_equal(router_link("/blah/"), "./#!/blah/")
})
