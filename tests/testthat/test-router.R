library(shiny)

# Build UI ---------------------------------------------------------------------

ui_dummy <- function(id) tagList()

page_home <- router_page("home", ui_dummy)

server_dummy <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$test <- renderText("Test")
  })
}

page_server <- router_page("server", ui_dummy, server_dummy)

# Restrict access to a page
is_admin <- function(session) "admin" %in% session$groups

page_admin <- router_page("admin", ui_dummy, authorised = is_admin)

page_403 <- router_page("403", ui_dummy)
page_404 <- router_page("404", ui_dummy)

page_meta <- router_page("meta", ui_dummy, title = "Test", metadata = list(test = TRUE))

my_router <- router(
  page_home,
  page_server,
  page_admin,
  page_meta,
  page_403 = page_403,
  page_404 = page_404
)

callback_router <- router(
  page_home,
  page_admin,
  page_meta,
  callback_pageload = function(page, session) {
    session$output$test <- renderText(get_router_page_title(session))
  },
  callback_default = function(session) {
    if ("admin" %in% session$groups) {
      "admin"
    } else {
      "home"
    }
  }
)

# router/router_page class tests -----------------------------------------------

test_that("router_page printing", {
  expect_snapshot(print(page_home))
  expect_snapshot(print(page_server))
  expect_snapshot(print(page_admin))
  expect_snapshot(print(page_meta))
})

test_that("router printing", {
  expect_snapshot(print(my_router))
  expect_snapshot(print(callback_router))
})

# Shiny tests ------------------------------------------------------------------

session <- MockShinySession$new()
session$clientData <- reactiveValues(url_hash = "")

testServer(
  function(input, output, session)
    router_server(my_router, input, output, session),
  session = session,
  {
    # Check the correct default page is loaded
    session$flushReact()
    expect_identical(get_router_page(), page_home)
    expect_equal(get_router_path(), "")

    # Check get_router_path()
    session$clientData$url_hash <- "#!/home"
    session$flushReact()
    expect_identical(get_router_page(), page_home)
    expect_equal(get_router_path(), "home")

    # Check the server module loads correctly
    session$clientData$url_hash <- "#!/server"
    session$flushReact()
    expect_identical(get_router_page(), page_server)
    expect_equal(output$`__router__server-test`, "Test")

    # Check 403 unauthorised for the admin page before groups are set
    session$clientData$url_hash <- "#!/admin"
    session$flushReact()
    expect_identical(get_router_page(), page_403)

    # Check 404 for non-existent page
    session$clientData$url_hash <- "#!/unknown"
    session$flushReact()
    expect_identical(get_router_page(), page_404)

    # Check admin page page loads when authorised groups are set
    session$groups <- "admin"
    session$clientData$url_hash <- "#!/admin"
    session$flushReact()
    expect_identical(get_router_page(), page_admin)

    # Check router page title and metadat retrieval
    session$clientData$url_hash <- "#!/meta"
    session$flushReact()
    expect_identical(get_router_page(), page_meta)
    expect_equal(get_router_page_title(), "Test")
    expect_equal(get_router_page_metadata(), list(test = TRUE))
  }
)


session <- MockShinySession$new()
session$clientData <- reactiveValues(url_hash = "")

testServer(
  function(input, output, session)
    router_server(callback_router, input, output, session),
  session = session,
  {
    # Check default page callback
    session$flushReact()
    expect_identical(get_router_page(), page_home)

    session$groups <- "admin"
    session$clientData$url_hash <- "#"
    session$flushReact()
    expect_identical(get_router_page(), page_admin)

    # Check pageload callback
    session$clientData$url_hash <- "#!/meta"
    session$flushReact()
    expect_identical(get_router_page(), page_meta)
    expect_equal(output$test, "Test")
  }
)
