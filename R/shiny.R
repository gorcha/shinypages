
#' Router UI output
#'
#' Placeholder that will be used to render the router page. This can be placed
#' anywhere within a shiny UI.
#'
#' @param id Router id.
#'
#' @return An HTML output element.
#' @export
router_ui <- function(id = "__router__") {
  shiny::uiOutput(id)
}

#' Router server function
#'
#' Register server code. This should be placed in your shiny server, and have a
#' corresponding [router_ui()] element.
#'
#' @param router A router object.
#' @param input Shiny server `input` object.
#' @param output Shiny server `output` object.
#' @param session Shiny server `session` object.
#' @param id Router id.
#'
#' @return A shiny observer reference class object. See [shiny::observe()] for
#'   details.
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui_test <- function(id) {
#'   htmltools::div(
#'     htmltools::p("Test!"),
#'     htmltools::p(htmltools::a(href = router_link("/"), "Home"))
#'   )
#' }
#'
#' test_router <- router(router_page("home", ui_test))
#'
#' ui <- shiny::fluidPage(
#'   router_ui()
#' )
#'
#' server <- function(input, output, session) {
#'   router_server(test_router, input, output, session)
#' }
#'
#' shinyApp(ui, server)
#' }
router_server <- function(router, input, output, session, id = "__router__") {
  stopifnot(inherits(router, "router"))

  session$userData$router_page <- shiny::reactiveVal()

  shiny::observeEvent(shiny::getUrlHash(session), priority = 1000, {
    url_path <- get_router_path()

    # Select route
    # - use default if blank
    # - check for matching pages
    # - set page to 404 if nothing else is matched
    if (url_path == "") {
      if (!is.null(router$callbacks$default)) {
        page <- router$routes[[router$callbacks$default(session)]]
      } else {
        page <- router$default
      }
      shiny::updateQueryString(clean_hash(page$path), mode = "replace", session)
    } else {
      page <- do.call(
        switch,
        c(list(url_path),
          router$routes,
          list(router$http$page_404))
      )
    }

    # If not authorised set page to 403
    if (!is.null(page$authorised) && !page$authorised(session)) {
      page <- router$http$page_403
    }

    # Update router_page reactive
    session$userData$router_page(page)


    if (!is.null(page$server_module)) {
      page$server_module(paste0(id, router_ns(get_router_page()$path)))
    }

    output[[id]] <- shiny::renderUI(
      page$ui_module(paste0(id, router_ns(get_router_page()$path)))
    )

    if (!is.null(router$callbacks$pageload)) {
      router$callbacks$pageload(page, session)
    }
  })
}

#' Created a page link
#'
#' Take a path to a page and convert it to a link for use in Shiny UI.
#'
#' @param path Path of the destination page.
#'
#' @return A link to the destination page.
#' @export
#'
#' @examples
#' htmltools::a(href = router_link("home"), "Home page")
router_link <- function(path) {
  paste0("./", clean_hash(path))
}

#' Change router page
#'
#' @param path Path of the destination page.
#' @param mode Argument passed to [shiny::updateQueryString()].
#' @param session The current shiny session object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' change_router_page("home")
#' }
change_router_page <- function(path, mode = "push", session = shiny::getDefaultReactiveDomain()) {
  shiny::updateQueryString(clean_hash(path), mode, session)
}

#' Get the current `router_page` object
#'
#' @param session The current shiny session object.
#'
#' @return The current [router_page()] object.
#' @export
#'
#' @examples
#' \dontrun{
#' get_router_page()
#' }
get_router_page <- function(session = shiny::getDefaultReactiveDomain()) {
  session$userData$router_page()
}

#' Get the title of the current router page
#'
#' @param session The current shiny session object.
#'
#' @return The title of the current [router_page()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_router_page_title()
#' }
get_router_page_title <- function(session = shiny::getDefaultReactiveDomain()) {
  page <- session$userData$router_page
  if (is.null(page))
    NULL
  else
    page()$title
}

#' Get the current router path from the URL
#'
#' @param session The current shiny session object.
#'
#' @return The current router path.
#' @export
#'
#' @examples
#' \dontrun{
#' get_router_path()
#' }
get_router_path <- function(session = shiny::getDefaultReactiveDomain()) {
  url_hash <- shiny::getUrlHash(session)
  url_path <- extract_path(clean_hash(url_hash))
  url_path
}
