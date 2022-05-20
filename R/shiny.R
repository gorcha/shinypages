
#' Router server and UI
#'
#' @description
#' The `router_server()` and `router_ui()` functions are used to render page
#' modules served by a `[router()]` into a Shiny application.
#'
#' The content for the current page will be rendered in place of `router_ui()`.
#' This can appear anywhere inside the UI definition, so that common UI elements
#' like `head()` tags don't need to be repeated in each page module.
#'
#' `router_server()` can be called from anywhere inside the `server()` function
#' of the Shiny application.
#'
#' @param id Router id.
#' @param router A shinypages `router()` object.
#' @param input Shiny server `input` object.
#' @param output Shiny server `output` object.
#' @param session Shiny server `session` object.
#'
#' @return `router_server()` returns a Shiny observer reference class object.
#'   See [shiny::observe()] for details.
#' @return `router_ui()` returns an HTML output element.
#'
#' @name router_shiny
#' @seealso [router()]
#'
#' @examples
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
#' shiny::shinyApp(ui, server)
#' }
NULL

#' @rdname router_shiny
#' @export
router_ui <- function(id = "__router__") {
  shiny::uiOutput(id)
}

router_serve_page <- function(router, session) {
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

  page
}

#' @rdname router_shiny
#' @export
router_server <- function(router, input, output, session, id = "__router__") {
  stopifnot("`router` is not a shinypages <router> object" = inherits(router, "router"))

  session$userData$router_page <- shiny::reactiveVal()

  shiny::observeEvent(shiny::getUrlHash(session), priority = 1000, {
    page <- router_serve_page(router, session)

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

#' Create a page link
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
#' Set the current router page to `path`.
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

#' Get the current page
#'
#' Get the `router_page()` object for the current page. Can be used to access
#' metadata and other page attributes.
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

#' Get the title of the current page
#'
#' Get the title attribute of the current page.
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

#' Get the metadata of the current page
#'
#' Get the list stored in the metadata attribute of the current page.
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
get_router_page_metadata <- function(session = shiny::getDefaultReactiveDomain()) {
  page <- session$userData$router_page
  if (is.null(page))
    NULL
  else
    page()$metadata
}

#' Get the path of the current page
#'
#' Extract the current router path from the URL.
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
