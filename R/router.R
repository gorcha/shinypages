
#' Create a router page
#'
#' @param path Path for this page.
#' @param ui_module Shiny UI module to render.
#' @param server_module Shiny server module to render.
#' @param title Page title.
#' @param authorised A function that determines whether the current session is
#'   authorised to view this page. The function should take a session object as
#'   input, and return `TRUE` if the session is authorised to view this page and
#'   `FALSE` otherwise.
#' @param metadata An arbitrary list of page metadata. This can be accessed as a
#'   reactive via `get_router_page()` or used in router callbacks.
#'
#' @return A `router_page` object.
#' @export
#' @examples
#' if (interactive()) {
#'
#' ui_test <- function(id) {
#'   htmltools::p("Test!")
#' }
#'
#' server_test <- function(id) {
#'   moduleServer(id, function(input, output, session) { })
#' }
#'
#' home_page <- router_page("home", ui_test)
#' server_page <- router_page("server", ui_test, server_test)
#'
#' is_admin <- function(session) {
#'   "admin" %in% session$groups
#' }
#'
#' admin_page <- router_page("admin", ui_test, authorised = is_admin)
#' }
router_page <- function(path,
                        ui_module,
                        server_module = NULL,
                        title = "",
                        authorised = NULL,
                        metadata = list()) {
  structure(
    list(
      path = path,
      ui_module = ui_module,
      server_module = server_module,
      title = title,
      authorised = authorised,
      metadata = metadata
    ),
    class = "router_page"
  )
}

#' Create a router
#'
#' @param ... A set of [router_page()] objects. The first page is considered the
#'   default, and will be used if no path is provided.
#' @param callback_pageload A function that is called immediately after each
#'   page is rendered. The function should take a `router_page` object and shiny
#'   session as input.
#' @param callback_default A function that determines the default page. The
#'   function should take a shiny session as input and return the path of the
#'   default page. If not provided, the first page is used as the default.
#' @param page_403 A `router_page` to be used when a user attempts to access an
#'   unauthorised page.
#' @param page_404 A `router_page` to be used when a user attempts to access a
#'   page that doesn't exist.
#'
#' @return A `router` object.
#' @export
#' @examples
#' if (interactive()) {
#'
#' ui_test <- function(id) {
#'   htmltools::p("Test!")
#' }
#'
#' home_page <- router_page("home", ui_test)
#' other_page - router_page("other", ui_test)
#'
#' test_router <- router(home_page, other_page)
#' }
router <- function(...,
                   callback_pageload = NULL,
                   callback_default = NULL,
                   page_403 = router_page("403", ui_403),
                   page_404 = router_page("404", ui_404)) {
  routes <- list(...)
  stopifnot(all(vapply(routes, function(x) inherits(x, "router_page"), logical(1))))
  stopifnot(length(routes) > 0)

  x <- structure(
    list(
      routes = list(...)
    ),
    class = "router"
  )

  names(x$routes) <- sapply(x$routes, function(x) { x$path })
  x$default <- x$routes[[1]]
  x$current <- new.env(parent = emptyenv())
  x$callbacks <- list(pageload = callback_pageload, default = callback_default)
  x$http <- list(page_403 = page_403, page_404 = page_404)

  invisible(x)
}

#' Set a router callback function
#'
#' @param router A router object.
#' @param callback The name of the callback to set. Currently `"pageload"` and
#'   `"default"` callbacks are supported.
#' @param func The callback function. The function should take a `router_page`
#'   object and shiny session as input.
#'
#' @return Invisibly returns the input `router` object.
#' @export
#' @examples
#' \dontrun{
#' add_router_callback(
#'   router,
#'   "pageload",
#'   function(page, session) {
#'     shinyjs::runjs(paste0("document.title = \"", page$title, "\";"))
#'   }
#' )
#'
#' add_router_callback(
#'   router,
#'   "default",
#'   function(session) {
#'     if ("dev" in session$groups) {
#'       "devlanding"
#'     } else {
#'       "home"
#'     }
#'   }
#' )
#' }
add_router_callback <- function(router, callback = c("pageload", "default"), func) {
  stopifnot(inherits(router, "router"))
  stopifnot(is.function(func))
  callback <- match.arg(callback)

  router$callbacks[[callback]] <- func
  invisible(router)
}
