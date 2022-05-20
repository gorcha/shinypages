
#' Create a router page
#'
#' @description
#' A page attaches a Shiny module to a path, to be included in a [router()]
#' definition.
#'
#' @section Shiny module:
#'
#' The `ui_module` and `server_module` arguments take shiny module UI and server
#' functions respectively. The module functions shouldn't take any arguments
#' apart from `id`.
#'
#' ```r
#' ui_test <- function(id) {
#'   htmltools::p("Test!")
#' }
#'
#' server_test <- function(id) {
#'   shiny::moduleServer(id, function(input, output, session) { })
#' }
#'
#' router_page("test", ui_test, server_test)
#' ```
#'
#' For an introduction to Shiny modules, see the [Modularizing Shiny app
#' code](https://shiny.rstudio.com/articles/modules.html) article.
#'
#' @section Authorising sessions:
#'
#' A page can be configured to check if a session is authorised to view it
#' before serving the page. To check authorisation, pass a function in the
#' `authorised` argument that takes a shiny session as input and returns `TRUE`
#' if a user is authorised to view the page and `FALSE` otherwise. For example:
#'
#' ```r
#' is_admin <- function(session) {
#'   "admin" %in% session$groups
#' }
#' ```
#'
#' The `authorised` function is run by the router before serving the page. If
#' the function returns `FALSE`, the router will serve the 403 Forbidden page.
#'
#' @section Metadata:
#'
#' The page can have two pieces of metadata attached:
#'
#' * A `title`, which can be retrieved inside a Shiny app with
#' [get_router_page_title()].
#'
#' * `metadata`, a list of arbitray data, which can be retrieved inside a Shiny
#' app with [get_router_page_metadata()].
#'
#' @param path Path for this page.
#' @param ui_module Shiny module UI to render.
#' @param server_module Shiny server module to render.
#' @param title Page title. This can be accessed as reactive via
#'   [get_router_page_title()].
#' @param authorised A function that determines whether the current session is
#'   authorised to view this page. The function should take a shiny session as
#'   input, and return `TRUE` if the session is authorised to view this page and
#'   `FALSE` otherwise.
#' @param metadata An arbitrary list of page metadata. This can be accessed as a
#'   reactive via [get_router_page_metadata()] or used in router callbacks.
#'
#' @return A `router_page` object.
#' @seealso [router()]
#' @export
#' @examples
#' if (interactive()) {
#'
#' ui_test <- function(id) {
#'   htmltools::p("Test!")
#' }
#'
#' server_test <- function(id) {
#'   shiny::moduleServer(id, function(input, output, session) { })
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
                        title = NULL,
                        authorised = NULL,
                        metadata = list()) {
  stopifnot("`path` is not a character value" = is.character(path))
  stopifnot("`ui_module` is not a function" = is.function(ui_module))
  stopifnot("`server_module` is not a function" = is.function(server_module) | is.null(server_module))
  stopifnot("`title` is not a character value" = is.character(title) | is.null(title))
  stopifnot("`authorised` is not a function" = is.function(authorised) | is.null(authorised))
  stopifnot("`metadata` is not a list" = is.list(metadata))

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

#' @rdname router_page
#' @param x A `router_page` object.
#' @param ... Additional arguments for specific methods.
#' @param short Should a one line summary be printed?
#' @export
print.router_page <- function(x, short = FALSE, ...) {
  if (has_cli()) {
    cli::cli_text("{.cls {class(x)}} {.emph #!/{x$path}} {if (!is.null(x$title)) paste0(' - ', x$title)}")

    if (!short) {
      tick_or_cross(!is.null(x$server_module), "Server module")
      tick_or_cross(!is.null(x$authorised), "Authorisation function")
      cli::cli_text("Has {cli::no(length(x$metadata))} metadata item{?s}")
    }
  } else {
    cat("<", class(x), "> #!/", x$path, if (!is.null(x$title)) paste0(' - ', x$title), "\n", sep = "")
  }

  invisible(x)
}

#' Create a router
#'
#' @description
#'
#' A [router()] defines a multi-page Shiny application using a set of
#' [router_page()]s. See [router_server()] and [router_ui()] to create a
#' multi-page application from the returned `router` object.
#'
#' @section Callbacks:
#'
#' The callback function arguments allow user customisation of the page serving
#' process.
#'
#' * `callback_pageload` is called after a new page is loaded. It should be a
#' function with the signature `function(page, session)`, taking a `router_page`
#' and a shiny session as input. Thh return value of the function is ignored.
#'
#'   Useful to perform operations based on page metadata.
#'
#'   ```r
#'   callback_pageload = function(page, session) {
#'     shinyjs::runjs(paste0("document.title = \"", page$title, "\";"))
#'   }
#'   ```
#'
#' * `callback_default()` is called to determine the path to serve when the
#' default page is requested. It should be a function with the signature
#' `function(session)`, taking a shiny session as input and returning the path
#' of the page to load.
#'
#'   Useful for setting per-user or per-group home pages.
#'
#'   ```r
#'   callback_default = function(session) {
#'     if ("dev" %in% session$groups) {
#'       "devlanding"
#'     } else {
#'       "home"
#'     }
#'   )
#'   ```
#'
#' @section HTTP response pages:
#'
#' `router_page`s are served in place of common HTTP response codes. These can
#' be customised:
#'
#' * `page_403` is served when the `authorised` function of a page returns
#' `FALSE`.
#'
#' * `page_404` is served when an unknown path is requested.
#'
#' @param ... A set of [router_page()] objects. The first page is considered the
#'   default, and will be used if no path is provided.
#' @param callback_pageload A function that is called immediately after page
#'   content is rendered. The function should take a `router_page` object and
#'   shiny session as input.
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
#' @seealso [router_page()] to create pages, [add_router_callback()] to add
#'   callback functions to an existing router, and [router_server()] and
#'   [router_ui()] to add a router to a Shiny application.
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
  stopifnot("Only <router_page> objects should be provided in `...`" = all(vapply(routes, function(x) inherits(x, "router_page"), logical(1))))
  stopifnot("Some paths are duplicated" = !any(duplicated(vapply(routes, function(x) x$path, character(1)))))
  stopifnot("At least 1 page must be provided in `...`" = length(routes) > 0)

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

  x
}

#' @rdname router
#' @param x A `router` object.
#' @param short Should a one line summary be printed?
#' @export
print.router <- function(x, short = FALSE, ...) {
  paths <- sapply(x$routes, function(x) x$path)
  if (has_cli()) {
    cli::cli_text("{.cls {class(x)}} with {cli::no(length(x$routes))} page{?s/:/s:} {.emph {paths}}")
    cli::cli_text("{.strong Callbacks:}")
    tick_or_cross(!is.null(x$callbacks$pageload), "pageload")
    tick_or_cross(!is.null(x$callbacks$default), "default")

    if (!short) {
      cli::cli_text("{.strong Pages:}")
      lapply(x$routes, print, short = TRUE)
    }
  } else {
    cat("<", class(x), "> with ", length(x$routes), " page(s): ", paste0(paths, collapse = ", "), "\n", sep = "")
    if (!short) {
      cat("Pages:\n")
      lapply(x$routes, print, short = TRUE)
    }
  }

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
#' @seealso [router()]
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
#'     if ("dev" %in% session$groups) {
#'       "devlanding"
#'     } else {
#'       "home"
#'     }
#'   }
#' )
#' }
add_router_callback <- function(router, callback = c("pageload", "default"), func) {
  stopifnot("`router` is not a shinypages <router> object" = inherits(router, "router"))
  stopifnot("`func` is not a function" = is.function(func))
  callback <- match.arg(callback)

  router$callbacks[[callback]] <- func
  invisible(router)
}
