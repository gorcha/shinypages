
ui_403 <- function(id) {
  htmltools::tagList(
    htmltools::h2("Forbidden"),
    htmltools::p("You are not authorised to access this page."),
    htmltools::p(htmltools::a(href = router_link("/"), "Home"))
  )
}

ui_404 <- function(id) {
  htmltools::tagList(
    htmltools::h2("Page not found"),
    htmltools::p("We're sorry, but the requested page could not be found."),
    htmltools::p(htmltools::a(href = router_link("/"), "Home"))
  )
}
