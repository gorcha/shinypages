
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinypages

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/gorcha/shinypages/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gorcha/shinypages/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

shinypages is a simple multi-page router for Shiny applications, very
much inspired by Appsilon’s
[shiny.router](https://github.com/Appsilon/shiny.router).

## Installation

You can install the development version of shinypages like so:

``` r
# install.packages("devtools")
devtools::install_github("gorcha/shinypages")
```

## Example

``` r
library(shiny)
library(shinypages)

# Create a simple page with only UI
ui_home <- function(id) {
  tagList(
    h2("Hello world!"),
    p(a(href = router_link("cars"), "Cars")),
    p(a(href = router_link("admin"), "Admin"))
  )
}

home_page <- router_page("home", ui_home)


# Create a page with UI and a module server
ui_cars <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Cars"),
    p(a(href = router_link("home"), "Home")),
    tableOutput(ns("cars"))
  )
}

server_cars <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$cars <- renderTable(mtcars)
  })
}

cars_page <- router_page("cars", ui_cars, server_cars)

# Restrict access to a page
is_admin <- function(session) {
  "admin" %in% session$groups
}

ui_admin <- function(id) {
  tagList(
    h2("Top Secret"),
    p(a(href = router_link("home"), "Home"))
  )
}

admin_page <- router_page("admin", ui_admin, authorised = is_admin)

# Create a router
my_router <- router(home_page, cars_page, admin_page)

# Add your router into a Shiny application
ui <- fixedPage(
  router_ui()
)

server <- function(input, output, session) {
  router_server(my_router, input, output, session)
}

shinyApp(ui, server)
```
