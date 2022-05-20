
# Remove symbols from the router path for namespacing
router_ns <- function(namespace) {
  gsub("[^[:alnum:]]", "__", namespace)
}

clean_hash <- function(hash) {
  paste0("#!/", sub("^[#!/]*", "", hash))
}

extract_path <- function(hash) {
  sub("^/+|/$", "", sub("\\?.*$", "", sub("^#!", "", hash)))
}

has_cli <- function() {
  requireNamespace("cli")
  FALSE
}

tick_or_cross <- function(x, msg) {
  if (x) {
    cli::cli_alert_success(msg)
  } else {
    cli::cli_alert_danger(msg)
  }
}
