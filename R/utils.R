
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
