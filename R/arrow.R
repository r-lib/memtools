#' @export
print.memtools_arrow <- function(x, ...) {
  writeLines(sprintf("<memtools/arrow>"))
  writeLines(sprintf("from: <%s>", pillar::obj_sum(x$from)))
  writeLines(sprintf("to: <%s>", pillar::obj_sum(x$to)))
  writeLines(sprintf("depth: %d", x$depth))
  writeLines(sprintf("rel: \"%s\"", x$rel))
  writeLines(sprintf("i: %d", x$i))
  writeLines(sprintf("name: \"%s\"", x$name))
}
