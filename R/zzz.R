.onLoad <- function(lib, pkg) {
  ns <- topenv(environment())
  .Call(c_ptr_init_library, ns)
  .Call(c_ptr_init_memtools, ns, size_node(), size_vector())

  bench::mark()
  tibble::tibble()

  options(memtools_verbose = TRUE)
}
