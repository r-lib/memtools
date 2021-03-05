alloc_list <- function(n) {
  vector("list", n)
}

list_poke <- function(x, i, value) {
  .Call(ffi_list_poke, x, i, value)
}

stop_internal <- function(fn, msg) {
  abort(sprintf("Internal error in `%s()`: %s"), fn, msg)
}
