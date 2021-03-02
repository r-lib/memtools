alloc_list <- function(n) {
  vector("list", n)
}

list_poke <- function(x, i, value) {
  .Call(ffi_list_poke, x, i, value)
}
