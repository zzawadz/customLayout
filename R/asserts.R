is.customlayout <- function(x) {
  inherits(x, "Layout")
}

assert_layout <- function(x) {
  if(!is.customlayout(x)) {
    call <- deparse(substitute(x))
    stop(
      paste(sprintf("%s is not a Layout object.", call),
      "Did you create it using 'lay_new' function?", sep = "\n")
    )
  }
}

assert_id_inlayout <- function(id, lay) {
  
  idname <- deparse(substitute(id))
  
  if(length(id) != 1) {
    stop(sprintf("length(%s) != 1", idname))
  }
  
  if(id < 0 || id > max(lay@mat)) {
    stop(sprintf("%s is not present in the layout. The max id value is equal to %s.", idname, max(lay@mat)))
  }
}
