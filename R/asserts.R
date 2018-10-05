make_assert_function <- function(class, constrName, pr = "a") {
  
  function(x) {
    if(!inherits(x, class)) {
      call <- deparse(substitute(x))
      stop(
        paste(
          sprintf("%s is not %s %s object.", call, pr, class),
          sprintf("Did you create it using '%s()' function?", constrName),
            sep = "\n")
      )
    }
    
    return(invisible(TRUE))
  }
}

assert_layout <- make_assert_function("CustomLayout", "lay_new")
assert_officerlayout <- 
  make_assert_function("OfficerCustomLayout", "phl_layout", "an")

maxid <- function (x) {
  UseMethod("maxid", x)
}

maxid.CustomLayout <- function(x) {
  max(x$mat)
}

maxid.OfficerCustomLayout <- function(x) {
  length(x)
}

assert_id_inlayout <- function(id, lay) {
  
  idname <- deparse(substitute(id))
  
  if(length(id) != 1) {
    stop(sprintf("length(%s) != 1", idname))
  }
  
  if(id < 1 || id > maxid(lay)) {
    stop(
      sprintf(
        "%s is not present in the layout. The max id value is equal to %s.",
        idname,
        maxid(lay)
      )
    )
  }
  
  return(invisible(TRUE))
}
