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

compare_pptx <- function(object, expected, checkImages = TRUE) {
  
  pptx <- read_pptx(object)
  expected <- read_pptx(expected)
  
  p1 <- pptx_summary(pptx)
  p2 <- pptx_summary(expected)
  
  if(nrow(p1) != nrow(p2)) return(FALSE)
  if(!all(p1 == p2)) return(FALSE)
  
  if(checkImages) {
    
  }
  return(TRUE)
}

pptx_testcase <- function(fnc, expected, checkImages = FALSE, ...) {
  
  testsPath <- "tests/pptx"
  context <- get(".context", envir = testthat::get_reporter())
  testsPath <- if(is.null(context)) testsPath else "../pptx"
  
  if(!dir.exists(testsPath)) dir.create(testsPath)
  
  expectedPath <- file.path(testsPath, expected)
  
  if(!file.exists(expectedPath)) {
    if(!is.null(context)) {
      stop(expected, " file does not exists! Did you 
           run tests wihtout evaluating pptx_testcase manually?")
    }
    pptx <- fnc(...)
    print(pptx, target = expectedPath)
  }
  
  pp   <- fnc(...)
  path <- tempfile(fileext = ".pptx")
  print(pp, target = path)
  compare_pptx(path, expectedPath)
}

expect_pptx_identical <- function(
  fnc, expected, checkImages = FALSE) {
  testthat::expect_true(pptx_testcase(fnc, expected, checkImages))
}
