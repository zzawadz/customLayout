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

get_img_threshold <- function() {
  if(identical(Sys.getenv("TRAVIS"), "true")) {
    0.90
  } else {
    0.99
  }
}

compare_pptx <- function(
  object, expected, checkImages = TRUE,
  imgThreshold = get_img_threshold()
) {
  
  pptx     <- officer::read_pptx(object)
  expected <- officer::read_pptx(expected)
  
  p1 <- officer::pptx_summary(pptx)
  p2 <- officer::pptx_summary(expected)
  
  compare_pptx_summary <- function(p1, p2) {
    
    if(nrow(p1) != nrow(p2)) return(FALSE)
    if(!isTRUE(all.equal(colnames(p1), colnames(p2)))) return(FALSE)
    
    if("media_file" %in% colnames(p1)) {
      p1 <- p1[,"media_file" != colnames(p1), drop = FALSE]
      p2 <- p2[,"media_file" != colnames(p2), drop = FALSE]
    }
    if(!all(p1 == p2)) return(FALSE)
    
    return(TRUE)
  }
  
  
  if(!compare_pptx_summary(p1, p2)) return(FALSE)
  
  if(checkImages && "media_file" %in% colnames(p1)) {
    
    extract_plots_paths <- function(x) {
      x <- x[["media_file"]]
      x[nchar(x) > 0]
    }
    plots1 <- extract_plots_paths(p1)
    plots2 <- extract_plots_paths(p2)
    
    if(length(plots1) != length(plots2)) return(FALSE)
    
    extract_temp_png <- function(pp, path) {
      tpath <- tempfile(fileext = ".png")
      officer::media_extract(pp, path = path, target = tpath)
      tpath
    }
    
    for(i in seq_along(plots1)) {
      pl1 <- extract_temp_png(pptx, plots1[i])
      pl2 <- extract_temp_png(expected, plots2[i])
      
      sim <- mean(png::readPNG(pl1) == png::readPNG(pl2))
      
      if(sim < imgThreshold) {
        res <- FALSE
        attr(res, "descr") <- sprintf("Similarity: %.3f", sim)
        return(res)
      }
    }
    
    
  }
  return(TRUE)
}

pptx_testcase <- function(fnc, expected, checkImages = TRUE, ...) {
  
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
  compare_pptx(path, expectedPath, checkImages = checkImages)
}

expect_pptx_identical <- function(
  fnc, expected, checkImages = TRUE, ...) {
  res <- pptx_testcase(fnc, expected, checkImages, ...)
  descr <- attr(res, "descr")  
  if(!is.null(descr)) stop(descr)
  testthat::expect_true(res)
}
