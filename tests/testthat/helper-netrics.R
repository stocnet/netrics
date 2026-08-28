options(manynet_verbosity = "quiet")
options(snet_verbosity = "quiet")

# The suite runs quiet, so a test that asserts on a message has to turn the
# messages on. `local_verbose()` does that for one test alone, and puts the
# options back when that test ends, whether it passes or fails. A test that
# sets the option itself leaves it set on a failure, and every later test then
# prints its messages to the console.
local_verbose <- function(env = parent.frame()){
  old <- options(manynet_verbosity = "verbose", snet_verbosity = "verbose")
  do.call(base::on.exit,
          list(substitute(options(OLD), list(OLD = old)), add = TRUE),
          envir = env)
  invisible(old)
}

expect_values <- function(object, ref, toler = 3) {
  # 1. Capture object and label
  # act <- quasi_label(rlang::enquo(object), arg = "object")
  act <- list(val = object, label = deparse(substitute(object)))
  
  # 2. Call expect()
  act$n <- round(c(unname(unlist(act$val))), toler)
  ref <- round(c(unname(unlist(ref))), toler)
  expect(
    act$n == ref,
    sprintf("%s has values %f, not values %f.", act$lab, act$n, ref)
  )
  
  # 3. Invisibly return the value
  invisible(act$val)
}

expect_mark <- function(object, ref, top = 3) {
  # 1. Capture object and label
  # act <- quasi_label(rlang::enquo(object), arg = "object")
  act <- list(val = object, label = deparse(substitute(object)))
  
  # 2. Call expect()
  act$n <- as.character(c(unname(unlist(act$val)))[1:top])
  ref <- as.character(c(unname(unlist(ref)))[1:top])
  expect(
    all(act$n == ref),
    sprintf("%s has values %s, not values %s.", act$lab, 
            paste(act$n, collapse = ", "), paste(ref, collapse = ", "))
  )
  
  # 3. Invisibly return the value
  invisible(act$val)
}


top3 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:3]  
  } else unname(res)[1:3]
}

bot3 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-2):lr]
  } else unname(res)[(lr-2):lr]
}

top5 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:5]
  } else unname(res)[1:3]
}

bot5 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-4):lr]
  } else unname(res)[(lr-2):lr]
}

collect_functions <- function(pattern, package = "netrics"){
  getNamespaceExports(package)[grepl(pattern, getNamespaceExports(package))]
}
# Renamed functions are kept as warning wrappers in R/netrics-defunct.R for one
# release. They delegate to their replacement, so sweeping them only produces
# deprecation warnings for a name on its way out.
defunct_fns <- c("node_by_coreness", "net_x_mixed",
                 "node_in_weak", "node_in_strong")
funs_objs <- mget(setdiff(ls("package:netrics"), defunct_fns), inherits = TRUE)

# data_objs <- mget(ls("package:manynet"), inherits = TRUE)
# # Filter to relevant objects 
# # data_objs <- data_objs[grepl("ison_|fict_|irps_|mpn_", names(data_objs))]
# # data_objs <- data_objs[!grepl("starwars|physicians|potter", names(data_objs))]
# objs <- table_data() %>% dplyr::filter(!grepl("starwars|physicians|potter", dataset)) %>% 
#   dplyr::distinct(directed, weighted, twomode, labelled, signed, multiplex, longitudinal, dynamic, changing, .keep_all = TRUE) %>% 
#   dplyr::pull(dataset) %>% as.character()
# data_objs <- data_objs[objs]

set.seed(1234)
data_objs <- list(directed = generate_random(12, directed = TRUE),
                  # The two modes must differ in size. A square incidence
                  # matrix passes silently through functions that assume a
                  # square sociomatrix, which hid real bugs in
                  # `node_by_core()`, `net_by_cyclicality()` and
                  # `node_by_information()`.
                  twomode = generate_random(c(6,8)),
                  labelled = to_signed(add_node_attribute(create_wheel(12), "name", 
                                                LETTERS[1:12])),
                  attribute = add_node_attribute(create_ring(12), "group", 
                                                 rep(c("A","B"), each = 6)),
                  weighted = add_tie_attribute(create_ring(12), "weight", 
                                               rep(c(1,2), each = 6)),
                  diffusion = play_diffusion(create_ring(12), seeds = 1, 
                                             steps = 5, latency = 0.75, 
                                             recovery = 0.25))

# `net_by_congruency()` needs two two-mode networks that share a mode: the
# second mode of the first must match the first mode of the second. The
# sweeping fixture cannot be paired with itself once its modes differ in size.
congruent_twomode <- generate_random(c(8,5))

find_pkg_tutorial_paths <- function(pkg) {
  tute_folders <- list.dirs(system.file("tutorials", package = pkg),
                             recursive = F)
  tute_files <- unlist(lapply(tute_folders, function(folder) {
    list.files(folder, pattern = "*.Rmd", full.names = TRUE)
  }))
  tute_files
}

# Extract the R code from an R Markdown / learnr tutorial's `{r}` chunks, in
# document order, dropping any chunk marked `purl = FALSE`. This replicates the
# only part of knitr's purl() we rely on, so that {knitr} need not be a package
# dependency (it was otherwise used solely to run these tutorial tests). These
# tutorials use no child documents, chunk references, or non-R engines, so this
# simple line scanner is sufficient and matches purl()'s output expression set.
extract_rmd_code <- function(path) {
  lines <- readLines(path, warn = FALSE)
  open_re  <- "^\\s*```+\\s*\\{[rR][\\s,}]"
  close_re <- "^\\s*```+\\s*$"
  code <- character()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    if (grepl(open_re, lines[i], perl = TRUE)) {
      keep <- !grepl("purl\\s*=\\s*(FALSE|F)\\b", lines[i])
      i <- i + 1L
      while (i <= n && !grepl(close_re, lines[i])) {
        if (keep) code <- c(code, lines[i])
        i <- i + 1L
      }
    }
    i <- i + 1L
  }
  code
}

# The tutorials' code chunks are extracted to a script and evaluated expression
# by expression, so that any chunk that errors or raises a deprecation warning
# fails the suite. Rendering the learnr tutorials themselves is deliberately not
# tested (that check was fragile and added no coverage value), mirroring
# {autograph}'s tests/testthat/helper-tutorials.R.
check_tute_functions <- function(path, skip = "ergm\\("){
  exprs <- parse(text = extract_rmd_code(path))
  env <- new.env(parent = globalenv())

  # A tutorial may call `?fn`. `help()` prints through a pager that writes
  # straight to the terminal, which `capture.output()` cannot take. A pager
  # that does nothing keeps that page off the console.
  op <- options(pager = function(files, header, title, delete.file){
    if(delete.file) unlink(files)
    invisible(NULL)
  })
  on.exit(options(op), add = TRUE)

  is_skipped_call <- function(expr) {
    any(grepl(skip, deparse(expr)))
  }

  for (i in seq_along(exprs)) {
    # Stop at the first slow call: it and any later (dependent) expressions
    # are skipped, but we return normally so the caller's loop over the
    # remaining tutorials continues. Using skip() here would unwind to the
    # enclosing test_that() and abort every subsequent tutorial too.
    if (is_skipped_call(exprs[[i]])) {
      break
    }

    w <- NULL
    e <- NULL
    m <- NULL
    
    # A tutorial chunk may print a result. `capture.output()` keeps that off
    # the console, so a test run reports expectations and nothing else.
    not_out <- utils::capture.output(withCallingHandlers(
      tryCatch(
        eval(exprs[[i]], envir = env),
        error = function(err) {
          e <<- err
          NULL
        }
      ),
      warning = function(wrn) {
        w <<- wrn
        invokeRestart("muffleWarning")
      },
      message = function(msg) {
        m <<- c(m, conditionMessage(msg))
        invokeRestart("muffleMessage")
      }
    ))
    
    # If there *was* a warning, check if it's a deprecated/defunct one
    if (!is.null(w)) {
      msg <- conditionMessage(w)
      
      # Only fail if it's a deprecated/defunct warning
      if (!grepl("deprecate|defunct|moved", msg, ignore.case = TRUE)) {
        w <- NULL
      }
    }
    
    # Now test what happened
    expect_null(
      e,
      info = paste0("Error in expression ", i,
                    " of ", basename(path), ": ", deparse(exprs[[i]]))
    )
    
    expect_null(
      w,
      info = paste("Warning in expression", i, ":", deparse(exprs[[i]]))
    )
  }
}
