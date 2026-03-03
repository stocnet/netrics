options(manynet_verbosity = "quiet")
options(snet_verbosity = "quiet")

collect_functions <- function(pattern, package = "netrics"){
  getNamespaceExports(package)[grepl(pattern, getNamespaceExports(package))]
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

# data_objs <- mget(ls("package:manynet"), inherits = TRUE)
# # Filter to relevant objects 
# # data_objs <- data_objs[grepl("ison_|fict_|irps_|mpn_", names(data_objs))]
# # data_objs <- data_objs[!grepl("starwars|physicians|potter", names(data_objs))]
# objs <- table_data() %>% dplyr::filter(!grepl("starwars|physicians|potter", dataset)) %>% 
#   dplyr::distinct(directed, weighted, twomode, labelled, signed, multiplex, longitudinal, dynamic, changing, .keep_all = TRUE) %>% 
#   dplyr::pull(dataset) %>% as.character()
# data_objs <- data_objs[objs]

data_objs <- list(directed = generate_random(12, directed = TRUE),
                  undirected = generate_random(12, directed = FALSE),
                  twomode = generate_random(c(6,6)),
                  labelled = add_node_attribute(generate_random(12, directed = TRUE), "name", paste0("Node", 1:12)),
                  signed = to_signed(generate_random(12, directed = TRUE)))

