funs_objs <- mget(ls("package:netrics"), inherits = TRUE)
# Filter to relevant objects 

node_marks <- funs_objs[grepl("node_is_", names(funs_objs))]
for(fn in names(node_marks)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("node_is_recovered|neighbor|min|max|mean|latent|infected|core", fn))
      if(fn == "node_is_exposed"){
        expect_s3_class(node_marks[[fn]](data_objs[[ob]], mark = c(1,3)), "node_mark")
      } else {
        expect_s3_class(node_marks[[fn]](data_objs[[ob]]), "node_mark")
      }
    })
  }
}
