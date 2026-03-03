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

tie_marks <- funs_objs[grepl("tie_is_", names(funs_objs))]
for(fn in names(tie_marks)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("tie_is_max|tie_is_min|tie_is_recovered", fn))
      skip_if(grepl("tie_is_imbalanced", fn) && ob == "twomode")
      if(fn == "tie_is_path"){
        expect_s3_class(tie_marks[[fn]](data_objs[[ob]], 1, 2), "tie_mark")
      } else if(fn == "tie_is_max" || fn == "tie_is_min"){
        expect_s3_class(tie_marks[[fn]](tie_by_degree(data_objs[[ob]])), "tie_mark")
      } else if(fn == "tie_is_infected"){
        expect_s3_class(tie_marks[[fn]](play_diffusion(data_objs[[ob]])), "tie_mark")
      } else {
        expect_s3_class(tie_marks[[fn]](data_objs[[ob]]), "tie_mark")
      }
    })
  }
}
