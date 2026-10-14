tie_meas <- funs_objs[grepl("tie_by_", names(funs_objs))]
for(fn in names(tie_meas)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      expect_s3_class(tie_meas[[fn]](data_objs[[ob]]), "tie_measure")
    })
  }
}