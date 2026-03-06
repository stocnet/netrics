node_meas <- funs_objs[grepl("node_by_", names(funs_objs))]
for(fn in names(node_meas)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("vitality|threshold|richness|recovery|posneg|multideg|hub|homophily|hetero|exposure|diversity|distance|authority|equivalency", fn))
      if(grepl("adoption",fn)){
        if(ob == "diffusion"){
          expect_s3_class(node_meas[[fn]](data_objs[[ob]]), "node_measure")
        }
      } else if (ob %in% c("directed","undirected","weighted","twomode","signed","labelled")){
        expect_s3_class(node_meas[[fn]](data_objs[[ob]]), "node_measure")
      }
    })
  }
}