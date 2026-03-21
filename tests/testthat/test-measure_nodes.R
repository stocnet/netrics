node_meas <- funs_objs[grepl("node_by_", names(funs_objs))]
for(fn in names(node_meas)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("multideg", fn))
      skip_if(grepl("equivalency", fn) && ob == "labelled")
      if(grepl("diversity|richness|heterophily|homophily", fn)){
        if(ob == "attribute")
          expect_s3_class(node_meas[[fn]](data_objs[[ob]], "group"), "node_measure") else
            succeed("Only used for attribute objects")
      } else if(grepl("adopt|recovery|exposure",fn)){
        if(ob == "diffusion")
          expect_s3_class(node_meas[[fn]](data_objs[[ob]]), "node_measure") else
            succeed("Only used for diffusion objects")
      } else if(grepl("posneg",fn)){
        if(ob == "signed")
          expect_s3_class(node_meas[[fn]](data_objs[[ob]]), "node_measure") else
            succeed("Only used for signed objects")
      } else if(grepl("distance",fn)){
          expect_s3_class(node_meas[[fn]](data_objs[[ob]], 1, 2), "node_measure")
      } else {
        expect_s3_class(node_meas[[fn]](data_objs[[ob]]), "node_measure")
      }
    })
  }
}