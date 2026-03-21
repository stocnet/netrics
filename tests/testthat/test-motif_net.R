net_motifs <- funs_objs[grepl("net_x_", names(funs_objs))]
for(fn in names(net_motifs)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("exposure|mixed|hazard", fn))
      skip_if(grepl("triad", fn) && is_twomode(data_objs[[ob]]))
      if(grepl("brokerage", fn)){
        if(ob == "attribute")
          expect_s3_class(net_motifs[[fn]](data_objs[[ob]], "group"), "network_motif") else
            succeed("Only used for attribute objects")
      } else if(grepl("correlation|change|stability", fn)){
        if(ob == "labelled")
          expect_s3_class(net_motifs[[fn]](data_objs[[ob]], data_objs[[ob]]), "network_motif") else
            succeed("Only used for multi objects")
      } else {
        expect_s3_class(net_motifs[[fn]](data_objs[[ob]]), "network_motif")
      }
    })
  }
}

