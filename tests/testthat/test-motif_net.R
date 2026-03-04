net_motifs <- funs_objs[grepl("net_x_", names(funs_objs))]
for(fn in names(net_motifs)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("exposure|brokerage|mixed|hazard", fn))
      skip_if(grepl("triad|dyad", fn) && is_twomode(data_objs[[ob]]))
      if(fn == "x"){
      } else {
        expect_s3_class(net_motifs[[fn]](data_objs[[ob]]), "network_motif")
      }
    })
  }
}

