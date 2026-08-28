node_membs <- funs_objs[grepl("node_in_", names(funs_objs))]
for(fn in names(node_membs)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      # These two need a connected network, and now abort rather than
      # returning nothing when they do not get one. The restriction is about
      # connectivity, not about two modes.
      skip_if(grepl("fluid|spinglass", fn) &&
                !igraph::is_connected(manynet::as_igraph(data_objs[[ob]])))
      if(grepl("roulette", fn)){
        if(ob != "twomode")
          expect_s3_class(node_membs[[fn]](data_objs[[ob]], num_groups = 3), 
                          "node_member") else
                            succeed("Roulette doesn't work on two-mode objects")
      } else if(grepl("adopter", fn)){
        if(ob == "diffusion")
          expect_s3_class(node_membs[[fn]](data_objs[[ob]]), "node_member") else
                            succeed("Only works on diffusion objects")
      } else if(grepl("equivalence", fn)){
          expect_s3_class(node_membs[[fn]](data_objs[[ob]], 
                                           node_x_tie(data_objs[[ob]])), 
                          "node_member")
      } else {
        expect_s3_class(node_membs[[fn]](data_objs[[ob]]), "node_member")
      }
    })
  }
}