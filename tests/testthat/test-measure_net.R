net_meas <- funs_objs[grepl("net_by_", names(funs_objs))]
for(fn in names(net_meas)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("core|spatial", fn))
      skip_if(grepl("net_by_factions", fn) && ob == "twomode")
      if(grepl("diversity|heterophily|homophily", fn)){
        if(ob == "attribute")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]], "group"), "network_measure") else
            succeed("Only used for attribute objects")
      } else if(grepl("balance", fn)){
        if(ob == "labelled")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure") else
            succeed("Only used for signed objects")
      } else if(grepl("congruency", fn)){
        if(ob == "twomode")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]], data_objs[[ob]]), "network_measure") else
            succeed("Only used for multiple two-mode objects")
      } else if(grepl("strength|toughness", fn)){ # why is this so slow??
        if(ob == "weighted")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure") else
            succeed("Testing only once because slow")
      } else if(grepl("infection|immunity|recovery|reproduction|transmiss", fn)){
        if(ob == "diffusion")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure") else
            succeed("Only used for diffusion objects")
      } else {
        expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure")
      }
    })
  }
}
