net_meas <- funs_objs[grepl("net_by_", names(funs_objs))]
for(fn in names(net_meas)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("congruency|correlation|core|change|infection|reach|immunity|recovery|reproduction|stability|balance|strength|waves|transmiss|spatial", fn))
      skip_if(grepl("net_by_factions", fn) && ob == "twomode")
      if(grepl("diversity|heterophily|homophily", fn)){
        if(ob == "attribute")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]], "group"), "network_measure")
      } else {
        expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure")
      }
    })
  }
}
