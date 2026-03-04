net_meas <- funs_objs[grepl("net_by_", names(funs_objs))]
for(fn in names(net_meas)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("congruency|correlation|degree|diversity|core|change|heterophily|infection|reach|immunity|recovery|reproduction|richclub|homophily|stability|spatial|balance|strength|waves|transmiss", fn))
      skip_if(grepl("net_by_factions", fn) && ob == "twomode")
      if(fn == "x"){
      } else {
        expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure")
      }
    })
  }
}
