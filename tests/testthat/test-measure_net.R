net_meas <- funs_objs[grepl("net_by_", names(funs_objs))]
for(fn in names(net_meas)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("core|spatial", fn))
      skip_if(grepl("net_by_factions", fn) && ob == "twomode")
      if(grepl("richness|diversity|heterophily|homophily", fn)){
        if(ob == "attribute")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]], "group"), "network_measure") else
            succeed("Only used for attribute objects")
      } else if(grepl("balance", fn)){
        if(ob == "labelled")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure") else
            succeed("Only used for signed objects")
      } else if(grepl("congruency", fn)){
        # paired with a network that shares a mode, since a two-mode network
        # whose modes differ in size cannot be congruent with itself
        if(ob == "twomode")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]], congruent_twomode), "network_measure") else
            succeed("Only used for multiple two-mode objects")
      } else if(grepl("strength|toughness", fn)){ # why is this so slow??
        if(ob == "weighted")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure") else
            succeed("Testing only once because slow")
      } else if(grepl("accuracy", fn)){
        # Accuracy compares the reports of a cognitive social structure,
        # which none of these fixtures hold; see test-measure_fit.R.
        expect_snet_abort(net_meas[[fn]](data_objs[[ob]]), "reporters")
      } else if(grepl("influence|convergence", fn)){
        # An influence matrix needs a one-mode network, and before
        # manynet 2.5.0 no negative ties
        if(ob == "twomode" ||
           (ob == "labelled" && utils::packageVersion("manynet") < "2.5.0"))
          expect_snet_abort(net_meas[[fn]](data_objs[[ob]]), "one-mode|negative") else
            expect_s3_class(suppressMessages(net_meas[[fn]](data_objs[[ob]])),
                            "network_measure")
      } else if(grepl("tipping", fn) && ob == "twomode"){
        # A tipping point needs a one-mode network
        expect_snet_abort(net_meas[[fn]](data_objs[[ob]]), "one-mode")
      } else if(grepl("polarisation|disagreement|concentration", fn)){
        # These measure a numeric attribute, which no fixture holds
        num <- manynet::add_node_attribute(data_objs[[ob]], "num",
                                           seq_len(manynet::net_nodes(data_objs[[ob]])))
        expect_s3_class(net_meas[[fn]](num, "num"), "network_measure")
      } else if(grepl("infection|immunity|recovery|reproduction|transmiss", fn)){
        if(ob == "diffusion")
          expect_s3_class(net_meas[[fn]](data_objs[[ob]]), "network_measure") else
            succeed("Only used for diffusion objects")
      } else {
        # An eigenvector score is zero for every node outside the main
        # component, so the eigenvector measures warn on the unconnected
        # fixtures. The sweep only checks the class, so that one warning is
        # muffled here. Every other warning still reports.
        expect_s3_class(without_snet_warn(net_meas[[fn]](data_objs[[ob]]),
                                          "eigenvector scores"),
                        "network_measure")
      }
    })
  }
}
