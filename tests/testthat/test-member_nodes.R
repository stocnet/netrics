node_membs <- funs_objs[grepl("node_in_", names(funs_objs))]
for(fn in names(node_membs)) {
  for (ob in names(data_objs)) { 
    test_that(paste(fn, "works on", ob), {
      skip_if(grepl("roulette|equivalence|adopter|brokering", fn))
      if(fn == "x"){
      } else  if (ob %in% c("directed","undirected","weighted","twomode","signed","labelled")){
        expect_s3_class(node_membs[[fn]](data_objs[[ob]]), "node_member")
      }
    })
  }
}