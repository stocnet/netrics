# tie_motifs <- funs_objs[grepl("tie_x_", names(funs_objs))]
# for(fn in names(tie_motifs)) {
#   for (ob in names(data_objs)) { 
#     test_that(paste(fn, "works on", ob), {
#       if(fn == "x"){
#       } else {
#         expect_s3_class(tie_motifs[[fn]](data_objs[[ob]]), "tie_motif")
#       }
#     })
#   }
# }

