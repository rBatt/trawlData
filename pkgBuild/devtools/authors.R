# authors
rdb <- person(given="Ryan D.", family="Batt", email="battrd@gmail.com", role=c("aut","cre"))
jwm <- person(given="James W.", family="Morley", email="jw.morley@rutgers.edu", role=c("aut"))
bls <- person(given="Rebecca L.", family="Selden", email="becca.selden@gmail.com", role=c("aut"))
mlp <- person(given="Malin L.", family="Pinsky", email="malin.pinsky@rutgers.edu", role=c("aut"))

dput(c(rdb, jwm, bls, mlp))



# c_aut <- function(...){
# 	# sapply(list(...), function(x)paste0("'",x,"'"))
# 	paste0("c(",paste(sapply(list(...), function(x)paste0("'",x,"'")), collapse=","),")")
#
# }
# # auts <- c_aut(rdb, jwm, bls, mlp)
# auts <- c_aut(rdb, jwm, bls, mlp)
# options(devtools.desc.author=rdb)