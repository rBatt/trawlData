# authors
rdb <- person(given="Ryan D.", family="Batt", email="battrd@gmail.com", role=c("aut","cre"))
jwm <- person(given="James W.", family="Morley", email="jw.morley@rutgers.edu", role=c("aut"))
bls <- person(given="Rebecca L.", family="Selden", email="becca.selden@gmail.com", role=c("aut"))
mlp <- person(given="Malin L.", family="Pinsky", email="malin.pinsky@rutgers.edu", role=c("aut"))



c.aut <- function(...){
	sapply(list(...), function(x)paste0("'",x,"'"))
}
auts <- c.aut(rdb, jwm, bls, mlp)
options(devtools.desc.author=auts)