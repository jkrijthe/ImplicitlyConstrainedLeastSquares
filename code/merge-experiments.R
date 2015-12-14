at1 <- new.env()
at2 <- new.env()
at3 <- new.env()
at4 <- new.env()
at5 <- new.env()
at6 <- new.env()

at_rest <- new.env()

load("data/learningcurves-1000repeats-enoughlabeled-testing.RData"
     ,envir=at_rest)

load("data/learningcurves-200repeats-enoughlabeled-testing-part2-3.RData"
     ,envir=at1)
load("data/learningcurves-200repeats-enoughlabeled-testing-part2-4.RData"
     ,envir=at2)
load("data/learningcurves-200repeats-enoughlabeled-testing-part2-5.RData"
     ,envir=at3)
load("data/learningcurves-200repeats-enoughlabeled-testing-part2-6.RData"
     ,envir=at4)
load("data/learningcurves-100repeats-enoughlabeled-testing-part2.RData"
     ,envir=at5)
load("data/learningcurves-100repeats-enoughlabeled-testing-part2-2.RData"
     ,envir=at6)

equals(paste0(as.character(at1$classifiers),collapse=""),
       paste0(as.character(at2$classifiers),collapse=""))
equals(paste0(as.character(at2$classifiers),collapse=""),
       paste0(as.character(at3$classifiers),collapse=""))
equals(paste0(as.character(at3$classifiers),collapse=""),
       paste0(as.character(at4$classifiers),collapse=""))
equals(paste0(as.character(at4$classifiers),collapse=""),
       paste0(as.character(at5$classifiers),collapse=""))
equals(paste0(as.character(at5$classifiers),collapse=""),
       paste0(as.character(at6$classifiers),collapse=""))
equals(paste0(as.character(at6$classifiers),collapse=""),
       paste0(as.character(at_rest$classifiers),collapse=""))


equals(paste0(as.character(at1$n_labeled),collapse=""),
       paste0(as.character(at2$n_labeled),collapse=""))
equals(paste0(as.character(at2$n_labeled),collapse=""),
       paste0(as.character(at3$n_labeled),collapse=""))
equals(paste0(as.character(at3$n_labeled),collapse=""),
       paste0(as.character(at4$n_labeled),collapse=""))
equals(paste0(as.character(at4$n_labeled),collapse=""),
       paste0(as.character(at5$n_labeled),collapse=""))
equals(paste0(as.character(at5$n_labeled),collapse=""),
       paste0(as.character(at6$n_labeled),collapse=""))

equals(paste0(as.character(at1$repeats),collapse=""),
       paste0(as.character(at2$repeats),collapse=""))
equals(paste0(as.character(at2$repeats),collapse=""),
       paste0(as.character(at3$repeats),collapse=""))
equals(paste0(as.character(at3$repeats),collapse=""),
       paste0(as.character(at4$repeats),collapse=""))
equals(paste0(as.character(at4$repeats),collapse=""),
       paste0(as.character(at5$repeats),collapse=""))
equals(paste0(as.character(at5$repeats),collapse=""),
       paste0(as.character(at6$repeats),collapse=""))


concat_errorcurves <- function(ec1,ec2) {
  stopifnot(names(ec1)==names(ec2))
  errorcurves <- ec1
  for (ds in names(errorcurves)) {
    stopifnot(errorcurves[[ds]]$n_l==ec2[[ds]]$n_l)
    stopifnot(errorcurves[[ds]]$n_test==ec2[[ds]]$n_test)
    errorcurves[[ds]]$results <- bind_rows(errorcurves[[ds]]$results,ec2[[ds]]$results)
  }
  errorcurves
}


ec_out <- at1$errorcurves %>% 
  concat_errorcurves(at2$errorcurves) %>% 
  concat_errorcurves(at3$errorcurves) %>% 
  concat_errorcurves(at4$errorcurves) %>% 
  concat_errorcurves(at5$errorcurves) %>% 
  concat_errorcurves(at6$errorcurves)

errorcurves <- c(at_rest$errorcurves,ec_out)
n_labeled <- at_rest$n_labeled
classifiers <- at_rest$classifiers
repeats <- at_rest$repeats

save(errorcurves,n_labeled,classifiers,repeats,file="data/learningcurves-1000repeats-enoughlabeled-combined.Rdata")


