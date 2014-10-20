speeches <- list("We begin.", "God bless us!")

patterns <- c("I", "we|We", "America", "democra", "republic", "Democrat",
             "Republican", "free", "war", "God[^ bless]",
             "God bless|God bless|god bless",
             "Jesus|Christ|Christian", "job/Job/jobs")


counts <- t(sapply(speeches, str_count, patterns))
colnames(counts) <- patterns

