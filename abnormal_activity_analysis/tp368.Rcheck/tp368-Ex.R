pkgname <- "tp368"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('tp368')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("preprocess_train_data")
### * preprocess_train_data

flush(stderr()); flush(stdout())

### Name: preprocess_train_data
### Title: preprocess_train_data
### Aliases: preprocess_train_data
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (original_set) 
{
    train_set = original_set[, pickedColums]
    train_base = train_set[complete.cases(train_set), ]
    train_base = as.data.frame(train_base)
    train_base$sign = as.factor(train_base$sign)
    return(train_base)
  }



cleanEx()
nameEx("tp368-package")
### * tp368-package

flush(stderr()); flush(stdout())

### Name: tp368-package
### Title: What the package does (short line)
### Aliases: tp368-package tp368
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
