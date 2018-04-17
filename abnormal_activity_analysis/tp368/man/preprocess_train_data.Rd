\name{preprocess_train_data}
\alias{preprocess_train_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
preprocess_train_data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is a good packages
}
\usage{
preprocess_train_data(original_set)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{original_set}{
%%     ~~Describe \code{original_set} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
