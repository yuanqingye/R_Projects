set.seed(1)
no_resp <- 500
resp <- 100
response <- factor(c(rep(0,no_resp),rep(1,resp)))
purchased_previously <- factor(c(sample(0:1,no_resp,prob=c(0.6,0.4),replace=T),
                                 sample(0:1,resp,prob=c(0.2,0.8),replace=T)))
opened_previously <- factor(sample(0:1,(no_resp+resp),prob=c(0.8,0.2),replace = T))
sales_12mo <- c(rnorm(n=no_resp,mean = 50, sd = 10),
                rnorm(n=resp,mean = 60, sd = 5))
none_open_buy <- factor(c(sample(0:1, no_resp,prob=c(0.8,0.2),replace=T),
                          rep(1,resp)))
test_var <- sample(LETTERS[1:2],(resp+no_resp),replace=T)

naive_data <- data.frame(purchased_previously = purchased_previously,
                         opened_previously = opened_previously,
                         sales_12mo = sales_12mo,
                         none_open_buy = none_open_buy,
                         test_var = test_var,
                         response = response)

naive_data <- naive_data[sample(1:nrow(naive_data),nrow(naive_data)),]

train <- naive_data[1:(nrow(naive_data)*.7),]
test <- naive_data[(nrow(naive_data)*.7+1):nrow(naive_data),]