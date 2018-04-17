# extract table from pdf

library(dplyr)
library(devtools)
# install_github("ropensci/tabulizer")
library(curl)
library(Rcpp)
library(tabulizer)

# location <- 'http://www.edd.ca.gov/jobs_and_training/warn/WARN-Report-for-7-1-2016-to-10-25-2016.pdf'
location = "C:/Users/qingye.yuan/Desktop/教程下载/机器学习实战.pdf"
sos_url <- "http://elections.cdn.sos.ca.gov/sov/2016-general/sov/04-historical-voter-reg-participation.pdf"
out <- extract_tables(sos_url,method = "data.frame")

install_github("ropensci/tesseract")
install.packages("curl")
install.packages("Rcpp")
library(tesseract)
text = ocr("http://jeroen.github.io/images/testocr.png")
# tesseract_download("chi_sim")
chinese = tesseract("chi_sim")
table = ocr("D://Downloads/view.jpg",engine = "chi_sim")

# Low quality:
text1 <- ocr("http://jeroen.github.io/files/dog_lq.png")
cat(text1)

# High quality:
text2 <- ocr("http://jeroen.github.io/files/dog_hq.png")
cat(text2)

digits = ocr("D://Downloads/view2.png")

out <- extract_tables("D://Downloads/view2.png")

zhejiang_mall = extract_tables("C://Users/qingye.yuan/Downloads/mall.pdf",pages = 10)

malls = extract_tables("C://Users/qingye.yuan/Downloads/mall.pdf",encoding = "UTF-8")
