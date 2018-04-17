library(RCurl)
library(rjson)

user = "blog125"
key = "21376174"

trans = function(word) {
  word = URLencode(word)
  web = getURL(
    paste(
      "http://fanyi.youdao.com/openapi.do?keyfrom=", user,
      "&key=", key,
      "&type=data&doctype=json&version=1.1&q=", word,
      sep = ""
    ))
  return(fromJSON(web)$translation)
}
