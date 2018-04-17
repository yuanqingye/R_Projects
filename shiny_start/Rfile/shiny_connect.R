rsconnect::setAccountInfo(name='yuanqingye', token='5D0E02F6D731C683FF120382F92621BC', secret='9loJXOopk/JBkT13fD3OAyTvicnJsPA+0uZXBFLm')
# proxy_url <- "http://127.0.0.1:16823/proxy_on.pac"
# Sys.setenv(http_proxy = proxy_url, https_proxy = proxy_url, ftp_proxy = proxy_url)
rsconnect::deployApp('./markdown_shiny.Rmd')
rsconnect::deployApp('./drilldown')

