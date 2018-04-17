if(!require(arules)){
  install.packages('arules')
  library(arules)
}
frequentsets=eclat(page_view_group_unique$page_name,parameter=list(support=0.05,maxlen=20))  #求频繁项集  
# test_freq = apriori(page_view_group_unique$page_name)
inspect(frequentsets[1:100])    #察看求得的频繁项集  
# inspect(test_freq[1:30])    #察看求得的测试集

inspect(sort(frequentsets,by="support")[1:100])    #根据支持度对求得的频繁项集排序并察看
rules=apriori(page_view_group_unique$page_name,parameter=list(support=0.01,confidence=0.01))    #求关联规则  
summary(rules)    #察看求得的关联规则之摘要  

x=subset(rules,subset=items%pin%"订单列表页"&lift>=1.2)    #求所需要的关联规则子集  
y = rules
inspect(sort(y,by="support")[1:30])    #根据支持度对求得的关联规则子集排序并察看  

