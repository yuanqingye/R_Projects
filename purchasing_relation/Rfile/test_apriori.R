source('~/Rfile/R_impala.R')
source('~/Rfile/toolfunc.R')
source2('~/Rfile/R_hive.R',7,17)

rules = apriori(Groceries,parameter = list(support = 0.001,confidence = 0.5))

inspect(head(sort(rules, by = "lift"), 30))

plot(rules)

plot(rules, measure = c("support", "lift"), shading = "confidence")

plot(rules, shading = "order", control = list(main = "Two-key plot"))

sel <- plot(rules, measure = c("support", "lift"), shading = "confidence",interactive = TRUE)

plot(rules, method = "grouped")

plot(rules, method = "grouped",control = list(k = 50))

plot(rules, method = "grouped",control = list(k = 50),interactive = TRUE)

subrules2 <- head(sort(rules, by = "lift"), 10)

plot(subrules2, method = "graph",control=list(cex=1.2))
plot(subrules2,method = "graph",control = list(type = 'itemsets',edgeCol = "red"))
plot(subrules2,method="graph",control=list(type="bipartite"))
#画矩阵图
plot(rules, method="matrix", measure=c("lift", "confidence"))

#画3d矩阵图
plot(rules, method="matrix3D", measure="lift")
plot(rules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
#带颜色的散点图
plot(rules, shading="order", control = list(main = "Two-key plot", 
                                            col=rainbow(max(size(rules))-1L)))

plot(subrules2, method = "graph", control = list(type = "items",edgeCol = 'red'))

#箭头图
plot(subrules2, method="paracoord")

plot(subrules2, method="doubledecker")

#double decker图 only apply to a single rule
oneRule = sample(rules,1)
inspect(oneRule)
plot(oneRule, method = "doubledecker", data = Groceries)
plot(oneRule, method = "mosaic", data = Groceries)

#对基础支持集合的应用
itemsets <- eclat(Groceries, parameter = list(support = 0.02, minlen=2))
plot(itemsets, method="graph")
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))

#使用plotly来观察思考
plotly_arules(rules, method = "scatterplot", measure = c("support", "confidence"),
              shading = "lift", max = 1000)
plotly_arules(rules, method = "matrix")

p <- plotly_arules(rules)
htmlwidgets::saveWidget(p, "~/Rmarkdown/arules.html", selfcontained = FALSE)
browseURL("arules.html")

plotly_arules(rules, jitter = 10, opacity = .7, size = 10, symbol = 1,
              colors = c("blue", "green"))

#可以以图表的方式方便的察看数据
inspectDT(rules)

#save graphs
saveAsGraph(rules, "rules.graphml")

# 存储以方便Gephi来引用
# saveDot(head(sort(rules,by="lift"),1000),file="rules.dot")

query = "select mobile,prod_name,cont_cat1_name,cont_cat2_name,cont_cat3_name,to_date(ordr_date) from dl.fct_ordr d
where to_date(ordr_date) >= '2017-06-21' and to_date(ordr_date) <= '2017-06-27'
and ((d.ordr_status not in (1,7,19) and d.sale_type<>'oms') or (d.ordr_status ='Y' and d.sale_type='oms'))"

order_list = dbGetQuery(con,query)
order_list = data.table(order_list)
order.list.nona = order_list[!is.na(cont_cat3_name),]
order.list.nodup = unique(order.list.nona, by=c("mobile", "cont_cat3_name"))
order.list.nodup[,number := .N,by = c("mobile", "cont_cat3_name")]
setorder(order.list.nodup,mobile)

prepare.list = aggregate(cont_cat3_name~mobile,data = order.list.nodup,paste)

frequentsets=eclat(prepare.list$cont_cat3_name,parameter=list(support=0.001,maxlen=20))  #求频繁项集  

inspect(frequentsets)
inspect(sort(frequentsets,by="support")[1:100])    #根据支持度对求得的频繁项集排序并察看

rules=apriori(prepare.list$cont_cat3_name,parameter=list(support=0.001,confidence=0.01))    #求关联规则  
y = rules
inspect(sort(y,by="confidence"))    #根据支持度对求得的关联规则子集排序并察看  
x=subset(rules,subset=(lhs%pin%"整体卫浴")&lift>=0.1)    #求所需要的关联规则子集  
inspect(x)

data("Adult")
rules <- apriori(Adult)
inspect(rules[1000])

#表示的特殊形式
inspect(rules[1000], ruleSep = "---->", itemSep = " + ", setStart = "", setEnd ="", 
        linebreak = FALSE)
