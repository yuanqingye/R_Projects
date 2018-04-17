behavior05140 = read_xlsx('~/data/behavior05140_copy.xlsx')
page_view_potential_buyer = read_xlsx('~/data/page_view_potential_buyer.xlsx')
temp4 = merge(page_view_potential_buyer,behavior05140,by = 'username')
temp4 = data.table(temp4)
temp3 = data.table(temp3)
temp4[,value:=1]
temp5 = temp4[,.(sumvalue = sum(value)),by = 'username']
temp6 = temp5[sumvalue<50,username]
temp6 = temp5[sumvalue>50,username]
temp7 = temp3[!(username %in% temp6),]
temp2=data.table(path=temp7$page_name,value=1)
sunburst(temp2)

# df0514 = read_xlsx('~/data/df0514.xlsx')
# page_buyer_wide = merge(page_view_potential_buyer,df0514,by.x = 'username',by.y = 'i.u_mid')
# page_buyer_wide = data.table(page_buyer_wide)
# page_buyer_wide[,value:=1]
page_buyer_wide1 = read_xlsx('~/data/page_buyer_wide1.xlsx')
page_buyer_wide1 = data.table(page_buyer_wide1)
page_buyer_stat1 = page_buyer_wide1[,.(sumvalue = sum(value)),by = 'username']
page_buyer_stat1_cut = cut(page_buyer_stat1$sumvalue,breaks = c(1,20,10000))
page_buyer_stat1_table = table(page_buyer_stat1_cut)
names(page_buyer_stat1_table) = c('buyer view page<20','buyer view page>20')
barplot(page_buyer_stat1_table,col=c('blue','red'))