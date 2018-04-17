item = factor(c("A","B","B","A","B","A","C","A","B","C","B","A","B","A","A","B","A","B"))
seqid = c(rep(1,5),rep(2,6),rep(3,3),rep(4,4))
eventids = c(10,15,20,30,35,20,25,30,35,40,50,10,30,40,30,35,40,50)

data = data.frame(item = item)
data.tran = as(data,"transactions")
transactionInfo(data.tran)$sequenceID = seqid
transactionInfo(data.tran)$eventID = eventids

result = cspade(data.tran,parameter = list(support = 0.5),control = list(verbose = TRUE))
result = sort(result,by = "support")
inspect(result)

result.1 = result[grep(".*item=B.*",as(result,"data.frame")$sequence)]
result.2 = result[grep(".*item=B[^\\}]*\\}>",as(result,"data.frame")$sequence)]
result.3 = result[grep("<\\{[^\\}]*item=B.*",as(result,"data.frame")$sequence)]

#网页访问的例子
tmpp = read.fwf("~/data/anonymous-msweb.test",widths = c(60))
train_list = tmpp$V1

tmp_page = c(0)
tmp_sequenceid = c(0)
tmp_eventid = c(0)
m = 0
sequenceid = 0
train_length = length(train_list)

for(i in 302:train_length){
  tmp = unlist(strsplit(as.character(train_list[i]),","))
  if(tmp[1]=="C"){
    sequenceid = sequenceid + 1
    eventid = 0
  }
  else if(tmp[1]=="V"){
    m = m+1
    eventid = eventid + 1
    tmp_sequenceid[m] = sequenceid
    tmp_eventid[m] = eventid
    tmp_page[m] = as.numeric(tmp[2])
  }
}
tmp_page = factor(tmp_page)
data = data.frame(page = tmp_page,seqid = tmp_sequenceid,eventid = tmp_eventid)

data$seqid[which(data$page == 1034)]

user.page = 1034

# data.user = data[data$seqid == unique(data$seqid[which(data$page==user.page)]),]
data.user = data[data$seqid %in% unique(data$seqid[which(data$page==user.page)]),]
tmp_data = data.frame(page=data.user$page)
data.tran = as(tmp_data,"transactions")
transactionInfo(data.tran)$sequenceID = data.user$seqid
transactionInfo(data.tran)$eventID = data.user$eventid

result = cspade(data.tran,parameter = list(support = 0,maxlen = 2),control = list(verbose = TRUE))
result = sort(result,by = "support")
inspect(result)
result.2 = result[grep(paste0(".*page=",user.page,"[^\\}]*\\}>"),as(result,"data.frame")$sequence)]

result.data.frame = as(result.2[-1],"data.frame")
persent = result.data.frame$support/sum(result.data.frame$support)
sum.persent = cumsum(persent)
result.data.frame = cbind(result.data.frame,persent,sum.persent)

page = 0;i=1
for(i.seq in result.data.frame$sequence){
  real_seq1 = regexpr("<\\{page=",i.seq)[1]+7
  real_seq2 = regexpr("\\}",i.seq)[1]-1
  page[i] = substr(i.seq,real_seq1,real_seq2)
  i = i+1
}

i=1;uv=0
for(i.page in page){
  uv[i]=length(unique(data$seqid[which(data$page==i.page)]))
  i=i+1
}

conf=result.data.frame$support*result@info$nsequences/uv

result.data.frame = cbind(result.data.frame,conf=conf,page=page)
result.data.frame = result.data.frame[1:165,]
#作图研究
barplot(as.matrix(result.data.frame[1:20,]$persent,nrow = 1),ylim = c(0,1),beside = TRUE,xlab ="page name",main = "ability to introduce user to page 1034")
lines(0.5+1:nrow(result.data.frame[1:20,]),result.data.frame[1:20,]$conf,col = "red",type = "b")
text(0.5+1:nrow(result.data.frame[1:20,]),result.data.frame[1:20,]$conf,labels = paste0(round(result.data.frame[1:20,]$conf*100,2),"%"))
axis(1,at=0.5+1:nrow(result.data.frame[1:20,]),labels = result.data.frame[1:20,]$page,tick = F)
