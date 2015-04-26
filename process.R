setwd('~/work/R/capstone')
ugram_index<-read.table("ugram.txt",header=F)
saveRDS(ugram_index,"u.rds")
bgram_index<-read.csv("bgram_index.csv",header=F)
saveRDS(bgram_index,"b.rds")
tgram_index<-read.csv("tgram_index.csv",header=F)
saveRDS(tgram_index,"t.rds")
qgram_index<-read.csv("qgram_index.csv",header=F)
saveRDS(qgram_index,"q.rds")


