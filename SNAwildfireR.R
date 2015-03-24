library (tm)
library(igraph)
a<-read.csv("c:/Data/SNAwildfireR.csv")[,c('Source','Target')]
a$ST<-paste(a$Source, a$Target, sep=",")
a<-subset(a, select=-c(Source, Target))
a<-sub(",", " ", a$ST)
myCorpus<-Corpus(VectorSource(a))
inspect(myCorpus[11:15])
myTdm <- TermDocumentMatrix(myCorpus)
findFreqTerms(myTdm, lowfreq=30)

#convert the term-document matrix to a normal matrix
myTdm<-as.matrix(myTdm)
myTdm[1:17,1:17]

# transform into a term-term adjacency matrix
termMatrix <- myTdm %*% t(myTdm)
# inspect terms numbered 5 to 10
termMatrix[1:17,1:17]
termMatrix[upper.tri(termMatrix)]<-0
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, mode="directed", weighted=TRUE, diag=FALSE)
# graph summary
summary(g)
plot.igraph(g,vertex.label=V(g)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(g)$weight/3, edge.arrow.size=1.5)
#calculate the indegree and outdegree
indegreeG <- degree(g, mode="in")
outdegreeG <- degree(g, mode="out")
totaldegreeG <- degree(g)
inclosenessG <- closeness(g, mode='in')
outclosenessG <- closeness(g, mode='out')
totalclosenessG <- closeness(g)
betweennessG <- betweenness(g)
forumG <- data.frame(V(g)$name, indegreeG, outdegreeG, totaldegreeG, inclosenessG,outclosenessG, totalclosenessG, betweennessG)
forumG[1:5,]
write.table(forumG,file="c:/Data/forumG.csv",sep=",")








