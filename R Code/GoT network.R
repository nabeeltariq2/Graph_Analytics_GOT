# setwd("~/Academic/MSBA files/Network Analytics files/Game of Thrones character Network/asoiaf-master/data")
library(igraph)

# build ROCplot function
ROCPlot <- function(Pvec,Cvec,Plot=T,Add=F) {
  NHam <- sum(Cvec==0)
  NSpam <- sum(Cvec==1)
  PvecS <- unique(sort(Pvec))
  x <- rep(NA,length(PvecS))
  y <- rep(NA,length(PvecS))
  for(i in 1:length(PvecS)) {
    x[i] <- sum(Pvec>=PvecS[i]&Cvec==0)/NHam
    y[i] <- sum(Pvec>=PvecS[i]&Cvec==1)/NSpam
  }
  x <- c(0,x,1)
  y <- c(0,y,1)
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  
  AUC <- sum((x[2:length(x)]-x[1:(length(x)-1)])*(y[2:length(y)]+y[1:(length(y)-1)])/2)
  
  if(Add) {
    plot(x,y,type="l",xlim=c(0,1),ylim=c(0,1),xlab="P( classified + | Is - )",ylab="P( classified + | Is + )")
    title("ROC Curve")
    mtext(paste("AUC =",round(AUC,3)),side=3,line=0.5)
    abline(0,1)
    par(pty="m")
  } else {
    if(Plot) {
      par(pty="s")
      plot(x,y,type="l",xlim=c(0,1),ylim=c(0,1),xlab="P( classified + | Is - )",ylab="P( classified + | Is + )")
      title("ROC Curve")
      mtext(paste("AUC =",round(AUC,3)),side=3,line=0.5)
      abline(0,1)
      par(pty="m")
    }
  }
  
  invisible(list(x=c(0,x,1),y=c(0,y,1),AUC=AUC))
}

# establish full network igraph object as "network"
table= read.csv("asoiaf-all-edges.csv")
edges= table[,1:2]
edges= cbind(edges,table$weight)

pruned= edges[edges[,3]>30,]
network= graph.data.frame(pruned, directed = FALSE)
summary(smallnet)
network$name= "GoT Character Network"

# attaching weights to the edges of both networks
network$weight= pruned[,3]

# calculating measures of centrality in the character network
btw= betweenness(network, directed = FALSE)
cls= closeness(network)
dgr= degree(network, mode = "all")
# ordering the lists and selecting the top 25
btw= btw[order(btw, decreasing = TRUE)]
btw= btw[1:25]
cls= cls[order(cls, decreasing = TRUE)]
cls= cls[1:25]
dgr= dgr[order(dgr, decreasing = TRUE)]
dgr= dgr[1:25]

# structural holes
burts= constraint(network)
burts= burts[order(burts)]
burts= burts[1:25]

# make a top 25 list
top25= data.frame(names(btw), names(cls), names(dgr), names(burts))
labs= c("Betweenness", "Closeness", "Degree", "Burt's Constraint")
top25= `colnames<-`(top25,labs)

#viewing the network
tkplot(network)


# finding largest cliques
for (i in 1:15){
  a= length(cliques(network, min = i))
  print(cat(i,':',a))
}
## so there is one 14-clique and 17 13-cliques
kliks= cliques(network, min = 10)

# measuring 'coreness'
core= coreness(network)

egn= eigen_centrality(network)

# building a table of variables for a predictive model
table= read.csv("GoT_metrics.csv", stringsAsFactors = TRUE)
table[,(length(table)+1)]= core
colnames(table)[length(table)]= "core"
table[,(length(table)+1)]= as.array(egn$vector)
colnames(table)[length(table)]= "eigen"
colnames(table)[1]= "names"

# modeling dead(binary) vs other factors
terms= colnames(table[3:length(table)])
eqn= paste("dead~",paste(terms,collapse = "+"))

mod= glm(eqn , data= table, family = "binomial")
summary(mod)
table[,(length(table)+1)]= mod$fitted.values
colnames(table)[length(table)]= "fitted"

ROCPlot(table$fitted,table$dead)

# building an ordered table to look at characters more or less likely to die
table= table[order(table$fitted, decreasing = TRUE),]
browse= data.frame(table$names,table$dead,table$fitted)
browse[browse$table.dead==0,]
