# Game of Thrones network changes over time
library(igraph)
# setwd("~/Academic/MSBA files/Network Analytics files/Game of Thrones character Network/asoiaf-master/data")

# Construct network objects for the 4 books in order
edges1= read.csv("asoiaf-book1-edges.csv")
edges2= read.csv("asoiaf-book2-edges.csv")
edges3= read.csv("asoiaf-book3-edges.csv")
edges45= read.csv("asoiaf-book45-edges.csv")
net1= graph.data.frame(edges1)
net2= graph.data.frame(edges2)
net3= graph.data.frame(edges3)
net4= graph.data.frame(edges45)

# Building a table of network level metrics
nodes1= length(V(net1))
nodes2= length(V(net2))
nodes3= length(V(net3))
nodes4= length(V(net4))
nodes= c(nodes1, nodes2, nodes3, nodes4)

btw1= mean(betweenness(net1))
btw2= mean(betweenness(net2))
btw3= mean(betweenness(net3))
btw4= mean(betweenness(net4))
btw= c(btw1, btw2, btw3, btw4)

cls1= mean(closeness(net1))
cls2= mean(closeness(net2))
cls3= mean(closeness(net3))
cls4= mean(closeness(net4))
cls= c(cls1, cls2, cls3, cls4)

dgr1= mean(degree(net1))
dgr2= mean(degree(net2))
dgr3= mean(degree(net3))
dgr4= mean(degree(net4))
dgr= c(dgr1, dgr2, dgr3, dgr4)

burt1= mean(constraint(net1))
burt2= mean(constraint(net2))
burt3= mean(constraint(net3))
burt4= mean(constraint(net4))
burt= c(burt1, burt2, burt3, burt4)

# character level measures over time

cat1= constraint(net1)[V(net1)["Catelyn-Stark"]]
cat2= constraint(net2)[V(net2)["Catelyn-Stark"]]
cat3= constraint(net3)[V(net3)["Catelyn-Stark"]]
cat4= constraint(net4)[V(net4)["Catelyn-Stark"]]
cat= c(cat1, cat2, cat3, cat4)

tyw1= constraint(net1)[V(net1)["Tywin-Lannister"]]
tyw2= constraint(net2)[V(net2)["Tywin-Lannister"]]
tyw3= constraint(net3)[V(net3)["Tywin-Lannister"]]
tyw4= constraint(net4)[V(net4)["Tywin-Lannister"]]
tywin= c(tyw1, tyw2, tyw3, tyw4)

robb1= constraint(net1)[V(net1)["Robb-Stark"]]
robb2= constraint(net2)[V(net2)["Robb-Stark"]]
robb3= constraint(net3)[V(net3)["Robb-Stark"]]
robb4= constraint(net4)[V(net4)["Robb-Stark"]]
robb= c(robb1, robb2, robb3, robb4)

joff1= constraint(net1)[V(net1)["Joffrey-Baratheon"]]
joff2= constraint(net2)[V(net2)["Joffrey-Baratheon"]]
joff3= constraint(net3)[V(net3)["Joffrey-Baratheon"]]
joff4= constraint(net4)[V(net4)["Joffrey-Baratheon"]]
joff= c(joff1, joff2, joff3, joff4)

ned1= constraint(net1)[V(net1)["Eddard-Stark"]]
ned2= constraint(net2)[V(net2)["Eddard-Stark"]]
ned3= constraint(net3)[V(net3)["Eddard-Stark"]]
ned4= constraint(net4)[V(net4)["Eddard-Stark"]]
ned= c(ned1, ned2, ned3, ned4)

df_burt= data.frame(cat, tywin, robb, joff, ned)
# df_btw, df_dgr, df_cls, df_burt, df_core

df= data.frame(nodes, btw, cls, dgr, burt)

blist= constraint(net1)
blist= blist[order(blist)]
