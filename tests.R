library("igraph")

el=read.csv(file="data/minimal_weights000.csv", header=TRUE, sep=",");
el[,1]=as.character(el[,1]);
el[,2]=as.character(el[,2]);
el=as.matrix(el)
g=graph.edgelist(el[,1:2],directed=FALSE)
E(g)$weight=as.numeric(el[,3]) 

g_mst <- mst(g,E(g)$weight)

non_isolated <- c()

isolated <- c()

total_vecseq <- list()
part <- make_empty_graph(n=0)
g_mst2 <- g_mst
j <- 0
i <- 0
while (vcount(g_mst2)>0) {
	j <- j + i
	i <- 0
	dg <- decompose.graph(g_mst2)
	for (part in dg) {
		i <- i + 1
		#cat(j + i,"\n")
		if (vcount(part)>=4) {
			g_dia <- get_diameter(part, directed=TRUE, weights=NA)
			non_isolated <- append(non_isolated, g_dia$name)
			total_vecseq[[j + i]] <- g_dia$name
			#non_isolated_vecseq[[j + i]] <- g_dia$name
			#cat("non_isolated : ", g_dia$name, "\n")
			#g_mst2 <- delete.vertices(g_mst, g_dia$name)
		} else {
			isolated <- append(isolated, V(part)$name)
			total_vecseq[[j + i]] <- V(part)$name
			#isolated_vecseq[[j + i]] <- V(part)$name
			#cat("isolated : ", V(part)$name, "\n")
			#g_mst2 <- delete.vertices(g_mst, V(part)$name)
		}
	}
	for_deletion <- append(non_isolated, isolated)
	g_mst2 <- delete.vertices(g_mst, unique(for_deletion))
}

num <- 1

plot.igraph(g,vertex.size=1,vertex.label=NA,layout=layout.fruchterman.reingold)