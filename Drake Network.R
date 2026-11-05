#Load library
library("igraph")

#Load nodelist and edgelist as objects
#Nodes: Drake's 13 albums and his 54 guest artists. order is the album's release order, or the average for an artist's albums.
#Edges: an artist is credited on an album. Weight = number of tracks.
album_nodes <- read.csv("~/Downloads/PAI 741/Data Sets/Problem Set 9 /drake_nodes.csv", header=T, as.is=T)
album_links <- read.csv("~/Downloads/PAI 741/Data Sets/Problem Set 9 /drake_edges.csv", header=T, as.is=T)


album_nodes
album_links
 
#Convert to an igraph network. "d" identifies the edgelist; "vertices" identifies the nodelist
album_net <- graph_from_data_frame(d=album_links, vertices=album_nodes, directed=F)
album_net
 
E(album_net)
V(album_net)
E(album_net)$weight
V(album_net)[type=="returning"]
 
#Degree: how many projects each artist is on
V(album_net)$degree <- degree(album_net)
 
#Left to right = release order. Height = number of projects.
 
#One-time guests stack downward under their project. Even projects start lower so names do not collide.
V(album_net)$stack <- ave(V(album_net)$degree, V(album_net)$order, V(album_net)$degree, FUN=seq_along)
V(album_net)$height <- ifelse(V(album_net)$type=="album", 0,
                 ifelse(V(album_net)$type=="scale", V(album_net)$level,
                 ifelse(V(album_net)$type=="returning", V(album_net)$degree,
                        -0.5 - 0.45*V(album_net)$stack - ifelse(V(album_net)$order %% 2 == 0, 0.22, 0))))
 
#Rows with 3 or more returning guests are spaced evenly, in order of their average position
V(album_net)$row <- ifelse(V(album_net)$type=="returning", V(album_net)$degree, 0)
V(album_net)$time_order <- rank(V(album_net)$order, ties.method="first")
V(album_net)$row_rank <- ave(V(album_net)$time_order, V(album_net)$row, FUN=rank)
V(album_net)$row_size <- ave(V(album_net)$order, V(album_net)$row, FUN=length)
V(album_net)$across <- ifelse(V(album_net)$row>=2 & V(album_net)$row_size>=3,
                        0.5 + 21*(V(album_net)$row_rank-0.5)/V(album_net)$row_size,
                        V(album_net)$order)
 
album_layout <- cbind(V(album_net)$across, V(album_net)$height)
 
#Scale the layout to run from -1 to 1 so the grid lines can use the same coordinates
album_layout <- norm_coords(album_layout)
 
#Grid line heights: one for each project level from 2 projects up
album_grid_lines <- album_layout[V(album_net)$type=="scale" & V(album_net)$level>=2, 2]
 
#Generate colors and write into network:
V(album_net)$color <- ifelse(V(album_net)$type=="album", "#1f1f1e",
                ifelse(V(album_net)$type=="returning", "#2a78d6", "#c3c2b7"))
E(album_net)$color <- ifelse(E(album_net)$role=="co-lead", "#c98500",
                ifelse(E(album_net)$role=="returning", "#2a78d673", "#c3c2b7"))
 
#Projects are rectangles; artists are circles sized by degree
V(album_net)$shape <- ifelse(V(album_net)$type=="album", "rectangle",
                      ifelse(V(album_net)$type=="scale", "none", "circle"))
V(album_net)$size <- ifelse(V(album_net)$type=="album", 8.3,
               ifelse(V(album_net)$type=="returning", 1.5 + V(album_net)$degree*0.7, 1.2))
V(album_net)$size2 <- ifelse(V(album_net)$type=="album", 19, V(album_net)$size)
 
#Tie strength: tracks, with co-lead albums drawn thick
E(album_net)$width <- ifelse(E(album_net)$role=="co-lead", 5, E(album_net)$weight*1.3)
 
#Label placement: names go above the dot
V(album_net)$label.degree <- -pi/2
V(album_net)$label.dist <- ifelse(V(album_net)$type=="album" | V(album_net)$type=="scale", 0,
                     ifelse(V(album_net)$type=="returning", 1.2, 0.42))
V(album_net)$label.font <- ifelse(V(album_net)$type=="one-time" | V(album_net)$type=="scale", 1, 2)
V(album_net)$label.cex <- ifelse(V(album_net)$type=="one-time", 0.66, 0.8)
V(album_net)$label.cex <- ifelse(V(album_net)$type=="album", 0.65, V(album_net)$label.cex)
V(album_net)$label.cex <- ifelse(V(album_net)$type=="scale", 0.95, V(album_net)$label.cex)
V(album_net)$label.color <- ifelse(V(album_net)$type=="album", "white",
                            ifelse(V(album_net)$type=="scale", "#52514e", "black"))
 
#Plot and save
png("drake_network.png", width=19, height=12, units="in", res=200)
 
#Draw an empty plot first so the grid lines sit behind the network
plot(0, type="n", xlim=c(-1, 1), ylim=c(-1, 1.06), axes=F, xlab="", ylab="",
   main="Drake's inner circle turned over, 2009 to 2026\n75 guests on 21 projects. Only 18 came back for a second project.")
 
segments(x0=-0.87, y0=album_grid_lines, x1=1, y1=album_grid_lines, col="#d9d7cf", lwd=1)
 
plot.igraph(album_net,
   vertex.label=V(album_net)$label,
   vertex.label.family="Helvetica",
   vertex.frame.color=NA,
   layout=album_layout,
   rescale=F,
   add=T)
 
legend("bottomright", bty="n",
   legend=c("Project", "Returning guest", "One-time guest", "Co-lead on whole project"),
   pch=c(15, 16, 16, 15),
   col=c("#1f1f1e", "#2a78d6", "#c3c2b7", "#c98500"),
   pt.cex=2)
 
dev.off()
 