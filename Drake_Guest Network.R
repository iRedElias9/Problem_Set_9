#Load library
library("igraph")

#Load node list and edge list as objects
#Nodes: each year and the 104 artists whose songs Drake appeared on.
#year is the year, or the average year of an artist's songs. songs is the number of songs.
#The "scale" nodes are not artists. They have no ties and only draw the song scale on the left.
#Edges: Drake was a guest on that artist's song that year. Weight = number of songs.
guest_nodes <- read.csv("~/Downloads/PAI 741/Data Sets/Problem Set 9 /drake_guest_nodes.csv", header=T, as.is=T)
guest_links <- read.csv("~/Downloads/PAI 741/Data Sets/Problem Set 9 /drake_guest_edges.csv", header=T, as.is=T)

guest_nodes
guest_links
 
#Convert to an igraph network. "d" identifies the edgelist; "vertices" identifies the nodelist
guest_net <- graph_from_data_frame(d=guest_links, vertices=guest_nodes, directed=F)
guest_net
 
E(guest_net)
V(guest_net)
E(guest_net)$weight
V(guest_net)[type=="two-way"]
E(guest_net)[weight>=2]
 
#Left to right = year. Height = number of songs.
 
#One-time hosts stack downward under their year. Even years start lower so names do not collide.
V(guest_net)$stack <- ave(V(guest_net)$songs, V(guest_net)$year, V(guest_net)$songs, FUN=seq_along)
V(guest_net)$height <- ifelse(V(guest_net)$type=="year", 0,
                 ifelse(V(guest_net)$type=="scale", V(guest_net)$songs,
                 ifelse(V(guest_net)$songs>=2, V(guest_net)$songs,
                        -0.9 - 0.6*V(guest_net)$stack - ifelse(V(guest_net)$year %% 2 == 0, 0.3, 0))))
 
#Rows with 3 or more repeat hosts are spaced evenly, in order of their average year
#The top hosts (8 or more songs) count as one row so they do not sit on top of each other
V(guest_net)$row <- ifelse(V(guest_net)$type=="year" | V(guest_net)$type=="scale", 0, pmin(V(guest_net)$songs, 8))
V(guest_net)$time_order <- rank(V(guest_net)$year, ties.method="first")
V(guest_net)$row_rank <- ave(V(guest_net)$time_order, V(guest_net)$row, FUN=rank)
V(guest_net)$row_size <- ave(V(guest_net)$year, V(guest_net)$row, FUN=length)
V(guest_net)$across <- ifelse(V(guest_net)$row>=2 & V(guest_net)$row_size>=3,
                        2006 + 21*(V(guest_net)$row_rank-0.5)/V(guest_net)$row_size,
                        V(guest_net)$year)
 
guest_layout <- cbind(V(guest_net)$across, V(guest_net)$height)
 
#Scale the layout to run from -1 to 1 so the grid lines can use the same coordinates
guest_layout <- norm_coords(guest_layout)
 
#Grid line heights: one for each song level from 2 songs up
grid_lines <- guest_layout[V(guest_net)$type=="scale" & V(guest_net)$songs>=2, 2]
 
#Generate colors and write into network:
#Blue artists are also credited on a Drake-led track; gold artists are not
V(guest_net)$color <- ifelse(V(guest_net)$type=="year", "#1f1f1e",
                ifelse(V(guest_net)$type=="two-way", "#2a78d6", "#c98500"))
 
#Dispute years turn the year box red
dispute_years <- c("2013", "2015", "2018", "2024")
V(guest_net)$color <- ifelse(V(guest_net)$name %in% dispute_years, "#c62828", V(guest_net)$color)
 
E(guest_net)$color <- ifelse(E(guest_net)$type=="two-way", "#2a78d66B", "#c9850080")
 
#Years are rectangles; artists are circles sized by number of songs
V(guest_net)$shape <- ifelse(V(guest_net)$type=="year", "rectangle",
                ifelse(V(guest_net)$type=="scale", "none", "circle"))
V(guest_net)$size <- ifelse(V(guest_net)$type=="year", 7.8,
               ifelse(V(guest_net)$songs>=2, 1.3 + sqrt(abs(V(guest_net)$songs))*1.25, 1.3))
V(guest_net)$size2 <- ifelse(V(guest_net)$type=="year", 9, V(guest_net)$size)
 
#Tie strength: songs that year
E(guest_net)$width <- E(guest_net)$weight*1.5
 
#Label placement: names go above the dot
V(guest_net)$label.degree <- -pi/2
V(guest_net)$label.dist <- ifelse(V(guest_net)$type=="year" | V(guest_net)$type=="scale", 0,
                     ifelse(V(guest_net)$songs>=2, 1.2, 0.42))
V(guest_net)$label.font <- ifelse(V(guest_net)$type=="year" | V(guest_net)$songs>=2, 2, 1)
V(guest_net)$label.font <- ifelse(V(guest_net)$type=="scale", 1, V(guest_net)$label.font)
V(guest_net)$label.cex <- ifelse(V(guest_net)$type=="year" | V(guest_net)$songs>=2, 0.78, 0.66)
V(guest_net)$label.cex <- ifelse(V(guest_net)$type=="scale", 0.95, V(guest_net)$label.cex)
V(guest_net)$label.color <- ifelse(V(guest_net)$type=="year", "white",
                      ifelse(V(guest_net)$type=="scale", "#52514e", "black"))
 
#Plot and save
png("drake_guest_network.png", width=19, height=12, units="in", res=200)
 
#Draw an empty plot first so the grid lines sit behind the network
plot(0, type="n", xlim=c(-1, 1), ylim=c(-1, 1), axes=F, xlab="", ylab="",
   main="Drake as a guest, 2005 to 2026\n194 songs by 104 artists. Height = number of songs together.")
 
segments(x0=-0.88, y0=grid_lines, x1=1, y1=grid_lines, col="#d9d7cf", lwd=1)
 
plot.igraph(guest_net,
   vertex.label=V(guest_net)$label,
   vertex.label.family="Helvetica",
   vertex.frame.color=NA,
   layout=guest_layout,
   rescale=F,
   add=T)
 
legend("bottomleft", bty="n",
   legend=c("Year", "Year of a major public dispute",
            "   2013: Kendrick's \"Control\" verse", "   2015: Meek Mill feud",
            "   2018: Kanye West feud", "   2024: Kendrick feud",
            "Also on a Drake-led track", "Hosted Drake only"),
   pch=c(15, 15, NA, NA, NA, NA, 16, 16),
   col=c("#1f1f1e", "#c62828", NA, NA, NA, NA, "#2a78d6", "#c98500"),
   pt.cex=2)
 
dev.off()
 