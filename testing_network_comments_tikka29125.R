# Following the previous work for doing the network plot (https://r-graph-gallery.com/network.html)
# Here the main data of the plot takes correlations:
tv_c=tv_c[,!colnames(tv_c) %in% c('Total_TG','PFAS','Perfluorodecyl.ethanoic.acid')]; # Remove specific columns from tv_c
tv_c=tv_c[,!colnames(tv_c) %in% x4] # Remove columns specified in x4 from tv_c
colnames(tv_c)[colnames(tv_c)=="17aOH-P4"]="17a-OHP4" # Rename column '17aOH-P4' to '17a-OHP4'
dat = tv_c; 
# Remove 'Gender' and 'PatientNumber' columns from dat
dat=dat[,!colnames(dat) %in% c('Gender','PatientNumber')] 
# Calculate Spearman correlation matrix
resulta <- (rcorr(as.matrix(dat), type = c('spearman')))$r 
# Set threshold level for connections between vertices (pampulat)
n_level=0.9 
# Calculate Nrr and replace NA values with 1
Nrr=qpNrr(resulta, verbose=FALSE);Nrr[is.na(Nrr)]=1;
# Create a condition matrix based on n_level threshold
cond=data.frame(as.matrix(Nrr<n_level)) 
# Elementwise matrix multiplication and update column names to match row names # https://www.geeksforgeeks.org/elementwise-matrix-multiplication-in-r/
RN=data.frame(resulta);tes_t=cond*RN;tes_t=as.matrix(tes_t);resulta=tes_t;colnames(resulta)=rownames(resulta) 
tes_t=resulta
# Calculate lengths of x1 to x6 and assign to variables a to f
a=length(x1)-2;b=length(x2);c=length(x3);d=length(x4);e=length(x5);f=length(x6); 

# Removing self-correlation
tes_t[1:a,1:a]=0
tes_t[(a+1):(a+b),(a+1):(a+b)]=0
tes_t[(a+b+1):(a+b+c),(a+b+1):(a+b+c)]=0
tes_t[(a+b+c+1):(a+b+c+e),(a+b+c+1):(a+b+c+e)]=0
tes_t[(a+b+c+e+1):(a+b+c+e+f),(a+b+c+e+1):(a+b+c+e+f)]=0

tes_t=tes_t[colnames(tes_t)[7:66],colnames(tes_t)[7:66]] # Select a subset of tes_t based on column names
g <- graph_from_adjacency_matrix(tes_t, mode="upper", weighted=TRUE, diag=FALSE) # Create a graph from adjacency matrix
e <- as_edgelist(g); df <- as.data.frame(cbind(e,E(g)$weight)); # Convert graph to edge list and create a data frame with weights
df[,3]=as.numeric(df[, 3]) # Convert the third column to numeric
hoi=df 
hoi=hoi[!duplicated(hoi[,c(1,2)]),] # Remove duplicate rows based on the first two columns

# https://r-graph-gallery.com/249-igraph-network-map-a-color.html
library(igraph) # Load igraph library
# # create data:
links <- data.frame(
  source=c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target=c("B","B", "C", "D", "J","A","E", "F", "G", "H", "I","I"),
  importance=(sample(1:4, 12, replace=T))) # Create a data frame for links with source, target, and importance
colnames(hoi)=colnames(links) # Set column names of hoi to match links
links=hoi
sources=hoi %>% distinct(source) %>% rename(source='label') # Get distinct sources and rename column to 'label'
destinations=hoi %>% distinct(target) %>% rename(target ='label') # Get distinct targets and rename column to 'label'
nodess <- full_join(sources, destinations, by = "label") # Merge sources and destinations into nodess

xc=x5[x5 %in% nodess$label]
xb=x3[x3 %in% nodess$label]
xl=x6[x6 %in% nodess$label]
x2[x2 =='17aOH-P4']='17a-OHP4' #Next time check these wrong names early on... :)
xs=x2[x2 %in% nodess$label]
nodess$label=c(xc,xb,xl,xs)



# Create a data frame 'nodes' with two columns: 'name' and 'carac'
# 'name' is taken from the first column of 'nodess'
# 'carac' is a categorical variable indicating the type of each node
nodes <- data.frame(name=nodess[,1], 
                    carac=( c(rep("Contaminants",length(xc)),rep("Bile Acids",length(xb)),rep("Lipids",length(xl)),rep("Steroids",length(xs))))) #range on kaikki +1

# Convert the data frame 'links' and 'nodes' into an igraph object
network <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 

# Load necessary libraries for color palettes and graphics
library(RColorBrewer)
library(ragg)

# Set the font to 'Calibri (Body)'
windowsFonts(A = windowsFont("Calibri (Body)"))

# Define a palette of 4 colors
coul  <- c('#B2BEB5','Green','Red','Orange') 

# Create a vector of colors corresponding to the 'carac' variable in the network
my_color <- coul[as.numeric(as.factor(V(network)$carac))]

# Save the plot as a JPEG file with specified dimensions and quality
jpeg('network.jpg', width=4, height=4.7, units="in", quality=100, pointsize=7, res=1000)

# Plot the network with specified parameters
plot(network, mode = "circle", vertex.color=my_color, vertex.size = 10,
     edge.arrow.size = 0.8, vertex.label.cex = 0.35, edge.width=as.numeric(E(network)$importance)*6.00 )

# Add a legend to the plot
legend("topright", legend=levels(as.factor(V(network)$carac)),
       col = coul, bty = "n", pch=20, pt.cex = 1.3, cex = 1.3, text.col=coul,
       horiz = FALSE, inset = c(0.65, 0.8))

# Close the JPEG device
dev.off()

# Read the saved JPEG image
eoh='network.jpg'
my_image <- image_read(eoh)

# Convert the image to SVG format and save it
my_svg <- image_convert(my_image, format="svg")
image_write(my_svg, paste(eoh,".svg"))

# Display the figure and convert it to PDF
knitr::include_graphics(eoh)
daiR::image_to_pdf(eoh, pdf_name=paste0(eoh,'.pdf'))
