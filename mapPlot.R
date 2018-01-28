library("ggmap")
library(maptools)
library(maps)

visited <- c("Pennsylvania", "Austrailia", "California", "Florida", "Pennsylvania",
             "Pennsylvania","Pennsylvania","Pennsylvania","Pennsylvania","Pennsylvania","Austrailia",
             "Austrailia", "California")

df = as.data.frame(matrix(nrow = 0, ncol = 3), col.names = c("lon", "lat", "occur"))
for(i in visited){
  if(i %in% rownames(df)){
    df[i, 'occur'] = df[i, 'occur'] + 1
  }
  else{
    r = data.frame(lon = NA, lat = NA, occur = 1, row.names = i)
    df = rbind(df, r)
  }
}

ll.visited <- geocode(rownames(df))
df$lon = ll.visited$lon
df$lat = ll.visited$lat

prob = df$occur / length(visited)
df$Proportion = prob

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot(df) + mapWorld

mp <- mp + geom_jitter(aes(x=lon, y=lat, col= Proportion), size = 2.5) +
  scale_color_gradient(low="pink",high='red',na.value="yellow") +
  theme(axis.text = element_blank(), axis.title = element_blank())
mp

