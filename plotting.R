r = paste0(df[1,1], df[1,2])
m = paste(df[1,1], df[1,2])
a = data.frame(Hits = 1, Date = m, row.names = r)
for(i in 2:nrow(df)){
  tmp = paste0(df[i,1], df[i,2])
  m = paste(df[i, 1], df[i, 2])
  if(tmp %in% rownames(a)){
    a[tmp, 1] = a[tmp, 1] + 1
  }
  else{
    x = data.frame(Hits = 1, Date = m, row.names = tmp)
    a = rbind(a, x)
  }
}
d = 1:nrow(a)
a = cbind(a, d)
a$Date = as.yearmon(a$Date)
plot(a$Date, a$Hits, type = 'l', xaxt = 'n')
axis(1, a$Date, format(a$Date, "%b %Y"))
