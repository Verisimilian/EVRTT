

first = df[1,]
last = df[nrow(df),]
ind = match(first[1,1], month.abb)
fyr = first[1,2]
lyr = last[1,2]
a = as.data.frame(matrix(nrow = 0, ncol = 2), col.names = c('Hits', 'Date'))
for(i in fyr:lyr){
  for(j in ind:12){
    r = paste0(month.abb[j], i)
    m = paste(month.abb[j], i)
    tmp = data.frame(Hits = 0, Date = m, row.names = r)
    a = rbind(a, tmp)
  }
  j = 1
}


for(i in 1:nrow(df)){
  tmp = paste0(df[i,1], df[i,2])
  a[tmp, 1] = a[tmp, 1] + 1
}

a$Date = as.yearmon(a$Date)
plot(a$Date, a$Hits, type = 'l', xaxt = 'n')
axis(1, a$Date, format(a$Date, "%b %Y"))
