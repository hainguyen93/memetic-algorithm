small.fils = read.table(file = "ma_small_fils", header = FALSE)
small.bils =  read.table(file = "ma_small_bils", header = FALSE)
large.fils = read.table(file = "ma_large_fils", header = FALSE)
large.bils = read.table(file = "ma_large_bils", header = FALSE)

proc <- function(data, n.from, n.to, n.by){
  res = c()
  for (i in seq(n.from, n.to, by=n.by)){
    for (r in seq(1,nrow(data))){
      if (data[r,1]==i){
        res = c(res, data[r,3])
      }
    }
  }
  return(matrix(res, ncol = 100, byrow = TRUE))
}

x = seq(10,100,by=10)

s.fils =rowMeans(proc(small.fils, 10, 100, 10), na.rm = TRUE)
s.bils = rowMeans(proc(small.bils, 10, 100, 10), na.rm = TRUE)
l.fils = rowMeans(proc(large.fils, 10, 100, 10), na.rm = TRUE)
l.bils = rowMeans(proc(large.bils, 10, 100, 10), na.rm = TRUE)

setEPS()
postscript(file="ma_hurdle_scale.pdf", width = 8, height = 8)
mar.default = c(5, 4, 4, 2) + 0.1
par(mar = mar.default+c(-1,1,-2,-1))
par(mfrow=c(2,2))
require(plotrix)
require(graphics)

boxplot(V3/(V1**3/log(V1)**2)~V1, data = small.bils, 
        xlab = expression(paste(plain(n))),
        ylab=expression(paste(bar(T)/(n^3/w^2))), 
        col = "red", tck=-0.03,
        main=expression(paste('(1+1) MA-BILS, w=log(n)')))

boxplot(V3/(V1**3/log(V1)**2)~V1, data = small.fils, 
        xlab = 'n',
        ylab= expression(paste(bar(T)/(n^3/w^2))), 
        col = 'red',tck=0.03,
        main = expression(paste('(1+1) MA-FILS, w=log(n)')))

boxplot(V3/(V1**2) ~ V1, data = large.bils, 
        xlab = 'n',
        ylab=expression(paste(bar(T)/n^2)), 
        col = "red", tck=0.03,
        main=expression(paste('(1+1) MA-BILS, w=',sqrt(n))))

boxplot(V3/(V1**2) ~ V1, data = large.fils, 
        xlab = 'n',
        ylab=expression(paste(bar(T)/n^2)), 
        col = 'red',tck=0.03,
        main = expression(paste('(1+1) MA-FILS, w=', sqrt(n))))

par(mfrow=c(1,1))
dev.off()