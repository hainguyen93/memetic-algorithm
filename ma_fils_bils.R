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
  return(rowMeans(matrix(res, ncol = 100, byrow = TRUE), na.rm=TRUE))
}

x = seq(10,100,by=10)

s.fils = proc(small.fils, 10, 100, 10)
s.bils = proc(small.bils, 10, 100, 10)
l.fils = proc(large.fils, 10, 100, 10)
l.bils = proc(large.bils, 10, 100, 10)

setEPS()
postscript(file="ma_fils_bils.pdf", width = 4, height = 3)
mar.default = c(5, 4, 4, 2) + 0.1
par(mar = mar.default+c(0,0,-3,-1))
par(mfrow=c(1,1))
require(plotrix)

plot(x, s.fils, ylim = c(0,max(s.fils, l.fils, s.bils, l.bils)), pch=24, col="red",
     #sub = "small hurdle width",
    # main = "(1+1) MA on Hurdle", 
     xlab = "Problem instance size n",
     ylab = "Average runtime", cex=1.5, cex.lab=1.2,
     tck=0.03)
points(x,l.fils, pch=17, col="red", cex=1.5)

points(x, s.bils, pch=22, col="blue", cex=1.5, cex.lab=1.2,
     tck=0.03)

points(x,l.bils, pch=15, col="blue", cex=1.5)

legend("topleft", legend = c("(1+1)MA-FILS; small hurdle",
                             "(1+1)MA-FILS; large hurdle",
                             "(1+1)MA-BILS; small hurdle",
                             "(1+1)MA-BILS; large hurdle"), 
       col = c("red","red","blue","blue"), 
       pch=c(24,17,22,15),
       cex=0.8)

grid(nx=NULL, ny = NULL, col = "gray48", lty = "dotted",
     lwd = 2, equilogs = TRUE)


par(mfrow=c(1,1))
dev.off()