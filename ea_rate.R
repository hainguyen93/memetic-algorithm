ea.rate = read.table(file = "ea_hurdle_rate", header = FALSE)
ma.fils.rate = read.table(file = "ma_fils_hurdle_rate", header = FALSE)
ma.bils.rate = read.table(file = "ma_bils_hurdle_rate", header = FALSE)

process <- function(data, rate.from, rate.to, rate.by){
  res = c()
  for (i in seq(rate.from, rate.to, by = rate.by)){
    for (r in seq(1, nrow(data))){
      if (data[r,1]==i){
        res = c(res, data[r,3])
      }
    }
  }
  return(rowMeans(matrix(res, ncol = 100, byrow = TRUE), na.rm = TRUE))
}


setEPS()
postscript(file="ea_ma_rate_hurdle_combined.pdf", width = 8, height = 4)
mar.default = c(5, 4, 4, 2) + 0.1
par(mar = mar.default+c(-0.5,0.5,-3.5,-1.5))
par(mfrow=c(1,2))
require(plotrix)

#par(mar=c(5, 5, 4, 2) + 0.1)
x = seq(1, 7, by=1)
y1 = process(ea.rate, 1, 7, 1)
y2= process(ma.fils.rate, 1,7,1)
y3 = process(ma.bils.rate, 1,7,1)

plot(x, log(y1), pch=16, col="red", tck=0.03, type="o",
     xlab = "Mutation rate (/n)",
     ylab = expression(paste(log(bar(T)))), 
     cex=1.5, cex.lab=1.2, 
     ylim = c(4,14))


points(x, log(y2), pch=17, col='blue',cex=1.5, type = 'o')
points(x, log(y3), pch=15, col="green", cex=1.5, type = 'o')

legend('bottomright', legend = c('(1+1)EA','(1+1)MA-FILS','(1+1)MA-BILS'),
       col=c('red','blue','green'), pch=c(16,17,15), cex=0.8)

arrows(4,12.5,4,11, length=0.1, lwd=1.5)
arrows(2,10.3,2,8.8, length=0.1, lwd=1.5)
arrows(1,5,1,6.5, length=0.1,  lwd=1.5)

ea = read.table(file = 'ea_hurdle_time', header = FALSE)
ma.fils = read.table(file = 'ma_fils_hurdle_time', header = FALSE)
ma.bils = read.table(file = 'ma_bils_hurdle_time', header = FALSE)


proc.ea = rowMeans(proc(ea, 2,3,1), na.rm = TRUE)
proc.ma.fils = rowMeans(proc(ma.fils, 2, 10, 1), na.rm = TRUE)
proc.ma.bils = rowMeans(proc(ma.bils, 2, 10, 1), na.rm = TRUE)

y_max= max(proc.ea, proc.ma.bils, proc.ma.fils)
x = seq(2,10,1)

plot(x,proc.ma.fils, ylim = c(0,y_max), 
     col='blue', type='o', tck=0.03,
     xlab = 'w',
     ylab = expression(paste(bar(T))), pch=17, cex=1.5)
points(x,proc.ma.bils, col='green', type='o', pch=15, cex=1.5)
points(seq(2,3,1), proc.ea, col='red', type = 'o', pch=16, cex=1.5)

legend('topright', legend = c('(1+1)EA', '(1+1)MA-FILS','(1+1)MA-BILS'),
       col = c('red','blue','green'), pch=c(16,17,15),
       cex=0.8)

par(mfrow=c(1,1))
dev.off()
