ea = read.table(file = 'ea_hurdle_time', header = FALSE)
ma.fils = read.table(file = 'ma_fils_hurdle_time', header = FALSE)
ma.bils = read.table(file = 'ma_bils_hurdle_time', header = FALSE)
ma.cros.fils = read.table(file = 'ma_crossover_fils', header=FALSE)
ma.cros.bils = read.table(file = 'ma_crossover_bils', header = FALSE)


proc <- function(data, n.from, n.to, n.by){
  res = c()
  for (i in seq(n.from, n.to, by=n.by)){
    for (r in seq(1,nrow(data))){
      if (data[r,1]==i){
        res = c(res, data[r,3])
      }
    }
  }
  return (matrix(res, ncol = 100, byrow = TRUE))
}

setEPS()
postscript(file="ma_crossover_hurdle.eps", width = 8, height = 4)
mar.default = c(5, 4, 4, 2) + 0.1
par(mar = mar.default+c(-0.5,0.5,-3.5,-1.5))
par(mfrow=c(1,2))
require(plotrix)

proc.ea = rowMeans(proc(ea, 2,3,1), na.rm = TRUE)
proc.ma.fils = rowMeans(proc(ma.fils, 2, 10, 1), na.rm = TRUE)
proc.ma.bils = rowMeans(proc(ma.bils, 2, 10, 1), na.rm = TRUE)
proc.ma.cros.fils = rowMeans(proc(ma.cros.fils,2,10,1), na.rm = TRUE)
proc.ma.cros.bils = rowMeans(proc(ma.cros.bils, 2, 10, 1), na.rm = TRUE)

y_max= max(proc.ma.bils, proc.ma.fils)
x = seq(2,10,1)

plot(x,proc.ma.fils, ylim = c(0,1.05*y_max), 
     col='blue', type='o', tck=0.03,
     xlab = 'w',
     ylab = expression(paste(bar(T))), pch=17, cex=1.5)
points(x,proc.ma.bils, col='green', type='o', pch=15, cex=1.5)
#points(seq(2,3,1), proc.ea, col='red', type = 'o', pch=16, cex=1.5)
points(x, proc.ma.cros.fils, col='purple', type = 'o',pch=17,cex=1.5)
points(x, proc.ma.cros.bils, col='red', type = 'o', pch=15, cex=1.5)

legend('topright',
       legend = c('(1+1)MA-FILS','(1+1)MA-BILS','(2+1)MA-FILS','(2+1)MA-BILS'),
       col = c('blue','green','purple','red'), 
       pch=c(17,15, 17,15))

########################################################################
 
fils.spdup = proc.ma.fils/proc.ma.cros.fils 
bils.spdup = proc.ma.bils/proc.ma.cros.bils

plot(x, bils.spdup, col='red', tck=0.03, xlab = 'w', ylab = 'speedup (times)', type = 'o', pch=15)
points(x, fils.spdup, col='blue', type = 'o', pch=17)
legend('topright', legend = c('MA-BILS','MA-FILS'), col = c('red','blue'),
       pch=c(15,17))


par(mfrow=c(1,1))
dev.off()
