data = read.table(file = "ea_rate.csv", header = FALSE)
w.2 =  read.table(file = "ea_w_2_standard_rate", header = FALSE)
w.3 = read.table(file = "ea_w_3_standard_rate", header = FALSE)
w.4 = read.table(file = "ea_w_4_standard_rate", header = FALSE)

process <- function(data, rate.from, rate.to, rate.by){
  res = c()
  for (i in seq(rate.from, rate.to, by = rate.by)){
    for (r in seq(1, nrow(data))){
      if (data[r,1]==i){
        res = c(res, data[r,3])
      }
    }
  }
  return(matrix(res, ncol = 100, byrow = TRUE))
}

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

model.fit <- function(x,y, line.col){
  ds = data.frame(x=x, y=y)
  str(ds)
  m = nls(y~a*x+b, data=ds, start = list(a=.5, b=2), trace = T)
  a = round(coef(m)[1],3)
  b = round(coef(m)[2],3)
  lines(x, predict(m, list(x=x)), lty=1, col=line.col)
  return(m)
}

power.model.fit <- function(x, y, w, line.col){
  ds = data.frame(x=x, y=y)
  str(ds)
  m = nls(y~a*x^w, data=ds, start = list(a=1,w=w), trace = T)
  a = round(coef(m)[1],3)
  b = round(coef(m)[2],3)
  lines(x, predict(m, list(x=x)), lty=1, col=line.col)
  return(m)
}

processed.data = process(data, 2, 7, 1)
mean.time = rowMeans(processed.data, na.rm = TRUE)

setEPS()
postscript(file="ea_hurdle_runtime.eps", width = 8, height = 3)
mar.default = c(5, 4, 4, 2) + 0.1
par(mar = mar.default+c(-1,0.5,-3.5,-1.5))
par(mfrow=c(1,2))
require(plotrix)


x = seq(10,50,by=5)
y1 = rowMeans(proc(w.2, 10, 50, 5), na.rm=TRUE)
y2 = rowMeans(proc(w.3, 10, 50, 5), na.rm=TRUE)
y3 = rowMeans(proc(w.4, 10, 50, 5), na.rm=TRUE)
plot(x, y1, col='red',
     ylim = c(0, max(y1,y2,y3)),
     xlab = expression(paste(n)),
     ylab = expression(paste(bar(T))), 
     tck=0.03,
     cex=1.5, cex.lab=1.2, pch=16)
points(x, y2, col='blue', cex=1.5, pch=17)
points(x, y3, col='green', cex=1.5, pch=15)

m11 = power.model.fit(x, y1, 2, 'red')
m22 = power.model.fit(x, y2, 3, 'blue')
m33 = power.model.fit(x, y3, 4, 'green')

legend("topleft", pch=c(16,17,15), col=c("red", "blue", "green"),
       legend = c(expression(paste('w=2, ', 'Eq. ', 0.815*n^2.376)),
                  expression(paste('w=3, ', 'Eq. ', 4.181*n^2.888)),
                  expression(paste('w=4, ', 'Eq. ', 0.407*n^4.493))),
       cex = 0.8)


##############################################################
x = log(seq(10, 50, by=5))
y1 = log(rowMeans(proc(w.2, 10, 50, 5), na.rm = TRUE))
y2 = log(rowMeans(proc(w.3, 10, 50, 5), na.rm = TRUE))
y3= log(rowMeans(proc(w.4, 10, 50, 5), na.rm = TRUE))

y_min = floor(min(y1,y2,y3))
y_max = ceiling(max(y1,y2,y3))

plot(x, y1, col="red", ylim = c(y_min,20), pch=16,
     xlab = expression(paste(log(n))),
     ylab = expression(paste(log(bar(T)))),
     cex=1.5, cex.lab=1.2, tck=0.03)

points(x, y2, col = "blue", pch=17, cex=1.5)

points(x, y3, col = "green", pch=15, cex=1.5)

m1 = model.fit(x,y1, "red")
m2 = model.fit(x,y2, "blue")
m3 = model.fit(x,y3, "green")

legend("topleft", pch=c(16,17,15), col=c("red", "blue", "green"),
       legend = c("w=2, Eq. 2.093log(n)+0.828",
                  "w=3, Eq. 3.188log(n)+0.341",
                  "w=4, Eq. 4.274log(n)-0.052"),
       cex = 0.8)

par(mfrow=c(1,1))
dev.off()