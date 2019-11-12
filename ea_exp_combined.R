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
  grid(nx=NULL, ny = NULL, col = "gray48", lty = "dotted",
       lwd = 2, equilogs = TRUE)
  return(m)
}

power.model.fit <- function(x, y, w, line.col){
  ds = data.frame(x=x, y=y)
  str(ds)
  m = nls(y~a*x^w+b, data=ds, start = list(a=1,w=w,b=1), trace = T)
  a = round(coef(m)[1],3)
  b = round(coef(m)[2],3)
  lines(x, predict(m, list(x=x)), lty=1, col=line.col)
  return(m)
}

processed.data = process(data, 2, 7, 1)
mean.time = rowMeans(processed.data, na.rm = TRUE)

setEPS()
postscript(file="ea_exp_combined.eps", width = 8, height = 4)
mar.default = c(5, 4, 4, 2) + 0.1
par(mar = mar.default+c(0,1,-3,-1))
#par(mfrow=c(2,2))
require(plotrix)


m11 = power.model.fit(x, y1, 2, 'red')
m22 = power.model.fit(x, y2, 3, 'blue')
m33 = power.model.fit(x, y3, 4, 'green')

x = log(seq(10, 50, by=5))
y1 = log(rowMeans(proc(w.2, 10, 50, 5), na.rm = TRUE))
y2 = log(rowMeans(proc(w.3, 10, 50, 5), na.rm = TRUE))
y3= log(rowMeans(proc(w.4, 10, 50, 5), na.rm = TRUE))


##############################################################

y_min = floor(min(y1,y2,y3))
y_max = ceiling(max(y1,y2,y3))

plot(x, y1, col="red", ylim = c(y_min,20), pch=16,
     xlab = expression(paste(log(n))),
     ylab = expression(paste(log(T))),
     cex=1.5, cex.lab=1.2, tck=0.03)

points(x, y2, col = "blue", pch=17, cex=1.5)

points(x, y3, col = "green", pch=15, cex=1.5)

m1 = model.fit(x,y1, "red")
m2 = model.fit(x,y2, "blue")
m3 = model.fit(x,y3, "green")

text(3.2,8.5,expression(paste("2.093*log(n)+0.828")), srt=5.5)
text(3.2,11.5,expression(paste("3.188*log(n)+0.341")), srt=7)
text(3.2,14.5,expression(paste("4.274*log(n)-0.052")), srt=9)

legend("topleft", pch=c(16,17,15), col=c("red", "blue", "green"),
       legend = c("w=2","w=3","w=4"),
       cex = 0.8)

##################################################################


#par(mfrow=c(1,1))
dev.off()