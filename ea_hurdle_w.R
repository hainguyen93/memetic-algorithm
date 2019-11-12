#PLOT log T ~ log n for the (1+1)EA on Hurdle with w \in {2,3,4}

w.2 =  read.table(file = "ea_w_2_standard_rate", header = FALSE)
w.3 = read.table(file = "ea_w_3_standard_rate", header = FALSE)
w.4 = read.table(file = "ea_w_4_standard_rate", header = FALSE)

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

setEPS()
postscript(file="ea_hurdle_regression.pdf", width = 4, height = 4)
mar.default = c(5, 4, 4, 2) + 0.1
par(mar = mar.default+c(0,0,-3,-1))
require(plotrix)
require(latex2exp)

x = log(seq(10, 50, by=5))
y1 = log(rowMeans(proc(w.2, 10, 50, 5), na.rm = TRUE))
y2 = log(rowMeans(proc(w.3, 10, 50, 5), na.rm = TRUE))
y3= log(rowMeans(proc(w.4, 10, 50, 5), na.rm = TRUE))

y_min = floor(min(y1,y2,y3))
y_max = ceiling(max(y1,y2,y3))

plot(x, y1, col="red", ylim = c(y_min,20), pch=16,
     xlab = "Logarithm of problem size n",
     ylab = "Logarithm of average runtime",
     cex=1.5, cex.lab=1.2)

points(x, y2, col = "blue", pch=17)

points(x, y3, col = "green", pch=15)

m1 = model.fit(x,y1, "red")
m2 = model.fit(x,y2, "blue")
m3 = model.fit(x,y3, "green")

legend("topleft", pch=c(16,17,15), col=c("red", "blue", "green"),
       legend = c("2.093log(n)+0.828","3.188log(n)+0.341","4.274log(n)-0.052"))

dev.off()
