# ELEC 321 Part 2 Assignment 1 Question 5
NUM_OF_SAMPLES <- 10000

# generate a number from 0 to 1 with uniform probability
pvecx <- runif(NUM_OF_SAMPLES, min = 0, max = 1)
pvecy <- runif(NUM_OF_SAMPLES, min = 0, max = 1)

# samples are calculated
xvec <- NULL
yvec <- NULL
for(idx in 1:NUM_OF_SAMPLES) {
  pvalx <- pvecx[idx]
  pvaly <- pvecy[idx]
  x <- (sqrt(1+8*pvalx)-1) / 2
  y <- (sqrt(1+8*pvaly)-1) / 2
  xvec <- c(xvec, x)
  yvec <- c(yvec, y)
}

##  Create cuts:
x_c <- cut(xvec, 20)
y_c <- cut(yvec, 20)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
x <- seq(0,1,length.out=100)
y <- seq(0,1,length.out=100)
z <- outer(0.5*x^2,0.5*y^2,`+`)
persp(x,y,z)

