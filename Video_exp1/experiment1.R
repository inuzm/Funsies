# Tons of constants (play with them)
e1 <- 3; e2 <- 3; e3 <- 1; e4 <- 1; e5 <- 2; e6 <- 2; e7 <- 1; e8 <- 1
c1 <- 1; c2 <- -pi; c3 <- 1; c4 <- 1; c5 <- 1; c6 <- 3; c7 <- 1; c8 <- -1

# Function that produces level sets (play with it)
# Absolute values are used as R doesn't know what to do with things like (-3)^(1.7)
# Absolute values are not needed if the exponents remain constant and integer valued
chale <- function(x, y){
    (
        (c1 * abs(x)^e1 + c2 *  abs(y)^e2) * 
            exp(sign( cos(c3 * abs(x)^e3) * sin(c4 * abs(y)^e4) )) * 
            sin(sqrt(c5* abs(x)^e5 + c6 * abs(y)^e6)) * 
            cos(c7 * abs(x)^e7 + c8 * abs(y)^e8) 
    ) %% 50.47
}

# x-y values to be used in a grid where chale will be evaluated
x <- seq(from = -7, to = 7, length = 300)
y <- seq(from = -7, to = 7, length = 300)

# useful packages for colors (to play with)
require(ghibli)
require(wesanderson)
require(colorRamps)
require(colorspace)

# This loop does it all
# Print the contour set and the level set (as image) as png-files of 800 × 800 with 100 ppi resolution
# Then we modify the constants (ensuring the exponents to be greater than one which I think is useless right now)
# Print which iteration has been done
for(k in 1:100){
    # %0nd ensures the file to have n leading zeroes (n should be the digits of the maximum number of iterations)
    png(sprintf("output-directory/%03d_bw.png", k), width = 800, height = 800, res = 100)
    par(mar = c(0,0,0,0) + 0.1)
    contour(x = x, y = y, z = outer(x, y, chale), drawlabels = FALSE, col = "black", nlevels = 4)
    dev.off()
    
    png(sprintf("output-directory/%03d_col.png", k), width = 800, height = 800, res = 100)
    par(mar = c(0,0,0,0) + 0.1)
    image(x = y, y = y, z = outer(x, y, chale), col = rainbow_hcl(n = 30))
    dev.off()
    
    # Comment whichever is not needed (I'm commenting the exponents variation to modify the absolute value)
    # The modification is done via steps distributed as gaussian random variables with variance 1e-2 (as the
    # value goes higher the difference between iterations will increase, probably 1e-3 is ok)
    e1 <- max(e1 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    e2 <- max(e2 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    e3 <- max(e3 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    e4 <- max(e4 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    e5 <- max(e5 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    e6 <- max(e6 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    e7 <- max(e7 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    e8 <- max(e8 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    c1 <- c1 + rnorm(n = 1, sd = sqrt(1e-2))
    c2 <- c2 + rnorm(n = 1, sd = sqrt(1e-2))
    c3 <- c3 + rnorm(n = 1, sd = sqrt(1e-2))
    c4 <- c4 + rnorm(n = 1, sd = sqrt(1e-2))
    c5 <- max(c5 + rnorm(n = 1, sd = sqrt(1e-2)), 0)
    c6 <- max(c6 + rnorm(n = 1, sd = sqrt(1e-2)), 0)
    c7 <- c7 + rnorm(n = 1, sd = sqrt(1e-2))
    c8 <- c8 + rnorm(n = 1, sd = sqrt(1e-2))
    
    cat(sprintf("iteración %3d\n", k))
}
