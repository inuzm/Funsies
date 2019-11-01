#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# First argument should be the directory where images will be saved
# Second argument is the length of the video
# Third argument is the framerate of the video

output_directory <- args[1]
nimages <- as.numeric(args[2]) * as.numeric(args[3])

cat(sprintf("The number of frames is:%4d.\n", nimages))

# Tons of constants (play with them)
e1 <- 2; e2 <- 1; e3 <- 2; e4 <- 1; e5 <- 2; e6 <- 2; e7 <- 3; e8 <- 2
c1 <- 1; c2 <- -1; c3 <- 1; c4 <- -1; c5 <- 1; c6 <- -1; c7 <- 1; c8 <- -1

# Function that produces level sets (play with it)
# Absolute values are used as R doesn't know what to do with things like (-3)^(1.7)
# Absolute values are not needed if the exponents remain constant and integer valued
chale <- function(x, y){
    (
        sinpi(c3 * (c1 * x^e1 / (1 + c2*y^e2)))^e3 -
            cospi(c6 * (c4 * y^e4 / (1 + c5*x^e5)))^e6 +
            c8 * (c7 * (x - y))^e8
    ) %% 73.31
}

# x-y values to be used in a grid where chale will be evaluated
x <- seq(from = -10, to = 10, length = 300)
y <- seq(from = -10, to = 10, length = 300)

# useful packages for colors (to play with)
require(ghibli, quietly = TRUE)
require(wesanderson, quietly = TRUE)
require(colorRamps, quietly = TRUE)
require(colorspace, quietly = TRUE)
require(viridis, quietly = TRUE)

# This loop does it all
# Print the contour set and the level set (as image) as png-files of 800 × 800 with 100 ppi resolution
# Then we modify the constants (ensuring the exponents to be greater than one which I think is useless right now)
# Print which iteration has been done
cat('Progress:\n')
pb <- txtProgressBar(max = nimages, style = 3)
for(k in 1:nimages){
    # %0nd ensures the file to have n leading zeroes (n should be the digits of the maximum number of iterations)
    png(paste0(output_directory, sprintf("/%04d_bw.png", k)), width = 800, height = 800, res = 100)
    par(mar = c(0,0,0,0) + 0.1)
    contour(x = x, y = y, z = outer(x, y, chale), drawlabels = FALSE, col = "black", nlevels = 20)
    dev.off()

    png(paste0(output_directory, sprintf("/%04d_col.png", k)), width = 800, height = 800, res = 100)
    par(mar = c(0,0,0,0) + 0.1)
    image(x = y, y = y, z = outer(x, y, chale), col = wes_palette("Darjeeling2", n = 50, type = "continuous"))
    dev.off()

    # Comment whichever is not needed (I'm commenting the exponents variation to modify the absolute value)
    # The modification is done via steps distributed as gaussian random variables with variance 1e-3 (as the
    # value goes higher the difference between iterations will increase, probably 1e-3 is ok, depending
    # on the number of frames per second and whatever)
    #e1 <- max(e1 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    #e2 <- max(e2 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    #e3 <- max(e3 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    #e4 <- max(e4 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    #e5 <- max(e5 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    #e6 <- max(e6 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    #e7 <- max(e7 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    #e8 <- max(e8 + rnorm(n = 1, sd = sqrt(1e-2)), 1)
    c1 <- c1 + rnorm(n = 1, sd = sqrt(1e-3))
    c2 <- c2 + rnorm(n = 1, sd = sqrt(1e-3))
    c3 <- c3 + rnorm(n = 1, sd = sqrt(1e-3))
    c4 <- c4 + rnorm(n = 1, sd = sqrt(1e-3))
    c5 <- c5 + rnorm(n = 1, sd = sqrt(1e-3))
    c6 <- c6 + rnorm(n = 1, sd = sqrt(1e-3))
    c7 <- c7 + rnorm(n = 1, sd = sqrt(1e-3))
    c8 <- c8 + rnorm(n = 1, sd = sqrt(1e-3))

    setTxtProgressBar(
        pb, 
        k
    )
}
close(pb)