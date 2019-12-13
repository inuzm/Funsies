#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# First argument should be the directory where images will be saved
# Second argument is the length of the video
# Third argument is the framerate of the video

output_directory <- args[1]
nimages <- as.numeric(args[2]) * as.numeric(args[3])

cat(sprintf("The number of frames is:%4d.\n", nimages))

# Tons of constants (play with them)
e1 <- 0.7; e2 <- 0.7; e3 <- 0.7; e4 <- 0.7; e5 <- 0.7; e6 <- 0.7; e7 <- 0.7; e8 <- 0.7; e9 <- 0.7; e10 <- 0.7; e11 <- 0.7; e12 <- 0.7; e13 <- 0.7; e14 <- 0.7; e15 <- 0.7
d1 <- -pi/10; d2 <- 0.1; d3 <- 0.3; d4 <- -0.17; d5 <- 0.21; d6 <- -exp(-2); d7 <- 0.5; d8 <- -0.74; d9 <- 0.3; d10 <- 0.21; d11 <- -0.2; d12 <- 0.3; d13 <- 0.1; d14 <- -exp(1)/10; d15 <- -0.3
c1 <- 1; c2 <- -1; c3 <- 1; c4 <- -1; c5 <- 1; c6 <- -1; c7 <- 1; c8 <- -1; c9 <- 1; c10 <- -1; c11 <- 1; c12 <- -1; c13 <- 1; c14 <- -1; c15 <- 1

# Function that produces level sets (play with it)
# Absolute values are used as R doesn't know what to do with things like (-3)^(1.7)
# Absolute values are not needed if the exponents remain constant and integer valued
chale1 <- function(x, y){
    (
        (ifelse(
            sinpi(x+y) > cospi(x-y),
            (sin(c1 * dnorm(2 * x + y, mean = d1, sd = e1)) + sin(x-y)) * (bitwXor(x, 1) + bitwXor(y/2, 1) * bitwXor(y, x)),
            (sin(c2 * dnorm(x - 2 * y, mean = d2, sd = e2)) + sin(x-y)) * (bitwXor(x/2, 5) + bitwXor(y, 5) * bitwXor(y, x))
        ) + c3 * dnorm(((x * y) %%  1.17) * sign(x * y), mean = d3, sd = e3) ) %% 27.32
    )
}

chale2 <- function(x, y){
    (x*y*cos(c4 * dnorm(-sinpi(x)^2 + y, mean = d4, sd = e4)) + 17*(c5 * dnorm(cospi(sinpi(x^2 + y^2)^3), mean = d5, sd = e5))^2 - 4 * (c6 * dnorm(x^2 - y^2, mean = d6, sd = e6))^3) %% 23.21
}

chale3 <- function(x, y){
    (
        (ifelse(
            x^2 > y^2,
            (sin(x+y) + sin(c7 * dnorm(cospi(x^2 * y^2) - sinpi(-x^2 + y^2), mean = d7, sd = e8))) * (bitwXor(x, 1) + bitwXor(y/2, 1) * bitwXor(y, x)),
            (sin(x+y) + sin(c8 * dnorm(exp(cospi(sinpi(-x^2 + y^2))), mean = d8, sd = e9))) * (bitwXor(x/2, 5) + bitwXor(y, 5) * bitwXor(y, x))
        ) + c9 * dnorm(((x^3 + y^3) %% 1.1) * sign(x^3 + y^3), mean = d9, sd = e9) ) %% 32.57
    )
}

chale4 <- function(x, y){
    (
        (ifelse(
            sinpi(x) > cospi(y),
            (sin(x+y) + sin(x-y)) * (bitwXor(c10 * dnorm(cospi(exp(x^3)) - sinpi(y)^3, mean = d10, sd = e10), 1) + bitwXor(c11 * dnorm(sinpi(x^3) * cos(y^3), mean = d11, sd = e11)/2, 1) * bitwXor(c11 * dnorm(sinpi(x^3) * cos(y^3), mean = d11, sd = e11), c10 * dnorm(cospi(exp(x^3)) - sinpi(y)^3, mean = d10, sd = e10) )),
            (sin(x+y) + sin(x-y)) * (bitwXor(x/2, 5) + bitwXor(y, 5) * bitwXor(y, x))
        ) + c12 * dnorm(((-x^3 + y^3) %% 2.3) * sign(-x^3 + y^3), mean = d12, sd = e12) ) %% 32.57
    )
}

chale5 <- function(x, y){
    (
        (ifelse(
            c13 * dnorm(sinpi(x) + cospi(y), mean = d13, sd = e13)  > c14 * dnorm(cospi(x) - sinpi(y), mean = d14, sd = e14),
            (sin(x+y) + sin(x-y)) * (bitwXor(x, 1) + bitwXor(y/2, 1) * bitwXor(y, x )),
            (sin(x+y) + sin(x-y)) * (bitwXor(x/2, 5) + bitwXor(y, 5) * bitwXor(y, x))
        ) + c15 * dnorm(sinpi(x + y) - cospi(x - y), mean = d15, sd = e15) ) %% 32.57
    )
}

# Simple function

chale_simple <- function(x, y){
    exp(0.1 * (exp(-x^2) * (cos(y)^2 + sin(y*x)) - 1))
}

# x-y values to be used in a grid where chale will be evaluated
x1 <- seq(from = 0, to = 10, length = 300)
y1 <- seq(from = 0, to = 10, length = 300)

# useful packages for colors (to play with)
require(ghibli, quietly = TRUE)
require(wesanderson, quietly = TRUE)
require(colorRamps, quietly = TRUE)
require(colorspace, quietly = TRUE)
require(viridis, quietly = TRUE)
require(rcartocolor, quietly = TRUE)
require(scico, quietly = TRUE)
require(scales, quietly = TRUE)

# Mixture variable
convex <- 0
convex_max <- 0.5
delta <- 1
xc <- 0
yc <- 0

# This loop does it all
# Print the contour set and the level set (as image) as png-files of 800 × 800 with 100 ppi resolution
# Then we modify the constants (ensuring the exponents to be greater than one which I think is useless right now)
# Print which iteration has been done
cat('Progress:\n')
pb <- txtProgressBar(max = nimages, style = 3)
for(k in 1:nimages){

    x <- delta * (x1 + xc)
    y <- delta * (y1 + yc)

    # %0nd ensures the file to have n leading zeroes (n should be the digits of the maximum number of iterations)
    png(paste0(output_directory, sprintf("/%04d_bw.png", k)), width = 600, height = 600, res = 75)
    par(mar = c(0,0,0,0) + 0.1)
    if(convex <= 0.2){
        contour(x = x, y = y, z = outer(x, y, chale_simple), drawlabels = FALSE, nlevels = 20, col = alpha("black", alpha = 1 - (5*convex)))
        contour(x = x, y = y, z = outer(x, y, chale1), drawlabels = FALSE, nlevels = 5, add = TRUE, col = alpha("black", alpha = (5*convex)))
    } else if (convex <= 0.4) {
        contour(x = x, y = y, z = outer(x, y, chale1), drawlabels = FALSE, nlevels = 5, col = alpha("black", alpha = 2 - (5 * convex)))
        contour(x = x, y = y, z = outer(x, y, chale2), drawlabels = FALSE, nlevels = 5, add = TRUE, col = alpha("black", alpha = (5 * convex) - 1))
    } else if (convex <= 0.6) {
        contour(x = x, y = y, z = outer(x, y, chale2), drawlabels = FALSE, nlevels = 5, col = alpha("black", alpha = 3 - (5 * convex)))
        contour(x = x, y = y, z = outer(x, y, chale3), drawlabels = FALSE, nlevels = 5, add = TRUE, col = alpha("black", alpha = (5 * convex) - 2))
    } else if (convex <= 0.8) {
        contour(x = x, y = y, z = outer(x, y, chale3), drawlabels = FALSE, nlevels = 5, col = alpha("black", alpha = 4 - (5 * convex)))
        contour(x = x, y = y, z = outer(x, y, chale4), drawlabels = FALSE, nlevels = 5, add = TRUE, col = alpha("black", alpha = (5 * convex) - 3))
    } else {
        contour(x = x, y = y, z = outer(x, y, chale4), drawlabels = FALSE, nlevels = 5, col = alpha("black", alpha = 5 - (5*convex)))
        contour(x = x, y = y, z = outer(x, y, chale5), drawlabels = FALSE, nlevels = 5, add = TRUE, col = alpha("black", alpha = (5*convex) - 4))
    }
    dev.off()

    png(paste0(output_directory, sprintf("/%04d_col.png", k)), width = 600, height = 600, res = 75)
    par(mar = c(0,0,0,0) + 0.1)
    if(convex <= 0.2){
        image(x = y, y = y, z = outer(x, y, chale_simple), col = viridis(n = 50, alpha = 1 - (5*convex)))
        image(x = y, y = y, z = outer(x, y, chale1), col = scico(n = 50, palette = "nuuk", alpha = (5 * convex)), add = TRUE)
    } else if (convex <= 0.4) {
        image(x = y, y = y, z = outer(x, y, chale1), col = scico(n = 50, palette = "nuuk", alpha = 2 - (5 * convex)))
        image(x = y, y = y, z = outer(x, y, chale2), col = scico(n = 50, palette = "acton", alpha = (5 * convex) - 1), add = TRUE)
    } else if (convex <= 0.6) {
        image(x = y, y = y, z = outer(x, y, chale2), col = scico(n = 50, palette = "acton", alpha = 3 - (5 * convex)))
        image(x = y, y = y, z = outer(x, y, chale3), col = alpha(wes_palette("Moonrise3", n = 50, type = "continuous"), alpha = (5 * convex) - 2), add = TRUE)
    } else if (convex <= 0.8) {
        image(x = y, y = y, z = outer(x, y, chale3), col = alpha(wes_palette("Moonrise3", n = 50, type = "continuous"), alpha = 4 - (5 * convex)))
        image(x = y, y = y, z = outer(x, y, chale4), col = alpha(ghibli_palette(name = "KikiMedium", n = 50, type = "continuous"), alpha = (5 * convex) - 3), add = TRUE)
    } else {
        image(x = y, y = y, z = outer(x, y, chale4), col = alpha(ghibli_palette(name = "KikiMedium", n = 50, type = "continuous"), alpha = 5 - (5 * convex)))
        image(x = y, y = y, z = outer(x, y, chale5), col = alpha(wes_palette("Darjeeling1", n = 50, type = "continuous"), alpha = (5 * convex) - 4), add = TRUE)
    }
    dev.off()

    # Comment whichever is not needed (I'm commenting the exponents variation to modify the absolute value)
    # The modification is done via steps distributed as gaussian random variables with variance 1e-3 (as the
    # value goes higher the difference between iterations will increase, probably 1e-3 is ok, depending
    # on the number of frames per second and whatever)
    e1 <- max(e1 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e2 <- max(e2 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e3 <- max(e3 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e4 <- max(e4 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e5 <- max(e5 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e6 <- max(e6 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e7 <- max(e7 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e8 <- max(e8 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e9 <- max(e9 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e10 <- max(e10 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e11 <- max(e11 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e12 <- max(e12 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e13 <- max(e13 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e14 <- max(e14 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    e15 <- max(e15 + rnorm(n = 1, sd = sqrt(1e-3)), 1e-6)
    c1 <- c1 + rnorm(n = 1, sd = sqrt(1e-3))
    c2 <- c2 + rnorm(n = 1, sd = sqrt(1e-3))
    c3 <- c3 + rnorm(n = 1, sd = sqrt(1e-3))
    c4 <- c4 + rnorm(n = 1, sd = sqrt(1e-3))
    c5 <- c5 + rnorm(n = 1, sd = sqrt(1e-3))
    c6 <- c6 + rnorm(n = 1, sd = sqrt(1e-3))
    c7 <- c7 + rnorm(n = 1, sd = sqrt(1e-3))
    c8 <- c8 + rnorm(n = 1, sd = sqrt(1e-3))
    c9 <- c9 + rnorm(n = 1, sd = sqrt(1e-3))
    c10 <- c10 + rnorm(n = 1, sd = sqrt(1e-3))
    c11 <- c11 + rnorm(n = 1, sd = sqrt(1e-3))
    c12 <- c12 + rnorm(n = 1, sd = sqrt(1e-3))
    c13 <- c13 + rnorm(n = 1, sd = sqrt(1e-3))
    c14 <- c14 + rnorm(n = 1, sd = sqrt(1e-3))
    c15 <- c15 + rnorm(n = 1, sd = sqrt(1e-3))
    d1 <- d1 + rnorm(n = 1, sd = sqrt(1e-3))
    d2 <- d2 + rnorm(n = 1, sd = sqrt(1e-3))
    d3 <- d3 + rnorm(n = 1, sd = sqrt(1e-3))
    d4 <- d4 + rnorm(n = 1, sd = sqrt(1e-3))
    d5 <- d5 + rnorm(n = 1, sd = sqrt(1e-3))
    d6 <- d6 + rnorm(n = 1, sd = sqrt(1e-3))
    d7 <- d7 + rnorm(n = 1, sd = sqrt(1e-3))
    d8 <- d8 + rnorm(n = 1, sd = sqrt(1e-3))
    d9 <- d9 + rnorm(n = 1, sd = sqrt(1e-3))
    d10 <- d10 + rnorm(n = 1, sd = sqrt(1e-3))
    d11 <- d11 + rnorm(n = 1, sd = sqrt(1e-3))
    d12 <- d12 + rnorm(n = 1, sd = sqrt(1e-3))
    d13 <- d13 + rnorm(n = 1, sd = sqrt(1e-3))
    d14 <- d14 + rnorm(n = 1, sd = sqrt(1e-3))
    d15 <- d15 + rnorm(n = 1, sd = sqrt(1e-3))
    delta <- delta * exp(rnorm(n = 1, sd = 1e-3))
    xc <- xc + rnorm(n = 1, sd = 1e-3)
    yc <- yc + rnorm(n = 1, sd = 1e-3)

    if(convex < 1){
        convex <- min(convex + 1 / nimages, 1)
    }

    setTxtProgressBar(
        pb,
        k
    )
}
close(pb)
