#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# First argument should be the directory where images will be saved
# Second argument is the length of the video
# Third argument is the framerate of the video

output_directory <- args[1]
nimages <- as.numeric(args[2]) * as.numeric(args[3])

cat(sprintf("The number of frames is:%4d.\n", nimages))

# useful packages for colors (to play with)
require(ghibli, quietly = TRUE)
require(wesanderson, quietly = TRUE)
require(colorRamps, quietly = TRUE)
require(colorspace, quietly = TRUE)
require(viridis, quietly = TRUE)
require(ambient, quietly = TRUE)

# Mixture variables
convex <- 0
noiise <- 10
N_max <- nimages  / (noiise - 1)

grid <- long_grid(seq(-3, 3, length.out = 300), seq(-3, 3, length.out = 300))
grid$simplex <- fracture(gen_simplex, ridged, octaves = 8,
                         gain = spectral_gain(), x = grid$x, y = grid$y)
X <- matrix(grid$simplex, ncol = 300, byrow = TRUE)

grid <- long_grid(seq(-3, 3, length.out = 300), seq(-3, 3, length.out = 300))
grid$simplex <- fracture(gen_simplex, ridged, octaves = 8,
                         gain = spectral_gain(), x = grid$x, y = grid$y)
Y <- matrix(grid$simplex, ncol = 300, byrow = TRUE)

# This loop does it all
# Print the contour set and the level set (as image) as png-files of 800 × 800 with 100 ppi resolution
# Then we modify the constants (ensuring the exponents to be greater than one which I think is useless right now)
# Print which iteration has been done
cat('Progress:\n')
pb <- txtProgressBar(max = nimages, style = 3)
for(k in 1:nimages){

    # %0nd ensures the file to have n leading zeroes (n should be the digits of the maximum number of iterations)
    png(paste0(output_directory, sprintf("/%04d_col.png", k)), width = 600, height = 600, res = 75)
    par(mar = c(0,0,0,0) + 0.1)
    image(convex * Y + (1 - convex) * X, col = viridis(40))
    dev.off()

    convex <- min(convex + 1/N_max, 1)

    if(floor(k %% N_max) == 0){
        convex <- 0
        X <- Y
        grid <- long_grid(seq(-3, 3, length.out = 300), seq(-3, 3, length.out = 300))
        grid$simplex <- fracture(gen_simplex, ridged, octaves = 8,
                         gain = spectral_gain(), x = grid$x, y = grid$y)
        Y <- matrix(grid$simplex, ncol = 300, byrow = TRUE)
    }

    setTxtProgressBar(
        pb,
        k
    )
}
close(pb)
