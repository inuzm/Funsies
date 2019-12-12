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
require(rcartocolor, quietly = TRUE)
require(scico, quietly = TRUE)
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)

# Mixture variables
convex <- 0
noiise <- 10
N_max <- nimages  / (noiise - 1)

X <- long_grid(x = seq(-5, 5, length.out = 700), 
          y = seq(-5, 5, length.out = 700)) %>% 
    mutate(
        x1 = x + gen_simplex(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2 - gen_cubic(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2, 
        y1 = y - gen_simplex(x, y,seed = rpois(n = 1, lambda = 5) + 1) / 2 + gen_cubic(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2,
        worley = gen_worley(x, y, value = 'distance', frequency = 0.5, seed = rpois(n = 1, lambda = 5) + 1),
        worley_frac = fracture(gen_worley, fbm, octaves = 7, x = x, y = y, seed = rpois(n = 1, lambda = 5) + 1),
        full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1))
    )
X <- matrix(X$full, ncol = 700, byrow = TRUE)

Y <- long_grid(x = seq(-5, 5, length.out = 700), 
          y = seq(-5, 5, length.out = 700)) %>% 
    mutate(
        x1 = x + gen_simplex(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2 - gen_cubic(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2, 
        y1 = y - gen_simplex(x, y,seed = rpois(n = 1, lambda = 5) + 1) / 2 + gen_cubic(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2,
        worley = gen_worley(x, y, value = 'distance', frequency = 0.5, seed = rpois(n = 1, lambda = 5) + 1),
        worley_frac = fracture(gen_worley, fbm, octaves = 7, x = x, y = y, seed = rpois(n = 1, lambda = 5) + 1),
        full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1))
    )
Y <- matrix(Y$full, ncol = 700, byrow = TRUE)

colores <- scico(n = 50, palette = "buda")

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
    image(convex * Y + (1 - convex) * X, col = colores)
    dev.off()

    convex <- min(convex + 1/N_max, 1)

    if(floor(k %% N_max) == 0){
        convex <- 0
        X <- Y
        Y <- long_grid(x = seq(-5, 5, length.out = 700), 
          y = seq(-5, 5, length.out = 700)) %>% 
    mutate(
        x1 = x + gen_simplex(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2 - gen_cubic(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2, 
        y1 = y - gen_simplex(x, y,seed = rpois(n = 1, lambda = 5) + 1) / 2 + gen_cubic(x, y, seed = rpois(n = 1, lambda = 5) + 1) / 2,
        worley = gen_worley(x, y, value = 'distance', frequency = 0.5, seed = rpois(n = 1, lambda = 5) + 1),
        worley_frac = fracture(gen_worley, fbm, octaves = 7, x = x, y = y, seed = rpois(n = 1, lambda = 5) + 1),
        full = blend(normalise(worley), normalise(worley_frac), gen_spheres(x1, y1))
    )
        Y <- matrix(Y$full, ncol = 700, byrow = TRUE)
    }

    setTxtProgressBar(
        pb,
        k
    )
}
close(pb)
