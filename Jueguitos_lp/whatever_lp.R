# 
# 

random_p_norm <- function(n = 1, p = 2){
    x <- rbeta(n = n, shape1 = p, shape2 = 1)
    y <- (1 - x^p)^(1/p)
    return(
        data.frame(
            x = x * (2 * (runif(n) > 0.5) - 1),
            y = y * (2 * (runif(n) > 0.5) - 1)
        )
    )
}

random_p_norm2 <- function(n = 1, p = 2){
    X <- data.frame(x = numeric(n), y = numeric(n))
    for(i in 1:n){
        x <- rnorm(2)
        while( sum( abs(x)^p ) < (1e-3)^p ){
            x <- rnorm(2)
        }
        X[i,] <- x / sum( abs(x)^p )^(1/p)
    }
    return(X)
}

#
#

consecutive_convex <- function(n, p, bg = rgb(72,50,77,maxColorValue = 255), col = rgb(254,126,79,maxColorValue = 255)){
    X <- random_p_norm2(n, p)
    par(mar = c(0,0,0,0) + 0.1, bg = bg)
    plot.new()
    plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
    lines(X, col = scales::alpha(colour = col, 0.1))
    lines(X[c(n,1), ], col = scales::alpha(colour = col, 0.1))
}

#
#

nonconvex <- function(n, p, bg = rgb(72,50,77,maxColorValue = 255), col = rgb(254,126,79,maxColorValue = 255)){
    X <- random_p_norm(n, p)
    Y <- matrix(nrow = 2, ncol = 2)
    reflection <- floor(2 * runif(n)) + 1
    par(mar = c(0,0,0,0) + 0.1, bg = bg)
    plot.new()
    plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
    for(i in 1:n){
        Y[1,] = Y[2,] <- as.numeric(X[i,])
        Y[2,reflection[i]] <- -Y[2,reflection[i]]
        lines(Y, col = scales::alpha(colour = col, 0.1))
    }
}

convex <- function(n, p, bg = rgb(72,50,77,maxColorValue = 255), col = rgb(254,126,79,maxColorValue = 255)){
    X <- random_p_norm(n, p)
    Y <- matrix(nrow = 2, ncol = 2)
    reflection <- floor(2 * runif(n)) + 1
    par(mar = c(0,0,0,0) + 0.1, bg = bg)
    plot.new()
    plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
    for(i in 1:n){
        Y[1,] = Y[2,] <- as.numeric(X[i,])
        Y[2,reflection[i]] <- -Y[2,reflection[i]]
        lines(Y, col = scales::alpha(colour = col, 0.1))
    }
}

#
#

consecutive_nonconvex <- function(n, p, bg = rgb(72,50,77,maxColorValue = 255), col = rgb(254,126,79,maxColorValue = 255)){
    X <- random_p_norm(n, p)
    par(mar = c(0,0,0,0) + 0.1, bg = bg)
    plot.new()
    plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
    for(i in 1:(n-1)){
        while ( sum( abs( (X[i,] + X[i+1,]) / 2 )^p) > 1 ) {
            X[i+1,1] <- rbeta(n = 1, shape1 = p, shape2 = 1) * (2 * (runif(1) > 0.5) - 1)
            X[i+1,2] <- (1 - abs(X[i+1,1])^p)^(1/p) * (2 * (runif(1) > 0.5) - 1)
        }
        lines(X[c(i,i+1),], col = scales::alpha(colour = col, 0.1))
    }
}

#
#

convex_multiple <- function(n = 1e3, p = 1, bg = rgb(72,50,77,maxColorValue = 255), col = rgb(254,126,79,maxColorValue = 255)){
    if( length(p) <= ceiling(sqrt(length(p))) * floor(sqrt(length(p))) ){
        windows <- c(ceiling(sqrt(length(p))), floor(sqrt(length(p))))
    } else {
        windows <- c(ceiling(sqrt(length(p))), ceiling(sqrt(length(p))))
    }
    par(
        mfrow = windows, 
        mar = c(0,0,0,0) + 0.1,
        bg = bg
    )
    k <- 1
    for(i in p){
        X <- random_p_norm2(n, i)
        plot.new()
        plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
        lines(X, col = scales::alpha(colour = col[k %% length(col) + 1], 0.1))
        lines(X[c(n,1), ], col = scales::alpha(colour = col[k %% length(col) + 1], 0.1))
        k <- k + 1
    }
}

multiple <- function(n = 1e3, p = 1, bg = rgb(72,50,77,maxColorValue = 255), col = rgb(254,126,79,maxColorValue = 255)){
    if( length(p) <= ceiling(sqrt(length(p))) * floor(sqrt(length(p))) ){
        windows <- c(ceiling(sqrt(length(p))), floor(sqrt(length(p))))
    } else {
        windows <- c(ceiling(sqrt(length(p))), ceiling(sqrt(length(p))))
    }
    par(
        mfrow = windows, 
        mar = c(0,0,0,0) + 0.1,
        bg = bg
    )
    k <- 1
    for(i in p){
        X <- random_p_norm2(n, i)
        plot.new()
        plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
        Y <- matrix(nrow = 2, ncol = 2)
        reflection <- floor(2 * runif(n)) + 1
        for(j in 1:n){
            Y[1,] = Y[2,] <- as.numeric(X[j,])
            Y[2,reflection[j]] <- -Y[2,reflection[j]]
            lines(Y, col = scales::alpha(colour = col[k %% length(col) + 1], 0.1))
        }
        k <- k + 1
    }
}

png("~/path-to-file.png", width = 1800, height = 1800, res = 200)
multiple(
    n = 1e3, 
    p = c(1/3, 1/2, 1, 1.5, 1.8, 2, 2.5, 10, 100),
    col = rcartocolor::carto_pal(n = 7, name = "Burg"),
    bg = "transparent"
)
dev.off()
