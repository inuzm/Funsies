require(pracma)
require(ggplot2)

brownian.xy <- function(n = 1, Time = 1, x, y){
    vtime <- seq(from = 0, to = Time, by = Time / n)
    brown <- c(0, cumsum(rnorm(n = n, sd = sqrt(Time / n))))
    bridge <- x + brown - vtime * (brown[length(brown)] + x - y) / Time
    return(bridge)
}

rare.brown <- function(n = 1e4, Time = 1){
    index1 <- c(floor(seq(1, 1e4, by = 1e4/1458))[-1], 1e4 + 1)
    index2 <- c(1, index1[-1458])
    X <- do.call(rbind.data.frame, fractalcurve(n = 5, which = "molecule"))
    X[1, ] <- X[1, ] + rnorm(n = 1459, sd = 1e-2)
    X[2, ] <- X[2, ] + rnorm(n = 1459, sd = 1e-2)
    result <- data.frame(x = numeric(1e4 + 1), y = numeric(1e4 + 1))
    for(i in 1:1458){
        result$x[index2[i]:index1[i]] <- brownian.xy(n = index1[i] - index2[i], 
                                                Time = Time * (index1[i] - index2[i])/1e4, 
                                                x = X[1, i], y = X[1, i+1])
        result$y[index2[i]:index1[i]] <- brownian.xy(n = index1[i] - index2[i], 
                                                Time = Time * (index1[i] - index2[i])/1e4, 
                                                x = X[2, i], y = X[2, i+1])
    }
    return(result)
}

rare.brown2 <- function(n = 1e5, Time = 1){
    index1 <- c(floor(seq(1, 1e5, by = 1e5/1458))[-1], 1e5 + 1)
    index2 <- c(1, index1[-1458])
    X <- do.call(rbind.data.frame, fractalcurve(n = 5, which = "molecule"))
    X[1, ] <- X[1, ] + rnorm(n = 1459, sd = 1e-2)
    X[2, ] <- X[2, ] + rnorm(n = 1459, sd = 1e-2)
    result <- data.frame(x = numeric(1e5 + 1), y = numeric(1e5 + 1))
    for(i in 1:1458){
        result$x[index2[i]:index1[i]] <- brownian.xy(n = index1[i] - index2[i], 
                                                     Time = Time * (index1[i] - index2[i])/1e4, 
                                                     x = X[1, i], y = X[1, i+1])
        result$y[index2[i]:index1[i]] <- brownian.xy(n = index1[i] - index2[i], 
                                                     Time = Time * (index1[i] - index2[i])/1e4, 
                                                     x = X[2, i], y = X[2, i+1])
    }
    return(result)
}

X <- rare.brown2(Time = 0.1)

ggplot(data = X, aes(x = x, y = y)) +
    geom_path(alpha = 1)

p <- ggplot() +
    theme_void() +
    theme(plot.background = element_rect(fill = "#fdf6e3", colour = "#fdf6e3"))

timey <- txtProgressBar(min = 0, max = 500, style = 3)
for(i in 1:300){
    X <- rare.brown(Time = 1)
    p <- p + 
        geom_path(data = X, aes(x = x, y = y), alpha = 0.009, colour = "darkorchid1")
    setTxtProgressBar(timey, i)
}
close(timey)

plot(p)
