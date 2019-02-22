require(ggplot2)

brownian.loop <- function(n = 1, Time = 1){
    vtime <- seq(from = 0, to = Time, by = Time / n)
    brown1 <- c(0, cumsum(rnorm(n = n, sd = sqrt(Time / n))))
    brown2 <- c(0, cumsum(rnorm(n = n, sd = sqrt(Time / n))))
    bridge1 <- brown1 - vtime * brown1[length(brown1)] / Time
    bridge2 <- brown2 - vtime * brown2[length(brown2)] / Time
    return(data.frame(t = vtime, x = bridge1, y = bridge2))
}

X <- brownian.loop(n = 1e4)
plot(X$x, X$y, type = 'l')
plot(X$t, X$x, type = 'l')
plot(X$t, X$y, type = 'l')

p <- ggplot() +
    theme_void() +
    #theme(plot.background = element_rect(fill = "#073642", colour = "#073642"))
    theme(plot.background = element_rect(fill = "gray20", colour = "gray20"))

for(i in 1:10){
    X <- brownian.loop(n = 1e4, Time = 1)
    p <- p + 
        geom_point(data = X, aes(x = x, y = y), 
                  alpha = 0.009, 
                  colour = "turquoise1")
}

plot(p)
