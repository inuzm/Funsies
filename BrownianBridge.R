require(ggplot2)

brownian.bridge <- function(n = 1, Time = 1){
    vtime <- seq(from = 0, to = Time, by = Time / n)
    brown <- c(0, cumsum(rnorm(n = n, sd = sqrt(Time / n))))
    bridge <- brown - vtime * brown[length(brown)] / Time
    return(data.frame(t = vtime, x = bridge))
}

X <- brownian.bridge(n = 1e4, Time = 10)
plot(X$t, X$x, type = 'l')

ggplot(data = X, aes(x = t, y = x)) +
    geom_point(alpha = 0.01, colour = "hot pink") +
    theme_void() +
    theme(plot.background = element_rect(fill = "#073642", colour = "#073642"))

p <- ggplot() +
    theme_void() +
    theme(plot.background = element_rect(fill = "#073642", colour = "#073642"))

for(i in 1:50){
    X <- brownian.bridge(n = 1e4, Time = 1)
    p <- p + 
        geom_point(data = X, aes(x = t, y = x), alpha = 0.009, colour = "hot pink")
}

plot(p)
