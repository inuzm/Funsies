# Técnica para "pintar con arena" sobre un disco de un radio dado.
# Es la parte importante del código.

sand_disk <- function(n, N, noise = 1e-6, radius = 1, paleta = "black", alpha = 0.1){
    colores <- rep(paleta, length = n)
    a1 <- 2 * pi * runif(1)
    a2 <- 2 * pi * runif(1)
    accel1 <- numeric(N)
    accel2 <- numeric(N)
    x1 <- numeric(N)
    x2 <- numeric(N)
    y1 <- numeric(N)
    y2 <- numeric(N)
    plot.new()
    plot.window(xlim = c(-radius, radius), ylim = c(-radius, radius))
    for(i in 1:n){
        accel1 <- cumsum((1-2*runif(N)) * noise) + accel1[N]
        accel2 <- cumsum((1-2*runif(N)) * noise) + accel2[N]
        x1 <- radius * cos(a1 + cumsum(accel1))
        x2 <- radius * cos(a2 + cumsum(accel2))
        y1 <- radius * sin(a1 + cumsum(accel1))
        y2 <- radius * sin(a2 + cumsum(accel2))
        lines(c(rbind(x1, x2, rep(NA, N))), c(rbind(y1,y2,rep(NA, N))), lwd = 1, col = scales::alpha(colores[i], alpha = alpha))
        a1 <- (a1 + sum(accel1)) %% (2*pi)
        a2 <- (a2 + sum(accel2)) %% (2*pi)
    }
}

# Ejemplo 1

png("path_to_file/holi.png", width = 2000, height = 2000, pointsize = 6)
par(mar = c(0,0,0,0)+0.1, bg = "transparent")
desert_circles(n = 10, N = 1e4, paleta = "floralwhite", alpha = 0.01, noise = 1e-6)
dev.off()

# Ejemplo 2

paleta2 <- rcartocolor::carto_pal(9, "Prism")
png("path_to_file/jojojo.png", width = 2000, height = 2000, pointsize = 6)
par(mar = c(0,0,0,0)+0.1, bg = "aliceblue", mfrow = c(3,3))
for(i in 1:9){
    desert_circles(n = 10, N = 1e4, paleta = paleta2[i], alpha = 0.01, noise = 1e-6)
}
dev.off()
