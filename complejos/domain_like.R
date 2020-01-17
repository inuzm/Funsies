mascara_gris <- function(func, xmin, xmax, ymin, ymax, nx = 100, ny = 100, alpha = 1){
    xaux <- seq(from = xmin, to = xmax, length = nx)
    yaux <- seq(from = ymin, to = ymax, length = ny)
    Z <- expand.grid(x = xaux, y = yaux)
    Z2 <- func(Z$x + 1i * Z$y)
    #colores <- ifelse(
    #    log2(Mod(Z2)) >= 0,
    #    log2(Mod(Z2)) - floor(log2(Mod(Z2))),
    #    ceiling(log2(Mod(Z2))) - log2(Mod(Z2))
    #)
    colores <- log2(Mod(Z2)) - floor(log2(Mod(Z2)))
    #summary(colores)
    points(Z, pch = '.', col = scales::alpha(colour = grey( colores ), alpha = alpha))
}

mascara_argumento <- function(func, xmin, xmax, ymin, ymax, nx = 100, ny = 100, paleta = rcartocolor::carto_pal(n = 7, name = "Vivid"), alpha = 1){
    xaux <- seq(from = xmin, to = xmax, length = nx)
    yaux <- seq(from = ymin, to = ymax, length = ny)
    Z <- expand.grid(x = xaux, y = yaux)
    Z2 <- func(Z$x + 1i * Z$y)
    colores <- ifelse(
        Im(Z2) >= 0,
        Arg(Z2),
        Arg(Z2) + 2*pi
    ) / (2 * pi)
    summary(colores)
    points(
        Z, 
        pch = '.', 
        col = scales::alpha(
            colour = scales::gradient_n_pal(colours = paleta)(colores),
            alpha = alpha
        )
    )
}

xmin <- -2
xmax <- 1
ymin <- -2
ymax <- 2
nx <- 3000
ny <- 3000
f1 <- function(z){ (z-2i)^3 / 10 * (z+1+1i) / (z -3)^2 }

# vamos a crear una imagen en grises para calendario

png("~/Downloads/gris.png", width = 1600, height = 1600, res = 200)
plot.new()
par(mar = c(0,0,0,0)+0.1)
plot.window(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
mascara_gris(func = f1, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, nx = nx, ny = ny, alpha = 1)
dev.off()

# vamos a crear una imagen con colores para calendario

png("~/Downloads/color.png", width = 1600, height = 1600, res = 200)
plot.new()
par(mar = c(0,0,0,0)+0.1)
plot.window(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
mascara_argumento(func = f1, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, nx = nx, ny = ny, alpha = 0.2, paleta = rcartocolor::carto_pal(7, "Pastel"))
dev.off()

# vamos a combinar a las criaturas

png("~/Downloads/gris.png", width = 1600, height = 1600, res = 200)
plot.new()
par(mar = c(0,0,0,0)+0.1)
plot.window(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
mascara_gris(func = f1, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, nx = nx, ny = ny, alpha = 1)
mascara_argumento(func = f1, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, nx = nx, ny = ny, alpha = 0.2, paleta = rcartocolor::carto_pal(7, "Pastel"))
dev.off()