# Para realizar la proyección de puntos en R^3 sobre R^2, creamos una función que compute el producto cruz.

vector.cross <- function(a, b) {
    if(length(a)!=3 || length(b)!=3){
        stop("Cross product is only defined for 3D vectors.");
    }
    i1 <- c(2,3,1)
    i2 <- c(3,1,2)
    return (a[i1]*b[i2] - a[i2]*b[i1])
}

# Hacemos una función que proyecte puntos sobre un plano que cruce por el origen con vector normal alpha.

plotprojection <- function(x,y,z, alpha = c(1,1,1)){
    alpha <- alpha / sqrt(sum(alpha^2))
    X <- cbind(x,y,z)
    beta <- c(0, 0, 1)
    v <- vector.cross(alpha, beta)
    s <- sqrt(sum(v^2))
    cc <- sum(alpha * beta)
    V <- matrix(
        data = c(
            0, -v[3], v[2],
            v[3], 0, -v[1],
            -v[2], v[1], 0
        ),
        nrow = 3, byrow = T
    )
    R <- diag(3) + V + (V %*% V) * (1 - cc) / s^2
    constante <- as.vector(X %*% alpha)
    X <- X -  t( t( matrix(rep(alpha, nrow(X)), nrow = nrow(X), byrow = TRUE) ) * constante )
    X <- t(R %*% t(X))
    xx <- X[,1]
    yy <- X[,2]

    return(rbind(xx,yy))
}

# La carnita del asunto
# Esta función realiza "pinturas de arena" sobre la esfera

sand_sphere <- function(n, N, noise = 1e-6, radius = 1, paleta = "black", alpha = 0.1){
    colores <- rep(paleta, length = n)
    a1 <- 2 * pi * runif(1)
    a2 <- 2 * pi * runif(1)
    a3 <- 2 * pi * runif(1)
    a4 <- 2 * pi * runif(1)
    accel1 <- numeric(N)
    accel2 <- numeric(N)
    accel3 <- numeric(N)
    accel4 <- numeric(N)
    x1 <- numeric(N)
    x2 <- numeric(N)
    y1 <- numeric(N)
    y2 <- numeric(N)
    z1 <- numeric(N)
    z2 <- numeric(N)
    plot.new()
    plot.window(xlim = c(-radius, radius), ylim = c(-radius, radius))
    for(i in 1:n){
        accel1 <- cumsum((1-2*runif(N)) * noise) + accel1[N]
        accel2 <- cumsum((1-2*runif(N)) * noise) + accel2[N]
        accel3 <- cumsum((1-2*runif(N)) * noise) + accel3[N]
        accel4 <- cumsum((1-2*runif(N)) * noise) + accel4[N]
        x1 <- radius * sin(a1 + cumsum(accel1)) * cos(a3 + cumsum(accel3))
        x2 <- radius * sin(a2 + cumsum(accel2)) * cos(a4 + cumsum(accel4))
        y1 <- radius * sin(a1 + cumsum(accel1)) * sin(a3 + cumsum(accel3))
        y2 <- radius * sin(a2 + cumsum(accel2)) * sin(a4 + cumsum(accel4))
        z1 <- radius * cos(a1 + cumsum(accel1))
        z2 <- radius * cos(a2 + cumsum(accel2))
        XX <- plotprojection(x1,y1,z1)
        YY <- plotprojection(x2,y2,z2)
        lines(c(rbind(XX[1,], YY[1,], rep(NA, N))), c(rbind(XX[2,],YY[2,],rep(NA, N))), lwd = 1, col = scales::alpha(colores[i], alpha = alpha))
        a1 <- (a1 + sum(accel1)) %% (2*pi)
        a2 <- (a2 + sum(accel2)) %% (2*pi)
        a3 <- (a3 + sum(accel3)) %% (2*pi)
        a4 <- (a4 + sum(accel4)) %% (2*pi)
    }
}

# Ejemplo para generar una "pintura de arena" sobre la esfera.

png("path_to_file/whatever.png", width = 2000, height = 2000, pointsize = 6)
par(mar = c(0,0,0,0)+0.1, bg = "honeydew")
sand_sphere(n = 10, N = 1e4, paleta = "black", alpha = 0.01, noise = 5e-6)
dev.off()

# Ejemplo para genera una colección de "pinturas de arena" sobre la esfera. Jugar al gusto

paleta2 <- rcartocolor::carto_pal(9, "Bold")
png("path_to_file/collection.png", width = 2000, height = 2000, pointsize = 6)
par(mar = c(0,0,0,0)+0.1, bg = "aliceblue", mfrow = c(3,3))
for(i in 1:9){
    sand_sphere(n = 10, N = 1e4, paleta = paleta2[i], alpha = 0.01, noise = 3e-6)
}
dev.off()
