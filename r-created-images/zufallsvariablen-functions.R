plot_pois_dens <- function(x, lambda, E=FALSE, ...){
    plot(x, dpois(x, lambda), col=1, pch=19, ylab="f(x)", ...)
    for(x_i in x){
        lines(c(x_i, x_i), c(0, dpois(x_i, lambda)), col=1, lwd=2)
    }
    if(E==TRUE){
        abline(v=lambda, lty=2)
        legend("topright", legend=c(paste0("X ~ Po(",lambda,")"), paste0("E(X) = ",lambda)), pch=19)
    }
}

plot_pois_dist <- function(x, lambda, q=FALSE, ...){
    plot(x, ppois(x, lambda), col=2, pch=19, ylab="F(x)", ylim=c(0,1), ...)
    for(x_i in x){
        F <- ppois(x_i, lambda)
        lines(c(x_i, x_i+1), c(F, F), col=2, lwd=2)
        lines(c(x_i+1, x_i+1), c(F, ppois(x_i+1, lambda)), col=2, lwd=2, lty=2)
    }
    ## F_X mit Quantil
    if(q != FALSE){
        lines(x=c(0, qpois(q, lambda)),
              y=c(q, q), lty=2)
        lines(x=c(qpois(q, lambda), qpois(q, lambda)),
              y=c(q, 0), lty=2)
    }
}

plot_gamma_dens <- function(x, shape, rate, E=FALSE, area=FALSE, text_y=0.1, ...){
    plot(x, dgamma(x, shape, rate), col=1, type="l", lwd=2, ylab="f(x)", ...)
    if(length(area)==2){
        from <- area[1]
        to <- area[2]
        x_sub <- x[x >=from & x<=to]
        polygon(x=c(x_sub[1], x_sub, x_sub[length(x_sub)]),
                y=c(0, dgamma(x_sub, shape, rate), 0),
                col=2)
        text(mean(area), text_y, round(pgamma(area[2],shape,rate)-pgamma(area[1],shape,rate), 3))
    }
    if(E==TRUE){
        abline(v=shape/rate, lty=2)
        legend("topright", legend=c(paste0("X~Gamma(", shape, ", ", rate, ")"), paste0("E(X) = ", round(shape/rate, 2))), pch=19)
    }
}

plot_gamma_dist <- function(x, shape, rate, q=FALSE, ...){
    plot(x, pgamma(x, shape, rate), col=2, type="l", lwd=2, ylab="F(x)", ...)
    if(q[1] != FALSE){
        for(q_i in q){
            lines(x=c(0, qgamma(q_i, shape, rate)),
                  y=c(q_i, q_i), lty=2)
            lines(x=c(qgamma(q_i, shape, rate), qgamma(q_i, shape, rate)),
                  y=c(q_i, 0), lty=2)
        }
    }
}
