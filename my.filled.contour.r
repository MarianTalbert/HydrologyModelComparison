 my.filled.contour <-
function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1,
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE),
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors,
    col = color.palette(length(levels) - 1), plot.title, plot.axes,
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1,
    axes = TRUE, frame.plot = axes, ...)
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")
    
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
    
           #putting degrees on the axis and painting rainbows in the sky
              xp<-pretty(x)
              xaxlab<-paste(xp,"~{}^o")
              xaxlab<-parse(text=xaxlab)
              
               yp<-pretty(y)
              yaxlab<-paste(yp,"~{}^o")
              yaxlab<-parse(text=yaxlab)
            title(main = "", xlab = "", ylab = "")
            Axis(x,at=xp,labels=xaxlab, side = 1,cex.axis=1.3)
            Axis(y,at=yp,labels=yaxlab, side = 2,cex.axis=1.3)
             plot.axes
            #some thoughts on a distance scale but it changes with latitude so I'd need to think about how to handle that
            #mi<-rdist.earth(as.matrix(cbind(xp[1],yp[floor(length(yp)/2)])),as.matrix(cbind(xp[2],yp[floor(length(yp)/2)])))
            #tics<-pretty(c(0,mi),3)
            #segments(x0=mean(xp[1:2]),y0=yp[2],y1=yp[2],x1=((xp[2]+mean(xp[1:2]))/2)*(max(tics)/mi))
  
    if (frame.plot)
        box()
    if (missing(plot.title))
        title(...)
    else plot.title
    invisible()
}

