# (c) William Christopher Lang
# Indiana University Southeast
# 2016-09-05

setwd("/Users/chris/Documents/R code/xyplot_js")

# helper functions

# this rounds to one decimal point
niceformat <- function(x) {format(round(10*x)/10)}

# scene bounds: list with four components: xmin, xmax, ymin, ymax
# canvas bounds: list with three components: width, height, margin

# convert scene coords to canvas (canvas) coords
canvasx <- function(x, scene, canvas) {
    u <- canvas$margin + (canvas$width - 2*canvas$margin) * 
        (x - scene$xmin)/(scene$xmax - scene$xmin)
    return(niceformat(u))
}

canvasy <- function(y, scene, canvas) {
    v <- canvas$height - (canvas$margin + (canvas$height - 2*canvas$margin) * 
        (y - scene$ymin)/(scene$ymax - scene$ymin))
    return(niceformat(v))
}

# write numeric vector in javascript format
# x should be in canvas coords
write_numeric_vector <- function(filename, varname, x) {
    cat(file=filename, "            var ", varname, " = [", sep="", append=TRUE)
    cat(file=filename, x, sep=",", append=TRUE)
    cat(file=filename, "];\n", append=TRUE)
}

write_character_vector <- function(filename, varname, s) {
    cat(file=filename, "            var ", varname, " = [\"", sep="", append=TRUE)
    cat(file=filename, s, sep="\",\"", append=TRUE)
    cat(file=filename, "\"];\n", append=TRUE)
}

build_labels <- function(x, y) {
    s <- c()
    n <- length(x)
    for (i in 1:n) {
        s[i] <- paste( i, ": (",niceformat(x[i]),", ",niceformat(y[i]),")",sep="")
    }
    return(s)
}

# canvas coords
draw_rectangle_canvas <- function(filename, x1, y1, x2, y2) {
    cat(file=filename, "            ctx.strokeStyle = \"black\";\n", append=TRUE)
    cat(file=filename, "            ctx.moveTo(", x1, ",", y1, ");\n", sep="", append=TRUE)
    cat(file=filename, "            ctx.lineTo(", x2, ",", y1, ");\n", sep="", append=TRUE)
    cat(file=filename, "            ctx.lineTo(", x2, ",", y2, ");\n", sep="", append=TRUE)
    cat(file=filename, "            ctx.lineTo(", x1, ",", y2, ");\n", sep="", append=TRUE)
    cat(file=filename, "            ctx.lineTo(", x1, ",", y1, ");\n", sep="", append=TRUE)
    cat(file=filename, "            ctx.stroke();\n", append=TRUE)
}

# text is placed relative to center coord of text
draw_text <- function(filename, xcenter, ycenter, txt, px) {
    cat(file=filename, "            ctx.font = \"", px, "px Arial\";\n", sep="", append=TRUE)
    cat(file=filename, "            ctx.textAlign = \"center\";\n", append=TRUE)
    cat(file=filename, "            ctx.fillStyle = \"black\";\n", append=TRUE)
    cat(file=filename, "            ctx.fillText(\"", txt, "\",", xcenter, ",", ycenter, ");\n", sep="", append=TRUE)
}

# text is placed relative to center coord of text; text reads upwards
draw_text_vertical <- function(filename, xcenter, ycenter, txt, px) {
    cat(file=filename, "            ctx.font = \"", px, "px Arial\";\n", sep="", append=TRUE)
    cat(file=filename, "            ctx.save();\n", append=TRUE)
    cat(file=filename, "            ctx.translate(", xcenter, ",", ycenter, ");\n", sep="", append=TRUE);
    cat(file=filename, "            ctx.rotate(-Math.PI/2);\n", append=TRUE)
    cat(file=filename, "            ctx.textAlign = \"center\";\n", append=TRUE)
    cat(file=filename, "            ctx.fillText(\"", txt, "\",0,0);\n", sep="", append=TRUE);
    cat(file=filename, "            ctx.restore();\n", append=TRUE)
 }

###################################################################################
###################################################################################

# x and y numeric vectors of same length (assumed no NAs etc)
xyplotjs <- function(filename, x, y,
                     canvas.width=500, canvas.height=500,
                     canvas.margin=50,
                     x.label=NULL, y.label=NULL, plot.title=NULL, stylesheet=NULL) {
    xmin <- min(x)
    xmax <- max(x)
    ymin <- min(y)
    ymax <- max(y)
    
    scene <- list(xmin = min(x), xmax = max(x), 
                  ymin = min(y), ymax = max(y))
    canvas <- list(width = canvas.width, height=canvas.height, 
                   margin = canvas.margin)
    
    # write header and title for html page (file is overwritten without warning)
    cat(file=filename, "<!doctype html><html>\n", append=FALSE)
    cat(file=filename, "    <head>\n", append=TRUE)
    cat(file=filename,"        <title>",plot.title,"</title>\n", sep="", append=TRUE)
    if (!is.null(stylesheet)) {
        cat(file=filename, "        <link rel=\"", 
            stylesheet, "stylesheet\" href=\"styles.css\">\n", append=TRUE)
    }
    
    # write beginning of script
    cat(file=filename,"        <script>\n", append=TRUE)
    cat(file=filename,"        <!--\n", append=TRUE)
    cat(file=filename,"            var previous_state = false;\n", append=TRUE)
    cat(file=filename,"            var active_point = false;\n", append=TRUE)
    cat(file=filename,"            var selected_index = 0\n", append=TRUE)
    cat(file=filename,"            var r = 4;\n", append=TRUE)
    
    # write canvas dimensions
    cat(file=filename, "            var canvas_xmin = ", 0, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var canvas_xmax = ", canvas.width, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var canvas_ymin = ", 0, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var canvas_ymax = ", canvas.height, ";\n", sep="", append=TRUE)
    
    # write point and label arrays
    n <- length(x)
    cat(file=filename, "            var npoints = ", n, ";\n", sep="", append=TRUE)
    write_vector(filename, "x", canvasx(x, scene, canvas))
    write_vector(filename, "y", canvasy(y, scene, canvas))
    s <- build_labels(x, y)
    write_character_vector(filename, "labels", s)
    
    # write init() and first half of draw_stuff() functions
    zz <- readLines("body_first_half.html", n=-1)
    cat(file=filename, zz, append=TRUE, sep="\n")
    
    # write middle portion of draw_stuff(), for graph labels
    # write bounding rectangle for graph
    x1 <- canvas.margin - 4
    x2 <- canvas.width - canvas.margin + 4
    y1 <- canvas.margin - 4
    y2 <- canvas.height - canvas.margin + 4
    draw_rectangle_canvas( filename, x1, y1, x2, y2)

    # write graph main title, and x and y labels
    xcenter <- (x1 + x2)/2
    ycenter <- canvas.margin/2
    draw_text(filename, xcenter, ycenter, plot.title, 24)
    xmid <- (x1 + x2)/2
    ylab <- canvas.height - canvas.margin/2
    draw_text(filename, xmid, ylab, x.label, 20)
    xlab <- canvas.margin/2
    ymid <- (y1 + y2)/2
    draw_text_vertical(filename, xlab, ymid, y.label, 20)

    # write conclusion of draw_stuff() function, and mouse event function
    z4 <- readLines("body_second_half.html", n=-1)
    cat(file=filename, z4, append=TRUE, sep="\n")
    
    # write canvas declaration and closing tags
    # cat(file=filename, "        <h2>", main.title, "</h2>\n", sep="", append=TRUE)
    cat(file=filename, "<canvas width=", canvas.width, 
                       " height=", canvas.height, 
                       " id=\"myCanvas\"></canvas>\n", sep="", append=TRUE)
    cat(file=filename, "    </body>\n", append=TRUE)
    cat(file=filename, "</html>\n", append=TRUE)
}


# test the function
filename = "faithful_plot.html"
xyplotjs(filename, faithful$eruptions, faithful$waiting, 
         canvas.width=700, canvas.height=700, stylesheet="style.css",
         plot.title="Old Faithful", x.label="eruption duration (min)",
         y.label="waiting time (min)")

