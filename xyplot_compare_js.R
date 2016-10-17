# (c) William Christopher Lang
# Indiana University Southeast
# 2016-09-05

setwd("/Users/chris/Documents/R code/xyplot_compare_js")

###################################################################################
###################################################################################

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

###################################################################################
###################################################################################

# x and y numeric vectors of same length (assumed no NAs etc)
xyplot_compare_js <- function(filename, 
                     x1, y1, x2, y2,
                     canvas.width=500, canvas.height=500,
                     canvas.margin=50,
                     x.label=NULL, y.label=NULL, plot.title=NULL, stylesheet=NULL) {
    xmin <- min(c(x1,x2))
    xmax <- max(c(x1,x2))
    ymin <- min(c(y1,y2))
    ymax <- max(c(y1,y2))
    
    scene <- list(xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax)
    canvas <- list(width = canvas.width, height=canvas.height, 
                   margin = canvas.margin)
    
    # write header and title for html page (file is overwritten without warning)
    zz <- readLines("plot_template_part1.html", n=-1)
    cat(file=filename, zz, append=FALSE, sep="\n")
    
    # write canvas dimensions
    cat(file=filename, "            var canvas_xmin = ", 0, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var canvas_xmax = ", canvas.width, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var canvas_ymin = ", 0, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var canvas_ymax = ", canvas.height, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var canvas_plot_margin = ", canvas.margin, ";\n", sep="", append=TRUE)
    
    # write title and axis labels
    cat(file=filename, "            var main_title = \"", plot.title, "\";\n", sep="", append=TRUE)
    cat(file=filename, "            var plot_label_x = \"", x.label, "\";\n", sep="", append=TRUE)
    cat(file=filename, "            var plot_label_y = \"", y.label, "\";\n", sep="", append=TRUE)
    
    # write point and label arrays
    n1 <- length(x1)
    n2 <- length(x2)
    cat(file=filename, "            var npoints1 = ", n1, ";\n", sep="", append=TRUE)
    cat(file=filename, "            var npoints2 = ", n2, ";\n", sep="", append=TRUE)
    write_numeric_vector(filename, "x1", canvasx(x1, scene, canvas))
    write_numeric_vector(filename, "y1", canvasy(y1, scene, canvas))
    write_numeric_vector(filename, "x2", canvasx(x2, scene, canvas))
    write_numeric_vector(filename, "y2", canvasy(y2, scene, canvas))
    s1 <- build_labels(x1, y1)
    s2 <- build_labels(x2, y2)
    write_character_vector(filename, "labels1", s1)
    write_character_vector(filename, "labels2", s2)
    
    # write (most of) the body of the html file
    zz <- readLines("plot_template_part2.html", n=-1)
    cat(file=filename, zz, append=TRUE, sep="\n")
    
    # write canvas declaration and closing tags
    cat(file=filename, "<canvas width=", canvas.width, 
                       " height=", canvas.height, 
                       " id=\"myCanvas\"></canvas>\n", sep="", append=TRUE)
    
    # write concluding portion of the html file
    zz <- readLines("plot_template_part3.html", n=-1)
    cat(file=filename, zz, append=TRUE, sep="\n")
}

###################################################################################
###################################################################################

# test the function
filename = "test_plot.html"
set.seed(123)
x1 = rnorm(1000,1)
y1 = rnorm(1000,1)
x2 = rnorm(1000,1) + 2
y2 = rnorm(1000,1) + 2
xyplot_compare_js(filename, x1, y1, x2, y2, 
         canvas.width=700, canvas.height=700, stylesheet="style.css",
         plot.title="Test Data", x.label="x",
         y.label="y")

