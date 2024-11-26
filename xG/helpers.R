# Utility variables and functions used to condense the code of the model itself and blog posts
library(ggplot2)
library(ggforce)
library(data.table)

# Actual PWHO team colors that should be used for plots
teamColors <<- c(`MTL` = "#862633",
								 `TOR` = "#307fe2", 
								 `NY`  = "#00b2a9", 
								 `BOS` = "#154734", 
								 `MIN` = "#A77BCA",
								 `OTT` = "#ffb81c"
)

# assignXGZone assignz xG zones for a discrete logaction xG model
assignXGZone <- function(x, y) {
	outVec = vector(mode="character", length=length(x))
	
	# netural zone and behind the net
	outVec[outVec == "" & x < 25.5] = "outside"
	outVec[outVec == "" & x > 89] = "outside"
	
	# three point zones
	outVec[outVec == "" & x < 54 & y < -22] = "right point"
	outVec[outVec == "" & x < 54 & y > 22] = "left point"
	outVec[outVec == "" & x < 54] = "center point"
	
	# do the easy center/wings (from top of the circle to the faceoff dot x values)
	outVec[outVec == "" & x < 69 & y < -22] = "right wing"
	outVec[outVec == "" & x < 69 & y > 22] = "left wing"
	outVec[outVec == "" & x < 69] = "center"
	
	# do the easy part of the wings (outside the faceoff dot y values)
	outVec[outVec == "" & y < -22] = "right wing"
	outVec[outVec == "" & y > 22] = "left wing"
	
	# what's left is in the center, between the faceoff dots.
	# we need to do some cross products to see if they're in the trapezoid of the home plate
	# https://math.stackexchange.com/questions/274712/calculate-on-which-side-of-a-straight-line-is-a-given-point-located
	
	# the right line is from (89,-4) to (69,-22)
	isRightRight = (x - 89)*(-22 - -4) - (y - -4)*(69-89)
	# the left line is from (89,4) to (69,22)
	isLeftLeft = (x - 89)*(22 - 4) - (y - 4)*(69-89)
	
	outVec[outVec == "" & isRightRight < 0] = "right wing"
	outVec[outVec == "" & isLeftLeft > 0] = "left wing"
	
	# Finally, we know what's left must be in home plate
	outVec[outVec == ""] = "center"
	
	return(outVec)
}

# makeXGModel creates a data.talbe that can be used as a PWHL that is continuous rather than discrete
makeXGModel <- function(origTable, byCols, maxDist, weightPower, matchMultiplier) {
	groupCols = paste(c("shotID","x","y",byCols))
	groupCols = groupCols[groupCols != ""]
	workingTable = copy(origTable)
	
	distanceFrom <- function(x, y, refX, refY, maxDist) {
		xDist = x - refX
		yDist = y - refY
		
		dist = sqrt(xDist^2 + yDist^2)
		dist[dist > maxDist] = maxDist
		
		return(dist)
	}
	
	CJ.table.1 <- function(X,Y) {
		setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
	}
	
	baselineData = origTable[scoringZone == TRUE,.(goalFreq = mean(isGoal), N=.N), by=groupCols]
	setnames(baselineData, old=groupCols, new=paste("base_", groupCols, sep=""))
	
	fullCombo = CJ.table.1(baselineData, origTable[scoringZone == TRUE,..groupCols])
	fullCombo = fullCombo[shotID != base_shotID,]
	fullCombo[,distance := distanceFrom(x, y, base_x, base_y, maxDist)]
	fullCombo[,weightMult := 1.0]
	
	iterCols = byCols[byCols != ""]
	if (length(iterCols) > 0) {
		for (cname in iterCols) {
			baseName = paste("base", cname, sep="_")
			#matches = nrow(fullCombo[get(cname) == get(baseName),])
			#print(c(cname, baseName, matches))
			fullCombo[get(cname) == get(baseName),weightMult := weightMult * matchMultiplier]
		}
	}
	
	fullCombo[,weight := weightMult * (1 - (distance / maxDist))^weightPower]
	xgValues = fullCombo[,.(xg = sum(goalFreq * weight * N) / sum(weight * N)), by=groupCols]

	return(xgValues)
}

# plot_nhl_rink is a function to easily plot data against a regulation NHL rink
# this code largely taken from: https://github.com/mrbilltran/the-win-column/blob/master/nhl_rink_plot.R
plot_nhl_rink <- function(size=c("half","whole"), theme="void") {
	if (size != "half" & size != "whole") {
		stop("NHL rink size must be 'half' or 'whole'")
	}
	
	if (theme != "void" & theme != "bw") {
		stop("Plot theme must be either 'void' or 'bw'")
	}
	
	# Setting up colour values
	NHL_red = "#C8102E" # Use #C8102E for original red in the rules, #FFE6EB for lighter hue
	NHL_blue = "#0033A0" # Use #0033A0 for original blue in the rules, #E6EFFF for lighter hue
	NHL_light_blue = "#41B6E6" # Use #41B6E6 for original crease blue in the rules, #E6F9FF for lighter hue
	
	# include everything for the half rink first (except the centre circle)
	g = ggplot() + coord_fixed() +
		# Faceoff circles
		geom_circle(aes(x0 = 69, y0 = 22, r = 15), colour = NHL_red, linewidth = 2 / 12) + # Top-Right
		geom_circle(aes(x0 = 69, y0 = -22, r = 15), colour = NHL_red, linewidth = 2 / 12) + # Bottom-Right
		
		# Centre line
		geom_tile(aes(x = 0, y = 0, width = 1, height = 85), fill = NHL_red) + # Centre line
		
		# Faceoff dots - Plot AFTER centre lines for centre ice circle to show up above
		geom_circle(aes(x0 = 0, y0 = 0, r = 6 / 12), colour = "black", fill = "black", linewidth = 1) + # Centre dot 
		geom_circle(aes(x0 = 69, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, linewidth = 0) + # Top-Right
		geom_circle(aes(x0 = 69, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, linewidth = 0) + # Bottom-Right
		geom_circle(aes(x0 = 20.5, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Top-Right
		geom_circle(aes(x0 = 20.5, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, linewidth = 0) + # Neutral Bottom-Right
		
		# Right goalie crease
		geom_tile(aes(x = 86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
		geom_arc_bar(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, linewidth = 1 / 12) + # manually adjusted arc
		geom_tile(aes(x = 86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
		geom_tile(aes(x = 86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
		geom_arc(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r = 6), colour = NHL_red, linewidth = 2 / 12) + # manually adjusted arc
		geom_tile(aes(x = 85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
		geom_tile(aes(x = 85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
		
		# Goalie nets placed as rectangles
		geom_tile(aes(x = 90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Right
		# Lines
		geom_tile(aes(x = 25.5, y = 0, width = 1, height = 85),  fill = NHL_blue) + # Right Blue line
		geom_tile(aes(x = 89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Right goal line
		
		# Borders as line segments - plotted last to cover up line ends, etc.
		geom_line(aes(x = c(0, 72), y = c(42.5, 42.5))) + # Top
		geom_line(aes(x = c(0, 72), y = c(-42.5, -42.5))) + # Bottom
		geom_line(aes(x = c(100, 100), y = c(-14.5, 14.5))) + # Right
		geom_arc(aes(x0 = 72, y0 = 14.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
		geom_arc(aes(x0 = 72, y0 = -14.5, start = pi, end =  pi / 2, r = 28)) # Bottom-Right
	
	# add the theme here before any returns
	if (theme == "void") {
		g = g + theme_void()
	} else {
		g = g + theme_bw()
	}
	
	# if we're only doing a half rink, return after adding a semicircle for the center faceoff
	if (size == "half") {
		g = g + geom_arc(aes(x0 = 0, y0 = 0, start = 0, end =  pi , r = 15), colour=NHL_red)
		return(g)
	}
	
	# if we get here, add all the other side
	g = g +
		# Faceoff circles
		geom_circle(aes(x0 = -69, y0 = 22, r = 15), colour = NHL_red, size = 2 / 12) + # Top-Left
		geom_circle(aes(x0 = -69, y0 = -22, r = 15), colour = NHL_red, size = 2 / 12) + # Bottom-Left
		
		# Faceoff dots - Plot AFTER centre lines for centre ice circle to show up above
		geom_circle(aes(x0 = -69, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Top-Left
		geom_circle(aes(x0 = -69, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Bottom-Left
		geom_circle(aes(x0 = -20.5, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Top-Left
		geom_circle(aes(x0 = -20.5, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Bottom-Left
		
		# Left goalie crease
		geom_tile(aes(x = -86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
		geom_arc_bar(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, size = 1 / 12) + # manually adjusted arc
		geom_tile(aes(x = -86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
		geom_tile(aes(x = -86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
		geom_arc(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r = 6), colour = NHL_red, size = 2 / 12) + # manually adjusted arc
		geom_tile(aes(x = -85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
		geom_tile(aes(x = -85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
		
		# Goalie nets placed as rectangles
		geom_tile(aes(x = -90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Left
		
		# Lines
		geom_tile(aes(x = -25.5, y = 0, width = 1, height = 85),  fill = NHL_blue) + # Left Blue line
		geom_tile(aes(x = -89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Left goal line
		
		# Borders as line segments - plotted last to cover up line ends, etc.
		geom_line(aes(x = c(-72, 0), y = c(42.5, 42.5))) + # Top
		geom_line(aes(x = c(-72, 0), y = c(-42.5, -42.5))) + # Bottom
		geom_line(aes(x = c(-100, -100), y = c(-14.5, 14.5))) + # Left
		geom_arc(aes(x0 = -72, y0 = 14.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
		geom_arc(aes(x0 = -72, y0 = -14.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
		
		# add the centre faceoff dot
		geom_circle(aes(x0 = 0, y0 = 0, r = 15), colour = NHL_red, size = 2 / 12)
	
	return(g)
}

