## simulation of performance results #########################################################################################################
perf <- rbeta(1000, 5.9, 4.9)

# distribution of true performance
hist(perf, main = "Histogram of true performance", xlab = "performance", freq = F)
# failure rate at "healthy" 16 %
sum(perf <= 0.4)/ length(perf)

## Grading policy ##########################################################################################################################
grade <- function(x, change_point = 0.5,
                  height_of_change = 0.5,
                  strength_of_giving_points = 0.5,
                  strength_of_taking_points = 3){
  if(x < change_point){
    # concave at the start (it would be unfair not to give points)...
    return(height_of_change*(x^(1-strength_of_giving_points)/change_point^(1-strength_of_giving_points)))
  } else {
    # convex at the end (taking away points from a good solution)
    return((((1-change_point)^(-1)*(x-change_point))^strength_of_taking_points)*(1-height_of_change)+height_of_change)
  }
}

# plotting grading bias

## untils for plotting the function #####################################################################################################
plot_grading_policy <- function(func) {
  plot(seq(0,1,length.out = 100),
       sapply(seq(0,1,length.out = 100), func),
       type = "l",
       main = "Grading policy against perfect scoring",
       xlab = "true performance",
       ylab = "grade")
  lines(seq(0,1,length.out = 100), seq(0,1,length.out = 100), col = "red")
}

plot_grading_policy(function(x){grade(x, 0.5, 0.4, 0.8, 5)})

## assessing results ####################################################################################################################

grades <- sapply(perf, function(x){grade(x, 0.5, 0.4, 0.8, 5)})
hist(grades, nbins = 100)
# failrate shoots up to over 37%!
#
sum(grades <= 0.4)/length(grades)

