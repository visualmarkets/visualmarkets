#----------------#
# Source Helpers #
#----------------#

source("VisualMarketsTheme.R")
source("UtilityHelpers.R")

#---------------------------#
# Define Security Functions #
#---------------------------#

returns <- XtsDataFltr() %$% returns

cov(returns)
cor(returns)
