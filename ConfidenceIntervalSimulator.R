library(tidyverse)
library(RColorBrewer)

"Usage:
      Inputs: n_sims = no. of experiments to run
      Outputs: List of Convergence Plot and Confidence Intervals plot
Example: obj <- simulator(100); obj[2] (or obj[1])
"

simulator <- function(n_sims){
    # true parameter value
    p <- 0.6
    
    # sample size
    # large sample size to converge to true proportion of 1 - alpha
    n <- 1000
    #n_sims <- 10
    # confidence intervals for 100 simulations
    cis <- data.frame(itr = numeric(n_sims), lb = numeric(n_sims), ub=numeric(n_sims), contains = numeric(n_sims), frac = numeric(n_sims))
    
    for(i in 1:n_sims)
    {
      # draw a sample 
      sample <- rbernoulli(n, p)
      # compute the estimate
      p_est <- sum(sample)/n
      # compute confidence intervals
      cis$itr[i] <- i
      # for higher precision use 1.959984
      cis$lb[i] <- p_est - 0.979982/sqrt(n)
      cis$ub[i] <-p_est + 0.979982/sqrt(n)
      cis$contains[i] <- p >= cis$lb[i] && p <= cis$ub[i]
      # log the convergence history of CIs
      cis$frac[i] <- sum(cis$contains)/i
    }
    
    # gather the dataframe for plotting
    cis <- as_tibble(cis)
    cis_gathered <- gather(cis, CI, Value, lb: ub) %>% select(itr, contains, Value, frac) %>% arrange(itr)
    cis_gathered$contains <- as.factor(cis_gathered$contains)
    
    # manual colors for stable mapping
    myColors <- brewer.pal(3,"Set2")
    names(myColors) <- c("1", "0", "-1")
  
    # plot confidence intervals
    pltCI <- ggplot(cis_gathered, aes(x=itr,y=Value, colour = contains)) + geom_point() + geom_line(aes(group=itr)) + geom_hline(yintercept = p, linetype='dashed', color='black') + scale_color_manual(name="contains", values = myColors)+scale_y_continuous('Probability', breaks=function(x) pretty(x, n=6)) + xlab("Number of experiments") + theme(legend.position = "none", axis.text = element_text(size=14), axis.title = element_text(size=14) ) 
    pltConvergenceHistory <- ggplot(cis_gathered, aes(x=itr, y=frac)) + geom_point()+scale_y_continuous('Probability', breaks=function(x) pretty(x, n=6)) + xlab("Number of experiments") + theme(legend.position = "none", axis.text = element_text(size=14), axis.title = element_text(size=14) ) 
    return (list(pltConvergenceHistory, pltCI))
}


