# Log-Normal Mode Simulation

n <- 10

print(n)
x <- rnorm(n)

# sort x and combine in a dataframe for ggplot2
x <- sort(x)
df <- data.frame(x, exp(x))
colnames(df) <- c('x','exp')

# pdf of sampled points
p1 <- ggplot(data=df, aes(x=x)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth = 0.05) + xlab('X') + ylab('Y') +
  ggtitle('P.D.F. from sampled points')+theme(axis.text=element_text(size=12),
                                              axis.title=element_text(size=12), title = element_text(size=14))

# label the positive and negative values of x
df['pos'] <- df['x'] >0

# compute the Point Density measure
szPos <- length(df[df['pos']==1,'exp'])/(max(df[df['pos']==1,'exp']) - min(df[df['pos']==1,'exp']))
szNeg <- length(df[df['pos']==0,'exp'])/(max(df[df['pos']==0,'exp']) - min(df[df['pos']==0,'exp']))

# Transformation plot
ttl <- paste("N = ", toString(n), "; " ,"Density of points (Neg X, Pos X): ", toString(round(szNeg,2)), ", " , toString(round(szPos,2)))
p2 <- ggplot(data = df) + geom_point(aes(x = x, y= exp, colour = factor(pos), size=3), shape="o",show.legend = FALSE) + 
geom_line(aes(x=x, y=exp),linetype="dotted")+ xlab("X ~ N(0,1)") + ylab("Y= exp(X) ") +
ggtitle(ttl)+theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=12), title = element_text(size=14))

# Log-Normal distribution
ttl1 <- paste("P.D.F. of Log-Normal; ", "Mode = ", round(exp(-1),2))
p3 <- ggplot(df, aes(x=exp)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth = 0.05)+
geom_vline(xintercept = exp(-1), linetype="dotted", colour='red', size=0.7) + xlab("exp(X)") + ylab("p") + ggtitle(ttl1)+
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=12), title = element_text(size=14))
