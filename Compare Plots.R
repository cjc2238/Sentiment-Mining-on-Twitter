source('~/GitHub/Sentiment Mining on Twitter/Hillary Twitter.R', echo=TRUE)
source('~/GitHub/Sentiment Mining on Twitter/DJT Twitter.R', echo=TRUE)

library(gridExtra)

both_plot <- ggplot() + 
  geom_line(aes(Date, Score, colour="Clinton"), size = 2, hrc2) +
  geom_line(aes(Date, Score, colour="Trump"), size = 2, djt2) + labs(title = "Mean Sentiment Score of
                                                                     Clinton/Trump Related Tweets")

grid.arrange(both_plot, hrc_plot, djt_plot, ncol=1)
