#!/usr/bin/env Rscript

# Author: David M. Louis
# Contact: dmlouis@stanford.edui
# Contact: dmlouis87@gmail.com

############################### Libraries ###############################

library(ggplot2)

############################### Arguments ###############################

# expected format of input file is
cmd_args = commandArgs(trailingOnly = TRUE);
# midread_table_file = cmd_args[6] ???????????

input_file=read.csv(cmd_args, header=T)
# tableLength=length(midread_table[,1])
# print(cmd_args) #prints file name
print(paste0("input length: ", length(rownames(input_file))))
#midread_matrix=data.matrix(midread_table)

############################################################################################
# Analysis Functions
############################################################################################
spec_Fig <- function(spec_df){
  mid=50
  spec_plot <- ggplot(environment = environment()) +
    geom_point(data = spec_df, aes(
      x = spec_df[,4],
      y = spec_df[,5],
      fill = spec_df[,6],
      size=.data[['sum']]),
      pch=21, alpha=1, position = position_jitter(w = max(c(spec_df[,4], spec_df[,5]) + sd(c(spec_df[,4], spec_df[,5])))*.02, h = max(c(spec_df[,4], spec_df[,5]) + sd(c(spec_df[,4], spec_df[,5])))*.02)) +
    scale_size_continuous(range = c(1, 15), guide = "none") +
    scale_fill_gradient2(midpoint=mid, low="blue", mid="white",high="red", name = colnames(spec_df[6])) +
    # geom_point(data=samp ,aes(MSfreq, Healthyfreq, fill=MSspec), pch=21, size=samp$sum, alpha=1, position = position_jitter(w = 0.002, h = 0.002)) +
    # scale_color_gradient(low = 'green', high = 'black') +
    xlim(NA,max(c(spec_df[,4], spec_df[,5]) + sd(c(spec_df[,4], spec_df[,5])))) +
    ylim(NA,max(c(spec_df[,4], spec_df[,5]) + sd(c(spec_df[,4], spec_df[,5])))) +
    # ggtitle("MS CD4") +
    xlab(colnames(spec_df[4])) +
    ylab(colnames(spec_df[5])) +
    theme(panel.background = element_blank(), axis.line = element_line(color = 'black'),panel.border = element_rect(colour = "black", fill=NA, size=2),
          axis.text = element_text(size = 20))
  return(spec_plot)
}

correlation_figure <- function(spec_table) {
  # data correlations with fstat
  plot(spec_table[,2] ~ spec_table[,3], main="Data Correlation",  xlab=colnames(spec_table[2]), ylab = colnames(spec_table[3])) #shows randomness of data (maybe make it jitter)
  #getting fstatistic(?) of observed data
  # summary(lm(data_specificity[,2] ~ data_specificity[,3]))$fstat[1]
  mtext(paste0("Fstat: ", summary(lm(spec_table[,2] ~ spec_table[,3]))$fstat[1]))
  corr_plot <- recordPlot()
  return(corr_plot)
}
#Function ends

############################### Analysis  #################################

bubble_specificity <- spec_Fig(input_file)
# correlation_plot <- correlation_figure(data_specificity) #moved to outputs

############################### Outputs #################################
#creating directory
mainDir <- "specificity_analysis"
dir.create(file.path(mainDir), showWarnings = FALSE)
setwd(file.path(mainDir))

############## exporting data and figures
#bubble plot
png("specificity_plot.png",height=400,width=600)
bubble_specificity
dev.off()

#correlation plot
png("correlation_plot.png",height=400,width=600)
# correlation_plot
correlation_figure(input_file)
dev.off()

