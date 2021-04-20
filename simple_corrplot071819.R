devtools::install_github("joepowers16/breadcrumbs")
library(fs)
library(ggplot2)
library(plyr)
library(dplyr)

dir_clean_data <- here::here('Data')

insv_dat<- path(dir_clean_data, 'INSV_long_randomized_addVars_07.03.19.csv')

long_dat<-read.csv(insv_dat)

#function creating high and low outlier threshold 
outlier_thresh_high <- function(x, thresh=3, data){
        mean(x, na.rm=T)+thresh*sd(x, na.rm=T)
}
outlier_thresh_low <- function(x, thresh=3){
        mean(x, na.rm=T)-thresh*sd(x, na.rm=T)
}

###create function that creates correlation plot, given two variables
plot_corr_one <- function(xvar, yvar, color, idvar,  data,  plotTitle='', xlab='', ylab='',colorLab='', sd_value=3 ){
        #col=add color to a variable 
        #creates a corr plot given one xvar and one y var
   if(!idvar %in% colnames(data)) stop(paste0('idvar not found in dataframe'))
        #create new DF with indivated variables 
        new_df<- select(data, xvar, yvar, color, idvar)
        #set outlier thresholds for each variable 
        x_high_thresh<- outlier_thresh_high(new_df[, xvar], thresh=sd_value)
        x_low_thresh<- outlier_thresh_low(new_df[, xvar], thresh=sd_value)
        y_high_thresh<- outlier_thresh_high(new_df[, yvar], thresh=sd_value)
        y_low_thresh<- outlier_thresh_low(new_df[, yvar], thresh=sd_value)
        #create new DF without outliers
        new_df2<- mutate(new_df, outlier= ifelse(new_df[xvar] > x_high_thresh, NA,
                                        ifelse(new_df[, xvar] < x_low_thresh, NA,
                                        ifelse(new_df[, yvar] > y_high_thresh, NA,
                                        ifelse(new_df[, yvar] < y_low_thresh, NA, 0)))))
        new_df3 <- subset(new_df2, outlier==0, select= c(xvar, yvar, idvar, color) )
        #create plot
        g <- ggplot(new_df3, aes_string(x=xvar, y=yvar, color=color))
        corr <- g+geom_point(alpha=1/2)+geom_smooth(method='lm')+
                scale_color_manual(values=c('steelblue3','navyblue', 'turquoise4'))+
                labs(title=plotTitle, color=colorLab)+ xlab(xlab)+ ylab(ylab)
        # add statistics for overall correlation
        r_stat_all <-cor(new_df3[, xvar], new_df3[, yvar], use='pairwise.complete.obs')
        #find r value within group 
        split_df <- ddply(new_df3, color, here(summarise), cor=cor(get(xvar), get(yvar), use='pairwise.complete.obs'))
        print(r_stat_all)
        print(split_df)
        print(corr)
}

#example for use 
long_dat2 <- filter(long_dat, time=='BL')
test<- select(long_dat2, record_id, ins_sev_total, demo_sex, color_in3_time )
plot_corr_one(yvar = 'color_in3_time', xvar='ins_sev_total', color='mci', idvar='record_id',  data=long_dat2, plotTitle = 'Plot',
              xlab='ISI', ylab='inhibition', colorLab='mci', sd_value = 3)