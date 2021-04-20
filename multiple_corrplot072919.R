#function creating high and low outlier threshold 
outlier_thresh_high <- function(x, thresh=3, data){
        mean(x, na.rm=T)+thresh*sd(x, na.rm=T)
}
outlier_thresh_low <- function(x, thresh=3){
        mean(x, na.rm=T)-thresh*sd(x, na.rm=T)
}


library(gridExtra)
#####create function that creates multiple correlation plots when given vectors of variables ###
plot_corr_many <- function(xvars, yvars, color, idvar,  data,  plotTitle='', xlab='', ylab='',colorLab='', sd_value=3 ){
        #create new DF with indivated variables 
        new_df<- select(data, color, idvar, xvars, yvars)
        #set outlier thresholds for each variable 
        all_high_thresh_x<- numeric()
        all_low_thresh_x<- numeric()
        for (x in xvars) {
        all_high_thresh_x <-c(all_high_thresh_x, outlier_thresh_high(new_df[, x], thresh=sd_value))
        all_low_thresh_x<- c( all_low_thresh_x, outlier_thresh_low(new_df[, x], thresh=sd_value))
        new_df[paste0("outlier_", x)] <- ifelse(new_df[, x] > all_high_thresh_x, NA,
                                   ifelse(new_df[, x] < all_low_thresh_x, NA,1))
        }
        all_high_thresh_y<- numeric()
        all_low_thresh_y<- numeric()
        for (y in yvars) {
                all_high_thresh_y <-c(all_high_thresh_y, outlier_thresh_high(new_df[, y], thresh=sd_value))
                all_low_thresh_y<- c( all_low_thresh_y, outlier_thresh_low(new_df[, y], thresh=sd_value))
                new_df[paste0("outlier_", y)] <- ifelse(new_df[, y] > all_high_thresh_y, NA,
                                                        ifelse(new_df[, y] < all_low_thresh_y, NA,1))
        }
        for (x in xvars){
               for (y in yvars){
                g<- ggplot(new_df[!is.na(paste0("outlier_", x)) & !is.na(paste0("outlier_", y))], aes_string(x=x, y=y, color=color))
                x_plot <- g+ geom_point(alpha=1/2)+geom_smooth(method='lm')+
                        scale_color_manual(values=c('steelblue3','navyblue', 'turquoise4')) #+
                        #facet_wrap(~yvar, ncol=3)
                ggsave(paste0(x,".png"))
                #labs(title=plotTitle, color=colorLab)+ xlab(xlab)+ ylab(ylab)
                
              }
      }
        #y_plot <- x_plot+facet_wrap(xvars, ncol=3)
        
        #print(x_plot)
}
        #marrangeGrob(x_plot, nrow= 2, ncol=2)
       # p <- list()
       #plots <- list()
        #for (var in xvars){
                #for (x in length(xvars)){
                       # plots[[var]]<-c(plots, ggplot(new_df2, aes_string(x=var, y=yvars[1], color=color)))
                        #corr_y<- c+geom_point(alpha=1/2)+geom_smooth(method='lm')#+
                                #scale_color_manual(values=c('steelblue3','navyblue', 'turquoise4'))+
                                #facet_wrap(~yvars, ncol=3)+
                               #labs(title=plotTitle, color=colorLab)+ xlab(xlab)+ ylab(ylab)
                        
                         
        

      
        
        
        
        
        
        
        


##Test
long_dat2 <- filter(long_dat, time=='BL')
test<- select(long_dat2, record_id, ins_sev_total, waso_mean, demo_sex, color_in3_time, color_is4_time )

plot_corr_many(yvars = c('color_in3_time', 'color_is4_time'), xvars= c('ins_sev_total', 'waso_mean'), color='mci', idvar='record_id',  data=long_dat2, plotTitle = 'Plot',
              xlab='ISI', ylab='inhibition', colorLab='mci', sd_value = 2)

