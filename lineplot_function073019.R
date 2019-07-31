########function to create change line plot and LMER models########

#function creating high and low outlier threshold 
outlier_thresh_high <- function(x, thresh=3, data){
        mean(x, na.rm=T)+thresh*sd(x, na.rm=T)
}
outlier_thresh_low <- function(x, thresh=3){
        mean(x, na.rm=T)-thresh*sd(x, na.rm=T)
}
#calculate standard error of the mean and +/- mean for plotting
sem <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
low_sem <- function(x){return(mean(x, na.rm=T)-sem(x))}
high_sem <- function(x){return(mean(x, na.rm=T)+sem(x))}


###function to create line plots####
line_plot <- function( indepvar, depvar, color, data, xlab='', ylab='', title= '') {
        data<- select(data, record_id, time, color, indepvar, depvar)
        data2 <- filter(data, !is.na(color))
        plot_table <- ddply(data2, c(indepvar, color), here(summarise), mean = mean(get(depvar), na.rm=T), 
                           high_thresh = high_sem(get(depvar)), low_thresh= low_sem(get(depvar)))
       print(plot_table)
       plot <- ggplot(plot_table, aes(x = get(indepvar), y = mean, group = get(color), color = get(color))) + 
               geom_line(size = 2) + 
               geom_errorbar(position = 'identity', aes(ymax = high_thresh, ymin = low_thresh), width = .1) + 
               scale_fill_manual(values = c('skyblue3', 'palegreen3')) + 
               scale_color_manual(values = c('skyblue3', 'palegreen3')) + 
               xlab(xlab) + 
               ylab(ylab) + 
               labs(color=color)
       print(plot)
}

#test
long_dat3 <-filter(long_dat, time=='BL' | time=='ETX'|time=='FUP')
line_plot(depvar='ins_sev_total', indepvar= 'time', color='mci', data=long_dat3)

##function to run lmer models for simple effects
lmer_model_simple <- function (data, outcomes, time='time', moderator, idvar='record_id'  ) {
        #need package lme and lmerTest
        for (var in outcomes) {
        print(paste("MODEL", var, sep = ""))
            model<-summary(lmer(get(var) ~ as.factor(time)+
                                        as.factor(get(moderator)) + (1|record_id), data=data ))
            print(model)

        }
}
#test
long_dat6 <- filter(long_dat5, time=='BL'| time=='FUP')
lmer_model_simple(data=long_dat6[long_dat$completer=='comp'], outcomes= c('color_in3_time', 'color_is4_time'), moderator= 'mci', time='time')

### fuction to run lmer models for main effect 
lmer_model_main <- function (data, outcomes, time='time', moderator, idvar='record_id'  ) {
        #need package lme and lmerTest
        for (var in outcomes) {
                print(paste("MODEL", var, sep = " "))
                model<-summary(lmer(get(var) ~ as.factor(time)* as.factor(get(moderator)) + (1|record_id), data=data ))
                print(model)
                
        }
}

#test
lmer_model_main(data=long_dat6[long_dat$completer=='comp'], outcomes= c('color_in3_time', 'color_is4_time'), moderator= 'mci', time='time')


#anova main effects 
anova_model_main <- function (data, outcomes, time='time', moderator, idvar='record_id'  ) {
        #need package lme
        for (var in outcomes) {
                print(paste("MODEL", var, sep = " "))
                model<-lmer(get(var) ~ as.factor(time)* as.factor(get(moderator)) + (1|record_id), data=data )
                print(anova(model))
                
        }
}

#test
anova_model_main(data=long_dat6[long_dat$completer=='comp'], outcomes= c('ins_sev_total', 'psg_waso_min'), moderator= 'mci', time='time')
