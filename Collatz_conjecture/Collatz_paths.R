library(tidyverse)
is.even = function(x){x %% 2 == 0}
is.odd = function(x){x %% 2 != 0}
remove(df)
remove(df2)
collatz_fn = function(mx,path){
  for(i in 2:mx){
    value = i
    step = 1
    if(i == 2){df = data.frame(starting = value,
                                step = step,
                                new_val = value)}
    if(i != 2){new_df = data.frame(starting = value,
                                    step = step,
                                    new_val = value)
               df <- rbind(new_df,df)}
    repeat {
      if(is.even(value) == TRUE){
        value = value / 2
        step = step + 1
        df2 = data.frame(starting = i,
                         step = step,
                         new_val = value)
        df = rbind(df2, df)}
      if(is.odd(value) == TRUE & value != 1){
        value = (value * 3) + 1
        step = step + 1
        df2 = data.frame(starting = i,
                         step = step,
                         new_val = value)
        df = rbind(df2, df)}
      if(value == 1) break
    }
  }
df_fin = df %>%
  group_by(starting) %>%
  mutate(step2 = rev(step),
         `Starting number` = as.factor(starting)) 

collatz_plot <<- ggplot(data = df_fin, aes(x = step2, y = new_val, group = starting)) +
  geom_line(alpha = 1/mx) +
  geom_point(alpha = 1/mx) +
  geom_line(data = df_fin %>% filter(starting %in% path),
            aes(x = step2, y = new_val, color = `Starting number`, linetype = `Starting number`),
            lwd = 1.25,alpha=1/length(path)) +
  xlab("Step")+
  ylab("Value")+
  theme_classic();print(collatz_plot)
collatz_df <<- df_fin
}