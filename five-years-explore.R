library(stringr)
library(ggplot2)
library(zeallot)
library(gridExtra)

source('code/five_years/five_years-utils.R')

outcomes <- c(
  '5Y. ODI - Score (%)', '5Y. SRS22 - SRS Subtotal score', '5Y. SF36 - PCS', '5Y. SF36 - MCS'
)

c(data_set_0, predictive_vars, .) %<-% get_data(clean=FALSE)
data_set <- data_set_0 %>% copy

demographics <- read_yaml('code/five_years/demographic.yml')$demographic

outcomes %>% 
  lapply(. %>% str_replace_all('5', '2')) ->
  outcomes_2y
names(outcomes_2y) <- outcomes

outcomes %>% 
  lapply(. %>% str_replace_all('5', '6')) ->
  outcomes_6y
names(outcomes_6y) <- outcomes

outcomes %>% 
  sapply(. %>% get_base_outcome(first_visit=TRUE)) ->
  base_outcomes 

for( outcome in names(outcomes_6y)){
  outcome_6y <- outcomes_6y[[outcome]]
  data_set %<>%
    .[is.na(get(outcome)), c(outcome_6y) := get(outcome)]
}

outcome_2y <- outcomes_2y[[1]]
outcome <- outcomes[[1]]


study_vars <- c(outcome, outcomes_2y %>% unlist, demographics, base_outcomes) %>% unique
data_set %<>% 
  .[Study=='Op'] %>% 
  .[, .SD, .SDcols=study_vars] %>% 
  .[, info_2y := !is.na(get(outcome_2y))] %>% 
  .[, info_5y := !is.na(get(outcome))]


data_set[, .N]
data_set[info_2y == TRUE, .N]
data_set[info_5y == TRUE, .N]




data_set[, 
  .SD, 
  .SDcols=c(get_base_outcome(outcome, first_visit=TRUE), outcomes_2y[[outcome]], outcome)
] %>% 
  melt() %>% 
  ggplot(aes(variable, value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=1))




demo_plots <- list()
for(demo_var in demographics){
  if( class(data_set[, get(demo_var)]) == 'numeric' ){
    data_set %>% 
      copy() %>% 
      setnames(demo_var, 'demo_var') %>% 
      setnames(outcome, 'outcome') %>% 
      setnames(outcomes_2y[[outcome]], 'outcome_2y') %>% 
      .[, .(demo_var, outcome, outcome_2y)] %>% 
      melt(id.vars='demo_var') %>% 
      ggplot(aes(demo_var, value, color=variable)) +
      geom_point(alpha=0.1) + 
      xlab(demo_var) +
      ylab('score') + 
      scale_color_discrete(labels = c(outcome, outcomes_2y[[outcome]])) +
      geom_smooth(method='lm') ->
      demo_plots[[demo_var]]
  }else{
    data_set %>% 
      copy() %>% 
      setnames(demo_var, 'demo_var') %>% 
      setnames(outcome, 'outcome') %>% 
      setnames(outcomes_2y[[outcome]], 'outcome_2y') %>% 
      .[, .(demo_var, outcome, outcome_2y)] %>% 
      melt(id.vars='demo_var') %>% 
      ggplot(aes(demo_var, value, fill=variable)) +
      geom_boxplot() + 
      xlab(demo_var) +
      ylab('score') + 
      scale_fill_discrete(labels = c(outcome, outcomes_2y[[outcome]])) ->
      demo_plots[[demo_var]]
  }
}
do.call("grid.arrange", c(demo_plots, ncol=3))


demo_plots <- list()
for(demo_var in demographics){
  if( class(data_set[, get(demo_var)]) == 'numeric' ){
    data_set %>% 
      copy() %>% 
      setnames(demo_var, 'demo_var') %>% 
      .[, .(demo_var, info_2y, info_5y)] %>% 
      melt(id.vars='demo_var') %>% 
      ggplot(aes(variable, demo_var, fill=value)) +
      geom_boxplot() + 
      coord_flip() +
      xlab('Info') +
      ylab(demo_var) + 
      scale_color_discrete(labels = c(outcome, outcomes_2y[[outcome]]))  ->
      demo_plots[[demo_var]]
  }else{
    data_set %>% 
      copy() %>% 
      setnames(demo_var, 'demo_var') %>% 
      .[, .(demo_var, info_2y, info_5y)] %>% 
      melt(id.vars='demo_var') %>% 
      .[, .(info=sum(value)), .(demo_var, variable)] %>% 
      # .[, value := as.numeric(value)] %>% 
      ggplot(aes(variable, info, fill=demo_var)) +
      geom_bar(position='fill', stat='identity') + 
      xlab('Info') +
      ylab('Patients with info') ->
      demo_plots[[demo_var]]
  }
}


do.call("grid.arrange", c(demo_plots, ncol=3))
# 
# 
# 
# ```{r echo=FALSE, include=FALSE}
# demo_plots <- list()
# for(demo_var in demographics){
#   if( class(data_set[, get(demo_var)]) == 'numeric' ){
#     data_set %>% 
#       copy() %>% 
#       setnames(demo_var, 'demo_var') %>% 
#       setnames(outcome, 'outcome') %>% 
#       setnames(outcomes_2y[[outcome]], 'outcome_2y') %>% 
#       .[, .(demo_var, outcome, outcome_2y)] %>% 
#       melt(id.vars='demo_var') %>% 
#       ggplot(aes(demo_var, value, color=variable)) +
#       geom_point(alpha=0.1) + 
#       xlab(demo_var) +
#       ylab('score') + 
#       scale_color_discrete(labels = c(outcome, outcomes_2y[[outcome]])) +
#       geom_smooth(method='lm') ->
#       demo_plots[[demo_var]]
#   }else{
#     data_set %>% 
#       copy() %>% 
#       setnames(demo_var, 'demo_var') %>% 
#       setnames(outcome, 'outcome') %>% 
#       setnames(outcomes_2y[[outcome]], 'outcome_2y') %>% 
#       .[, .(demo_var, outcome, outcome_2y)] %>% 
#       melt(id.vars='demo_var') %>% 
#       ggplot(aes(demo_var, value, fill=variable)) +
#       geom_boxplot() + 
#       xlab(demo_var) +
#       ylab('score') + 
#       scale_fill_discrete(labels = c(outcome, outcomes_2y[[outcome]])) ->
#       demo_plots[[demo_var]]
#   }
# }
# ```
# 
# 
# ```{r echo=FALSE, message=FALSE, fig.height=30, fig.width=10, warning=FALSE}
# do.call("grid.arrange", c(demo_plots, ncol=ncols_plots))
# ```
# 
# ```{r echo=FALSE, include=FALSE}
# demo_plots <- list()
# for(demo_var in demographics){
#   if( class(data_set[, get(demo_var)]) == 'numeric' ){
#     data_set %>% 
#       copy() %>% 
#       setnames(demo_var, 'demo_var') %>% 
#       .[, .(demo_var, followup_2y, followup_5y)] %>% 
#       melt(id.vars='demo_var') %>% 
#       ggplot(aes(variable, demo_var, fill=value)) +
#       geom_boxplot() + 
#       coord_flip() +
#       xlab('Info') +
#       ylab(demo_var) + 
#       scale_color_discrete(labels = c(outcome, outcomes_2y[[outcome]]))  ->
#       demo_plots[[demo_var]]
#   }else{
#     data_set %>% 
#       copy() %>% 
#       setnames(demo_var, 'demo_var') %>% 
#       .[, .(demo_var, followup_2y, followup_5y)] %>% 
#       melt(id.vars='demo_var') %>% 
#       .[, .(info=sum(value)), .(demo_var, variable)] %>% 
#       # .[, value := as.numeric(value)] %>% 
#       ggplot(aes(variable, info, fill=demo_var)) +
#       geom_bar(position='fill', stat='identity') + 
#       xlab('Info') +
#       ylab('Patients with info') ->
#       demo_plots[[demo_var]]
#   }
# }
# ```
