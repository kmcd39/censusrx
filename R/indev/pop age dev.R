library(tidycensus)


tidycensus::get_acs()

ages <-
  map_dfr(2019,
          ~tidycensus::get_acs(
            geography = 'county' #'tract'
            ,year = .x
            ,state = 37
            ,county =
              '055'
            #fcos$countyfp
            ,table = 'B01001'
          ) %>%
            rename_with(tolower) %>%
            select(-name) %>%
            mutate(yr = .x)
  )

ages

?multiyr.acs.wrapper()

ages <- multiyr.acs.wrapper(
 'B01001'
 ,'37'
 ,geo ='county'
 ,years = c(2010,2016,2021)
)

ages$label

# drop aggs & remove Total: prefix
ages <- ages %>%
  filter(grepl('years', label)) %>%
  mutate(label =
           gsub('^Total: ', '', label))

# remove gender & remove redundant "years" suffix
ages <- ages %>%
  mutate(label =
           gsub('^Male: |^Female: ', '', label)) %>%
  mutate(label =
           gsub(' years', '', label))

# extract var/label for factor order
lbls <- ages %>%
  select(variable, var, label) %>%
  distinct() %>%
  arrange(var)

# agg to just age
ages <- ages %>%
  filter(geoid == '37055') %>%
  group_by(yr, geoid, label) %>%
  summarise(n = sum(estimate)) %>%
  ungroup() %>%
  mutate(label = factor(
    label,
    levels = unique(lbls$label)
  ))


ages %>%
  filter(yr %in% c(2010,2021)) %>%
  ggplot(
    aes(
       y = label
      ,x = n
      ,fill = factor(yr)
      ,color = factor(yr)
      ,group = yr
    )
  ) +
  #geom_col(    position = 'dodge'  )
  geom_point() +
  geom_line()

# Here's the one i was looking for.
ages %>%
  filter(yr %in% c(2010,2021)) %>%
  ggplot(
    aes(
       x = label
      ,y = n
      ,fill = factor(yr)
      ,color = factor(yr)
      ,group = yr
    )
  ) +
  #geom_col(    position = 'dodge'  )
  #geom_point() +
#  geom_line(linewidth = 1.1
#            ) +
  geom_smooth(linewidth = .4
              ,method = 'loess'
              ,span =
                .3 # can play w this parameter maybe
              ,alpha = .2
              ,se = F) +
  coord_flip()



ages %>%
  #mutate(label = gsub(' years$', '', label)) %>%
  filter(yr %in% c(2010,2021)) %>%
  ggplot(
    aes(
      y = label
      ,x = n
      ,fill = factor(yr)
      ,color = factor(yr)
    )
  ) +
  geom_col() +
  facet_wrap(
    vars(yr)
  )


?tidycensus::get_acs
ages %>%
  filter(grepl('years', label)) %>%
  .$variable %>% unique()

# xt %>% count(label) %>% View()

# parse categories w colons
tmp <- xt %>%
  mutate(splitl =
           map(label,
               ~str_split(.x,
                          ': '
               )) )

tmp$splitl  %>% head()

tmp <- tmp %>%
  mutate(edu =
           map_chr(splitl,
                   ~unlist(.x)[1]
           )
         ,lf =
           map_chr(splitl,
                   ~unlist(.x)[2]
           )
         ,lang =
           map_chr(splitl,
                   ~unlist(.x)[3]
           )
  )
