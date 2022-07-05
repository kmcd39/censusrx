library(tidyverse)
rm(list = ls())
devtools::document()
devtools::load_all()


# meta --------------------------------------------------------------------

meta <- pull.acs.metadata(year = 2019)


# pull --------------------------------------------------------------------


vacancyts <- multiyr.acs.wrapper('B25034'
                               ,state = 44
                               , geo = 'tract'
                               ,metadata = meta
                               )

vacancyts %>%
  count(label, yr)



# recode ------------------------------------------------------------------

# ?acs.bldg.age.recode

vacancyts <- vacancyts %>%
  acs.bldg.age.recode()

tmp <- vacancyts %>%
  group_by(geoid, yr, recode) %>%
  summarise(n = sum(estimate))
tmp$recode


# demos -------------------------------------------------------------------


demots <- multiyr.acs.wrapper('B03002'
                              ,state = 44
                              , geo = 'county subdivision'
                              ,metadata = meta)

tmp <- demots %>%
  acs.demographic.recode()

tmp <- tmp %>%
  group_by(yr, recode) %>%
  summarise(n = sum(estimate))
tmp %>%
  ggplot( aes(x = recode
              ,y = n
              ,fill = n) ) +
  geom_col() +
  facet_wrap( vars(yr)) +
  coord_flip()


# try it with dif groups pulled out.

tmp <- demots %>%
  acs.demographic.recode(other.vars = c(8:11))

tmp <- tmp %>%
  group_by(yr, recode) %>%
  summarise(n = sum(estimate))
tmp %>%
  ggplot( aes(x = recode
              ,y = n
              ,fill = n) ) +
  geom_col() +
  facet_wrap( vars(yr)) +
  coord_flip()

# tmp$recode <- factor(tmp$recode, levels = rev(levels(tmp$recode)))
# dope!


# commutes ----------------------------------------------------------------



commutes <- multiyr.acs.wrapper('B08006'
                              ,state = 9
                              , geo = 'county'
                              ,metadata = meta)
devtools::load_all()
tmp <- acs.commute.recode(commutes)
tmp <- tmp %>%
  group_by(yr, recode) %>%
  summarise(n = sum(estimate))

tmp %>%
  ggplot( aes(  x = yr
                ,color = recode
                ,group = recode
                ,y = n)
  ) +
  geom_path(alpha = .7) +
  visaux::ts.labels.geom(tmp, 'recode', 'yr') +
  scale_x_discrete( name = NULL) +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(reverse=TRUE))

if(!is.null(time.col))
  p <- p +
  facet_wrap(vars(!!.tc))


tmp
#devtools::install_github('kmcd39/regionprofileR')
regionprofileR::
