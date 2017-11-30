---
title: A Not That Great Attempt at Simulating the World Cup Draw
author: Alex
date: '2017-11-29'
slug: a-not-that-great-attempt-at-simulating-the-world-cup-draw
categories:
  - football
  - simulations
tags:
  - rstats
  - ggplot
  - tidyverse
  - football
---

On Friday, famous crisp addict Gary Lineker will host the [FIFA World Cup 2018 draw](http://www.fifa.com/worldcup/news/y=2017/m=11/news=the-final-draw-how-it-works-2921565.html) in at the Kremlin, to determine the groups for next summer's men's football World Cup, in Russia. This is quite an exciting event, with lots of pomp and circumstance. (It goes on too long, but the drama of the draw itself is good). It may also  bring some hubris: after the 2010 draw, where England were grouped with the USA, Slovenia, and Algeria, we were overjoyed that such an ostensibly easy group would allow *easy* progress.
![](/img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/2010-easy.jpg)
Alas, three dire performances later and the England captain was shouting at the England fans through the TV cameras for booing their terrible performance.

If you're not familiar with how it works:

* 31 countries from across the globe have qualified, and have been put into four pots of 8 teams, along with Russia who qualify automatically.

* Russia will be in pot 1, followed by the next seven best ranked teams in the most recent FIFA world rankings

* Pots 2 through 4 are filled the same way, by world rankings. So pot 4 contains the eight lowest ranked nations in the competition (apart from Russia)

![](/img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/fifa-pots.png)

* Each nation is inside a ball in each pot, and the balls are drawn at randomw

* First pot 1 will be emptied first into groups A to H, drawn randomly, except that Russia will be automatically given pot A (as their matches have already been scheduled)

* Then pots 2 to 4 will be done in turn, also at random

* **But**, you cannot have more than one team from the same Confederation (i.e. continental group of football nations, essentially: Europe, Asia, Africa, South America, North & Central America) in the same group, except for European teams (because there are more) of which there cannot be more than 2.

**So** it is a complicated affair, and I thought it might be interesting to simulate in R to see which combinations of teams are most frequent, given the constraints of the draw. [Some newspapers and other websites have already got similar tools that you can try out yourself.](https://www.theguardian.com/football/2017/nov/16/world-cup-finals-create-your-own-draw-for-russia-2018-interactive)

Sadly, I found this a much more difficult task than expected. This post comes with several caveats that are explained further below as to the flaws in my methodology, and some things to bear in mind. I don't think this is completely useless, I just don't think it fully represents what will happen during the draw on Friday.

### Caveats

* It's not exactly clear to me the exact method for the draw. Whether groups will be filled in order A to H every time, or whether they will draw out a team from a pot, and then a group letter from another.

* Then, what is the protocol for if a team is drawn to a group that it cannot go into, due to the rules: will they return the team to the pot and draw again, or do they place the team drawn into the next available group that works?

* What happens in the event that you get to a point in the draw where the team drawn cannot go into _any_ of the available groups. It would be possible, for instance, for all the European teams from pot 2 to be drawn ahead of the South American teams, and all into the groups containing European pot 1 teams - and you could be left with Colombia and Peru trying to fit into groups that already contain Brazil or Argentina. I realise that FIFA will sort that out, but *how* is the question, and I don't know how to encorporate that into my simulation.

### How I Did It

I tried out a few things, and they didn't really work - I've not done something like this before, and was really lost as to how to approach it. I tried some things with `for` loops, but it didn't work. Then I came across [this reddit post which was great! It worked!](https://www.reddit.com/r/rstats/comments/7dumr7/simulating_fifa_world_cup_draws_how_to_improve/dq3nzis/)

The code is fairly comprehensible, and it works well, and quickly, without any loops or lots of `if else` conditions. In fact, the function there is better than anything I managed to make myself, and I'd recommend it. [Another reddit user has posted the results of their simulation](https://www.reddit.com/r/soccer/comments/7fxslg/oc_fifa_world_cup_draw_probabilities/), but has not published the method, and I couldn't say whether they've used R to do it or not - but their outcomes look accurate and better than mine!

I tried to copy the first method, and improve on it and recode some of it in a way that was more understandable to me. The process followed is to put Russia into group A, then randomly allocate the rest of the pot 1 teams to the groups. Then go through pot by pot and draw each set of teams in a confederation, depending on how restrained they would be. So that means for the 2nd round of the draw, taking the South American teams, determining which groups they are able to go to, and randomly assigning them to one of those - and then putting the European teams and Mexico into the rest of the groups. Then repeat that process through the draw until the end. 


```r
library(tidyverse)

seeds <- read_csv("https://gist.github.com/eldenvo/fb0d27fcb5d322b538d06cdc06b1c559/raw/3eaaab91c29d2dde6c9a28377b325844b834df13/seeds.csv")
```


```r
print(seeds)
```

```
## # A tibble: 32 x 4
##          nation  rank   pot        confed
##           <chr> <int> <int>         <chr>
##  1       Russia    65     1        Europe
##  2       Brazil     2     1 South America
##  3         Iran    34     3          Asia
##  4        Japan    44     4          Asia
##  5       Mexico    16     2 North America
##  6      Belgium     5     1        Europe
##  7  South Korea    62     4          Asia
##  8 Saudi Arabia    63     4          Asia
##  9      Germany     1     1        Europe
## 10      England    12     2        Europe
## # ... with 22 more rows
```

This is the function to do the draw:

```r
sim_draw <- function(x) {
  
  ## Round 1 of draw
    r1.a <- tibble(group_names = "A", r1 = "Russia")

    r1.b <- tibble(group_names = sample(LETTERS[2:8], 7),
                  r1 = x %>% 
                    filter(pot == 1, nation != "Russia") %>% 
                    pull(nation))
    
  r1 <- bind_rows(r1.a, r1.b)

    confeds <- left_join(r1, seeds %>% 
                           filter(nation %in% r1$r1) %>% 
                           select(nation, confed), by = c("r1" = "nation")) %>%
              select(-r1) %>%
              rename(r1 = confed)
    
    ## Round 2
   
    r2_sa <- x %>% 
      filter(pot == 2, confed == "South America") %>% 
      pull(nation)
    
    r2_sa_ok <- confeds %>% 
      filter(r1 != "South America") %>% 
      pull(group_names)
    
    r2.sa <- tibble(group_names = sample(r2_sa_ok, length(r2_sa)),
                    r2 = r2_sa)
    
    
    r2_oth_teams <- x %>% 
      filter(pot == 2, confed != "South America") %>% 
      pull(nation)
    
    r2.other <- tibble(group_names = setdiff(LETTERS[1:8],r2.sa$group_names),
                       r2 = sample(r2_oth_teams))
    
    r2 <- bind_rows(r2.sa, r2.other)
    
    r2.confeds <- left_join(r2, seeds %>% 
                              filter(nation %in% r2$r2) %>% 
                              select(nation, confed), by = c("r2" = "nation")) %>%
                    select(-r2) %>% 
                    rename(r2 = confed)
    
    draw <- left_join(r1, r2, by = "group_names")
    
    draw.confeds <- left_join(confeds, r2.confeds, by = "group_names")

    ## Round 3
    r3_eu_teams <- x %>% 
      filter(pot == 3, confed == "Europe") %>% 
      pull(nation)
    
    r3_eu_groups <- draw.confeds %>% 
      filter(r1 != "Europe" | r2 != "Europe") %>% 
      pull(group_names)
    
    r3.eu <- tibble(group_names = sample(r3_eu_groups, length(r3_eu_teams)),
                    r3 = r3_eu_teams)
    
  r3_na_teams <- x %>% 
    filter(pot == 3, confed == "North America") %>% 
    pull(nation)
  
  r3_na_groups <- draw.confeds %>% 
    filter(r1 != "North America", r2 != "North America") %>% 
    pull(group_names)
  
  r3_na_groups <- setdiff(r3_na_groups, r3.eu$group_names)
  
  r3.na <- tibble(group_names = sample(r3_na_groups, length(r3_na_teams)),
                  r3 = r3_na_teams)
 
  r3_oth_teams <- x %>% 
    filter(pot == 3, confed != "Europe", confed != "North America") %>% 
    pull(nation)
  
  r3_oth_groups <- setdiff(LETTERS[1:8], c(r3.eu$group_names, r3.na$group_names))
  
  r3.other <- tibble(group_names = sample(r3_oth_groups, length(r3_oth_teams)),
                     r3 = r3_oth_teams)
  
  r3 <- bind_rows(r3.eu, r3.na, r3.other)
  
  r3.confeds <-  left_join(r3, seeds %>% 
                             filter(nation %in% r3$r3) %>% 
                             select(nation, confed), by = c("r3" = "nation")) %>%
                  select(-r3) %>%
                  rename(r3 = confed)
  
  draw <- left_join(draw, r3, by = "group_names")
  
  draw.confeds <- left_join(draw.confeds, r3.confeds, by = "group_names")
 
  ## Round 4
    r4_eu_teams <- x %>% filter(pot == 4, confed == "Europe") %>% pull(nation)
    r4_eu_groups <- draw.confeds %>%
      filter(r1 != "Europe" | r2 != "Europe") %>%
      filter(r1 != "Europe" | r3 != "Europe") %>%
      filter(r2 != "Europe" | r3 != "Europe") %>%
      pull(group_names)
    
    r4.eu <- tibble(group_names = sample(r4_eu_groups, length(r4_eu_teams)),
                    r4 = r4_eu_teams)
    
    r4_afr_teams <- x %>% 
      filter(pot == 4, confed == "Africa") %>% 
      pull(nation)
    
    r4_afr_groups <- draw.confeds %>% 
      filter(r3 != "Africa") %>% 
      pull(group_names)
    
    r4_afr_groups <- setdiff(r4_afr_groups, r4.eu$group_names)
    
    r4.afr <- tibble(group_names = sample(r4_afr_groups, length(r4_afr_teams)),
                     r4 = r4_afr_teams)
    
    r4.sofar <- bind_rows(r4.eu, r4.afr)

    # A special case is if the group with the Asian team from pot 3 still has a spot open, The CONCACAF team must get this spot, otherwise there will not be enough spaces for the remaining 4 Asian teams

    r3_asian_group <- draw.confeds %>% 
      filter(r3 == "Asia") %>% 
      pull(group_names)
    
    r4_remaining_groups <- setdiff(LETTERS[1:8], r4.sofar$group_names)
    
    r4_na_teams <- x %>% 
      filter(pot == 4, confed == "North America") %>% 
      pull(nation)
    
    r4_na_groups <- draw.confeds %>% 
      filter(r2 != "North America", r3 != "North America") %>% 
      pull(group_names)
    
    r4_na_groups <- setdiff(r4_na_groups, r4.sofar$group_names)
    
    test <- case_when(r3_asian_group %in% r4_remaining_groups ~ T,
              !r3_asian_group %in% r4_remaining_groups ~ F)
    
    r4_na_groups_tested <- case_when(test ~ r3_asian_group, !test ~ r4_na_groups)

    r4.na <- tibble(group_names = sample(r4_na_groups_tested, 1),
                    r4 = r4_na_teams)
    

    r4_asia_teams <- x %>% 
      filter(pot == 4, confed == "Asia") %>% 
      pull(nation)
    
    r4_asia_groups <- draw.confeds %>% 
      filter(r3 != "Asia") %>% 
      pull(group_names)
    
    r4_asia_groups <- setdiff(r4_asia_groups, c(r4.eu$group_names, r4.afr$group_names, r4.na$group_names))
    
    r4.asia <- tibble(group_names = sample(r4_asia_groups, 4),
                      r4 = r4_asia_teams)
    
    r4 <- bind_rows(r4.eu, r4.na, r4.afr, r4.asia)
    
    r4.confeds <-  left_join(r4, seeds %>% 
                               filter(nation %in% r4$r4) %>% 
                               select(nation, confed), by = c("r4" = "nation")) %>%
                    select(-r4) %>%
                    rename(r4 = confed)
    
    draw <- left_join(draw, r4, by = "group_names")
    
    draw.confeds <- left_join(draw.confeds, r4.confeds,by = "group_names")
    
    groups <- gather(draw, round, nation, -group_names) %>%
      mutate(seed = rep(1:4, each = 8)) %>%
      select(nation, seed, group = group_names) %>%
      arrange(seed, group)
    
    
    return(groups)
}

sim_draw(seeds)
```

```
## # A tibble: 32 x 3
##       nation  seed group
##        <chr> <int> <chr>
##  1    Russia     1     A
##  2    Brazil     1     B
##  3   Belgium     1     C
##  4  Portugal     1     D
##  5   Germany     1     E
##  6    Poland     1     F
##  7    France     1     G
##  8 Argentina     1     H
##  9      Peru     2     A
## 10   England     2     B
## # ... with 22 more rows
```

Phew! If you want to run the simulation lots of times, you can do `replicate(10000, sim_draw(seeds))` You can get [my file with 1,000,000 simulations here.](https://github.com/eldenvo/world-cup-2018-draw/blob/master/data/sims.rds)

### Findings and flaws
Cutting to the chase, here are the results:

![](img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/most-likely-opponents-grid.png)

Interesting!

Clearly, one value stands out there, and that is the Iran-Panama match up. If you compare mine to the [earlier person 
from reddit's findings](https://i.imgur.com/CwWYdFV.png) then you'll see we disagree, and I think they are right. The flaw in my simulation comes in pot 4: the constraints of the draw can be such that there are lots of ways you could correctly draw the teams up to that point that make it difficult to find space for the remaining teams. In my sim, I draw Serbia, then the two African teams, then Panama, then the 4 remaining Asian teams.

When I draw Panama, the simulation says that if the group containing the only Asian team not in pot 4 (Iran), has not yet been given a pot 4 team, then Panama must go into that - because if it didn't, there would only be three groups remaining with no Asian confederation teams, and four Asian teams to draw. I imagine that the proper draw will do *something* like this, but I believe the way I'm doing it means it quite drastically over-estimates the frequency of Iran and Panama being in the same group. Also, my method has the sad side effect of creating a small chance (4%) of Panama  and Mexico being in the same group - which cannot happen in reality.

**The order of the draw matters**. My simulation, and I believe the other one I've shared, don't simulate the draw in order, picking one team at a time to go into groups A through H in order - which I think is the most likely way the real draw will happen. The impact of *not* doing this is kinda hard to tell from the results of the simulation, and trying to work out mathematically what the probabilities would actually be, is really complicated, and way above my pay grade.

However, one place where you can see the effects, is with Russia and the pot 2 teams. We know that Russia will always be group A. Because Russia is a UEFA team, any of the nations in pot 2 could be paired with Russia without breaking the rules. If the draw is done in order, that means that the first team drawn out of pot 2 will always go in Russia's group; there is no reason it could not. That should mean therefore that every team in pot 2 has a 12.5% chance of being in Russia's group, rather than a 10% chance as these simulations show. That would then have knock on effects on the rest of the draw (I think).

Nevertheless, a lot of my probabilities match those found by others, and it's still a helpful exercise. In terms of thinking who England's likely opponents are:

![](img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/most-frequent-england-opponents-by-pot.png)

![](/img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/most frequent england groups.png)

![](/img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/most frequent england groups excl Iran Panama.png)

There showing both with and without the Iran-Panama combination which potentially skews results.

I recommend having a play around with the data, as there are some interesting results. For example, there's only 8 different ways Serbia could end up without another European opponent - each with about a 1% chance. As the only European team in pot 4, the probabilities of Serbia's opponents are quite severe one way or another.

![](/img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/most groups containing Serbia.png)

![](/img/2017-11-29-a-not-that-great-attempt-at-simulating-the-world-cup-draw/Serbian groups with no EU.png)

I would please love to know any advances on the simulation model, both in terms of how to make it run faster (mine is slow), and how to more accurately represent reality (but you haven't got long - in little more than 36 hours, these probabilities will be moot, and we'll know whether or not England have it EASY again or get a group of death - Argentina, Iceland and Australia would be a lovely little match up).




