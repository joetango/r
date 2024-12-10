## Deck vector, how do I keep track of whats been played
## Dealer logic
## Player logic
## Keep track of whats been played in a list

require(tidyverse)

deck1 <- rep(1:13, each = 4)
suit1 <- rep(c("C", "S", "D", "H"), 13)
deck6 <- rep(deck1, 6)
suit6 <- rep(suit1, 6)
deck <- data.frame(value = deck6,
                   suit = suit6,
                   played = rep(0, 52*6))

#### Pretend hand 1
hand <- data.frame()

# Deal
samp <- sample((1:nrow(deck))[deck$played == 0], 2, replace = FALSE)
hand <- bind_rows(hand,
                  deck[samp,])
deck$played[samp] <- 1

if(sum(value) <= 17) {
  samp <- sample((1:nrow(deck))[deck$played == 0], 1, replace = FALSE)
  hand <- bind_rows(hand,
                    deck[samp,])
  deck$played[samp] <- 1
}