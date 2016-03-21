# Steam user counts from https://store.steampowered.com/stats on 
# 2016-02-16 and 2016-02-18.
# Overall figure from http://venturebeat.com/2014/01/15/steam-has-75-million-registered-users-third-party-steam-controllers-and-other-tidbits-from-valves-dev-days/
steam.connected.users <- c(11645185, 6527571)
steam.overall.users <- 75e6

epsilon.steam <- steam.overall.users / steam.connected.users

# Average game prices from this repository's `steam.R`,
# representing Lunar New Year 2016's sale and plain Summer 2015 prices
#
# XXX We should reread them from there, not hardcode the values!
p.bar <- matrix(c(5.30, 12.39), ncol=2, dimnames=list("p.bar", c("Steam low price", "Steam high price")))


# Video on Demand subscription (``sub'') data scraped from 
# N. H. Thanh et al., "Enabling experiments for energy-efficient 
# data center networks on openflow-based platform,” in 
# Communications and Electronics (ICCE), 2012 Fourth International 
# Conference on, IEEE, 2012, pp. 239–244.
#
# Maximum bandwidth utilization at 17.530 (decimal) hours after midnight,
# i.e. almost 6 PM.
sub.bandwidth.utilization <- 0.71190
epsilon.sub <- 1/sub.bandwidth.utilization


# Collect all epsilons in a nice column vector
epsilon <- matrix(c(epsilon.sub, epsilon.steam), dimnames=list(c("sub eps", "Steam low eps", "Steam high eps"), "epsilon"))

# Desired profit margin
m <- 0.03

# Cost per user (including margin) for different epsilons and p.bars,
# $\mathcal{C}_u$
epsilon %*% p.bar / (1 + m)

# Cost for the whole capacity, $\mathcal{C}_{Cap}$
epsilon %*% p.bar / (1 + m) * steam.connected.users[1]

