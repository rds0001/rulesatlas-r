.ra_blackjack_value <- function(c) if(c%%13==12) 11 else min(c%%13+2,10)
.ra_blackjack_total <- function(cards) {
  values<-vapply(cards,.ra_blackjack_value,numeric(1));n<-sum(values);aces<-sum(values==11)
  while(n>21 && aces>0) {n<-n-10;aces<-aces-1L};n
}
.ra_blackjack_natural <- function(h) !h$split && length(h$cards)==2 && .ra_blackjack_total(h$cards)==21
.ra_blackjack_dealer_natural <- function(d) length(d$dealer)==2 && .ra_blackjack_total(d$dealer)==21
.ra_blackjack_draw <- function(d) {
  if(!length(d$deck)) .ra_fail("Deck is exhausted")
  list(card=d$deck[[1]],deck=d$deck[-1])
}
.ra_blackjack_returns <- function(d) {
  dn<-.ra_blackjack_dealer_natural(d);dt<-.ra_blackjack_total(d$dealer)
  lapply(d$hands,function(h) {
    wager<-h$wager;pt<-.ra_blackjack_total(h$cards)
    if(h$status=="surrendered") wager%/%2 else if(pt>21) 0 else if(dn) if(.ra_blackjack_natural(h)) wager else 0 else if(.ra_blackjack_natural(h)) 5*wager%/%2 else if(dt>21 || pt>dt) 2*wager else if(pt==dt) wager else 0
  })
}
.ra_blackjack_settle <- function(d) {
  if(!.ra_blackjack_dealer_natural(d) && any(vapply(d$hands,function(h) !h$status %in% c("bust","surrendered") && !.ra_blackjack_natural(h),logical(1)))) {
    while(.ra_blackjack_total(d$dealer)<17) {draw<-.ra_blackjack_draw(d);d$dealer<-c(d$dealer,list(draw$card));d$deck<-draw$deck}
  }
  for(i in seq_along(d$hands)) if(d$hands[[i]]$status=="active") d$hands[[i]]$status<-"stood"
  d$returns<-.ra_blackjack_returns(d);d$insurance_return<-if(.ra_blackjack_dealer_natural(d)) 3*d$insurance else 0;d$balance<-d$balance+sum(unlist(d$returns))+d$insurance_return;d$phase<-"finished";d
}
.ra_blackjack_advance <- function(d) {
  while(d$current<length(d$hands)) {
    i<-d$current+1L;h<-d$hands[[i]]
    if(h$status=="waiting") {
      draw<-.ra_blackjack_draw(d);h$cards<-c(h$cards,list(draw$card));d$deck<-draw$deck;h$status<-if(h$split_aces || .ra_blackjack_total(h$cards)==21) "stood" else "active";d$hands[[i]]<-h
    }
    if(h$status=="active") return(d)
    d$current<-d$current+1L
  }
  d$current<-length(d$hands)-1L;.ra_blackjack_settle(d)
}
.ra_blackjack_peek <- function(d) {if(.ra_blackjack_dealer_natural(d) || .ra_blackjack_natural(d$hands[[1]])) return(.ra_blackjack_settle(d));d$phase<-"player";d}
.ra_blackjack_validate <- function(d) {
  hands<-d$hands
  if(d$phase=="bet") {
    if(length(hands) || length(d$dealer) || length(d$deck) || d$base_wager || d$insurance || length(d$returns) || d$insurance_return || d$balance!=2000) .ra_fail("Initial bankroll state must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$dealer),unlist(d$deck),unlist(lapply(hands,function(h) h$cards)))
  if(length(cards)!=52 || !identical(as.integer(sort(cards)),0:51) || !length(hands) || d$current>=length(hands) || length(d$dealer)<2) .ra_fail("Invalid card inventory or active hand index",code="INVALID_STATE")
  base<-d$base_wager
  if(base<2 || base%%2!=0 || !d$insurance %in% c(0,base%/%2) || (d$insurance>0 && .ra_blackjack_value(d$dealer[[1]])!=11)) .ra_fail("Invalid stake or insurance amount",code="INVALID_STATE")
  if(length(d$returns)!=length(hands) || d$balance!=2000-sum(vapply(hands,function(h) h$wager,numeric(1)))-d$insurance+sum(unlist(d$returns))+d$insurance_return) .ra_fail("Bankroll and stakes do not reconcile",code="INVALID_STATE")
  for(i in seq_along(hands)) {
    h<-hands[[i]];status<-h$status;n<-length(h$cards);v<-.ra_blackjack_total(h$cards)
    if(!h$wager %in% c(base,2*base) || (status=="waiting")!=(n==1) || n<1) .ra_fail("Invalid hand size or wager",code="INVALID_STATE")
    if(status=="waiting" && (!h$split || i-1<=d$current)) .ra_fail("Only later split hands may await their first draw",code="INVALID_STATE")
    if((status=="bust")!=(v>21) || (status=="active" && v>=21)) .ra_fail("Hand status does not match its total",code="INVALID_STATE")
    if(h$split_aces && (!h$split || .ra_blackjack_value(h$cards[[1]])!=11 || n>2 || !status %in% c("waiting","stood"))) .ra_fail("Split aces get exactly one additional card",code="INVALID_STATE")
    if(status=="surrendered" && (h$split || n!=2 || h$wager!=base)) .ra_fail("Surrender is only available on an original two-card hand",code="INVALID_STATE")
  }
  if(.ra_blackjack_dealer_natural(d) && (length(hands)!=1 || hands[[1]]$split || length(hands[[1]]$cards)!=2 || hands[[1]]$status=="surrendered")) .ra_fail("Dealer blackjack ends before player actions",code="INVALID_STATE")
  if(length(hands)>1 && any(!vapply(hands,function(h) h$split,logical(1)))) .ra_fail("Multiple hands must all arise from splitting",code="INVALID_STATE")
  if(d$phase=="finished") {
    if(any(vapply(hands,function(h) h$status %in% c("active","waiting"),logical(1))) || any(unlist(d$returns)!=unlist(.ra_blackjack_returns(d))) || d$insurance_return!=if(.ra_blackjack_dealer_natural(d)) 3*d$insurance else 0) .ra_fail("Final hand settlement is inconsistent",code="INVALID_STATE")
    if(!.ra_blackjack_dealer_natural(d) && any(vapply(hands,function(h) !h$status %in% c("bust","surrendered") && !.ra_blackjack_natural(h),logical(1))) && .ra_blackjack_total(d$dealer)<17) .ra_fail("Dealer must draw through sixteen",code="INVALID_STATE")
  } else {
    if(any(unlist(d$returns)!=0) || d$insurance_return!=0 || length(d$dealer)!=2) .ra_fail("Dealer drawing and returns occur only at settlement",code="INVALID_STATE")
    if(d$phase=="insurance") {
      if(.ra_blackjack_value(d$dealer[[1]])!=11 || length(hands)!=1 || length(hands[[1]]$cards)!=2 || d$insurance!=0) .ra_fail("Insurance is offered only before the ace-upcard peek",code="INVALID_STATE")
    } else if(hands[[d$current+1L]]$status!="active" || any(vapply(head(hands,d$current),function(h) h$status=="active",logical(1))) || .ra_blackjack_dealer_natural(d) || any(vapply(hands,.ra_blackjack_natural,logical(1)))) .ra_fail("Invalid player decision state",code="INVALID_STATE")
  }
}
.ra_blackjack_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="bet") {
    .ra_fields(p,list(stake=list(type="integer",minimum=2,maximum=1000),deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51))),"/params")
    if(d$phase!="bet" || p$stake%%2!=0 || anyDuplicated(unlist(p$deck))) .ra_fail("Bet an even number of half-chip units and supply all fifty-two cards")
    stake<-p$stake;deck<-p$deck;d$base_wager<-stake;d$balance<-d$balance-stake;cards<-deck[c(1,3)];d$hands<-list(list(cards=cards,wager=stake,status=if(.ra_blackjack_total(cards)==21) "stood" else "active",split=FALSE,split_aces=FALSE));d$dealer<-deck[c(2,4)];d$deck<-deck[5:52];d$returns<-list(0)
    if(.ra_blackjack_value(d$dealer[[1]])==11) {d$phase<-"insurance";return(d)}
    return(.ra_blackjack_peek(d))
  }
  if(kind=="insurance") {
    .ra_fields(p,list(take=list(type="boolean")),"/params")
    if(d$phase!="insurance") .ra_fail("No insurance decision is pending")
    if(p$take) {
      amount<-d$base_wager%/%2
      if(d$balance<amount) .ra_fail("Insufficient bankroll for insurance")
      d$insurance<-amount;d$balance<-d$balance-amount
    }
    return(.ra_blackjack_peek(d))
  }
  if(d$phase!="player") .ra_fail("No player decision is pending")
  .ra_fields(p,structure(list(),names=character()),"/params");i<-d$current+1L;h<-d$hands[[i]]
  if(kind=="hit") {
    draw<-.ra_blackjack_draw(d);h$cards<-c(h$cards,list(draw$card));d$deck<-draw$deck;v<-.ra_blackjack_total(h$cards);h$status<-if(v>21) "bust" else if(v==21) "stood" else "active";d$hands[[i]]<-h;return(.ra_blackjack_advance(d))
  }
  if(kind=="stand") {d$hands[[i]]$status<-"stood";return(.ra_blackjack_advance(d))}
  if(kind=="surrender") {
    if(h$split || length(h$cards)!=2) .ra_fail("Late surrender requires an original two-card hand after the dealer peek")
    d$hands[[i]]$status<-"surrendered";return(.ra_blackjack_advance(d))
  }
  if(kind=="double") {
    if(length(h$cards)!=2 || d$balance<h$wager) .ra_fail("Doubling requires two cards and an equal available stake")
    d$balance<-d$balance-h$wager;h$wager<-2*h$wager;draw<-.ra_blackjack_draw(d);h$cards<-c(h$cards,list(draw$card));d$deck<-draw$deck;h$status<-if(.ra_blackjack_total(h$cards)>21) "bust" else "stood";d$hands[[i]]<-h;return(.ra_blackjack_advance(d))
  }
  if(kind=="split") {
    if(length(h$cards)!=2 || h$cards[[1]]%%13!=h$cards[[2]]%%13 || length(d$hands)>=4 || d$balance<h$wager) .ra_fail("Split requires an equal-rank pair, available stake and fewer than four hands")
    d$balance<-d$balance-h$wager;new<-lapply(h$cards,function(c) list(cards=list(c),wager=h$wager,status="waiting",split=TRUE,split_aces=.ra_blackjack_value(c)==11));d$hands<-append(d$hands[-i],new,after=i-1L);d$returns<-append(d$returns,list(0),after=i-1L);return(.ra_blackjack_advance(d))
  }
  .ra_fail("Unknown action")
}
.ra_blackjack_legal <- function(state) {
  d<-state$data
  if(d$phase=="bet") return(list(mode="event_types",actions=list(),action_types=list(list(type="bet",params_schema=list(stake=list(type="integer",minimum=2,maximum=1000),deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51))),preconditions=list("Stake is even in half-chip units; deck is a permutation of all fifty-two cards."),external_observations=list("External deck order."))),constraints=list(balance=d$balance),completeness=FALSE,reason="external_deal"))
  if(d$phase=="insurance") return(lapply(if(d$balance>=d$base_wager%/%2) c(FALSE,TRUE) else FALSE,function(t) list(type="insurance",params=list(take=t))))
  h<-d$hands[[d$current+1L]];kinds<-c("hit","stand")
  if(!h$split && length(h$cards)==2) kinds<-c(kinds,"surrender")
  if(length(h$cards)==2 && d$balance>=h$wager) {
    kinds<-c(kinds,"double")
    if(h$cards[[1]]%%13==h$cards[[2]]%%13 && length(d$hands)<4) kinds<-c(kinds,"split")
  }
  lapply(kinds,function(k) list(type=k,params=structure(list(),names=character())))
}
.ra_blackjack_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";net<-d$balance-2000;pid<-state$players[[1]]$id
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(net>0) "win" else if(net==0) "draw" else "other" else "none",winners=if(done && net>0) list(pid) else list(),losers=if(done && net<0) list(pid) else list(),placements=if(done) list(list(player_id=pid,place=1,balance_half_chips=d$balance,net_half_chips=net)) else list(),reason=if(done) "single_round_settled" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
