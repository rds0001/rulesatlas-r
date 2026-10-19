.ra_euchre_suit <- function(c,trump) {s<-c%/%6;r<-c%%6;if(r==2 && s==bitwXor(as.integer(trump),1L)) trump else s}
.ra_euchre_strength <- function(c,trump) {s<-c%/%6;r<-c%%6;if(s==trump && r==2) 8 else if(s==bitwXor(as.integer(trump),1L) && r==2) 7 else r}
.ra_euchre_active <- function(d) if(d$alone) setdiff(0:3,(d$maker+2)%%4) else 0:3
.ra_euchre_next <- function(d,p) {for(i in 1:4) {q<-(p+i)%%4;if(q %in% .ra_euchre_active(d)) return(q)}}
.ra_euchre_playable <- function(d) {
  hand<-d$hands[[d$turn+1L]];if(!length(d$trick)) return(hand)
  lead<-.ra_euchre_suit(d$trick[[1]]$card,d$trump);follow<-Filter(function(c) .ra_euchre_suit(c,d$trump)==lead,hand);if(length(follow)) follow else hand
}
.ra_euchre_validate <- function(d) {
  cards<-c(unlist(d$hands),unlist(d$stock),unlist(d$won),vapply(d$trick,function(x) x$card,numeric(1)),d$upcard)
  if(d$phase=="setup") {
    if(length(cards) || any(unlist(d$scores)!=0) || !is.null(d$trump) || !is.null(d$maker)) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  if(length(cards)!=24 || !identical(as.integer(sort(cards)),0:23)) .ra_fail("Every card must occur exactly once",code="INVALID_STATE")
  bidding<-d$phase %in% c("bid_first","bid_second")
  if(bidding) {
    if(!is.null(d$trump) || !is.null(d$maker) || d$alone || is.null(d$upcard) || length(d$stock)!=3 || any(lengths(d$hands)!=5) || sum(lengths(d$won))>0 || length(d$trick) || d$turn!=(d$dealer+1+d$passes)%%4) .ra_fail("Invalid bidding state",code="INVALID_STATE")
  } else {
    if(is.null(d$trump) || is.null(d$maker)) .ra_fail("Contract is missing",code="INVALID_STATE")
    if(d$phase=="discard") {
      if(d$turn!=d$dealer || !is.null(d$upcard) || length(d$stock)!=3 || any(lengths(d$hands)!=5+as.integer(0:3==d$dealer)) || sum(lengths(d$won))>0 || length(d$trick)) .ra_fail("Dealer must discard one of six cards",code="INVALID_STATE")
    } else {
      n<-length(.ra_euchre_active(d));completed<-sum(lengths(d$won))%/%n
      if(length(d$stock)!=4 || !is.null(d$upcard) || any(lengths(d$won)%%n!=0) || completed>5) .ra_fail("Invalid stock or trick piles",code="INVALID_STATE")
      actors<-vapply(d$trick,function(x) x$player,numeric(1))
      if(anyDuplicated(actors) || length(actors)>=n || any(!actors %in% .ra_euchre_active(d))) .ra_fail("Invalid partial trick",code="INVALID_STATE")
      if(length(actors)) {
        if(d$turn!=.ra_euchre_next(d,tail(actors,1)) || (length(actors)>1 && any(vapply(2:length(actors),function(i) actors[i]!=.ra_euchre_next(d,actors[i-1]),logical(1))))) .ra_fail("Incorrect trick order",code="INVALID_STATE")
      }
      expected<-vapply(0:3,function(p) if(p %in% .ra_euchre_active(d)) 5-completed-as.integer(p %in% actors) else 5,numeric(1))
      if(any(lengths(d$hands)!=expected)) .ra_fail("Hand sizes do not match completed tricks",code="INVALID_STATE")
      if(any(lengths(d$won)[setdiff(0:3,.ra_euchre_active(d))+1L]>0)) .ra_fail("Sitting-out partner cannot win tricks",code="INVALID_STATE")
      if(!d$turn %in% .ra_euchre_active(d) || (d$phase %in% c("round_end","finished"))!=(completed==5)) .ra_fail("Round phase or active player mismatch",code="INVALID_STATE")
    }
  }
  if((d$phase=="finished")!=(max(unlist(d$scores))>=10)) .ra_fail("Match target and phase mismatch",code="INVALID_STATE")
}
.ra_euchre_contract <- function(d,trump,alone) {
  d$trump<-trump;d$maker<-d$turn;d$alone<-alone
  if(d$phase=="bid_first") {
    d$hands[[d$dealer+1L]]<-c(d$hands[[d$dealer+1L]],list(d$upcard));d["upcard"]<-list(NULL);d$turn<-d$dealer;d$phase<-"discard"
  } else {
    d$stock<-c(d$stock,list(d$upcard));d["upcard"]<-list(NULL);d$turn<-.ra_euchre_next(d,d$dealer);d$phase<-"play"
  }
  d
}
.ra_euchre_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn
  if(kind=="deal") {
    .ra_fields(p,list(deck=list(type="array",minItems=24,maxItems=24,items=list(type="integer",minimum=0,maximum=23))),"/params")
    if(!d$phase %in% c("setup","round_end") || anyDuplicated(unlist(p$deck))) .ra_fail("Deal a complete deck only between rounds")
    if(d$phase=="round_end") d$dealer<-(d$dealer+1L)%%4
    d$hands<-rep(list(list()),4);deck<-p$deck
    for(i in 0:19) {q<-(d$dealer+1L+i)%%4;d$hands[[q+1L]]<-c(d$hands[[q+1L]],list(deck[[i+1L]]))}
    d$stock<-deck[22:24];d$upcard<-deck[[21]];d["trump"]<-list(NULL);d["maker"]<-list(NULL);d$alone<-FALSE;d$passes<-0L;d$turn<-(d$dealer+1L)%%4;d$won<-rep(list(list()),4);d$trick<-list();d$phase<-"bid_first";return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(d$phase %in% c("bid_first","bid_second")) {
    if(kind=="pass") {
      .ra_fields(p,structure(list(),names=character()),"/params")
      if(d$phase=="bid_second" && d$passes==3) .ra_fail("Dealer must choose a trump in the second bidding round")
      d$passes<-d$passes+1L;d$turn<-(t+1L)%%4
      if(d$passes==4) {d$passes<-0L;d$phase<-"bid_second"};return(d)
    }
    if(kind!="call") .ra_fail("Pass or name trump")
    .ra_fields(p,list(trump=list(type="integer",minimum=0,maximum=3),alone=list(type="boolean")),"/params");up<-d$upcard%/%6
    if((p$trump==up)!=(d$phase=="bid_first")) .ra_fail("First round must order the upcard suit; second round must choose another suit")
    return(.ra_euchre_contract(d,p$trump,p$alone))
  }
  if(d$phase=="discard") {
    if(kind!="discard") .ra_fail("Dealer must discard")
    .ra_fields(p,list(card=list(type="integer",minimum=0,maximum=23)),"/params")
    if(!p$card %in% unlist(d$hands[[t+1L]])) .ra_fail("Discard must be held")
    d$hands[[t+1L]]<-Filter(function(c) c!=p$card,d$hands[[t+1L]]);d$stock<-c(d$stock,list(p$card));d$turn<-.ra_euchre_next(d,d$dealer);d$phase<-"play";return(d)
  }
  if(kind!="play" || d$phase!="play") .ra_fail("Play a card during a trick")
  .ra_fields(p,list(card=list(type="integer",minimum=0,maximum=23)),"/params");c<-p$card
  if(!c %in% unlist(.ra_euchre_playable(d))) .ra_fail("Card must be held and follow effective suit, including the left bower")
  d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$trick[[length(d$trick)+1L]]<-list(player=t,card=c);d$turn<-.ra_euchre_next(d,t);n<-length(.ra_euchre_active(d))
  if(length(d$trick)==n) {
    trump<-d$trump;lead<-.ra_euchre_suit(d$trick[[1]]$card,trump)
    weights<-vapply(d$trick,function(x) {s<-.ra_euchre_suit(x$card,trump);(if(s==trump) 2 else if(s==lead) 1 else 0)*10+.ra_euchre_strength(x$card,trump)},numeric(1))
    winner<-d$trick[[which.max(weights)]]$player;d$won[[winner+1L]]<-c(d$won[[winner+1L]],lapply(d$trick,function(x) x$card));d$trick<-list();d$turn<-winner
    if(sum(lengths(d$won))==5*n) {
      team<-d$maker%%2;tricks<-sum((lengths(d$won)%/%n)[(0:3)%%2==team])
      if(tricks<3) d$scores[[2L-team]]<-d$scores[[2L-team]]+2L else d$scores[[team+1L]]<-d$scores[[team+1L]]+if(tricks==5) if(d$alone) 4 else 2 else 1
      d$phase<-if(max(unlist(d$scores))>=10) "finished" else "round_end"
    }
  }
  d
}
.ra_euchre_legal <- function(state) {
  d<-state$data;phase<-d$phase
  if(phase %in% c("setup","round_end")) return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=24,maxItems=24,items=list(type="integer",minimum=0,maximum=23))),preconditions=list("Permutation of all 24 cards, supplied between rounds."),external_observations=list("Externally shuffled deck."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deal"))
  if(phase %in% c("bid_first","bid_second")) {
    up<-d$upcard%/%6;suits<-if(phase=="bid_first") up else setdiff(0:3,up);actions<-list()
    if(phase!="bid_second" || d$passes<3) actions[[1]]<-list(type="pass",params=structure(list(),names=character()))
    for(s in suits) for(a in c(FALSE,TRUE)) actions[[length(actions)+1L]]<-list(type="call",params=list(trump=s,alone=a))
    return(actions)
  }
  lapply(if(phase=="discard") d$hands[[d$turn+1L]] else .ra_euchre_playable(d),function(c) list(type=if(phase=="discard") "discard" else "play",params=list(card=c)))
}
.ra_euchre_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";team<-which.max(unlist(d$scores))-1L;wins<-if(terminal) which((0:3)%%2==team) else integer()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(terminal) lapply(setdiff(1:4,wins),function(p) state$players[[p]]$id) else list(),placements=list(),reason=if(terminal) "team_reached_ten" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
