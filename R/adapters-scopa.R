.ra_scopa_rank <- function(c) c%%10+1L
.ra_scopa_possible <- function(table,target) {
  reachable<-0
  for(c in unlist(table)) {new<-reachable+.ra_scopa_rank(c);reachable<-union(reachable,new[new<=target])}
  target %in% reachable
}
.ra_scopa_points <- function(d) {
  piles<-d$won;points<-unlist(d$sweeps);categories<-list(lengths(piles),vapply(piles,function(p) sum(unlist(p)%/%10==0),integer(1)));prime<-c(16,12,13,14,15,18,21,10,10,10)
  primiera<-vapply(piles,function(pile) {
    cards<-unlist(pile);values<-vapply(0:3,function(s) max(c(0,prime[.ra_scopa_rank(cards[cards%/%10==s])])),numeric(1));if(all(values>0)) sum(values) else 0
  },numeric(1));categories[[3]]<-primiera
  for(values in categories) if(values[1]!=values[2]) {p<-which.max(values);points[p]<-points[p]+1L}
  for(p in 1:2) if(6 %in% unlist(piles[[p]])) points[p]<-points[p]+1L
  points
}
.ra_scopa_validate <- function(d) {
  cards<-c(unlist(d$stock),unlist(d$table),unlist(d$hands),unlist(d$won))
  if(d$phase=="setup") {
    if(length(cards) || !is.null(d$last) || any(unlist(d$sweeps)!=0) || any(unlist(d$scores)!=0)) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  if(length(cards)!=40 || !identical(as.integer(sort(cards)),0:39) || length(d$stock)%%6!=0 || length(d$stock)>30) .ra_fail("Invalid deck inventory or undealt stock",code="INVALID_STATE")
  if(is.null(d$last) && sum(lengths(d$won))>0) .ra_fail("Captured cards require a last capturing player",code="INVALID_STATE")
  if(!is.null(d$last) && !length(d$won[[d$last+1L]])) .ra_fail("Last capturing player has no captured cards",code="INVALID_STATE")
  if(any(unlist(d$sweeps)>lengths(d$won)%/%2)) .ra_fail("More sweeps than captured pairs",code="INVALID_STATE")
  if(d$phase=="playing") {
    first<-1L-d$dealer;sizes<-lengths(d$hands)
    if(!sizes[d$turn+1L] || !(sizes[first+1L]-sizes[2L-first]) %in% c(0,-1) || d$turn!=if(sizes[1]==sizes[2]) first else 1L-first) .ra_fail("Hand sizes and turn mismatch",code="INVALID_STATE")
    if(max(unlist(d$scores))>=11 && d$scores[[1]]!=d$scores[[2]]) .ra_fail("Match should already have ended",code="INVALID_STATE")
  } else {
    if(length(d$stock) || length(d$table) || sum(lengths(d$hands))>0 || sum(lengths(d$won))!=40) .ra_fail("Round must be fully collected before scoring",code="INVALID_STATE")
    ended<-max(unlist(d$scores))>=11 && d$scores[[1]]!=d$scores[[2]]
    if((d$phase=="finished")!=ended) .ra_fail("Match target and phase mismatch",code="INVALID_STATE")
  }
}
.ra_scopa_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(kind=="deal") {
    .ra_fields(p,list(deck=list(type="array",minItems=40,maxItems=40,items=list(type="integer",minimum=0,maximum=39))),"/params")
    if(!d$phase %in% c("setup","round_end") || anyDuplicated(unlist(p$deck))) .ra_fail("Deal requires a complete new shuffled deck between rounds")
    if(d$phase=="round_end") d$dealer<-1L-d$dealer
    if(sum(vapply(p$deck[7:10],.ra_scopa_rank,numeric(1))==10)>=3) .ra_fail("Initial table with three or four kings requires a redeal")
    first<-1L-d$dealer;deck<-p$deck;d$hands<-list(list(),list());d$hands[[first+1L]]<-deck[c(1,3,5)];d$hands[[2L-first]]<-deck[c(2,4,6)];d$table<-deck[7:10];d$stock<-deck[11:40];d$won<-list(list(),list());d$sweeps<-list(0,0);d["last"]<-list(NULL);d$turn<-first;d$phase<-"playing";return(d)
  }
  if(kind!="play" || d$phase!="playing") .ra_fail("Play a card during a round or deal between rounds")
  .ra_fields(p,list(card=list(type="integer",minimum=0,maximum=39),take=list(type="array",minItems=0,maxItems=10,items=list(type="integer",minimum=0,maximum=39))),"/params");t<-d$turn;card<-p$card;take<-unlist(p$take)
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(!card %in% unlist(d$hands[[t+1L]]) || anyDuplicated(take) || any(!take %in% unlist(d$table))) .ra_fail("Card must be in hand and distinct selected cards on the table")
  target<-.ra_scopa_rank(card);equals<-unlist(d$table)[.ra_scopa_rank(unlist(d$table))==target]
  if(length(take)) {
    if(sum(.ra_scopa_rank(take))!=target || (length(equals)>0 && length(take)!=1)) .ra_fail("Capture must match rank; a matching single card takes priority over sums")
  } else if(.ra_scopa_possible(d$table,target)) .ra_fail("Available capture is compulsory")
  d$hands[[t+1L]]<-Filter(function(c) c!=card,d$hands[[t+1L]])
  if(length(take)) {
    d$table<-Filter(function(c) !c %in% take,d$table);d$won[[t+1L]]<-c(d$won[[t+1L]],as.list(take),list(card));d$last<-t
    if(!length(d$table) && (length(d$stock)>0 || sum(lengths(d$hands))>0)) d$sweeps[[t+1L]]<-d$sweeps[[t+1L]]+1L
  } else d$table[[length(d$table)+1L]]<-card
  d$turn<-1L-t
  if(sum(lengths(d$hands))==0) {
    if(length(d$stock)) {
      first<-1L-d$dealer;deck<-d$stock;d$hands[[first+1L]]<-deck[c(1,3,5)];d$hands[[2L-first]]<-deck[c(2,4,6)];d$stock<-deck[-(1:6)];d$turn<-first
    } else {
      if(length(d$table)) {
        if(is.null(d$last)) .ra_fail("Final collection requires a last capturing player")
        d$won[[d$last+1L]]<-c(d$won[[d$last+1L]],d$table);d$table<-list()
      }
      points<-.ra_scopa_points(d);d$scores<-as.list(unlist(d$scores)+points);d$phase<-if(max(unlist(d$scores))>=11 && d$scores[[1]]!=d$scores[[2]]) "finished" else "round_end"
    }
  }
  d
}
.ra_scopa_legal <- function(state) {
  d<-state$data;playing<-d$phase=="playing"
  spec<-if(playing) list(card=list(type="integer",minimum=0,maximum=39),take=list(type="array",minItems=0,maxItems=10,items=list(type="integer",minimum=0,maximum=39))) else list(deck=list(type="array",minItems=40,maxItems=40,items=list(type="integer",minimum=0,maximum=39)))
  list(mode="event_types",actions=list(),action_types=list(list(type=if(playing) "play" else "deal",params_schema=spec,preconditions=if(playing) list("Play a held card; capture a matching single table card if present, otherwise a subset with matching sum if available.") else list("Supply a permutation of all 40 cards."),external_observations=if(playing) list() else list("Externally shuffled deck."))),constraints=list(turn=d$turn),completeness=FALSE,reason="guarded_card_selection")
}
.ra_scopa_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";scores<-unlist(d$scores);wins<-if(terminal) which(scores==max(scores)) else integer()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(terminal) lapply(setdiff(1:2,wins),function(p) state$players[[p]]$id) else list(),placements=if(terminal) lapply(1:2,function(p) list(player_id=state$players[[p]]$id,place=1+sum(scores>scores[p]),score=scores[p])) else list(),reason=if(terminal) "round_target_reached" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
