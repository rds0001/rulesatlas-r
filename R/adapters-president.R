.ra_president_rank <- function(c) (c%%13-1)%%13
.ra_president_living <- function(d) which(lengths(d$hands)>0)-1L
.ra_president_next <- function(d,p,eligible) {for(i in 1:3) {q<-(p+i)%%3;if(q %in% eligible) return(q)}}
.ra_president_resolve <- function(d,played) {
  alive<-.ra_president_living(d);last<-d$last$player;eligible<-alive[!unlist(d$passed)[alive+1L] & alive!=last]
  if(!length(eligible)) {
    d["last"]<-list(NULL);d$passed<-list(FALSE,FALSE,FALSE);d$turn<-if(last %in% alive) last else .ra_president_next(d,last,alive)
  } else d$turn<-.ra_president_next(d,if(played) last else d$turn,eligible)
  d
}
.ra_president_validate <- function(d) {
  if(d$phase=="setup") {
    if(sum(lengths(d$hands))>0 || length(d$discard) || length(d$order) || !is.null(d$last) || !d$opening) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$hands),unlist(d$discard))
  if(length(cards)!=52 || !identical(as.integer(sort(cards)),0:51)) .ra_fail("Every card must occur exactly once",code="INVALID_STATE")
  order<-unlist(d$order);alive<-.ra_president_living(d)
  if(anyDuplicated(order)) .ra_fail("Finishing order must contain distinct players",code="INVALID_STATE")
  if(d$phase=="finished") {
    if(length(order)!=3 || length(alive)!=1 || tail(order,1)!=alive[1] || any(lengths(d$hands)[head(order,-1)+1L]>0)) .ra_fail("Final ranking must put the remaining hand last",code="INVALID_STATE")
  } else if(length(alive)<2 || !setequal(order,setdiff(0:2,alive)) || !d$turn %in% alive || d$passed[[d$turn+1L]]) .ra_fail("Active players, turn and finishing order mismatch",code="INVALID_STATE")
  if(d$opening && (length(d$discard)>0 || !is.null(d$last) || !1 %in% unlist(d$hands[[d$turn+1L]]))) .ra_fail("Opening belongs to the holder of the three of clubs",code="INVALID_STATE")
  if(is.null(d$last)) {
    if(any(unlist(d$passed))) .ra_fail("A new trick clears all passes",code="INVALID_STATE")
  } else {
    last<-d$last;n<-last$count;cards<-tail(unlist(d$discard),n)
    if(length(cards)!=n || any(.ra_president_rank(cards)!=last$rank) || d$passed[[last$player+1L]]) .ra_fail("Last group and discarded cards mismatch",code="INVALID_STATE")
    if(d$phase=="playing" && d$turn==last$player) .ra_fail("A player cannot overbid their own group",code="INVALID_STATE")
  }
}
.ra_president_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn
  if(kind=="deal") {
    .ra_fields(p,list(deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51))),"/params")
    if(d$phase!="setup" || anyDuplicated(unlist(p$deck))) .ra_fail("Supply a complete deck for the initial deal")
    d$hands<-lapply(0:2,function(q) p$deck[seq(q+1,52,by=3)]);d$turn<-which(vapply(d$hands,function(h) 1 %in% unlist(h),logical(1)))-1L;d$phase<-"playing";return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(kind=="pass") {
    .ra_fields(p,structure(list(),names=character()),"/params")
    if(is.null(d$last)) .ra_fail("The leader must play a group")
    d$passed[[t+1L]]<-TRUE;return(.ra_president_resolve(d,FALSE))
  }
  if(kind!="play") .ra_fail("Unknown action")
  .ra_fields(p,list(cards=list(type="array",minItems=1,maxItems=4,items=list(type="integer",minimum=0,maximum=51))),"/params");cards<-unlist(p$cards)
  if(anyDuplicated(cards) || any(!cards %in% unlist(d$hands[[t+1L]])) || length(unique(.ra_president_rank(cards)))!=1) .ra_fail("Play distinct held cards of one rank")
  if(d$opening && !1 %in% cards) .ra_fail("Opening group must contain the three of clubs")
  if(!is.null(d$last) && (length(cards)!=d$last$count || .ra_president_rank(cards[1])<=d$last$rank)) .ra_fail("Match the group size and strictly exceed its rank")
  d$hands[[t+1L]]<-Filter(function(c) !c %in% cards,d$hands[[t+1L]]);d$discard<-c(d$discard,as.list(cards));d$opening<-FALSE;d$last<-list(player=t,count=length(cards),rank=.ra_president_rank(cards[1]))
  if(!length(d$hands[[t+1L]])) d$order<-c(d$order,list(t))
  if(length(d$order)==2) {d$order<-c(d$order,as.list(.ra_president_living(d)));d$phase<-"finished";return(d)}
  .ra_president_resolve(d,TRUE)
}
.ra_president_legal <- function(state) {
  d<-state$data
  if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51))),preconditions=list("Permutation of all 52 cards."),external_observations=list("Externally shuffled deck."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deal"))
  actions<-if(!is.null(d$last)) list(list(type="pass",params=structure(list(),names=character()))) else list()
  for(r in 0:12) {
    hand<-sort(unlist(Filter(function(c) .ra_president_rank(c)==r,d$hands[[d$turn+1L]])))
    sizes<-if(!is.null(d$last)) d$last$count else seq_along(hand)
    if(!is.null(d$last) && r<=d$last$rank) next
    for(n in sizes) if(length(hand)>=n) {
      groups<-utils::combn(seq_along(hand),n,FUN=function(ix) hand[ix],simplify=FALSE)
      for(group in groups) if(!d$opening || 1 %in% group) actions[[length(actions)+1L]]<-list(type="play",params=list(cards=as.list(group)))
    }
  }
  actions
}
.ra_president_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";order<-unlist(d$order)
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=if(terminal) list(state$players[[order[1]+1L]]$id) else list(),losers=if(terminal) lapply(order[-1],function(p) state$players[[p+1L]]$id) else list(),placements=if(terminal) lapply(seq_along(order),function(i) list(player_id=state$players[[order[i]+1L]]$id,place=i)) else list(),reason=if(terminal) "single_deal_ranking_complete" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
