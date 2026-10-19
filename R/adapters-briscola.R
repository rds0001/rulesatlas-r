.ra_briscola_points <- c(11,0,10,0,0,0,0,2,3,4)
.ra_briscola_power <- c(10,1,9,2,3,4,5,6,7,8)
.ra_briscola_winner <- function(a,b,trump) {if(a%/%10==b%/%10) return(if(.ra_briscola_power[b%%10+1L]>.ra_briscola_power[a%%10+1L]) 1L else 0L);if(b%/%10==trump%/%10) 1L else 0L}
.ra_briscola_validate <- function(d) {
  if(!d$configured) {if(sum(lengths(d$hands))+length(d$stock)+sum(lengths(d$won))+length(d$trick)>0 || !is.null(d$trump) || d$leader!=0 || d$turn!=0) .ra_fail("Unconfigured deal contains cards",code="INVALID_STATE");return(invisible(NULL))}
  cards<-unlist(c(d$stock,unlist(d$hands,recursive=FALSE),unlist(d$won,recursive=FALSE),d$trick))
  if(length(cards)!=40 || !all(sort(cards)==0:39)) .ra_fail("Card inventory must contain all forty cards once",code="INVALID_STATE")
  if(is.null(d$trump) || length(d$stock)%%2!=0 || any(lengths(d$won)%%2!=0)) .ra_fail("Invalid trump, stock or captured-pair count",code="INVALID_STATE")
  if(length(d$stock)>0 && tail(unlist(d$stock),1)!=d$trump) .ra_fail("Exposed trump must remain at the bottom of the stock",code="INVALID_STATE")
  if(d$turn!=if(length(d$trick)) 1L-d$leader else d$leader) .ra_fail("Trick and turn mismatch",code="INVALID_STATE")
  sizes<-vapply(1:2,function(p) length(d$hands[[p]])+if(length(d$trick)>0 && p-1L==d$leader) 1L else 0L,numeric(1))
  if(sizes[1]!=sizes[2] || (length(d$stock)>0 && any(sizes!=3))) .ra_fail("Hand sizes do not match trick and draw phase",code="INVALID_STATE")
}
.ra_briscola_apply <- function(state,action) {
  d<-state$data;params<-action$params
  if(!d$configured) {
    if(action$type!="deal") .ra_fail("Supply the shuffled deck first")
    .ra_fields(params,list(cards=list(type="array",minItems=40,maxItems=40,items=list(type="integer",minimum=0,maximum=39))),"/params");cards<-params$cards
    if(anyDuplicated(unlist(cards))) .ra_fail("Deck must contain every card once")
    d$configured<-TRUE;d$hands<-list(cards[c(1,3,5)],cards[c(2,4,6)]);d$stock<-cards[7:40];d$trump<-cards[[40]];return(d)
  }
  if(action$type!="play") .ra_fail("Unknown action")
  .ra_fields(params,list(card=list(type="integer",minimum=0,maximum=39)),"/params");p<-d$turn+1L;card<-params$card
  if(!is.null(action$actor) && action$actor!=state$players[[p]]$id) .ra_fail("Wrong actor")
  if(!card %in% unlist(d$hands[[p]])) .ra_fail("Card is not in the current hand")
  d$hands[[p]]<-Filter(function(c) c!=card,d$hands[[p]]);d$trick[[length(d$trick)+1L]]<-card
  if(length(d$trick)==1) {d$turn<-2L-p;return(d)}
  w<-(d$leader+.ra_briscola_winner(d$trick[[1]],d$trick[[2]],d$trump))%%2+1L
  d$won[[w]]<-c(d$won[[w]],d$trick);d$trick<-list()
  if(length(d$stock)) {d$hands[[w]][[length(d$hands[[w]])+1L]]<-d$stock[[1]];d$hands[[3L-w]][[length(d$hands[[3L-w]])+1L]]<-d$stock[[2]];d$stock<-tail(d$stock,length(d$stock)-2L)}
  d$turn<-w-1L;d$leader<-w-1L;d
}
.ra_briscola_legal <- function(state) {
  d<-state$data
  if(!d$configured) return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(cards=list(type="array",minItems=40,maxItems=40,items=list(type="integer",minimum=0,maximum=39))),preconditions=list("The supplied order is a permutation of all forty cards."),external_observations=list("First six cards alternate between players; the remaining stock ends with the exposed trump."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deck_required"))
  lapply(sort(unlist(d$hands[[d$turn+1L]])),function(c) list(type="play",params=list(card=c)))
}
.ra_briscola_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$configured && sum(lengths(d$hands))==0 && !length(d$trick);scores<-vapply(d$won,function(pile) sum(.ra_briscola_points[unlist(pile)%%10+1L]),numeric(1));tie<-scores[1]==scores[2];w<-if(scores[1]>scores[2]) 1L else 2L
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(tie) "draw" else "win"} else "none",winners=if(terminal && !tie) list(state$players[[w]]$id) else list(),losers=if(terminal && !tie) list(state$players[[3L-w]]$id) else list(),placements=if(terminal) lapply(1:2,function(p) list(player_id=state$players[[p]]$id,place=1L+sum(scores>scores[p]),score=scores[p])) else list(),reason=if(terminal) "twenty_tricks_completed" else if(d$configured) "ongoing" else "awaiting_deck",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
