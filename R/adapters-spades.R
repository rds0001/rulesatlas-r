.ra_spades_ended <- function(d) max(unlist(d$scores))>=500 && d$scores[[1]]!=d$scores[[2]]
.ra_spades_playable <- function(d) {
  p<-(d$leader+length(d$trick))%%4+1L;hand<-sort(unlist(d$hands[[p]]))
  if(length(d$trick)) {following<-hand[hand%/%13==d$trick[[1]]%/%13];return(if(length(following)) following else hand)}
  nontrumps<-hand[hand%/%13!=3];if(.ra_hearts_broken(d) || !length(nontrumps)) hand else nontrumps
}
.ra_spades_validate <- function(d) {
  cards<-unlist(c(unlist(d$hands,recursive=FALSE),unlist(d$won,recursive=FALSE),d$trick))
  if(d$round==0 && d$phase=="awaiting_deal") {if(length(cards)>0 || any(unlist(d$scores)!=0) || any(unlist(d$bags)!=0) || any(vapply(d$bids,function(b) !is.null(b),logical(1))) || d$leader!=0) .ra_fail("Initial game contains prior play",code="INVALID_STATE");return(invisible(NULL))}
  if(length(cards)!=52 || !all(sort(cards)==0:51)) .ra_fail("Card inventory must contain all fifty-two cards once",code="INVALID_STATE")
  if(any(lengths(d$won)%%4!=0)) .ra_fail("Captured piles must contain complete tricks",code="INVALID_STATE")
  if((d$phase=="finished")!=.ra_spades_ended(d)) .ra_fail("Scores and game phase mismatch",code="INVALID_STATE")
  if(d$phase %in% c("awaiting_deal","finished")) {if(sum(lengths(d$hands))+length(d$trick)>0) .ra_fail("Completed deal still contains playable cards",code="INVALID_STATE");return(invisible(NULL))}
  if(d$round==0) .ra_fail("Active deal requires a positive round number",code="INVALID_STATE")
  if(d$phase=="bidding") {
    if(any(lengths(d$hands)!=13) || length(d$trick)>0 || sum(lengths(d$won))>0 || d$leader!=(d$round-1)%%4) .ra_fail("Invalid bidding phase",code="INVALID_STATE")
    seen_empty<-FALSE
    for(i in 0:3) {p<-(d$leader+i)%%4+1L;if(is.null(d$bids[[p]])) seen_empty<-TRUE else if(seen_empty) .ra_fail("Bids must follow player order",code="INVALID_STATE")}
    if(!seen_empty) .ra_fail("Completed bidding must advance",code="INVALID_STATE")
  } else if(d$phase=="playing") {
    if(any(vapply(d$bids,is.null,logical(1)))) .ra_fail("Bidding is incomplete",code="INVALID_STATE")
    sizes<-vapply(1:4,function(p) length(d$hands[[p]])+as.integer((p-1L-d$leader)%%4<length(d$trick)),numeric(1))
    if(length(unique(sizes))!=1 || sum(lengths(d$hands))==0) .ra_fail("Hand sizes do not match the trick",code="INVALID_STATE")
  }
}
.ra_spades_finish <- function(d) {
  tricks<-lengths(d$won)/4
  for(team in 1:2) {
    players<-c(team,team+2L);bid<-sum(unlist(d$bids[players]));taken<-sum(tricks[players[vapply(d$bids[players],function(b) b>0,logical(1))]])
    nils<-players[vapply(d$bids[players],function(b) b==0,logical(1))];bags<-sum(tricks[nils])+max(0,taken-bid)
    points<-(if(taken>=bid) 10*bid else -10*bid)+bags+sum(ifelse(tricks[nils]==0,100,-100))
    accumulated<-d$bags[[team]]+bags;points<-points-100*(accumulated%/%10);d$bags[[team]]<-accumulated%%10;d$scores[[team]]<-d$scores[[team]]+points
  }
  d$phase<-if(.ra_spades_ended(d)) "finished" else "awaiting_deal";d
}
.ra_spades_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(d$phase=="awaiting_deal") {
    if(kind!="deal") .ra_fail("Supply the next shuffled deck")
    .ra_fields(p,list(cards=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51))),"/params")
    if(anyDuplicated(unlist(p$cards))) .ra_fail("Deck must contain each card once")
    d$round<-d$round+1L;d$hands<-lapply(1:4,function(t) p$cards[seq(t,52,by=4)]);d$won<-rep(list(list()),4);d$trick<-list();d$bids<-rep(list(NULL),4);d$leader<-(d$round-1)%%4;d$phase<-"bidding";return(d)
  }
  if(d$phase=="bidding") {
    if(kind!="bid") .ra_fail("Complete bidding before playing")
    .ra_fields(p,list(tricks=list(type="integer",minimum=0,maximum=13)),"/params")
    order<-(d$leader+0:3)%%4+1L;t<-order[vapply(d$bids[order],is.null,logical(1))][1]
    if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
    d$bids[[t]]<-p$tricks
    if(all(vapply(d$bids,function(b) !is.null(b),logical(1)))) d$phase<-"playing"
    return(d)
  }
  if(kind!="play") .ra_fail("Unknown action")
  .ra_fields(p,list(card=list(type="integer",minimum=0,maximum=51)),"/params");t<-(d$leader+length(d$trick))%%4+1L;card<-p$card
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  if(!card %in% .ra_spades_playable(d)) .ra_fail("Card violates ownership, follow-suit or trump restrictions")
  d$hands[[t]]<-Filter(function(c) c!=card,d$hands[[t]]);d$trick[[length(d$trick)+1L]]<-card
  if(length(d$trick)==4) {
    suit<-if(any(unlist(d$trick)%/%13==3)) 3 else d$trick[[1]]%/%13;eligible<-which(unlist(d$trick)%/%13==suit);offset<-eligible[which.max(unlist(d$trick)[eligible]%%13)]-1L;winner<-(d$leader+offset)%%4+1L
    d$won[[winner]]<-c(d$won[[winner]],d$trick);d$trick<-list();d$leader<-winner-1L
    if(sum(lengths(d$hands))==0) d<-.ra_spades_finish(d)
  }
  d
}
.ra_spades_legal <- function(state) {
  d<-state$data
  if(d$phase=="playing") return(lapply(.ra_spades_playable(d),function(c) list(type="play",params=list(card=c))))
  if(d$phase=="bidding") return(lapply(0:13,function(n) list(type="bid",params=list(tricks=n))))
  list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(cards=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51))),preconditions=list("The supplied deck is a permutation of all fifty-two cards."),external_observations=list("Supplied shuffled deck."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deck_required")
}
.ra_spades_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";team<-if(d$scores[[1]]>d$scores[[2]]) 0L else 1L
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) "win" else "none",winners=if(terminal) lapply(c(team,team+2),function(p) state$players[[p+1L]]$id) else list(),losers=if(terminal) lapply(c(1L-team,3L-team),function(p) state$players[[p+1L]]$id) else list(),placements=if(terminal) lapply(0:3,function(p) list(player_id=state$players[[p+1L]]$id,place=if(p%%2==team) 1L else 2L,score=d$scores[[p%%2+1L]],bags=d$bags[[p%%2+1L]])) else list(),reason=if(terminal) "five_hundred_point_game_completed" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
