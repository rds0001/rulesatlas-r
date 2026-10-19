.ra_hearts_points <- function(card) if(card%/%13==3) 1L else if(card==36) 13L else 0L
.ra_hearts_broken <- function(d) any(unlist(c(unlist(d$won,recursive=FALSE),d$trick))%/%13==3)
.ra_hearts_playable <- function(d) {
  p<-(d$leader+length(d$trick))%%4+1L;hand<-sort(unlist(d$hands[[p]]));first<-sum(lengths(d$won))==0
  if(!length(d$trick)) {
    if(first) return(if(0 %in% hand) 0L else numeric())
    nonhearts<-hand[hand%/%13!=3];return(if(.ra_hearts_broken(d) || !length(nonhearts)) hand else nonhearts)
  }
  suit<-d$trick[[1]]%/%13;following<-hand[hand%/%13==suit]
  if(length(following)) return(following)
  nonpoints<-Filter(function(c) .ra_hearts_points(c)==0,hand)
  if(first && length(nonpoints)) nonpoints else hand
}
.ra_hearts_validate <- function(d) {
  cards<-unlist(c(unlist(d$hands,recursive=FALSE),unlist(d$won,recursive=FALSE),d$trick))
  if(d$round==0 && d$phase=="awaiting_deal") {if(length(cards)>0 || any(unlist(d$scores)!=0) || any(vapply(d$passes,function(p) !is.null(p),logical(1))) || d$leader!=0) .ra_fail("Initial game contains prior play",code="INVALID_STATE");return(invisible(NULL))}
  if(length(cards)!=52 || !all(sort(cards)==0:51)) .ra_fail("Card inventory must contain all fifty-two cards once",code="INVALID_STATE")
  if(any(lengths(d$won)%%4!=0)) .ra_fail("Captured piles must contain complete tricks",code="INVALID_STATE")
  if((d$phase=="finished")!=(max(unlist(d$scores))>=100)) .ra_fail("Scores and game phase mismatch",code="INVALID_STATE")
  if(d$phase %in% c("awaiting_deal","finished")) {if(sum(lengths(d$hands))+length(d$trick)>0) .ra_fail("Completed deal still contains playable cards",code="INVALID_STATE");return(invisible(NULL))}
  if(d$round==0) .ra_fail("Active deal requires a positive round number",code="INVALID_STATE")
  if(d$phase=="passing") {
    if((d$round-1)%%4==3 || any(lengths(d$hands)!=13) || length(d$trick)>0 || sum(lengths(d$won))>0) .ra_fail("Invalid passing phase",code="INVALID_STATE")
    for(p in 1:4) {cards<-d$passes[[p]];if(!is.null(cards) && (anyDuplicated(unlist(cards)) || !all(unlist(cards) %in% unlist(d$hands[[p]])))) .ra_fail("Pass must select three cards from its player hand",code="INVALID_STATE")}
    if(all(vapply(d$passes,function(c) !is.null(c),logical(1)))) .ra_fail("Completed passing must advance to play",code="INVALID_STATE")
  } else if(d$phase=="playing") {
    if((d$round-1)%%4==3) {if(any(vapply(d$passes,function(c) !is.null(c),logical(1)))) .ra_fail("No-pass round cannot contain pass selections",code="INVALID_STATE")} else if(any(vapply(d$passes,is.null,logical(1)))) .ra_fail("Passing is incomplete",code="INVALID_STATE")
    sizes<-vapply(1:4,function(p) length(d$hands[[p]])+as.integer((p-1L-d$leader)%%4<length(d$trick)),numeric(1))
    if(length(unique(sizes))!=1 || sum(lengths(d$hands))==0) .ra_fail("Hand sizes do not match the trick",code="INVALID_STATE")
    if(sum(lengths(d$won))==0 && ((length(d$trick)>0 && d$trick[[1]]!=0) || (!length(d$trick) && !0 %in% unlist(d$hands[[d$leader+1L]])))) .ra_fail("Two of clubs must lead the first trick",code="INVALID_STATE")
  }
}
.ra_hearts_finish <- function(d) {
  scores<-vapply(d$won,function(pile) sum(vapply(pile,.ra_hearts_points,numeric(1))),numeric(1))
  if(26 %in% scores) {moon<-which(scores==26)[1];scores<-rep(26,4);scores[moon]<-0}
  d$scores<-as.list(unlist(d$scores)+scores);d$phase<-if(max(unlist(d$scores))>=100) "finished" else "awaiting_deal";d
}
.ra_hearts_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type
  if(d$phase=="awaiting_deal") {
    if(kind!="deal") .ra_fail("Supply the next shuffled deck")
    .ra_fields(p,list(cards=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51))),"/params")
    if(anyDuplicated(unlist(p$cards))) .ra_fail("Deck must contain each card once")
    d$round<-d$round+1L;d$hands<-lapply(1:4,function(t) p$cards[seq(t,52,by=4)]);d$won<-rep(list(list()),4);d$trick<-list();d$passes<-rep(list(NULL),4);d$leader<-which(vapply(d$hands,function(h) 0 %in% unlist(h),logical(1)))[1]-1L;d$phase<-if((d$round-1)%%4==3) "playing" else "passing";return(d)
  }
  if(d$phase=="passing") {
    if(kind!="pass_cards") .ra_fail("Complete all card passes before playing")
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=3),cards=list(type="array",minItems=3,maxItems=3,items=list(type="integer",minimum=0,maximum=51))),"/params");t<-p$player+1L
    if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
    if(!is.null(d$passes[[t]]) || anyDuplicated(unlist(p$cards)) || !all(unlist(p$cards) %in% unlist(d$hands[[t]]))) .ra_fail("Pass is repeated or does not select three owned cards")
    d$passes[[t]]<-p$cards
    if(all(vapply(d$passes,function(c) !is.null(c),logical(1)))) {
      shift<-c(1,3,2)[(d$round-1)%%4+1L]
      d$hands<-lapply(1:4,function(i) Filter(function(c) !c %in% unlist(d$passes[[i]]),d$hands[[i]]))
      for(i in 1:4) {target<-(i-1L+shift)%%4+1L;d$hands[[target]]<-c(d$hands[[target]],d$passes[[i]])}
      d$leader<-which(vapply(d$hands,function(h) 0 %in% unlist(h),logical(1)))[1]-1L;d$phase<-"playing"
    }
    return(d)
  }
  if(kind!="play") .ra_fail("Unknown action")
  .ra_fields(p,list(card=list(type="integer",minimum=0,maximum=51)),"/params");t<-(d$leader+length(d$trick))%%4+1L;card<-p$card
  if(!is.null(action$actor) && action$actor!=state$players[[t]]$id) .ra_fail("Wrong actor")
  if(!card %in% .ra_hearts_playable(d)) .ra_fail("Card violates ownership, follow-suit or heart restrictions")
  d$hands[[t]]<-Filter(function(c) c!=card,d$hands[[t]]);d$trick[[length(d$trick)+1L]]<-card
  if(length(d$trick)==4) {
    suit<-d$trick[[1]]%/%13;eligible<-which(unlist(d$trick)%/%13==suit);offset<-eligible[which.max(unlist(d$trick)[eligible]%%13)]-1L;winner<-(d$leader+offset)%%4+1L
    d$won[[winner]]<-c(d$won[[winner]],d$trick);d$trick<-list();d$leader<-winner-1L
    if(sum(lengths(d$hands))==0) d<-.ra_hearts_finish(d)
  }
  d
}
.ra_hearts_legal <- function(state) {
  d<-state$data
  if(d$phase=="playing") return(lapply(.ra_hearts_playable(d),function(c) list(type="play",params=list(card=c))))
  if(d$phase=="awaiting_deal") schemas<-list(deal=list(cards=list(type="array",minItems=52,maxItems=52,items=list(type="integer",minimum=0,maximum=51)))) else schemas<-list(pass_cards=list(player=list(type="integer",minimum=0,maximum=3),cards=list(type="array",minItems=3,maxItems=3,items=list(type="integer",minimum=0,maximum=51))))
  types<-lapply(names(schemas),function(k) list(type=k,params_schema=schemas[[k]],preconditions=list("Deck cards are unique; each pass selects three owned cards once per player."),external_observations=list("Supplied shuffled deck or player pass selection.")))
  list(mode="event_types",actions=list(),action_types=types,constraints=list(phase=d$phase,pass_shift=if(d$round>0) c(1,3,2,0)[(d$round-1)%%4+1L] else NULL),completeness=FALSE,reason="external_deal_or_guarded_card_pass")
}
.ra_hearts_outcome <- function(state,pack) {
  d<-state$data;terminal<-d$phase=="finished";scores<-unlist(d$scores);winners<-if(terminal) lapply(which(scores==min(scores)),function(p) state$players[[p]]$id) else list()
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(length(winners)>1) "draw" else "win"} else "none",winners=winners,losers=if(terminal) lapply(Filter(function(p) !p$id %in% unlist(winners),state$players),function(p) p$id) else list(),placements=if(terminal) lapply(1:4,function(p) list(player_id=state$players[[p]]$id,place=1L+sum(scores<scores[p]),score=scores[p])) else list(),reason=if(terminal) "hundred_point_game_completed" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
