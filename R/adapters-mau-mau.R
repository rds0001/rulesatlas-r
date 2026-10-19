.ra_mmau_rank <- function(c) c%%8
.ra_mmau_suit <- function(c) c%/%8
.ra_mmau_playable <- function(d,c) {
  if(d$penalty>0) return(.ra_mmau_rank(c)==0)
  if(.ra_mmau_rank(c)==4) return(.ra_mmau_rank(tail(unlist(d$discard),1))!=4)
  .ra_mmau_suit(c)==d$suit || .ra_mmau_rank(c)==.ra_mmau_rank(tail(unlist(d$discard),1))
}
.ra_mmau_validate <- function(d) {
  if(d$phase=="setup") {
    if(any(lengths(d$hands)) || length(d$stock) || length(d$discard) || d$penalty>0 || !is.null(d$winner) || d$agreed_draw) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$hands),unlist(d$stock),unlist(d$discard))
  if(length(cards)!=32 || !identical(as.integer(sort(cards)),0:31) || !length(d$discard)) .ra_fail("Every card must occur once and a top discard is required",code="INVALID_STATE")
  top<-tail(unlist(d$discard),1)
  if(.ra_mmau_rank(top)!=4 && d$suit!=.ra_mmau_suit(top)) .ra_fail("Active suit must match the top card except for a jack",code="INVALID_STATE")
  if(d$penalty>0 && (.ra_mmau_rank(top)!=0 || d$penalty%%2!=0)) .ra_fail("Only sevens carry even draw penalties",code="INVALID_STATE")
  if(d$phase=="finished") {
    if(d$agreed_draw) {if(!is.null(d$winner)) .ra_fail("An agreed draw has no winner",code="INVALID_STATE")}
    else if(is.null(d$winner) || length(d$hands[[d$winner+1L]]) || d$winner!=d$turn) .ra_fail("Winner must have emptied the current hand",code="INVALID_STATE")
  } else if(!is.null(d$winner) || d$agreed_draw || any(lengths(d$hands)==0)) .ra_fail("An empty hand ends the game",code="INVALID_STATE")
}
.ra_mmau_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn;cs<-list(type="integer",minimum=0,maximum=31)
  if(k=="deal") {
    .ra_fields(p,list(deck=list(type="array",minItems=32,maxItems=32,items=cs)),"/params")
    if(d$phase!="setup" || anyDuplicated(unlist(p$deck)) || .ra_mmau_rank(p$deck[[11]])==4) .ra_fail("Deal the complete deck with a non-jack opening discard")
    d$hands<-list(p$deck[seq(1,10,2)],p$deck[seq(2,10,2)]);d$discard<-p$deck[11];d$stock<-p$deck[12:32];d$suit<-.ra_mmau_suit(d$discard[[1]]);r<-.ra_mmau_rank(d$discard[[1]]);d$penalty<-if(r==0) 2L else 0L;d$turn<-if(r==1) 1L else 0L;d$phase<-"play";return(d)
  }
  if(d$phase!="play") .ra_fail("Deal before playing")
  if(k %in% c("offer_draw","accept_draw")) {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");q<-p$player
    if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong actor")
    if(k=="offer_draw") d$offer<-q else {
      if(is.null(d$offer) || d$offer==q) .ra_fail("Only the opponent may accept a draw offer")
      d$agreed_draw<-TRUE;d$phase<-"finished"
    }
    return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(k=="recycle") {
    .ra_fields(p,list(order=list(type="array",minItems=1,maxItems=31,items=cs)),"/params")
    if(length(d$stock)>=max(1,d$penalty) || length(d$discard)<2 || !identical(sort(unlist(p$order)),sort(unlist(head(d$discard,-1))))) .ra_fail("Recycle all older discards only when the stock cannot satisfy the draw")
    d$stock<-c(d$stock,p$order);d$discard<-tail(d$discard,1);return(d)
  }
  if(k=="play") {
    .ra_fields(p,list(card=cs,suit=list(type="integer",minimum=0,maximum=3)),"/params");c<-p$card;r<-.ra_mmau_rank(c)
    if(!c %in% unlist(d$hands[[t+1L]]) || !.ra_mmau_playable(d,c) || (r!=4 && p$suit!=.ra_mmau_suit(c))) .ra_fail("Play a matching held card, or a jack on a non-jack; respond to sevens only with sevens")
    d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$discard<-c(d$discard,list(c));d$suit<-p$suit;d$penalty<-if(r==0) d$penalty+2L else 0L
    if(!length(d$hands[[t+1L]])) {d$winner<-t;d$phase<-"finished"} else if(r!=1) d$turn<-1L-t
  } else if(k=="draw") {
    .ra_fields(p,structure(list(),names=character()),"/params");n<-max(1,d$penalty)
    if(length(d$stock)<n && length(d$discard)>1) .ra_fail("Recycle available discards before drawing")
    n<-min(n,length(d$stock));d$hands[[t+1L]]<-c(d$hands[[t+1L]],head(d$stock,n));d$stock<-tail(d$stock,length(d$stock)-n);d$penalty<-0L;d$turn<-1L-t
  } else .ra_fail("Unknown action")
  if(!is.null(d$offer) && d$offer!=t) d["offer"]<-list(NULL)
  d
}
.ra_mmau_legal <- function(state) {
  d<-state$data;t<-d$turn;types<-list();actions<-list();cs<-list(type="integer",minimum=0,maximum=31)
  if(d$phase=="setup") types<-list(list(type="deal",params_schema=list(deck=list(type="array",minItems=32,maxItems=32,items=cs)),preconditions=list("Every card once; card ten (zero-based) must not be a jack."),external_observations=list("External deck order."))) else {
    actions<-lapply(0:1,function(p) list(type="offer_draw",params=list(player=p)))
    if(!is.null(d$offer)) actions<-c(actions,list(list(type="accept_draw",params=list(player=1L-d$offer))))
    for(c in d$hands[[t+1L]]) if(.ra_mmau_playable(d,c)) for(s in if(.ra_mmau_rank(c)==4) 0:3 else .ra_mmau_suit(c)) actions<-c(actions,list(list(type="play",params=list(card=c,suit=s))))
    if(length(d$stock)>=max(1,d$penalty) || length(d$discard)==1) actions<-c(actions,list(list(type="draw",params=structure(list(),names=character())))) else types<-list(list(type="recycle",params_schema=list(order=list(type="array",minItems=1,maxItems=31,items=cs)),preconditions=list("Permutation of all discards except the top; existing stock stays first."),external_observations=list("External reshuffle order.")))
  }
  list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=t,penalty=d$penalty),completeness=FALSE,reason="card_actions_and_external_deck_order")
}
.ra_mmau_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";w<-d$winner
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(d$agreed_draw) "draw" else "win" else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) if(d$agreed_draw) "agreed_draw" else "hand_emptied" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
