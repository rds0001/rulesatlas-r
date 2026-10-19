.ra_rummy_rank <- function(c) c%%13+1L
.ra_rummy_suit <- function(c) c%/%13
.ra_rummy_type <- function(cards) {
  x<-unlist(cards);n<-length(x);r<-sort(.ra_rummy_rank(x))
  if(n<3 || anyDuplicated(x)) return(NULL)
  if(n<=4 && length(unique(r))==1) return("set")
  if(length(unique(.ra_rummy_suit(x)))==1 && identical(as.integer(r),seq.int(r[1],length.out=n)) && tail(r,1)<=13) return("run")
  NULL
}
.ra_rummy_sort <- function(x) {v<-unlist(x);as.list(v[order(.ra_rummy_rank(v),.ra_rummy_suit(v))])}
.ra_rummy_finish <- function(d) {if(!length(d$hands[[d$turn+1L]])) {d$phase<-"finished";d$winner<-d$turn};d}
.ra_rummy_validate <- function(d) {
  if(d$phase=="setup") {
    if(any(lengths(d$hands)) || length(d$stock) || length(d$discard) || length(d$melds) || !is.null(d$winner) || !is.null(d$drawn_discard) || d$agreed_draw) .ra_fail("Setup must be empty",code="INVALID_STATE")
    return(invisible(NULL))
  }
  cards<-c(unlist(d$hands),unlist(d$stock),unlist(d$discard),unlist(lapply(d$melds,function(m) m$cards)))
  if(length(cards)!=52 || !identical(as.integer(sort(cards)),0:51)) .ra_fail("Every card must occur exactly once",code="INVALID_STATE")
  for(m in d$melds) if(!identical(.ra_rummy_type(m$cards),m$kind)) .ra_fail("Invalid group or same-suit run",code="INVALID_STATE")
  if(d$phase=="draw" && !is.null(d$drawn_discard)) .ra_fail("A new turn must clear the discarded-card restriction",code="INVALID_STATE")
  if(d$phase=="finished") {
    if(d$agreed_draw) {if(!is.null(d$winner)) .ra_fail("An agreed draw has no winner",code="INVALID_STATE")}
    else if(is.null(d$winner) || d$winner!=d$turn || length(d$hands[[d$winner+1L]])) .ra_fail("Winner must have emptied the current hand",code="INVALID_STATE")
  } else if(!is.null(d$winner) || d$agreed_draw || any(lengths(d$hands)==0)) .ra_fail("An empty hand must finish the round",code="INVALID_STATE")
}
.ra_rummy_apply <- function(state,action) {
  d<-state$data;p<-action$params;kind<-action$type;t<-d$turn;card_spec<-list(type="integer",minimum=0,maximum=51)
  if(kind=="deal") {
    .ra_fields(p,list(deck=list(type="array",minItems=52,maxItems=52,items=card_spec)),"/params")
    if(d$phase!="setup" || anyDuplicated(unlist(p$deck))) .ra_fail("Supply the full deck once")
    d$hands<-list(p$deck[seq(1,20,2)],p$deck[seq(2,20,2)]);d$discard<-p$deck[21];d$stock<-p$deck[22:52];d$phase<-"draw";return(d)
  }
  if(kind %in% c("offer_draw","accept_draw")) {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");q<-p$player
    if(d$phase=="setup") .ra_fail("Deal before requesting a draw")
    if(!is.null(action$actor) && action$actor!=state$players[[q+1L]]$id) .ra_fail("Wrong actor")
    if(kind=="offer_draw") d$offer<-q else {
      if(is.null(d$offer) || d$offer==q) .ra_fail("Only the opponent may accept a draw offer")
      d$agreed_draw<-TRUE;d$phase<-"finished"
    }
    return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(kind=="recycle") {
    .ra_fields(p,list(order=list(type="array",minItems=1,maxItems=51,items=card_spec)),"/params")
    if(d$phase!="draw" || length(d$stock) || length(d$discard)<2 || !identical(sort(unlist(p$order)),sort(unlist(head(d$discard,-1))))) .ra_fail("Recycle all discards except the top only when the stock is empty")
    d$stock<-p$order;d$discard<-tail(d$discard,1);return(d)
  }
  if(kind=="draw") {
    .ra_fields(p,list(source=list(type="string",enum=list("stock","discard"))),"/params")
    if(d$phase!="draw" || !length(d[[p$source]])) .ra_fail("Draw exactly one available card before melding")
    i<-if(p$source=="stock") 1L else length(d$discard);c<-d[[p$source]][[i]];d[[p$source]]<-d[[p$source]][-i];d$hands[[t+1L]]<-c(d$hands[[t+1L]],list(c));d["drawn_discard"]<-list(if(p$source=="discard") c else NULL);d$phase<-"meld";return(d)
  }
  if(d$phase!="meld") .ra_fail("Draw a card before melding, laying off or discarding")
  if(kind=="meld") {
    .ra_fields(p,list(cards=list(type="array",minItems=3,maxItems=13,items=card_spec)),"/params");mkind<-.ra_rummy_type(p$cards)
    if(is.null(mkind) || any(!unlist(p$cards) %in% unlist(d$hands[[t+1L]]))) .ra_fail("Meld must consist of distinct held cards forming a group or run")
    d$hands[[t+1L]]<-Filter(function(c) !c %in% unlist(p$cards),d$hands[[t+1L]]);d$melds[[length(d$melds)+1L]]<-list(cards=.ra_rummy_sort(p$cards),kind=mkind,owner=t);return(.ra_rummy_finish(d))
  }
  if(kind=="lay_off") {
    .ra_fields(p,list(card=card_spec,meld=list(type="integer",minimum=0,maximum=16)),"/params");i<-p$meld+1L;c<-p$card
    if(i>length(d$melds) || !c %in% unlist(d$hands[[t+1L]]) || !identical(.ra_rummy_type(c(d$melds[[i]]$cards,list(c))),d$melds[[i]]$kind)) .ra_fail("Held card must legally extend the selected existing meld")
    d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$melds[[i]]$cards<-.ra_rummy_sort(c(d$melds[[i]]$cards,list(c)));return(.ra_rummy_finish(d))
  }
  if(kind=="discard") {
    .ra_fields(p,list(card=card_spec),"/params");c<-p$card
    if(!c %in% unlist(d$hands[[t+1L]]) || (!is.null(d$drawn_discard) && c==d$drawn_discard)) .ra_fail("Discard a held card other than the card just taken from the discard pile")
    d$hands[[t+1L]]<-Filter(function(x) x!=c,d$hands[[t+1L]]);d$discard<-c(d$discard,list(c));d<-.ra_rummy_finish(d)
    if(d$phase!="finished") {d$turn<-1L-t;d$phase<-"draw";d["drawn_discard"]<-list(NULL)}
    if(!is.null(d$offer) && d$offer!=t) d["offer"]<-list(NULL)
    return(d)
  }
  .ra_fail("Unknown action")
}
.ra_rummy_legal <- function(state) {
  d<-state$data;t<-d$turn;cs<-list(type="integer",minimum=0,maximum=51)
  if(d$phase=="setup") return(list(mode="event_types",actions=list(),action_types=list(list(type="deal",params_schema=list(deck=list(type="array",minItems=52,maxItems=52,items=cs)),preconditions=list("Supply every card once, in deal/draw order."),external_observations=list("External shuffled deck."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_deal"))
  actions<-lapply(0:1,function(p) list(type="offer_draw",params=list(player=p)));types<-list()
  if(!is.null(d$offer)) actions<-c(actions,list(list(type="accept_draw",params=list(player=1L-d$offer))))
  if(d$phase=="draw") {
    for(s in c("stock","discard")) if(length(d[[s]])) actions<-c(actions,list(list(type="draw",params=list(source=s))))
    if(!length(d$stock) && length(d$discard)>1) types<-list(list(type="recycle",params_schema=list(order=list(type="array",minItems=1,maxItems=51,items=cs)),preconditions=list("Permutation of all discards except the retained top card."),external_observations=list("External reshuffle order.")))
  } else {
    hand<-d$hands[[t+1L]];groups<-list()
    for(r in 1:13) {same<-Filter(function(c) .ra_rummy_rank(c)==r,hand);for(n in 3:4) if(length(same)>=n) groups<-c(groups,utils::combn(same,n,simplify=FALSE))}
    for(s in 0:3) {
      same<-.ra_rummy_sort(Filter(function(c) .ra_rummy_suit(c)==s,hand));n<-length(same)
      if(n>=3) for(i in seq_len(n-2L)) for(j in seq.int(i+2L,n)) if(identical(.ra_rummy_type(same[i:j]),"run")) groups<-c(groups,list(same[i:j]))
    }
    actions<-c(actions,lapply(groups,function(g) list(type="meld",params=list(cards=g))))
    for(c in hand) for(i in seq_along(d$melds)) if(identical(.ra_rummy_type(c(d$melds[[i]]$cards,list(c))),d$melds[[i]]$kind)) actions<-c(actions,list(list(type="lay_off",params=list(card=c,meld=i-1L))))
    for(c in hand) if(is.null(d$drawn_discard) || c!=d$drawn_discard) actions<-c(actions,list(list(type="discard",params=list(card=c))))
  }
  list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=t,phase=d$phase),completeness=FALSE,reason="legal_card_actions_and_external_recycle")
}
.ra_rummy_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";w<-d$winner;points<-vapply(d$hands,function(h) sum(pmin(10,.ra_rummy_rank(unlist(h)))),numeric(1))
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) if(d$agreed_draw) "draw" else "win" else "none",winners=if(done && !is.null(w)) list(state$players[[w+1L]]$id) else list(),losers=if(done && !is.null(w)) list(state$players[[2L-w]]$id) else list(),placements=if(done) lapply(0:1,function(p) list(player_id=state$players[[p+1L]]$id,place=if(d$agreed_draw || (!is.null(w) && p==w)) 1L else 2L,hand_points=points[p+1L],round_score=if(!is.null(w) && p==w) points[2L-p] else 0)) else list(),reason=if(done) if(d$agreed_draw) "agreed_draw" else "hand_emptied" else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
