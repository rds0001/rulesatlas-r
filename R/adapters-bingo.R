.ra_bingo_cards <- list(type="array",minItems=2,maxItems=2,items=list(type="array",minItems=25,maxItems=25,items=list(type="integer",minimum=0,maximum=75)))
.ra_bingo_lines <- c(lapply(0:4,function(r) r*5+0:4),lapply(0:4,function(c) (0:4)*5+c),list(c(0,6,12,18,24),c(4,8,12,16,20)))
.ra_bingo_winners <- function(d,calls=d$called) {
  if(!d$configured) return(integer())
  called<-c(0,unlist(calls))
  Filter(function(p) any(vapply(.ra_bingo_lines,function(line) all(unlist(d$cards[[p]][line+1L]) %in% called),logical(1))),1:2)
}
.ra_bingo_validate <- function(d) {
  if(!d$configured) {if(length(d$cards)+length(d$called)>0) .ra_fail("Unconfigured game contains data",code="INVALID_STATE");return(invisible(NULL))}
  if(length(d$cards)!=2) .ra_fail("Two cards are required",code="INVALID_STATE")
  for(card in d$cards) {
    if(card[[13]]!=0 || anyDuplicated(unlist(card))) .ra_fail("Card requires a free center and distinct numbers",code="INVALID_STATE")
    for(i in setdiff(0:24,12)) if(card[[i+1L]]<15*(i%%5)+1 || card[[i+1L]]>15*(i%%5+1)) .ra_fail("Number is outside its column range",code="INVALID_STATE")
  }
  if(anyDuplicated(unlist(d$called))) .ra_fail("Number has been called twice",code="INVALID_STATE")
  if(length(d$called)>0 && length(.ra_bingo_winners(d,head(d$called,-1)))>0) .ra_fail("Calls continued after bingo",code="INVALID_STATE")
}
.ra_bingo_apply <- function(state,action) {
  d<-state$data;p<-action$params
  if(!d$configured) {
    if(action$type!="set_cards") .ra_fail("Supply both cards first")
    .ra_fields(p,list(cards=.ra_bingo_cards),"/params");d$configured<-TRUE;d$cards<-p$cards;return(d)
  }
  if(action$type!="call") .ra_fail("Unknown action")
  .ra_fields(p,list(number=list(type="integer",minimum=1,maximum=75)),"/params")
  if(p$number %in% unlist(d$called)) .ra_fail("Number has already been called")
  d$called[[length(d$called)+1L]]<-p$number;d
}
.ra_bingo_legal <- function(state) {
  d<-state$data
  if(!d$configured) return(list(mode="event_types",actions=list(),action_types=list(list(type="set_cards",params_schema=list(cards=.ra_bingo_cards),preconditions=list("Each card has distinct numbers in the B/I/N/G/O column ranges and zero at the free center."),external_observations=list("Two supplied 5-by-5 bingo cards in row-major order."))),constraints=structure(list(),names=character()),completeness=FALSE,reason="external_cards_required"))
  lapply(setdiff(1:75,unlist(d$called)),function(n) list(type="call",params=list(number=n)))
}
.ra_bingo_outcome <- function(state,pack) {
  wins<-.ra_bingo_winners(state$data)
  list(status=if(length(wins)) "terminal" else "ongoing",result_kind=if(length(wins)) {if(length(wins)>1) "draw" else "win"} else "none",winners=lapply(wins,function(p) state$players[[p]]$id),losers=if(length(wins)) lapply(setdiff(1:2,wins),function(p) state$players[[p]]$id) else list(),placements=list(),reason=if(length(wins)) "completed_line" else if(state$data$configured) "ongoing" else "awaiting_cards",evidence_required=list(),rule_id=if(length(wins)) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
