.ra_9_validate <- function(d) {
  if(sum(unlist(d$fouls))>d$shots || (d$phase!="break" && d$shots==0) || (d$phase=="push_choice" && d$shots<2)) .ra_fail("Shot count is inconsistent with play or fouls",code="INVALID_STATE")
  if(anyDuplicated(unlist(d$remaining))) .ra_fail("Remaining object balls must be distinct",code="INVALID_STATE")
  if(d$phase!="finished" && !9 %in% unlist(d$remaining)) .ra_fail("The nine remains on the table until a legal winning shot",code="INVALID_STATE")
  if(d$phase=="break" && (!identical(as.integer(sort(unlist(d$remaining))),1:9) || d$shots>0 || any(unlist(d$fouls)>0) || any(unlist(d$warned)) || d$push_available || !is.null(d$pusher) || !d$ball_in_hand)) .ra_fail("Invalid opening rack",code="INVALID_STATE")
  if(any(unlist(d$warned) & unlist(d$fouls)!=2)) .ra_fail("Two-foul warning requires exactly two consecutive fouls",code="INVALID_STATE")
  if(d$phase=="push_choice") {
    if(is.null(d$pusher) || d$turn==d$pusher || d$push_available || d$ball_in_hand) .ra_fail("Push-out choice belongs to the incoming player after a legal push",code="INVALID_STATE")
  } else if(!is.null(d$pusher)) .ra_fail("A pending pusher is only retained for the push-out choice",code="INVALID_STATE")
  if(d$push_available && (d$phase!="play" || d$shots!=1 || d$ball_in_hand)) .ra_fail("Push-out is available only immediately after a legal break",code="INVALID_STATE")
  if(d$phase=="finished") {
    if(is.null(d$winner) || is.null(d$reason) || !d$reason %in% c("nine_pocketed","three_fouls")) .ra_fail("Finished rack needs a winner and reason",code="INVALID_STATE")
    if(d$reason=="nine_pocketed" && (9 %in% unlist(d$remaining) || d$winner!=d$turn)) .ra_fail("Winning shooter must have legally pocketed the nine",code="INVALID_STATE")
    if(d$reason=="three_fouls" && (d$fouls[[2L-d$winner]]!=3 || !9 %in% unlist(d$remaining))) .ra_fail("Three-foul loss requires three consecutive warned fouls",code="INVALID_STATE")
  } else if(!is.null(d$winner) || !is.null(d$reason) || max(unlist(d$fouls))>=3) .ra_fail("Terminal facts require a finished rack",code="INVALID_STATE")
}
.ra_9_specs <- function() {
  ball<-list(type="integer",minimum=1,maximum=9);first<-ball;first$nullable<-TRUE
  list(first_contact=first,pocketed=list(type="array",minItems=0,maxItems=9,items=ball),off_table=list(type="array",minItems=0,maxItems=9,items=ball),rail_after_contact=list(type="boolean"),break_rail_count=list(type="integer",minimum=0,maximum=9),foul=list(type="boolean"))
}
.ra_9_apply <- function(state,action) {
  d<-state$data;p<-action$params;k<-action$type;t<-d$turn
  if(k=="warn_two_fouls") {
    .ra_fields(p,list(player=list(type="integer",minimum=0,maximum=1)),"/params");q<-p$player
    if(d$fouls[[q+1L]]!=2 || d$warned[[q+1L]]) .ra_fail("Warn only an unwarned player on two consecutive fouls")
    d$warned[[q+1L]]<-TRUE;return(d)
  }
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  if(k=="choose_push") {
    .ra_fields(p,list(take_shot=list(type="boolean")),"/params")
    if(d$phase!="push_choice") .ra_fail("Choose only after a legal push-out")
    if(!p$take_shot) d$turn<-d$pusher
    d["pusher"]<-list(NULL);d$phase<-"play";return(d)
  }
  if(!k %in% c("shot","push")) .ra_fail("Unknown action")
  .ra_fields(p,.ra_9_specs(),"/params")
  if(!d$phase %in% c("break","play")) .ra_fail("Resolve the push-out choice before shooting")
  if(k=="push" && !d$push_available) .ra_fail("Push-out is only available immediately after a legal break")
  removed<-c(unlist(p$pocketed),unlist(p$off_table))
  if(anyDuplicated(removed) || any(!removed %in% unlist(d$remaining)) || (!is.null(p$first_contact) && !p$first_contact %in% unlist(d$remaining))) .ra_fail("Shot balls must be distinct and present on the table")
  breaking<-d$phase=="break";pushing<-k=="push"
  if(!breaking && p$break_rail_count!=0) .ra_fail("Break rail count applies only to the opening break")
  foul<-p$foul || length(p$off_table)>0
  if(!pushing) {
    foul<-foul || is.null(p$first_contact) || p$first_contact!=min(unlist(d$remaining))
    if(breaking) foul<-foul || (!length(p$pocketed) && p$break_rail_count<4) else foul<-foul || (!length(p$pocketed) && !p$rail_after_contact)
  }
  d$shots<-d$shots+1L;d$push_available<-FALSE;d$ball_in_hand<-FALSE;d$remaining<-Filter(function(b) !b %in% removed,d$remaining);d$phase<-"play"
  if(9 %in% removed && (foul || pushing)) d$remaining<-as.list(sort(c(unlist(d$remaining),9L)))
  if(foul) {
    if(d$fouls[[t+1L]]==2 && d$warned[[t+1L]]) {d$fouls[[t+1L]]<-3L;d$warned[[t+1L]]<-FALSE;d$winner<-1L-t;d$reason<-"three_fouls";d$phase<-"finished"}
    else {d$fouls[[t+1L]]<-min(2,d$fouls[[t+1L]]+1L);d$warned[[t+1L]]<-FALSE}
    d$turn<-1L-t;d$ball_in_hand<-TRUE;return(d)
  }
  d$fouls[[t+1L]]<-0L;d$warned[[t+1L]]<-FALSE
  if(pushing) {d$phase<-"push_choice";d$pusher<-t;d$turn<-1L-t}
  else if(9 %in% unlist(p$pocketed)) {d$phase<-"finished";d$winner<-t;d$reason<-"nine_pocketed"}
  else {if(!length(p$pocketed)) d$turn<-1L-t;d$push_available<-breaking}
  d
}
.ra_9_legal <- function(state) {
  d<-state$data;actions<-list();types<-list()
  for(p in 0:1) if(d$fouls[[p+1L]]==2 && !d$warned[[p+1L]]) actions<-c(actions,list(list(type="warn_two_fouls",params=list(player=p))))
  if(d$phase=="push_choice") actions<-c(actions,lapply(c(TRUE,FALSE),function(x) list(type="choose_push",params=list(take_shot=x)))) else {
    types<-lapply(if(d$push_available) c("shot","push") else "shot",function(k) list(type=k,params_schema=.ra_9_specs(),preconditions=list("Report distinct currently present balls. A normal shot must contact the lowest ball first and pocket a ball or reach a rail; an opening break instead pockets a ball or drives four object balls to rails."),external_observations=list("Official first object contact, pockets, object balls off table, rail facts and any additional foul.")))
  }
  list(mode="event_types",actions=actions,action_types=types,constraints=list(turn=d$turn,lowest_ball=min(unlist(d$remaining)),ball_in_hand=d$ball_in_hand,hand_region=if(d$phase=="break") "behind_head_string" else "anywhere",push_available=d$push_available),completeness=FALSE,reason="adjudicated_shot_facts_without_ball_motion")
}
.ra_9_outcome <- function(state,pack) {
  d<-state$data;done<-d$phase=="finished";w<-d$winner
  list(status=if(done) "terminal" else "ongoing",result_kind=if(done) "win" else "none",winners=if(done) list(state$players[[w+1L]]$id) else list(),losers=if(done) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(done) d$reason else "ongoing",evidence_required=list(),rule_id=if(done) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
