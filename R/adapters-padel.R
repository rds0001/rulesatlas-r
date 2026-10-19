.ra_pd_service <- function(d) {
 first<-(sum(unlist(d$sets))+d$first_team)%%2L
 n<-sum(unlist(d$games))+if(d$phase=="tiebreak") (sum(unlist(d$points))+1L)%/%2L else 0L
 team<-(first+n)%%2L;member<-(d$first_members[[team+1L]]+n%/%2L)%%2L;receiver<-(d$right_receivers[[2L-team]]+sum(unlist(d$points))%%2L)%%2L
 c(team,2L*team+member,2L*(1L-team)+receiver)
}
.ra_pd_validate <- function(d) {
 normalized<-d;normalized$server<-.ra_tennis_server(d);.ra_tennis_validate(normalized)
 if(d$server!=.ra_pd_service(d)[1]) .ra_fail("Incorrect serving team",code="INVALID_STATE")
 if(!d$started && (length(d$sets)>0 || any(unlist(d$games)!=0) || any(unlist(d$points)!=0) || d$faults!=0 || d$set_started)) .ra_fail("Play data requires a started match",code="INVALID_STATE")
 if(!d$set_started && (any(unlist(d$games)!=0) || any(unlist(d$points)!=0) || d$faults!=0)) .ra_fail("Current-set play requires a started set",code="INVALID_STATE")
 if(d$set_started && !all(unlist(d$configured))) .ra_fail("Both pairs must fix service and receiving order before play",code="INVALID_STATE")
 if(!is.null(d$decision) && d$decision$kind=="walkover" && d$started) .ra_fail("Walkover is unavailable after play starts",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_pd_apply <- function(state,action) {
 d<-state$data;k<-action$type;p<-action$params;player<-list(type="integer",minimum=0,maximum=1)
 if(k=="choose_first_team") {.ra_fields(p,list(team=player),"/params");if(d$started) .ra_fail("Choose initial serving team before play");d$first_team<-p$team;d$server<-.ra_pd_service(d)[1];return(d)}
 if(k=="configure_pair") {
  .ra_fields(p,list(team=player,first_server=player,right_receiver=player),"/params");t<-p$team
  if(d$set_started || d$configured[[t+1L]]) .ra_fail("Each pair fixes its order once before the current set starts")
  d$first_members[[t+1L]]<-p$first_server;d$right_receivers[[t+1L]]<-p$right_receiver;d$configured[[t+1L]]<-TRUE;return(d)
 }
 if(k=="end_match") {
  .ra_fields(p,list(kind=list(type="string",enum=list("retirement","disqualification","walkover")),player=player),"/params")
  if(p$kind=="walkover" && d$started) .ra_fail("Walkover is unavailable after play starts")
  d$decision<-p;d$phase<-"finished";return(d)
 }
 if(!all(unlist(d$configured))) .ra_fail("Both pairs must configure their order before play")
 d$started<-TRUE;d$set_started<-TRUE
 if(k=="service_let") {.ra_fields(p,list(),"/params");return(d)}
 if(k=="replay") {.ra_fields(p,list(),"/params");d$faults<-0L;return(d)}
 if(k=="service_fault") {.ra_fields(p,list(),"/params");if(d$faults==0) {d$faults<-1L;return(d)};winner<-1L-d$server} else if(k=="rally") {.ra_fields(p,list(winner=player),"/params");winner<-p$winner} else .ra_fail("Unknown action")
 before<-length(d$sets);state$data<-d;d<-.ra_tennis_apply(state,list(type="award_point",params=list(player=winner)));d$faults<-0L
 if(length(d$sets)>before && d$phase!="finished") {d$set_started<-FALSE;d$configured<-list(FALSE,FALSE)}
 d$server<-.ra_pd_service(d)[1];d
}
.ra_pd_legal <- function(state) {
 d<-state$data;types<-list();player<-list(type="integer",minimum=0,maximum=1);empty<-structure(list(),names=character())
 if(!d$started) types<-c(types,list(list(type="choose_first_team",params_schema=list(team=player),preconditions=list("Before the match starts."),external_observations=list("Initial service choice."))))
 if(!d$set_started) types<-c(types,list(list(type="configure_pair",params_schema=list(team=player,first_server=player,right_receiver=player),preconditions=list("Each pair chooses once before each set. Member indices are zero or one within the pair."),external_observations=list("Pair service order and right-court receiver choice."))))
 if(all(unlist(d$configured))) {
  kinds<-c("rally","service_fault","service_let","replay");obs<-c("Officially awarded rally winner.","Adjudicated service fault; the second fault loses the point.","Official service let repeats that service attempt, preserving any first fault.","Official replay of the whole point restores first service.")
  for(i in 1:4) types<-c(types,list(list(type=kinds[i],params_schema=if(i==1) list(winner=player) else empty,preconditions=list("Both pairs have configured the current set."),external_observations=list(obs[i]))))
 }
 types<-c(types,list(list(type="end_match",params_schema=list(kind=list(type="string",enum=as.list(c("retirement","disqualification",if(!d$started) "walkover"))),player=player),preconditions=list("Walkover only before play."),external_observations=list("Official match-ending decision."))))
 s<-.ra_pd_service(d)
 list(mode="event_types",actions=list(),action_types=types,constraints=list(serving_team=s[1],server_member=s[2],receiver_member=s[3],next_service_attempt=d$faults+1L,pairs_configured=d$configured,service_court=if(sum(unlist(d$points))%%2L==0) "right" else "left",current_set_end_changes=(sum(unlist(d$games))+1L)%/%2L+if(d$phase=="tiebreak") sum(unlist(d$points))%/%6L else 0L),completeness=FALSE,reason="externally_adjudicated_rallies_and_pair_order")
}
.ra_pd_outcome <- function(state,pack) .ra_tennis_outcome(state,pack)
