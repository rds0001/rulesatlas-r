.ra_bt_server <- function(d) (.ra_tennis_server(d)+d$first_server)%%2L
.ra_bt_validate <- function(d) {
 wins<-.ra_tennis_counts(d);finished<-max(wins)==2;tie<-identical(as.integer(unlist(d$games)),c(6L,6L));p<-unlist(d$points);g<-unlist(d$games);decision<-d$decision
 expected<-if(finished || !is.null(decision)) "finished" else if(tie) "tiebreak" else "game"
 if(d$phase!=expected) .ra_fail("Scoring phase mismatch",code="INVALID_STATE")
 if(!tie && max(g)>=6 && abs(g[1]-g[2])>=2) .ra_fail("Unclosed completed set",code="INVALID_STATE")
 if(tie) {if(max(p)>=7 && abs(p[1]-p[2])>=2) .ra_fail("Unclosed completed tiebreak",code="INVALID_STATE")} else if(max(p)>3) .ra_fail("No-ad games never retain an advantage point",code="INVALID_STATE")
 if(d$server!=.ra_bt_server(d)) .ra_fail("Incorrect service order",code="INVALID_STATE")
 if(finished && (!is.null(decision) || any(g!=0) || any(p!=0))) .ra_fail("Completed match must have reset current scores",code="INVALID_STATE")
 if(!d$started && (length(d$sets)>0 || any(g!=0) || any(p!=0))) .ra_fail("Scored play requires a started match",code="INVALID_STATE")
 if(!is.null(decision) && decision$kind=="walkover" && d$started) .ra_fail("Walkover is unavailable after play starts",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_bt_apply <- function(state,action) {
 d<-state$data;p<-action$params;k<-action$type;player<-list(type="integer",minimum=0,maximum=1)
 if(k=="choose_first_server") {.ra_fields(p,list(player=player),"/params");if(d$started) .ra_fail("Choose initial service before play");d$first_server<-p$player;d$server<-p$player;return(d)}
 if(k=="end_match") {
  .ra_fields(p,list(kind=list(type="string",enum=list("retirement","disqualification","walkover")),player=player),"/params")
  if(p$kind=="walkover" && d$started) .ra_fail("Walkover is unavailable after play starts")
  d$decision<-p;d$phase<-"finished";return(d)
 }
 if(k=="replay") {.ra_fields(p,list(),"/params");d$started<-TRUE;return(d)}
 if(k=="service_fault") {.ra_fields(p,list(),"/params");winner<-1L-d$server} else if(k=="rally") {.ra_fields(p,list(winner=player),"/params");winner<-p$winner} else .ra_fail("Unknown action")
 d$started<-TRUE;d$points[[winner+1L]]<-d$points[[winner+1L]]+1L
 if(d$phase=="tiebreak") {
  if(d$points[[winner+1L]]>=7 && d$points[[winner+1L]]-d$points[[2L-winner]]>=2) {score<-list(6L,6L);score[[winner+1L]]<-7L;d$sets<-c(d$sets,list(score));d$games<-list(0L,0L);d$points<-list(0L,0L);d$phase<-"game"}
 } else if(d$points[[winner+1L]]==4) {
  d$games[[winner+1L]]<-d$games[[winner+1L]]+1L;d$points<-list(0L,0L)
  if(d$games[[winner+1L]]>=6 && d$games[[winner+1L]]-d$games[[2L-winner]]>=2) {d$sets<-c(d$sets,list(d$games));d$games<-list(0L,0L)} else if(all(unlist(d$games)==6)) d$phase<-"tiebreak"
 }
 if(max(.ra_tennis_counts(d))==2) d$phase<-"finished"
 d$server<-.ra_bt_server(d);d
}
.ra_bt_legal <- function(state) {
 d<-state$data;player<-list(type="integer",minimum=0,maximum=1);empty<-structure(list(),names=character());kinds<-c("retirement","disqualification",if(!d$started) "walkover")
 types<-list(list(type="rally",params_schema=list(winner=player),preconditions=list("Match is ongoing."),external_observations=list("Officially awarded rally winner.")),list(type="service_fault",params_schema=empty,preconditions=list("Only one service attempt per point; a fault awards the receiver the point."),external_observations=list("Adjudicated service fault; net contact alone on an otherwise valid service is not a fault.")),list(type="replay",params_schema=empty,preconditions=list("Official replay for external interference; no automatic service-net let."),external_observations=list("Official replay decision.")),list(type="end_match",params_schema=list(kind=list(type="string",enum=as.list(kinds)),player=player),preconditions=list("Walkover only before play."),external_observations=list("Official match-ending decision.")))
 if(!d$started) types<-c(types,list(list(type="choose_first_server",params_schema=list(player=player),preconditions=list("Before play begins."),external_observations=list("Initial service choice."))))
 list(mode="event_types",actions=list(),action_types=types,constraints=list(server=d$server,service_attempts=1L,deciding_point=d$phase=="game" && all(unlist(d$points)==3),current_set_end_changes=(sum(unlist(d$games))+1L)%/%2L+if(d$phase=="tiebreak") sum(unlist(d$points))%/%6L else 0L),completeness=FALSE,reason="externally_adjudicated_rallies")
}
.ra_bt_outcome <- function(state,pack) .ra_tennis_outcome(state,pack)
