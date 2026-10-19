.ra_racketscore_player<-list(type="integer",minimum=0L,maximum=1L)
.ra_racketscore_decisions<-list("retirement","disqualification","walkover")
.ra_racketscore_game_winner<-function(score,sport) {
 high<-max(unlist(score));low<-min(unlist(score))
 valid<-if(sport=="table_tennis") (high==11 && low<=9) || (high>11 && high-low==2) else (high==21 && low<=19) || (high>=22 && high<=29 && high-low==2) || (high==30 && low %in% c(28,29))
 if(!valid) .ra_fail("Invalid completed game score",code="INVALID_STATE")
 if(score[[1]]>score[[2]]) 0L else 1L
}
.ra_racketscore_counts<-function(data,sport) {
 target<-if(sport=="table_tennis") 3L else 2L;wins<-c(0L,0L)
 for(score in data$games) {if(max(wins)==target) .ra_fail("Games continued after match end",code="INVALID_STATE");w<-.ra_racketscore_game_winner(score,sport)+1L;wins[w]<-wins[w]+1L}
 wins
}
.ra_racketscore_tt_server<-function(data) {
 total<-sum(unlist(data$points));base<-length(data$games)%%2L;exp<-data$expedite
 if(!is.null(exp)) {if(exp$game_index==length(data$games)) return((exp$server+total-exp$point_total)%%2L);return((base+total)%%2L)}
 (base+if(min(unlist(data$points))<10) total%/%2L else total)%%2L
}
.ra_racketscore_won<-function(points,sport) {high<-max(unlist(points));low<-min(unlist(points));if(sport=="table_tennis") high>=11 && high-low>=2 else (high>=21 && high-low>=2) || high==30}
.ra_racketscore_validate<-function(data,sport) {
 target<-if(sport=="table_tennis") 3L else 2L;wins<-.ra_racketscore_counts(data,sport);finished<-max(wins)==target;points<-data$points;decision<-data$decision
 if(data$phase!=if(finished || !is.null(decision)) "finished" else "game") .ra_fail("Scoring phase mismatch",code="INVALID_STATE")
 if(.ra_racketscore_won(points,sport)) .ra_fail("Unclosed completed game",code="INVALID_STATE")
 if(finished && (!is.null(decision) || any(unlist(points)!=0))) .ra_fail("Invalid completed match",code="INVALID_STATE")
 if(!is.null(decision) && decision$kind=="walkover" && (length(data$games)>0 || any(unlist(points)!=0))) .ra_fail("Walkover requires pre-play state",code="INVALID_STATE")
 if(sport=="table_tennis") {
  exp<-data$expedite
  if(!is.null(exp)) {
   index<-exp$game_index
   if(index>length(data$games)) .ra_fail("Expedite game index is in the future",code="INVALID_STATE")
   total<-sum(unlist(if(index==length(data$games)) points else data$games[[index+1L]]))
   anchor<-if(exp$in_play || exp$point_total==0) (index+exp$point_total%/%2L)%%2L else (index+(exp$point_total-1L)%/%2L+1L)%%2L
   if(exp$point_total>total || exp$server!=anchor) .ra_fail("Invalid expedite anchor",code="INVALID_STATE")
  }
  if(data$server!=.ra_racketscore_tt_server(data)) .ra_fail("Incorrect service order",code="INVALID_STATE")
 } else if(sum(unlist(points))==0) {
  expected<-if(length(data$games)) .ra_racketscore_game_winner(data$games[[length(data$games)]],sport) else 0L
  if(data$server!=expected) .ra_fail("Incorrect opening server",code="INVALID_STATE")
 } else if(points[[data$server+1L]]==0) .ra_fail("Server has not won a rally",code="INVALID_STATE")
 invisible(TRUE)
}
.ra_racketscore_outcome<-function(state,pack,sport) {
 data<-state$data;target<-if(sport=="table_tennis") 3L else 2L;decision<-data$decision;terminal<-data$phase=="finished"
 w<-if(!is.null(decision)) 1L-decision$player else if(terminal) which(.ra_racketscore_counts(data,sport)==target)[1]-1L else NULL
 kind<-if(!is.null(decision)) switch(decision$kind,retirement="technical",disqualification="disqualification",walkover="forfeit") else if(terminal) "win" else "none"
 list(status=if(terminal) "terminal" else "ongoing",result_kind=kind,winners=if(is.null(w)) list() else list(state$players[[w+1L]]$id),losers=if(is.null(w)) list() else list(state$players[[2L-w]]$id),placements=list(),reason=if(!is.null(decision)) decision$kind else if(terminal) "required_games_won" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
.ra_racketscore_apply<-function(state,action,sport) {
 data<-state$data;params<-action$params
 if(action$type=="end_match") {.ra_fields(params,list(kind=list(type="enum",values=.ra_racketscore_decisions),player=.ra_racketscore_player),"/params");data$decision<-params;data$phase<-"finished";.ra_racketscore_validate(data,sport);return(data)}
 if(action$type=="start_expedite" && sport=="table_tennis") {
  .ra_fields(params,list(mode=list(type="enum",values=list("timed","agreed")),elapsed_seconds=list(type="integer",minimum=0L),in_play=list(type="boolean")),"/params")
  if(!is.null(data$expedite) || sum(unlist(data$points))>=18) .ra_fail("Expedite cannot start now")
  if(params$mode=="timed" && params$elapsed_seconds<600) .ra_fail("Timed expedite requires ten minutes")
  total<-sum(unlist(data$points));anchor<-if(params$in_play || total==0) data$server else (length(data$games)+(total-1L)%/%2L+1L)%%2L
  data$expedite<-list(game_index=length(data$games),point_total=total,server=anchor,in_play=params$in_play);data$server<-anchor;return(data)
 }
 if(action$type!="award_point") .ra_fail("Unknown action")
 .ra_fields(params,list(player=.ra_racketscore_player),"/params");p<-params$player;data$points[[p+1L]]<-data$points[[p+1L]]+1L
 if(sport=="badminton") data$server<-p
 if(.ra_racketscore_won(data$points,sport)) {
  data$games[[length(data$games)+1L]]<-data$points;data$points<-list(0L,0L)
  if(max(.ra_racketscore_counts(data,sport))==if(sport=="table_tennis") 3L else 2L) data$phase<-"finished"
 }
 if(sport=="table_tennis") data$server<-.ra_racketscore_tt_server(data)
 data
}
.ra_racketscore_legal<-function(state,sport) {
 data<-state$data;kinds<-if(!length(data$games) && all(unlist(data$points)==0)) .ra_racketscore_decisions else .ra_racketscore_decisions[1:2]
 types<-list(list(type="award_point",params_schema=list(player=.ra_racketscore_player),preconditions=list("Match is ongoing."),external_observations=list("An adjudicator has awarded this rally point.")),list(type="end_match",params_schema=list(kind=list(type="enum",values=kinds),player=.ra_racketscore_player),preconditions=list("Walkover is allowed only before play."),external_observations=list("Official match-ending decision.")))
 if(sport=="table_tennis" && is.null(data$expedite) && sum(unlist(data$points))<18) types[[length(types)+1L]]<-list(type="start_expedite",params_schema=list(mode=list(type="enum",values=list("timed","agreed")),elapsed_seconds=list(type="integer",minimum=0L),in_play=list(type="boolean")),preconditions=list("Timed mode requires at least 600 elapsed seconds; agreed mode requires both players to request it."),external_observations=list("Official elapsed playing time or mutual agreement."))
 list(mode="event_types",actions=list(),action_types=types,constraints=list(server=data$server),completeness=FALSE,reason="externally_adjudicated_events")
}
