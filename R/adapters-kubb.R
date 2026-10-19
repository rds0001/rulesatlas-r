.ra_ku_i<-function(a,b) list(type="integer",minimum=a,maximum=b)
.ra_ku_b<-list(type="boolean")
.ra_ku_schemas<-list(choose_first_team=list(team=.ra_ku_i(0,1)),throw_kubb=list(kubb=.ra_ku_i(0,9),valid=.ra_ku_b,king=.ra_ku_b),place_penalty_kubb=list(kubb=.ra_ku_i(0,9)),throw_baton=list(knocked=list(type="array",minItems=0,maxItems=10,items=.ra_ku_i(0,9)),king=.ra_ku_b,from_baseline=.ra_ku_b),end_turn=setNames(list(),character()),concede=setNames(list(),character()))
.ra_ku_insert<-function(d) {d$phase<-if(any(vapply(d$kubbs,function(x) x$status=="down",logical(1)))) "insert" else "batons";d}
.ra_ku_advance<-function(d) {d$team<-1L-d$team;d$batons_left<-6L;d$failed_insertion<-0L;d["inserting"]<-list(NULL);.ra_ku_insert(d)}
.ra_ku_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="setup") {
  if(!is.null(d$team) || !is.null(d$winner) || !is.null(d$reason) || d$batons_left!=6 || !is.null(d$inserting) || d$failed_insertion!=0 || any(vapply(1:10,function(i) d$kubbs[[i]]$status!="base" || d$kubbs[[i]]$half!=(i-1L)%/%5L,logical(1)))) bad("Invalid initial Kubb position")
  return(invisible(TRUE))
 }
 if(is.null(d$team)) bad("An active game needs a team")
 if((d$phase=="finished")!=(!is.null(d$winner) && !is.null(d$reason))) bad("Terminal result mismatch")
 if(d$phase!="finished" && (!is.null(d$winner) || !is.null(d$reason))) bad("Premature winner")
 for(i in 1:10) if(d$kubbs[[i]]$status=="base" && d$kubbs[[i]]$half!=(i-1L)%/%5L) bad("A base kubb cannot switch baseline")
 if(d$phase %in% c("insert","penalty_placement")) {
  if(d$batons_left!=6 || !any(vapply(d$kubbs,function(x) x$status=="down",logical(1)))) bad("Insertion requires returned kubbs before batons")
  if(any(vapply(d$kubbs,function(x) x$status=="down" && x$half!=d$team,logical(1)))) bad("Insert the kubbs knocked down on your half")
  if(!is.null(d$inserting) && d$kubbs[[d$inserting+1L]]$status!="down") bad("Insertion target must still be down")
  if((d$phase=="penalty_placement")!=(d$failed_insertion==2)) bad("Penalty placement follows two failed attempts")
  if(is.null(d$inserting)!=(d$failed_insertion==0)) bad("Attempt counter needs its selected kubb")
 } else if(!is.null(d$inserting) || d$failed_insertion!=0) bad("Insertion counters exist outside insertion")
 if(d$phase=="batons" && d$batons_left==0) bad("Exhausted batons must change the turn")
 if(d$phase=="finished" && d$reason=="king" && (d$winner!=d$team || any(vapply(d$kubbs,function(x) x$half!=d$team && x$status %in% c("base","field"),logical(1))))) bad("Winning king requires a cleared opposing half")
 if(d$phase=="finished" && d$reason %in% c("premature_king","illegal_king","concede") && d$winner==d$team) bad("The offending team cannot win")
}
.ra_ku_available<-function(d) switch(d$phase,setup="choose_first_team",insert=c("throw_kubb","concede"),penalty_placement=c("place_penalty_kubb","concede"),batons=c("throw_baton","end_turn","concede"),character())
.ra_ku_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!k %in% .ra_ku_available(d)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,.ra_ku_schemas[[k]],"/params")
 if(k=="choose_first_team") {d$team<-p$team;d$phase<-"batons"} else if(k %in% c("throw_kubb","place_penalty_kubb")) {
  j<-p$kubb;x<-d$kubbs[[j+1L]]
  if(x$status!="down" || (!is.null(d$inserting) && d$inserting!=j)) .ra_fail("Complete the selected returned kubb first")
  if(k=="throw_kubb" && p$king) {d$phase<-"finished";d$winner<-1L-d$team;d$reason<-"premature_king";d["inserting"]<-list(NULL);d$failed_insertion<-0L;return(d)}
  if(k=="throw_kubb" && !p$valid) {d$inserting<-j;d$failed_insertion<-d$failed_insertion+1L;if(d$failed_insertion==2) d$phase<-"penalty_placement"} else {d$kubbs[[j+1L]]$status<-"field";d$kubbs[[j+1L]]$half<-1L-d$team;d["inserting"]<-list(NULL);d$failed_insertion<-0L;d<-.ra_ku_insert(d)}
 } else if(k=="throw_baton") {
  if(!p$from_baseline && !p$king && !any(vapply(d$kubbs,function(x) x$half==d$team && x$status=="field",logical(1)))) .ra_fail("An advanced throwing line requires a standing field kubb on your half")
  ids<-unlist(p$knocked)
  if(anyDuplicated(ids)) .ra_fail("A kubb can be knocked at most once by a baton")
  for(j in ids) {
   x<-d$kubbs[[j+1L]]
   if(x$half==d$team || !x$status %in% c("base","field")) .ra_fail("Only standing opposing kubbs are scoring targets")
   if(x$status=="base" && any(vapply(d$kubbs,function(y) y$half!=d$team && y$status=="field",logical(1)))) next
   d$kubbs[[j+1L]]$status<-"down"
  }
  d$batons_left<-d$batons_left-1L
  if(p$king) {
   premature<-any(vapply(d$kubbs,function(x) x$half!=d$team && x$status %in% c("base","field"),logical(1)))
   d$reason<-if(premature) "premature_king" else if(!p$from_baseline) "illegal_king" else "king";d$winner<-if(d$reason!="king") 1L-d$team else d$team;d$phase<-"finished"
  } else if(d$batons_left==0) d<-.ra_ku_advance(d)
 } else if(k=="end_turn") d<-.ra_ku_advance(d) else if(k=="concede") {d$phase<-"finished";d$winner<-1L-d$team;d$reason<-"concede";d["inserting"]<-list(NULL);d$failed_insertion<-0L}
 d
}
.ra_ku_legal<-function(state) {
 d<-state$data;team<-d$team;field<-integer();bases<-integer()
 if(!is.null(team)) for(i in 1:10) if(d$kubbs[[i]]$half!=team) {if(d$kubbs[[i]]$status=="field") field<-c(field,i-1L);if(d$kubbs[[i]]$status=="base") bases<-c(bases,i-1L)}
 list(mode="event_types",actions=list(),action_types=lapply(.ra_ku_available(d),function(k) list(type=k,params_schema=.ra_ku_schemas[[k]],preconditions=list("Throw order, insertion attempts, field-kubb priority and king protection apply."),external_observations=list("Legal throw and placement rulings; ordered physical knockdowns."))),constraints=list(phase=d$phase,team=team,batons_left=d$batons_left,field_targets=as.list(field),base_targets=if(!length(field)) as.list(bases) else list(),king_available=!is.null(team) && !length(field) && !length(bases),advanced_line_available=!is.null(team) && any(vapply(d$kubbs,function(x) x$half==team && x$status=="field",logical(1))),penalty_placer=if(d$phase!="penalty_placement") NULL else 1L-team),completeness=FALSE,reason="official_kubb_throw_results")
}
.ra_ku_outcome<-function(state,pack) {
 d<-state$data;t<-d$phase=="finished";w<-d$winner
 list(status=if(t) "terminal" else "ongoing",result_kind=if(t && d$reason=="concede") "forfeit" else if(t) "win" else "none",winners=if(t) list(state$players[[w+1L]]$id) else list(),losers=if(t) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(t) d$reason else "ongoing",evidence_required=list(),rule_id=if(t) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
