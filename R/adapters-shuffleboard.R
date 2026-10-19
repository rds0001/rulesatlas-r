.ra_sb_i<-function(a,b,nullable=FALSE) {x<-list(type="integer",minimum=a,maximum=b);if(nullable) x$nullable<-TRUE;x}
.ra_sb_puck<-list(type="object",properties=list(status=list(type="enum",values=list("unplayed","live","dead")),zone=.ra_sb_i(1,4,TRUE),order=.ra_sb_i(1,8,TRUE)),required=list("status","zone","order"))
.ra_sb_board<-list(type="array",minItems=8,maxItems=8,items=.ra_sb_puck)
.ra_sb_schemas<-list(choose_first=list(team=.ra_sb_i(0,1)),shoot=list(pucks=.ra_sb_board),correct_board=list(pucks=.ra_sb_board),confirm_frame=setNames(list(),character()),concede=list(team=.ra_sb_i(0,1)))
.ra_sb_blank<-function() lapply(1:8,function(i) list(status="unplayed",zone=NULL,order=NULL))
.ra_sb_next<-function(d) if(!is.null(d$first_team) && d$shots<8) (d$first_team+d$shots)%%2L*4L+d$shots%/%2L else NULL
.ra_sb_played<-function(first,shots) {if(shots==0) return(integer());j<-0:(shots-1L);(first+j)%%2L*4L+j%/%2L}
.ra_sb_board_valid<-function(board,played) {
 for(j in 1:8) {p<-board[[j]];if((p$status=="unplayed")!=!(j-1L)%in%played || (p$status=="live")!=(!is.null(p$zone) && !is.null(p$order)) || (p$status!="live" && (!is.null(p$zone) || !is.null(p$order)))) return(FALSE)}
 live<-Filter(function(p) p$status=="live",board)
 for(a in live) for(b in live) if(a$zone>b$zone && a$order>=b$order) return(FALSE)
 TRUE
}
.ra_sb_frame_score<-function(d) {
 best<-vapply(0:1,function(t) {r<-Filter(function(p) p$status=="live",d$pucks[(t*4L+1L):(t*4L+4L)]);min(c(9,vapply(r,function(p) p$order,numeric(1))))},numeric(1))
 if(best[1]==best[2]) return(list(0L,0L))
 w<-if(best[1]<best[2]) 0L else 1L;s<-list(0L,0L);s[[w+1L]]<-sum(vapply(d$pucks[(w*4L+1L):(w*4L+4L)],function(p) if(p$status=="live" && p$order<best[2L-w]) p$zone else 0,numeric(1)));s
}
.ra_sb_validate<-function(d) {
 bad<-function(m) .ra_fail(m,code="INVALID_STATE")
 if(d$phase=="setup") {if(!is.null(d$first_team) || d$shots!=0 || any(unlist(d$scores)!=0) || d$frame!=1) bad("Invalid unstarted game")} else if(is.null(d$first_team)) bad("Started game requires its first shooter")
 if(!.ra_sb_board_valid(d$pucks,.ra_sb_played(if(is.null(d$first_team)) 0 else d$first_team,d$shots))) bad("Puck status, order or scoring zone is inconsistent")
 if((d$phase=="shooting" && d$shots>=8) || (d$phase=="review" && d$shots!=8)) bad("Frame phase and shot count mismatch")
 if(d$phase=="finished") {
  if(is.null(d$winner) || is.null(d$reason)) bad("Terminal game needs its result")
  if(d$reason=="target" && (d$scores[[d$winner+1L]]<21 || d$scores[[2L-d$winner]]>=21 || d$shots!=8)) bad("Invalid target-score victory")
 } else if(!is.null(d$winner) || !is.null(d$reason) || max(unlist(d$scores))>=21) bad("Unresolved or premature result")
}
.ra_sb_available<-function(d) switch(d$phase,setup="choose_first",shooting=c("shoot","concede"),review=c("correct_board","confirm_frame","concede"),character())
.ra_sb_apply<-function(state,action) {
 d<-state$data;k<-action$type;p<-action$params
 if(!k %in% .ra_sb_available(d)) .ra_fail("Action is not permitted in this phase")
 .ra_fields(p,.ra_sb_schemas[[k]],"/params")
 if(k=="choose_first") {d$first_team<-p$team;d$phase<-"shooting"} else if(k %in% c("shoot","correct_board")) {
  shots<-d$shots+as.integer(k=="shoot");new<-p$pucks
  if(!.ra_sb_board_valid(new,.ra_sb_played(d$first_team,shots))) .ra_fail("Board must contain exactly the delivered pucks with consistent official ranks and zones")
  if(k=="shoot" && any(vapply(1:8,function(j) d$pucks[[j]]$status=="dead" && new[[j]]$status!="dead",logical(1)))) .ra_fail("A removed puck cannot return on a later shot")
  d$pucks<-new;d$shots<-shots;d$phase<-if(shots==8) "review" else "shooting"
 } else if(k=="confirm_frame") {
  gains<-.ra_sb_frame_score(d);d$scores<-as.list(unlist(d$scores)+unlist(gains))
  if(max(unlist(d$scores))>=21) {d$phase<-"finished";d$winner<-if(d$scores[[1]]>=21) 0L else 1L;d$reason<-"target"} else {
   if(any(unlist(gains)!=0)) d$first_team<-if(gains[[1]]>0) 0L else 1L
   d$frame<-d$frame+1L;d$shots<-0L;d$pucks<-.ra_sb_blank();d$phase<-"shooting"
  }
 } else if(k=="concede") {d$phase<-"finished";d$winner<-1L-p$team;d$reason<-"concede"}
 d
}
.ra_sb_legal<-function(state) {
 d<-state$data
 list(mode="event_types",actions=list(),action_types=lapply(.ra_sb_available(d),function(k) list(type=k,params_schema=.ra_sb_schemas[[k]],preconditions=list("Alternate four pucks per team; only official settled boards may be recorded."),external_observations=list("Official live/dead puck rulings, scoring zones and distance order."))),constraints=list(phase=d$phase,next_puck=.ra_sb_next(d),frame=d$frame,provisional_frame_points=if(d$phase=="review") .ra_sb_frame_score(d) else NULL),completeness=FALSE,reason="official_table_shuffleboard_positions")
}
.ra_sb_outcome<-function(state,pack) {
 d<-state$data;t<-d$phase=="finished";w<-d$winner
 list(status=if(t) "terminal" else if(d$phase=="review") "awaiting_adjudication" else "ongoing",result_kind=if(t) {if(d$reason=="concede") "forfeit" else "win"} else "none",winners=if(t) list(state$players[[w+1L]]$id) else list(),losers=if(t) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(t) d$reason else if(d$phase=="review") "frame_review" else "ongoing",evidence_required=if(d$phase=="review") list("Official settled-board confirmation.") else list(),rule_id=if(t) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
