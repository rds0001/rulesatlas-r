.ra_morris_mills <- c(unlist(lapply(c(0,8,16),function(o) lapply(list(c(0,1,2),c(2,3,4),c(4,5,6),c(6,7,0)),function(line) o+line)),recursive=FALSE),lapply(c(1,3,5,7),function(i) c(i,i+8,i+16)))
.ra_morris_adj <- lapply(0:23,function(i) {neighbors<-c(i%/%8*8+(i+1)%%8,i%/%8*8+(i-1)%%8);if(i%%8 %in% c(1,3,5,7)) neighbors<-c(neighbors,if(i>=8) i-8 else numeric(),if(i<16) i+8 else numeric());neighbors})
.ra_morris_key <- function(d) paste0(paste0(ifelse(unlist(d$board)<0,2,unlist(d$board)),collapse=""),"|",d$turn,"|",d$placed[[1]],"|",d$placed[[2]])
.ra_morris_in_mill <- function(board,i) any(vapply(.ra_morris_mills,function(line) i %in% line && all(unlist(board[line+1L])==board[[i+1L]]),logical(1)))
.ra_morris_captures <- function(d) {opponent<-which(unlist(d$board)==1-d$turn)-1L;unprotected<-Filter(function(i) !.ra_morris_in_mill(d$board,i),opponent);if(length(unprotected)) unprotected else opponent}
.ra_morris_ordinary <- function(d) {
  b<-unlist(d$board);p<-d$turn;empty<-which(b==-1)-1L
  if(sum(unlist(d$placed))<18) return(lapply(empty,function(i) list(type="place",params=list(cell=i))))
  result<-list()
  for(i in which(b==p)-1L) for(j in empty) if(sum(b==p)==3 || j %in% .ra_morris_adj[[i+1L]]) result[[length(result)+1L]]<-list(type="move",params=list(source=i,target=j))
  result
}
.ra_morris_result <- function(d) {
  if(d$pending) return(list(winner=NULL,reason=NULL))
  for(p in 0:1) if(sum(unlist(d$board)==p)+9-d$placed[[p+1L]]<3) return(list(winner=1L-p,reason="fewer_than_three_stones"))
  if(sum(unlist(d$placed))==18 && !length(.ra_morris_ordinary(d))) return(list(winner=1L-d$turn,reason="no_legal_move"))
  if(sum(unlist(d$history)==.ra_morris_key(d))>=3) return(list(winner=-1L,reason="threefold_repetition"))
  if(d$quiet_turns>=100) return(list(winner=-1L,reason="fifty_moves_each_without_capture"))
  list(winner=NULL,reason=NULL)
}
.ra_morris_validate <- function(d) {
  b<-unlist(d$board);p<-d$turn;a<-d$placed[[1]];c<-d$placed[[2]]
  if(any(vapply(0:1,function(t) sum(b==t)>d$placed[[t+1L]],logical(1)))) .ra_fail("Board contains unplaced stones",code="INVALID_STATE")
  if(!a %in% c(c,c+1)) .ra_fail("Invalid placement counts",code="INVALID_STATE")
  expected<-if(d$pending) {if(a==c) 1L else 0L} else if(a==c) 0L else 1L
  if(a+c<18 && p!=expected) .ra_fail("Placement turn mismatch",code="INVALID_STATE")
  if(d$pending && (!any(vapply(which(b==p)-1L,function(i) .ra_morris_in_mill(d$board,i),logical(1))) || !length(.ra_morris_captures(d)))) .ra_fail("Capture requires a mill and an opposing stone",code="INVALID_STATE")
  if(any(!grepl("^[012]{24}[|][01][|][0-9][|][0-9]$",unlist(d$history)))) .ra_fail("Malformed repetition position",code="INVALID_STATE")
  if(!length(d$history) || length(d$history)!=d$quiet_turns+1) .ra_fail("Repetition history and quiet count mismatch",code="INVALID_STATE")
  if(!d$pending && tail(unlist(d$history),1)!=.ra_morris_key(d)) .ra_fail("History does not end at the current position",code="INVALID_STATE")
  if(a+c<18 && d$quiet_turns!=0) .ra_fail("Placement resets the quiet counter",code="INVALID_STATE")
  wins<-Filter(function(t) sum(b==t)+9-d$placed[[t+1L]]<3,0:1)
  if(length(wins)>1 || (length(wins)>0 && (d$pending || wins[1]!=p))) .ra_fail("Impossible material result",code="INVALID_STATE")
}
.ra_morris_finish <- function(d,reset) {
  d$turn<-1L-d$turn;d$pending<-FALSE
  if(reset) {d$quiet_turns<-0L;d$history<-list(.ra_morris_key(d))} else {d$quiet_turns<-d$quiet_turns+1L;d$history[[length(d$history)+1L]]<-.ra_morris_key(d)}
  d
}
.ra_morris_apply <- function(state,action) {
  d<-state$data;p<-d$turn;params<-action$params;kind<-action$type
  if(!is.null(action$actor) && action$actor!=state$players[[p+1L]]$id) .ra_fail("Wrong actor")
  if(d$pending) {
    if(kind!="capture") .ra_fail("Capture an opposing stone first")
    .ra_fields(params,list(cell=list(type="integer",minimum=0,maximum=23)),"/params");i<-params$cell
    if(!i %in% .ra_morris_captures(d)) .ra_fail("Stone cannot be captured while an unprotected stone exists")
    d$board[[i+1L]]<- -1L;return(.ra_morris_finish(d,TRUE))
  }
  placing<-sum(unlist(d$placed))<18;expected<-if(placing) "place" else "move"
  if(kind!=expected) .ra_fail("Action is not permitted in this phase")
  schema<-if(placing) list(cell=list(type="integer",minimum=0,maximum=23)) else list(source=list(type="integer",minimum=0,maximum=23),target=list(type="integer",minimum=0,maximum=23))
  .ra_fields(params,schema,"/params")
  candidates<-.ra_morris_ordinary(d)
  if(!any(vapply(candidates,function(a) all(vapply(names(schema),function(k) a$params[[k]]==params[[k]],logical(1))),logical(1)))) .ra_fail("Illegal placement or movement")
  i<-if(placing) params$cell else params$target
  if(placing) d$placed[[p+1L]]<-d$placed[[p+1L]]+1L else d$board[[params$source+1L]]<- -1L
  d$board[[i+1L]]<-p
  if(.ra_morris_in_mill(d$board,i)) {d$pending<-TRUE;return(d)}
  .ra_morris_finish(d,placing)
}
.ra_morris_legal <- function(state) {d<-state$data;if(d$pending) lapply(.ra_morris_captures(d),function(i) list(type="capture",params=list(cell=i))) else .ra_morris_ordinary(d)}
.ra_morris_outcome <- function(state,pack) {
  result<-.ra_morris_result(state$data);w<-result$winner;terminal<-!is.null(w)
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(w==-1) "draw" else "win"} else "none",winners=if(terminal && w>=0) list(state$players[[w+1L]]$id) else list(),losers=if(terminal && w>=0) list(state$players[[2L-w]]$id) else list(),placements=list(),reason=if(!is.null(result$reason)) result$reason else if(state$data$pending) "awaiting_capture" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
