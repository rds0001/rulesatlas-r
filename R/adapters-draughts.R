.ra_draughts_owner <- function(v) if(v>=0) v%%2 else -1
.ra_draughts_index <- function(r,c) if(r>=0 && r<8 && c>=0 && c<8 && (r+c)%%2==1) r*4+c%/%2 else NULL
.ra_draughts_moves <- function(d,capture) {
  b<-d$board;t<-d$turn;result<-list()
  for(i in 0:31) {
    v<-b[[i+1L]]
    if(.ra_draughts_owner(v)!=t || (!is.null(d$pending) && i!=d$pending)) next
    r<-i%/%4;c<-2*(i%%4)+if(r%%2==0) 1 else 0
    for(dr in if(v>=2) c(-1,1) else if(t==0) 1 else -1) for(dc in c(-1,1)) {
      j<-.ra_draughts_index(r+dr,c+dc);if(is.null(j)) next
      if(capture) {
        k<-.ra_draughts_index(r+2*dr,c+2*dc)
        if(.ra_draughts_owner(b[[j+1L]])==1-t && !is.null(k) && b[[k+1L]]==-1) result[[length(result)+1L]]<-list(start=i,end=k,captured=j)
      } else if(b[[j+1L]]==-1) result[[length(result)+1L]]<-list(start=i,end=j,captured=NULL)
    }
  }
  result
}
.ra_draughts_options <- function(d) {x<-.ra_draughts_moves(d,TRUE);if(length(x)) x else if(!is.null(d$pending)) list() else .ra_draughts_moves(d,FALSE)}
.ra_draughts_key <- function(d) paste0(paste0(unlist(d$board)+1,collapse=""),d$turn)
.ra_draughts_validate <- function(d) {
  b<-unlist(d$board);owners<-vapply(b,.ra_draughts_owner,numeric(1));rows<-(0:31)%/%4
  if(any(vapply(0:1,function(p) sum(owners==p)>12,logical(1))) || any((b==0 & rows==7) | (b==1 & rows==0))) .ra_fail("Invalid piece counts or uncrowned back-rank man",code="INVALID_STATE")
  if(!is.null(d$pending)) {
    if(!length(.ra_draughts_moves(d,TRUE)) || d$quiet!=0 || length(d$history)) .ra_fail("Pending multi-capture must have another jump and reset draw history",code="INVALID_STATE")
  } else {
    if(length(d$history)!=d$quiet+1 || !length(d$history) || tail(unlist(d$history),1)!=.ra_draughts_key(d)) .ra_fail("Draw history and quiet counter mismatch",code="INVALID_STATE")
    if(any(!grepl("^[01234]{32}[01]$",unlist(d$history)))) .ra_fail("Malformed repetition key",code="INVALID_STATE")
  }
}
.ra_draughts_apply <- function(state,action) {
  if(action$type!="move") .ra_fail("Unknown action")
  p<-action$params;.ra_fields(p,list(start=list(type="integer",minimum=0,maximum=31),end=list(type="integer",minimum=0,maximum=31)),"/params");d<-state$data;t<-d$turn
  if(!is.null(action$actor) && action$actor!=state$players[[t+1L]]$id) .ra_fail("Wrong actor")
  match<-Filter(function(m) m$start==p$start && m$end==p$end,.ra_draughts_options(d))
  if(!length(match)) .ra_fail("Illegal move: capture is compulsory and a capture sequence must continue with the same piece")
  m<-match[[1]];i<-m$start;j<-m$end;piece<-d$board[[i+1L]];d$board[[i+1L]]<--1L;d$board[[j+1L]]<-piece
  crown<-piece<2 && j%/%4==if(t==0) 7 else 0
  if(crown) d$board[[j+1L]]<-piece+2L
  if(!is.null(m$captured)) {
    d$board[[m$captured+1L]]<--1L;d$pending<-j;d$quiet<-0L;d$history<-list()
    if(!crown && length(.ra_draughts_moves(d,TRUE))) return(d)
  }
  d["pending"]<-list(NULL);d$turn<-1L-t
  if(!is.null(m$captured) || piece<2) {d$quiet<-0L;d$history<-list(.ra_draughts_key(d))} else {d$quiet<-d$quiet+1L;d$history[[length(d$history)+1L]]<-.ra_draughts_key(d)}
  d
}
.ra_draughts_legal <- function(state) lapply(.ra_draughts_options(state$data),function(m) list(type="move",params=list(start=m$start,end=m$end)))
.ra_draughts_outcome <- function(state,pack) {
  d<-state$data;blocked<-is.null(d$pending) && !length(.ra_draughts_options(d));draw<-is.null(d$pending) && !blocked && (d$quiet>=80 || sum(unlist(d$history)==.ra_draughts_key(d))>=3);terminal<-blocked || draw;win<-1L-d$turn
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(blocked) "win" else if(draw) "draw" else "none",winners=if(blocked) list(state$players[[win+1L]]$id) else list(),losers=if(blocked) list(state$players[[d$turn+1L]]$id) else list(),placements=list(),reason=if(blocked) "no_legal_move" else if(draw) "quiet_or_threefold_draw" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
