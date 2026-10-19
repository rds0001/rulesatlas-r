.ra_connection_winners <- function(board,config) {
  b<-unlist(board);n<-config$size
  wins<-numeric()
  if(config$mode=="gomoku") {
    dirs<-list(c(0,1),c(1,0),c(1,1),c(1,-1))
    for(r in 0:(n-1L)) for(c in 0:(n-1L)) {
      p<-b[r*n+c+1L];if(p==-1) next
      for(dir in dirs) {
        rr<-r+4*dir[1];cc<-c+4*dir[2]
        if(rr>=0 && rr<n && cc>=0 && cc<n && all(vapply(1:4,function(k) b[(r+k*dir[1])*n+c+k*dir[2]+1L]==p,logical(1)))) wins<-unique(c(wins,p))
      }
    }
    return(sort(wins))
  }
  dirs<-list(c(-1,0),c(-1,1),c(0,-1),c(0,1),c(1,-1),c(1,0))
  for(p in 0:1) {
    idx<-0:(n*n-1L);starts<-idx[b==p & (if(p==0) idx%/%n==0 else idx%%n==0)]
    seen<-rep(FALSE,n*n);seen[starts+1L]<-TRUE;todo<-starts
    while(length(todo)) {
      i<-tail(todo,1);todo<-head(todo,-1);r<-i%/%n;c<-i%%n
      if(if(p==0) r==n-1 else c==n-1) {wins<-c(wins,p);break}
      for(dir in dirs) {
        rr<-r+dir[1];cc<-c+dir[2];j<-rr*n+cc
        if(rr>=0 && rr<n && cc>=0 && cc<n && !seen[j+1L] && b[j+1L]==p) {seen[j+1L]<-TRUE;todo<-c(todo,j)}
      }
    }
  }
  wins
}
.ra_connection_validate <- function(d,config) {
  b<-unlist(d$board);a<-sum(b==0);c<-sum(b==1);wins<-.ra_connection_winners(d$board,config)
  if(!a %in% c(c,c+1) || d$turn!=if(a==c) 0 else 1) .ra_fail("Invalid mark counts or turn",code="INVALID_STATE")
  if(length(wins)>1) .ra_fail("Simultaneous winners",code="INVALID_STATE")
  if(length(wins) && a-c!=if(wins[1]==0) 1 else 0) .ra_fail("Winner is not the last mover",code="INVALID_STATE")
}
.ra_connection_legal <- function(state,config) lapply(which(unlist(state$data$board)==-1)-1L,function(i) list(type="place",params=list(cell=i)))
.ra_connection_apply <- function(state,action,config) {
  if(action$type!="place") .ra_fail("Unknown action")
  .ra_fields(action$params,list(cell=list(type="integer",minimum=0,maximum=config$size^2-1)),"/params")
  d<-state$data;i<-action$params$cell+1L;p<-d$turn
  if(d$board[[i]]!=-1) .ra_fail("Occupied square")
  if(!is.null(action$actor) && action$actor!=state$players[[p+1L]]$id) .ra_fail("Wrong actor")
  d$board[[i]]<-p;d$turn<-1L-p;d
}
.ra_connection_outcome <- function(state,pack,config) {
  b<-unlist(state$data$board);wins<-.ra_connection_winners(state$data$board,config);terminal<-length(wins)>0 || !(-1 %in% b);w<-if(length(wins)) wins[1] else NULL
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(terminal) {if(is.null(w)) "draw" else "win"} else "none",winners=if(is.null(w)) list() else list(state$players[[w+1L]]$id),losers=if(is.null(w)) list() else list(state$players[[2L-w]]$id),placements=list(),reason=if(!is.null(w)) {if(config$mode=="gomoku") "five_in_a_row" else "connected_edges"} else if(terminal) "board_full" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
