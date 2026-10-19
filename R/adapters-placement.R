.ra_reversi_flips <- function(board,cell,player) {
  b<-unlist(board);if(b[cell+1L]!=-1) return(integer())
  row<-cell%/%8;col<-cell%%8;result<-integer()
  for(dr in -1:1) for(dc in -1:1) {
    if(dr==0 && dc==0) next
    r<-row+dr;c<-col+dc;run<-integer()
    while(r>=0 && r<8 && c>=0 && c<8 && b[r*8+c+1L]==1-player) {run<-c(run,r*8+c);r<-r+dr;c<-c+dc}
    if(length(run) && r>=0 && r<8 && c>=0 && c<8 && b[r*8+c+1L]==player) result<-c(result,run)
  }
  result
}
.ra_reversi_moves <- function(board,player) {
  cells<-which(unlist(board)==-1)-1L
  cells[vapply(cells,function(cell) length(.ra_reversi_flips(board,cell,player))>0,logical(1))]
}
.ra_reversi_validate <- function(data) {if(any(unlist(data$board)[c(28,29,36,37)]==-1)) .ra_fail("Starting center discs cannot be empty",code="INVALID_STATE")}
.ra_reversi_outcome <- function(state,pack) {
  b<-state$data$board;terminal<-!length(.ra_reversi_moves(b,0)) && !length(.ra_reversi_moves(b,1));counts<-c(sum(unlist(b)==0),sum(unlist(b)==1));w<-if(!terminal || counts[1]==counts[2]) NULL else if(counts[1]>counts[2]) 0L else 1L
  list(status=if(terminal) "terminal" else "ongoing",result_kind=if(!terminal) "none" else if(is.null(w)) "draw" else "win",winners=if(is.null(w)) list() else list(state$players[[w+1L]]$id),losers=if(is.null(w)) list() else list(state$players[[2L-w]]$id),placements=list(),reason=if(terminal) "no_legal_moves" else "ongoing",evidence_required=list(),rule_id=if(terminal) paste0(pack$ruleset_id,"@",pack$ruleset_version) else NULL)
}
.ra_reversi_apply <- function(state,action) {
  board<-state$data$board;turn<-state$data$turn
  if(!is.null(action$actor) && action$actor!=state$players[[turn+1L]]$id) .ra_fail("Wrong actor")
  if(identical(action$type,"pass")) {
    .ra_fields(action$params,.ra_object(),"/params")
    if(length(.ra_reversi_moves(board,turn)) || !length(.ra_reversi_moves(board,1-turn))) .ra_fail("Pass is not forced")
  } else if(identical(action$type,"place")) {
    .ra_fields(action$params,list(cell=list(type="integer",minimum=0,maximum=63)),"/params")
    cell<-action$params$cell;captured<-.ra_reversi_flips(board,cell,turn)
    if(!length(captured)) .ra_fail("Move captures no discs")
    for(i in c(cell,captured)) board[[i+1L]]<-turn
  } else .ra_fail("Unknown action")
  list(board=board,turn=1-turn)
}
.ra_reversi_legal <- function(state) {
  cells<-.ra_reversi_moves(state$data$board,state$data$turn)
  if(length(cells)) lapply(cells,function(cell) list(type="place",params=list(cell=cell))) else list(list(type="pass",params=.ra_object()))
}
