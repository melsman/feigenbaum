
module type STMT = {
  type reg
  val ax : reg
  val bx : reg
  val cx : reg
  val dx : reg

  type stmt = #f64 f64
            | #add reg   -- add r : ax <- ax + r
            | #sub reg   -- add r : ax <- ax - r
            | #mul reg   -- mul r : ax <- ax * r
            | #sin       -- sin   : ax <- sin ax
            | #cos       -- cos   : ax <- cos ax
            | #sto reg   -- sto r :  r <- ax
            | #ld  reg   --  ld r : ax <- r

  type rfile
  val emp : f64 -> rfile
  val set : rfile -> reg -> f64 -> rfile
  val get : rfile -> reg -> f64

  val eval [n] : rfile -> [n]stmt -> rfile
}

module stmt : STMT = {
  type reg = #ax | #bx | #cx | #dx
  let ax : reg = #ax
  let bx : reg = #bx
  let cx : reg = #cx
  let dx : reg = #dx

  type stmt = #f64 f64
            | #add reg   -- add r : ax <- ax + r
            | #sub reg   -- add r : ax <- ax - r
            | #mul reg   -- mul r : ax <- ax * r
            | #sin       -- sin   : ax <- sin ax
            | #cos       -- cos   : ax <- cos ax
            | #sto reg   -- sto r :  r <- ax
            | #ld  reg   --  ld r : ax <- r

  type rfile = (f64,f64,f64,f64)

  let emp (v:f64) : rfile = (v,v,v,v)

  let set ((a,b,c,d):rfile) (r:reg) (v:f64) : rfile =
    match r
    case #ax -> (v,b,c,d)
    case #bx -> (a,v,c,d)
    case #cx -> (a,b,v,d)
    case #dx -> (a,b,c,v)

  let get ((a,b,c,d):rfile) (r:reg) : f64 =
    match r
    case #ax -> a
    case #bx -> b
    case #cx -> c
    case #dx -> d

  let ev (rf: rfile) (s:stmt) : rfile =
    match s
    case #f64 v -> set rf ax v
    case #add r -> let v = get rf ax + get rf r in set rf ax v
    case #sub r -> let v = get rf ax - get rf r in set rf ax v
    case #mul r -> let v = get rf ax * get rf r in set rf ax v
    case #sin -> let v = f64.sin(get rf ax) in set rf ax v
    case #cos -> let v = f64.cos(get rf ax) in set rf ax v
    case #sto r -> let v = get rf ax in set rf r v
    case #ld r -> let v = get rf r in set rf ax v

  let eval [n] (rf: rfile) (ss:[n]stmt) : rfile =
    loop rf for i < n do ev rf ss[i]
}
