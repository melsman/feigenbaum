-- Application that allows for inspection of various chaotic maps in
-- terms of bifurcation diagrams.
--
-- See https://en.wikipedia.org/wiki/List_of_chaotic_maps
--
-- Copyright (c) 2020, Martin Elsman
-- MIT License

import "lib/github.com/diku-dk/lys/lys"
import "interp"

type text_content = (i32, i32, f32, f32, f32, f32, f32, i32)
module lys: lys with text_content = text_content = {

  type kind = #logistic | #sincos | #tent | #gauss | #henon | #logistic_interp
  let kinds : []kind = [#logistic,#sincos,#tent,#gauss,#henon,#logistic_interp]

  let pp_kind (k:kind) =
    match k
    case #logistic -> "Logistic"
    case #sincos -> "SinCos"
    case #tent -> "Tent"
    case #gauss -> "Gauss"
    case #henon -> "Henon"
    case #logistic_interp -> "Logistic (Interp)"

  type^ sysdef 'p =
      {kind: kind,
       init: p,                -- initial state
       prj: p -> f64,          -- projection of vertical value
       next: f64 -> p -> p,    -- parameterised recurrence
       ns: (i32,i32),          -- pair of warm-up iteration number
                               -- and hot iteration number (drawing)
       p_rng: (f32,f32),       -- parameter range (horizontal axis)
       x_rng: (f32,f32),       -- range of vertical axis
       upd_xtr: f64 -> p -> p, -- update extra parameter
       xtr: p -> f64           -- extract extra parameter
       }

  let sysdef_logistic : sysdef f64 =
    {kind=#logistic,
     init=0.25,
     prj=\x -> x,
     next=\a x -> a*x*(1.0-x),
     ns=(1000, 10000),
     p_rng=(3.5, 4.0),
     x_rng=(0.0, 1.0),
     upd_xtr=\ _ x -> x,
     xtr=\ _ -> 0.0}

  let sysdef_sincos : sysdef (f64,f64,f64) =
    {kind=#sincos,
     init=(0.1, 0.1, 2.82),
     prj=(.0),
     next=\b (x,y,a) -> (f64.sin(x + a*y),
			 f64.cos(b*x + y),
			 a),
     ns=(1000, 10000),
     p_rng=(0.0, 3.0),
     x_rng=(-1.0, 1.0),
     upd_xtr=\a (x,y,_) -> (x,y,a),
     xtr=(.2)}

  let sysdef_tent : sysdef f64 =
    {kind=#tent,
     init=0.1,
     prj=\x->x,
     next=\mu x -> mu * f64.min x (1.0 - x),
     ns=(1000, 10000),
     p_rng=(1.0, 2.0),
     x_rng=(0.0,1.0),
     upd_xtr=\ _ x -> x,
     xtr=\ _ -> 0.0}

  let sysdef_gauss : sysdef (f64,f64) =
    {kind=#gauss,
     init=(0.1,6.2),
     prj=(.0),
     next=\b (x,a) -> (f64.exp(-a * x * x) + b, a),
     ns=(1000, 10000),
     p_rng=(-1.0, 1.0),
     x_rng=(-1.0, 1.5),
     upd_xtr=\a (x,_) -> (x,a),
     xtr=(.1)}

  let sysdef_henon : sysdef (f64,f64,f64) =
    {kind=#henon,
     init=(0.1, 0.1, 0.3),
     prj=(.0),
     next=\a (x,y,b) -> (1.0 - a*x*x + y,
			 b * x,
			 b),
     ns=(1000, 10000),
     p_rng=(1.0, 1.45),
     x_rng=(-1.5, 1.5),
     upd_xtr=\b (x,y,_) -> (x,y,b),
     xtr=(.2)}

  let sysdef_logistic_interp : sysdef stmt.rfile =
    {kind=#logistic_interp,
     init=stmt.set (stmt.emp 0.0) stmt.ax 0.25,
     prj=\rf -> stmt.get rf stmt.ax,
     next=\a rf -> let ss = stmt.([#sto bx,       -- bx <- ax
				   #f64 1.0,      -- ax <- 1.0
				   #sub bx,       -- ax <- ax - bx
				   #mul bx,       -- ax <- ax * bx
				   #sto bx,       -- bx <- ax
				   #f64 a,        -- ax <- a
				   #mul bx        -- ax <- ax*bx
				  ])
		   in stmt.eval rf ss,
     ns=(1000, 10000),
     p_rng=(3.5, 4.0),
     x_rng=(0.0, 1.0),
     upd_xtr=\ _ x -> x,
     xtr=\ _ -> 0.0}

  -- Internal stuff

  let pp_kinds [n] (ks:[n]kind) =
    (loop s="[" for i < n do
       let v = s ++ pp_kind (ks[i])
       in if i == n-1 then v
 	  else v ++ "|")
    |> (++"]")

  let kinds_string = pp_kinds kinds
  let kinds_idx_max : i32 = length kinds - 1
  let kinds_idx (k:kind) : i32 =
    reduce (\ (k1,i1) (k2,i2) ->
	      if k1==k then if k2==k then (k,i32.max i1 i2)
			    else (k1,i1)
			    else (k2,i2)
	   ) (k,0) (zip kinds (iota(length(kinds))))
    |> (.1)

  let grab_mouse = false
  let header_height = 55i32

  type state = {h: i32, w: i32,      -- window height and width
		kind: kind,
		n0: i32,             -- warmup iterations
		n: i32,              -- number of iterations
		p_rng: (f32, f32),
                x_rng: (f32, f32),
                moving: {zoom:i32, horiz:i32, vert:i32, xtr:i32},
                paused: bool,
		xtr: f64
               }

  let mk_init 'a (sd: sysdef a) (h:i32) (w:i32) : state =
    {h,w,
     kind=sd.kind,
     n0=sd.ns.0,
     n=sd.ns.1,
     p_rng=sd.p_rng,
     x_rng=sd.x_rng,
     moving={zoom=0,horiz=0,vert=0,xtr=0},
     paused=false,
     xtr=sd.xtr(sd.init)}

  let init_logistic : i32 -> i32 -> state = mk_init sysdef_logistic
  let init_sincos : i32 -> i32 -> state = mk_init sysdef_sincos
  let init_tent   : i32 -> i32 -> state = mk_init sysdef_tent
  let init_gauss  : i32 -> i32 -> state = mk_init sysdef_gauss
  let init_henon  : i32 -> i32 -> state = mk_init sysdef_henon
  let init_logistic_interp : i32 -> i32 -> state = mk_init sysdef_logistic_interp

  let init (_seed: u32) (h: i32) (w: i32) : state =
    init_logistic (h-header_height) w

  let resize (h: i32) (w: i32) (s: state) =
    s with h = h-header_height with w = w

  let keydown (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.horiz = -1
    else if key == SDLK_LEFT then s with moving.horiz = 1
    else if key == SDLK_UP then s with moving.vert = -1
    else if key == SDLK_DOWN then s with moving.vert = 1
    else if key == SDLK_z then s with moving.zoom = 1
    else if key == SDLK_x then s with moving.zoom = -1
    else if key == SDLK_u then s with moving.xtr = 1
    else if key == SDLK_j then s with moving.xtr = -1
    else if key == SDLK_SPACE then s with paused = !s.paused
    else if key == SDLK_1 then init_logistic s.h s.w
    else if key == SDLK_2 then init_sincos s.h s.w
    else if key == SDLK_3 then init_tent s.h s.w
    else if key == SDLK_4 then init_gauss s.h s.w
    else if key == SDLK_5 then init_henon s.h s.w
    else if key == SDLK_6 then init_logistic_interp s.h s.w
    else s

  let keyup (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.horiz = 0
    else if key == SDLK_LEFT then s with moving.horiz = 0
    else if key == SDLK_UP then s with moving.vert = 0
    else if key == SDLK_DOWN then s with moving.vert = 0
    else if key == SDLK_z then s with moving.zoom = 0
    else if key == SDLK_x then s with moving.zoom = 0
    else if key == SDLK_u then s with moving.xtr = 0
    else if key == SDLK_j then s with moving.xtr = 0
    else s

  let event (e: event) (s: state) : state =
    match e
    case #step td ->
      let norma (v:f32) : f32 =
	f32.f64(f64.f32 v * (f64.f32 s.p_rng.1 - f64.f32 s.p_rng.0) / 15.0)
      let normx (v:f32) : f32 =
	f32.f64(f64.f32 v * (f64.f32 s.x_rng.1 - f64.f32 s.x_rng.0) / 5.0)
      let ar =                                     -- move left/right
	let da = norma(td*r32(s.moving.horiz))
	in (s.p_rng.0 + da, s.p_rng.1 + da)
      let ar2 =                                    -- zoom horiz in/out
	let da = norma(td*r32(s.moving.zoom))
	in (ar.0 + da, ar.1 - da)
      let xr =                                   -- move up/down
	  let dx = normx(td*r32(s.moving.vert))
	  in (s.x_rng.0 + dx,	s.x_rng.1 + dx)
      let xr2 =                                    -- zoom vert in/out
	let dx = normx(td*r32(s.moving.zoom))
	in (xr.0 + dx, xr.1 - dx)
      in s with p_rng = ar2
           with x_rng = xr2
           with xtr = s.xtr + f64.f32 td * r64(s.moving.xtr) / 25.0

    case #keydown {key} -> keydown key s
    case #keyup {key} -> keyup key s
    case #mouse _ -> s
    case #wheel _ -> s

  let gen_column0 'p (sd:sysdef p)
                     (s:state) (h:i32) (v:i32) : [h]argb.colour =
    let a = f64.f32 s.p_rng.0 + r64 v * f64.f32(s.p_rng.1-s.p_rng.0)/ r64 s.w
    let nxt (x:p) : p = sd.next a x
    let x : p = loop x=sd.upd_xtr s.xtr sd.init for _i < s.n0 do nxt x
    let counts = replicate h 0
    let nz = 0
    let hits = 1
    let (_,counts,nz,hits) =
      unsafe
      loop (x,counts,nz,hits) for _i < s.n do
        let x' = nxt x
	let i : i32 = i32.f64((sd.prj x' - f64.f32 s.x_rng.0)
			      / f64.f32 (s.x_rng.1-s.x_rng.0) * r64 s.h)
	let (counts,nz,hits) = if i >= s.h || i < 0 then (counts,nz,hits)
			       else let nz = if counts[i] == 0 then nz + 1 else nz
				    let hits = hits + 1
				    let counts[i] = counts[i] + 1
  				    in (counts,nz,hits)
        in (x',counts,nz,hits)
    let cs = map (\c -> let c' = c * nz in if c' > hits then hits else c') counts
    let fs = map (\c -> r32 c / r32 hits) cs
    let col = map (\f ->
		     let r = r32 v / r32 s.h
		     let b = 1.0 - r
		     let g = (r + b) / 2.0f32
		     in argb.from_rgba (1.0-r*f) (1.0-g*f) (1.0-b*f) 1.0) fs
    in col
    |> reverse

  let gen_column (s:state) (h:i32) (v:i32) : [h]argb.colour =
    match s.kind
    case #logistic -> gen_column0 sysdef_logistic s h v
    case #sincos -> gen_column0 sysdef_sincos s h v
    case #tent -> gen_column0 sysdef_tent s h v
    case #gauss -> gen_column0 sysdef_gauss s h v
    case #henon -> gen_column0 sysdef_henon s h v
    case #logistic_interp -> gen_column0 sysdef_logistic_interp s h v

  let render (s: state) =
    map (\x -> (replicate header_height argb.white ++
		gen_column s s.h x)) (iota s.w)
    |> transpose

  type text_content = text_content

  let text_format () =
    "FPS: %d | Kind: %" ++ kinds_string ++
    " | X-range: (%f, %f) | Y-range: (%f,%f) | Xtr: %f\nControls: z/x (zoom), arrows (move), 1-%d (kinds), u/j (Xtr incr/decr), esc (quit)"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration, kinds_idx s.kind,
     s.p_rng.0, s.p_rng.1,
     s.x_rng.0, s.x_rng.1,
     f32.f64(s.xtr), kinds_idx_max+1)

  let text_colour = const argb.black
}
