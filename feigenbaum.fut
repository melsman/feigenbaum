-- Application that allows for inspection of various chaotic maps in
-- terms of bifurcation diagrams.
--
-- See https://en.wikipedia.org/wiki/List_of_chaotic_maps
--
-- Copyright (c) 2020, Martin Elsman
-- MIT License

import "lib/github.com/diku-dk/lys/lys"
import "interp"

type text_content = (i32, i32, f32, f32, f32, f32, f32, i32, i32)
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

  -- colour schemes
  module colour_scheme : {
    type t
    val init : t
    val next : t -> t
    val swap 'a : t -> (a,a,a) -> (a,a,a)
    val adjust : t -> (f32,f32,f32) -> (f32,f32,f32)
    val to_i32 : t -> i32  -- for pretty printing
  } = {
    type t = i32
    let init = 0i32
    let max = 7i32
    let next (cs:t) : t =
      (cs + 1) % (max+1)
    let swap (cs:t) (r,g,b) =
      if cs == 0 then (r,g,b)
      else if cs == 1 then (r,b,g)
      else if cs == 2 then (g,r,b)
      else if cs == 3 then (g,b,r)
      else if cs == 4 then (b,r,g)
      else (b,g,r)
    let adjust (cs:t) (h:f32,s:f32,l:f32) : (f32,f32,f32) =
      if cs == 2 then      (0.50 * h,        s,        l)
      else if cs == 3 then (1.25 * h,        s,        l)
      else if cs == 5 then (       h, 1.25 * s,        l)
      else if cs == 6 then (0.80 * h, 0.80 * s, 0.80 * l)
      else if cs == 7 then (0.80 * h, 1.25 * s, 1.25 * l)
      else                 (       h,        s,        l)
    let to_i32 (x:t) = x
  }

  let pp_kinds [n] (ks:[n]kind) =
    (loop s="[" for i < n do
       let v = s ++ pp_kind (ks[i])
       in if i == n-1 then v
 	  else v ++ "|")
    |> (++"]")

  let kinds_string = pp_kinds kinds
  let kinds_idx_max : i64 = length kinds - 1

  let indices [n] 't (_: [n]t) = iota n

  let kinds_idx (k:kind) : i64 =
    reduce (\ (k1,i1) (k2,i2) ->
	      if k1==k then if k2==k then (k,i64.max i1 i2)
			    else (k1,i1)
			    else (k2,i2)
	   ) (k,0) (zip kinds (indices kinds))
    |> (.1)

  let grab_mouse = false
  let header_height = 55i64

  type state = {h: i64, w: i64,      -- window height and width
		kind: kind,
		n0: i32,             -- warmup iterations
		n: i32,              -- number of iterations
		p_rng: (f32, f32),
                x_rng: (f32, f32),
                moving: {zoom:i32, horiz:i32, vert:i32, xtr:i32},
                paused: bool,
		xtr: f64,
		colour_scheme : colour_scheme.t
               }

  let mk_init 'a (sd: sysdef a) (h:i64) (w:i64) : state =
    {h,w,
     kind=sd.kind,
     n0=sd.ns.0,
     n=sd.ns.1,
     p_rng=sd.p_rng,
     x_rng=sd.x_rng,
     moving={zoom=0,horiz=0,vert=0,xtr=0},
     paused=false,
     xtr=sd.xtr(sd.init),
     colour_scheme = colour_scheme.init}

  let init_logistic : i64 -> i64 -> state = mk_init sysdef_logistic
  let init_sincos : i64 -> i64 -> state = mk_init sysdef_sincos
  let init_tent   : i64 -> i64 -> state = mk_init sysdef_tent
  let init_gauss  : i64 -> i64 -> state = mk_init sysdef_gauss
  let init_henon  : i64 -> i64 -> state = mk_init sysdef_henon
  let init_logistic_interp : i64 -> i64 -> state = mk_init sysdef_logistic_interp

  let init (_seed: u32) (h: i64) (w: i64) : state =
    init_logistic (h-header_height) w

  let resize (h: i64) (w: i64) (s: state) =
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
    else if key == SDLK_c then s with colour_scheme = colour_scheme.next s.colour_scheme
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
                     (s:state) (h:i64) (v:i32) : [h]i32 =
    let a = f64.f32 s.p_rng.0 + r64 v * f64.f32(s.p_rng.1-s.p_rng.0)/ f64.i64 s.w
    let nxt (x:p) : p = sd.next a x
    let x : p = loop x=sd.upd_xtr s.xtr sd.init for _i < s.n0 do nxt x
    let counts = replicate h 0
    let nz = 0
    let hits = 1
    let (_,counts,_nz,_hits) =
      loop (x,counts,nz,hits) for _i < s.n do
        let x' = nxt x
	let i = i64.f64((sd.prj x' - f64.f32 s.x_rng.0)
			/ f64.f32 (s.x_rng.1-s.x_rng.0) * f64.i64 s.h)
	let (counts,nz,hits) = if i >= s.h || i < 0 then (counts,nz,hits)
			       else let nz : i32 = if counts[i] == 0 then nz + 1 else nz
				    let hits : i32 = hits + 1
				    let counts[i] = counts[i] + 1
  				    in (counts,nz,hits)
        in (x',counts,nz,hits)
    -- let cs = map (\c -> let c' = c * nz in if c' > hits then hits else c') counts
    -- let fs = map (\c -> r32 c / r32 hits) cs
    -- let col = map (\f ->
    -- 		     let r = r32 v / r32 s.h
    -- 		     let b = 1.0 - r
    -- 		     let g = (r + b) / 2.0f32
    -- 		     in argb.from_rgba (1.0-r*f) (1.0-g*f) (1.0-b*f) 1.0) fs
    in counts
    |> reverse

  let gen_column (s:state) (h:i64) (v:i32) : [h]i32 =
    match s.kind
    case #logistic -> gen_column0 sysdef_logistic s h v
    case #sincos -> gen_column0 sysdef_sincos s h v
    case #tent -> gen_column0 sysdef_tent s h v
    case #gauss -> gen_column0 sysdef_gauss s h v
    case #henon -> gen_column0 sysdef_henon s h v
    case #logistic_interp -> gen_column0 sysdef_logistic_interp s h v

  let hsl_value (n1: f32) (n2: f32) (hue: f32): f32 =
    let hue' = if hue > 6.0
               then hue - 6.0
               else if hue < 0.0
               then hue + 6.0
               else hue
    in if hue' < 1.0
       then n1 + (n2 - n1) * hue'
       else if hue' < 3.0
       then n2
       else if hue' < 4.0
       then n1 + (n2 - n1) * (4.0 - hue')
       else n1

  let hsl_to_rgb (h: f32) (s: f32) (l: f32): (f32, f32, f32) =
    if s == 0.0
    then (l, l, l)
    else let m2 = if l <= 0.5
                  then l * (1.0 + s)
                  else l + s - l * s
	 let m1 = 2.0 * l - m2
	 let r = hsl_value m1 m2 (h * 6.0 + 2.0)
	 let g = hsl_value m1 m2 (h * 6.0)
	 let b = hsl_value m1 m2 (h * 6.0 - 2.0)
	 in (r, g, b)

  let mapi [n] 'a 'b (f:a->i64->b) (xs:[n]a) : [n]b =
    let xsi = zip xs (iota n)
    in map (\(x,i) -> f x i) xsi

  let colourise [h][w] (st:state) (frame: [h][w]i32) : [h][w]argb.colour =
    let m = reduce i32.max 0 (flatten frame)
        -- colours are between 0 (white) and 256*256*256-1 (black)

    let f = map (mapi (\c i ->
			if c == 0 then argb.white
			else let (h,s,l) = (0.5 + r32 c / r32 m, 0.5, 0.2 + 0.5 * f32.i64 i/f32.i64 w)
		                           |> colour_scheme.adjust st.colour_scheme
		             let (r,g,b) = hsl_to_rgb h s l
			                   |> colour_scheme.swap st.colour_scheme
			     in argb.from_rgba r g b 1.0)) frame
    in f

  let render (s: state) =
    map (\x -> replicate header_height 0 ++ gen_column s s.h (i32.i64 x))
        (iota s.w)
    |> transpose
    |> colourise s

  type text_content = text_content

  let text_format () =
    "FPS: %d | Kind: %" ++ kinds_string ++
    " | X-range: (%.2f, %.2f) | Y-range: (%.2f,%.2f) | Xtr: %.2f | Color: %d\nControls: z/x (zoom), arrows (move), 1-%d (kinds), u/j (Xtr incr/decr), c (color toggle), esc (quit)"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration, i32.i64 (kinds_idx s.kind),
     s.p_rng.0, s.p_rng.1,
     s.x_rng.0, s.x_rng.1,
     f32.f64(s.xtr), colour_scheme.to_i32 s.colour_scheme,i32.i64 (kinds_idx_max+1))

  let text_colour = const argb.black
}
