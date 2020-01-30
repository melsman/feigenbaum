import "lib/github.com/diku-dk/lys/lys"

type text_content = (i32, f32, f32, f32, f32)
module lys: lys with text_content = text_content = {
  type state = {h: i32, w: i32,      -- window height and width
		n0: i32,             -- warmup iterations
		n: i32,              -- number of iterations
		a_range: (f32, f32),
                x_range: (f32, f32),
                moving: {zoom:i32, horiz:i32, vert:i32},
                paused: bool
               }

  let grab_mouse = false

  let init (_seed: u32) (h: i32) (w: i32): state =
    {h,w,
     n0=1000,
     n=10000,
     a_range=(3.5,4.0),
     x_range=(0.0,1.0),
     moving={zoom=0,horiz=0,vert=0},
     paused = false
    }

  let resize (h: i32) (w: i32) (s: state) =
    s with h = h with w = w

  let keydown (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.horiz = -1
    else if key == SDLK_LEFT then s with moving.horiz = 1
    else if key == SDLK_UP then s with moving.vert = -1
    else if key == SDLK_DOWN then s with moving.vert = 1
    else if key == SDLK_z then s with moving.zoom = 1
    else if key == SDLK_x then s with moving.zoom = -1
    else if key == SDLK_SPACE then s with paused = !s.paused
    else s

  let keyup (key: i32) (s: state) =
    if key == SDLK_RIGHT then s with moving.horiz = 0
    else if key == SDLK_LEFT then s with moving.horiz = 0
    else if key == SDLK_UP then s with moving.vert = 0
    else if key == SDLK_DOWN then s with moving.vert = 0
    else if key == SDLK_z then s with moving.zoom = 0
    else if key == SDLK_x then s with moving.zoom = 0
    else s

  let event (e: event) (s: state) =
    match e
    case #step td ->
      let norma (v:f32) : f32 =
	f32.f64(f64.f32 v * (f64.f32 s.a_range.1 - f64.f32 s.a_range.0) / 15.0)
      let normx (v:f32) : f32 =
	f32.f64(f64.f32 v * (f64.f32 s.x_range.1 - f64.f32 s.x_range.0) / 5.0)
      let ar =                                     -- move left/right
	let da = norma(td*r32(s.moving.horiz))
	in (s.a_range.0 + da, s.a_range.1 + da)
      let ar2 =                                    -- zoom horiz in/out
	let da = norma(td*r32(s.moving.zoom))
	in (ar.0 + da, ar.1 - da)
	let xr =                                   -- move up/down
	  let dx = normx(td*r32(s.moving.vert))
	  in (s.x_range.0 + dx,	s.x_range.1 + dx)
      let xr2 =                                    -- zoom vert in/out
	let dx = normx(td*r32(s.moving.zoom))
	in (xr.0 + dx, xr.1 - dx)
      in s with a_range = ar2 with x_range = xr2
    case #keydown {key} -> keydown key s
    case #keyup {key} -> keyup key s
    case #mouse _ -> s
    case #wheel _ -> s

  let gen_column (s:state) (h:i32) (v:i32) : [h]argb.colour =
    let a = f64.f32 s.a_range.0 + r64 v * f64.f32(s.a_range.1-s.a_range.0)/ r64 s.w
    let next x = a * x * (1.0-x)
    let x = loop x=0.25 for _i < s.n0 do next x
    let counts = replicate h 0
    let nz = 0
    let hits = 1
    let (_,counts,nz,hits) =
      unsafe
      loop (x,counts,nz,hits) for _i < s.n do
        let x' = next x
	let i : i32 = i32.f64((x' - f64.f32 s.x_range.0)
			      / f64.f32 (s.x_range.1-s.x_range.0) * r64 s.h)
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

  let render (s: state) =
    map (\x -> gen_column s s.h x) (iota s.w)
    |> transpose

  type text_content = text_content

  let text_format () =
    "FPS: %d\nA-range: (%f, %f)\nX-range: (%f,%f)\nZoom: z/x\nMove: Arrows\nQuit: ESC"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration,
     s.a_range.0, s.a_range.1,
     s.x_range.0, s.x_range.1)

  let text_colour = const argb.blue
}
