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
      let norma (v:f32) : f32 = v * (s.a_range.1 - s.a_range.0) / 15.0f32
      let normx (v:f32) : f32 = v * (s.x_range.1 - s.x_range.0) / 5.0f32
      let ar = (s.a_range.0 + norma(td*r32(s.moving.horiz)),  -- move left/right
		s.a_range.1 + norma(td*r32(s.moving.horiz)))
      let ar2 = (ar.0 + norma(td*r32(s.moving.zoom)),         -- zoom horiz in/out
		 ar.1 - norma(td*r32(s.moving.zoom)))
      let xr = (s.x_range.0 + normx(td*r32(s.moving.vert)),   -- move up/down
		s.x_range.1 + normx(td*r32(s.moving.vert)))
      let xr2 = (xr.0 + normx(td*r32(s.moving.zoom)),         -- zoom vert in/out
		 xr.1 + normx(td*r32(s.moving.zoom)))
      in s with a_range = ar2
           with x_range = xr2
    case #keydown {key} -> keydown key s
    case #keyup {key} -> keyup key s
    case #mouse _ -> s
    case #wheel _ -> s

  let gen_column (s:state) (v:i32) : []argb.colour =
    let a = f64.f32 s.a_range.0 + r64 v * f64.f32(s.a_range.1-s.a_range.0)/ r64 s.w
    let next x = a * x * (1.0f64-x)
    let x = loop x=0.25f64 for _i < s.n0 do next x
    let counts = replicate s.h 0
    let nz = 0
    let (_,counts,nz) =
      unsafe
      loop (x,counts,nz) for _i < s.n do
        let x' = next x
	let i : i32 = i32.f64((x' - f64.f32 s.x_range.0) / f64.f32 (s.x_range.1-s.x_range.0) * r64 s.h)
	let (counts,nz) = if i >= s.h || i < 0 then (counts,nz)
			  else let nz = if counts[i] == 0 then nz + 1 else nz
                               let counts[i] = counts[i] + 1
  		               in (counts,nz)
        in (x',counts,nz)
    let cs = map (\c -> let c' = c * nz in if c' > s.n then s.n else c') counts
    let fs = map (\c -> r32(c) / r32 s.n) cs
    let col = map (\f ->
		     let r = r32 v / r32 s.h
		     let b = 1.0 - r
		     let g = (r + b) / 2.0f32
		     in argb.from_rgba (1.0-r*f) (1.0-g*f) (1.0-b*f) 1.0) fs
    in col
    |> reverse

  let render (s: state) =
    map (\x -> gen_column s x) (iota s.w)
    |> transpose

  type text_content = text_content

  let text_format () =
    "FPS: %d\nA-range: (%f, %f)\nX-range: (%f,%f)\nZoom: z/x\nMove: Arrows"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration,
     s.a_range.0, s.a_range.1,
     s.x_range.0, s.x_range.1)

  let text_colour = const argb.blue
}
