module App

open System
open Browser.Dom
open Browser.Types
open Fable.Core
open Voronoi

type Point = float * float

let rnd = Random()

let height = 400.
let width = 400.

let body = (document.getElementsByTagName "body").[0] :?> HTMLBodyElement

let canvas = document.getElementById "canvas" :?> HTMLCanvasElement 
canvas.height <- height
canvas.width <- width

let ctx = canvas.getContext_2d()

let clear (ctx: CanvasRenderingContext2D) =
    ctx.clearRect (0., 0., width, height)

let point (ctx: CanvasRenderingContext2D) (p: Point) =
    let (x, y) = p
    ctx.beginPath ()
    ctx.arc (x, y, 1., 0., 2. * Math.PI, true)
    ctx.fill ()

let line (ctx: CanvasRenderingContext2D) fromPoint toPoint =
    ctx.moveTo fromPoint
    ctx.lineTo toPoint
    ctx.stroke ()

let horizontalLine (ctx: CanvasRenderingContext2D) y =
    line ctx (0., y) (width, y)

let stroke (ctx: CanvasRenderingContext2D) (points: Point list) =
    ctx.beginPath ()
    points |> List.head |> ctx.moveTo
    points |> List.tail |> List.iter ctx.lineTo
    ctx.stroke ()

let fill (ctx: CanvasRenderingContext2D) (color: string) (points: Point list) =
    ctx.fillStyle <- U3.Case1(color)
    ctx.beginPath ()
    points |> List.head |> ctx.moveTo
    points |> List.tail |> List.iter ctx.lineTo
    ctx.fill ()

let curve (ctx: CanvasRenderingContext2D) (parabola: float -> float) =
    ctx.moveTo (0., (parabola 0.))
    [0. .. width]
    |> List.map (fun x -> (x, (parabola x)))
    |> stroke ctx

let draw (ctx: CanvasRenderingContext2D) points directix =
    clear ctx
    points |> List.iter (point ctx)
    directix |> horizontalLine ctx
    points
    |> List.filter (fun (_, y) -> y < directix)
    |> List.map (fun focus -> parabola focus directix)
    |> List.iter (fun parabola -> curve ctx parabola)

let getRandomPoints count: Point list =
    List.init count (fun _ ->
        let x = (float (rnd.Next (int width)))
        let y = (float (rnd.Next (int height)))
        (x, y))

let focus = (200., 200.)
let mutable directix = 180.
let points = getRandomPoints 10

body.onkeypress <- fun ev ->
    directix <-
        match ev.key with
        | "j" -> directix + 5.
        | "k" -> directix - 5.
        | _ -> directix
    draw ctx points directix

draw ctx points directix