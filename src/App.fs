module App

open System
open Browser.Dom
open Browser.Types
open Fable.Core

type Point = float * float

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

let parabola (focusX: float, focusY: float) (directix: float) =
    fun x ->
        (1. / (2. * (focusY - directix))) * ((x - focusX) ** 2.) + ((focusY + directix) / 2.)

let draw (ctx: CanvasRenderingContext2D) focus directix =
    clear ctx
    point ctx focus
    horizontalLine ctx directix
    parabola focus directix |> curve ctx

let focus = (200., 200.)
let mutable directix = 180.

body.onkeypress <- fun ev ->
    directix <-
        match ev.key with
        | "j" -> directix + 5.
        | "k" -> directix - 5.
        | _ -> directix
    draw ctx focus directix

draw ctx focus directix