module Voronoi

type Vector = float * float

type Arc = {
    focus: Vector;
}

type Edge = {
    start: Vector;
    direction: Vector;
}

// The "beachline" is defined as a tree where each leaf node
// is an arc and all non-leaf nodes are edges.
//
//      E
//     / \
//    A   E
//       / \
//      A   A
//
type BeachLineItem =
    | Empty
    | Arc of Arc
    | Edge of Edge * BeachLineItem seq

type SweepEvent =
    | Site of Vector

// This defines a parabola that marks all points on the plane where the distance to
// the focus is equal to the distance to the nearest point on the directix.
// For directix y = yd and focus (xf, yf), the parabola can be defined as
// y = (1 / 2(yf - yd))(x - xf)^2 + (yf + yd) / 2
// See https://jacquesheunis.com/post/fortunes-algorithm/
let parabola (focusX: float, focusY: float) (directix: float) =
    fun x ->
        (1. / (2. * (focusY - directix))) * ((x - focusX) ** 2.) + ((focusY + directix) / 2.)

let handleEvent (beachLine: BeachLineItem) (event: SweepEvent) =
    match event with
    | Site v ->
        match beachLine with
        | Empty -> Arc { focus = v }
        | _ -> beachLine

let generateDiagram (points: Vector list) (width: float) (height: float) =
    let completeEdges: Edge list = []
    let events =
        points
        |> List.sortBy (fun (_, y) -> y)
        |> List.map (Site)
    let beachLine = Empty
    let sweepLine = 0.

    let firstSiteEvent = List.head events
    let events = List.tail events
    let beachLine = handleEvent beachLine firstSiteEvent
    completeEdges