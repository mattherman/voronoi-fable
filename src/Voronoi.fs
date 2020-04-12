module Voronoi

// This defines a parabola that marks all points on the plane where the distance to
// the focus is equal to the distance to the nearest point on the directix.
// For directix y = yd and focus (xf, yf), the parabola can be defined as
// y = (1 / 2(yf - yd))(x - xf)^2 + (yf + yd) / 2
// See https://jacquesheunis.com/post/fortunes-algorithm/
let parabola (focusX: float, focusY: float) (directix: float) =
    fun x ->
        (1. / (2. * (focusY - directix))) * ((x - focusX) ** 2.) + ((focusY + directix) / 2.)