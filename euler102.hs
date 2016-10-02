--------------------------------------------------------------------------------
-- Project Euler
-- Problem 102
-- Triangle containment
--------------------------------------------------------------------------------
-- Three distinct points are plotted at random on a Cartesian plane, for
-- which -1000 <= x, y <= 1000, such that a triangle is formed.
-- 
-- Consider the following two triangles:
-- 
-- A(-340,495), B(-153,-910), C(835,-947)
-- 
-- X(-175,41), Y(-421,-714), Z(574,-645)
-- 
-- It can be verified that triangle ABC contains the origin, whereas triangle
-- XYZ does not.
-- 
-- Using euler102_triangles.txt, a 27K text file containing the co-ordinates of
-- one thousand "random" triangles, find the number of triangles for which the
-- interior contains the origin.
-- 
-- NOTE: The first two examples in the file represent the triangles in the
-- example given above.
--------------------------------------------------------------------------------

type Point = (Double, Double)
type Triangle = (Point, Point, Point)

-- express the given point in barycentric coordinates of the given triangle.
barycentric :: Point -> Triangle -> (Double, Double, Double)
barycentric (x,y) ((x1,y1),(x2,y2),(x3,y3)) = (l1, l2, l3)
  where l1 = ((y2-y3)*(x-x3) + (x3-x2)*(y-y3)) / det
        l2 = ((y3-y1)*(x-x3) + (x1-x3)*(y-y3)) / det
        l3 = 1 - l1 - l2
        det = (y2-y3)*(x1-x3) + (x3-x2)*(y1-y3)

-- a triangle contains a point if the barycentric coordinates of that point
-- are all in the range [0, 1].
contains_origin :: Triangle -> Bool
contains_origin t = all (\x -> 0<=x && x<=1) [l1,l2,l3]
    where (l1,l2,l3) = barycentric (0,0) t
        
-------- IO --------

getTriangles :: IO [Triangle]
getTriangles = do
    f <- readFile "euler102_triangles.txt"
    let ls = map (read.(\s->"["++s++"]")) (lines f)
    return (map (\[a,b,c,d,e,f]->((a,b),(c,d),(e,f))) ls)

main :: IO ()
main = print . length . filter contains_origin =<< getTriangles
