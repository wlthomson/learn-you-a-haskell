import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

main = do
  print (Sphere.area 2)
  print (Sphere.volume 2)

  print (Cuboid.area 2 2 2)
  print (Cuboid.volume 2 2 2)

  print (Cube.area 2)
  print (Cube.volume 2)
