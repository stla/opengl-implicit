module MarchingCubes.MarchingCubes
  (marchingCubes)
  where
import           Control.Monad.Extra   (concatMapM)
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Storable      (poke, sizeOf)
import           MarchingCubes.CTypes

polygonise :: Double    -- isolevel
           -> GridCell  -- grid cell
           -> IO [Triangle]
polygonise iso gridcell = do
    let cgridcell = gridCellToCGridCell gridcell
    cgridcellPtr <- mallocBytes (sizeOf (undefined :: CGridCell))
    poke cgridcellPtr cgridcell
    ctrianglesPtr <- mallocBytes (5 * sizeOf (undefined :: CTriangle))
    ntri <- c_Polygonise cgridcellPtr (realToFrac iso) ctrianglesPtr
    ctriangles <- peekArray (fromIntegral ntri) ctrianglesPtr
    let triangles = map cTriangleToTriangle ctriangles
    free ctrianglesPtr
    free cgridcellPtr
    return triangles

toGridCell :: [XYZ] -> [Double] -> GridCell
toGridCell xyzs vals = GridCell { _p = xyzs, _val = vals }

cube :: (Int,Int,Int) -> [XYZ]
cube (i,j,k) = [(dbl a, dbl b, dbl c) | a <- [i,i+1], b <- [j,j+1], c <- [k,k+1]]
  where
    dbl :: Int -> Double
    dbl = realToFrac

baseGrid :: Int -> [[XYZ]]
baseGrid n = map cube [(i, j, k) | i <- [0 .. n], j <- [0 .. n], k <- [0 .. n]]

scaleCube :: Int -> (Double,Double) -> [XYZ] -> [XYZ]
scaleCube n (a,b) = map scale
    where
    scale (x, y, z) = (s x, s y, s z)
      where
      s u = a + (b-a)*u / realToFrac (n+1)

voxelGrid :: Int -> (Double,Double) -> [[XYZ]]
voxelGrid n ab = map (scaleCube n ab) (baseGrid n)

-- ~~ MAIN FUNCTION ~~ --
marchingCubes :: (XYZ -> Double)   -- function
              -> Double            -- isolevel
              -> (Double, Double)  -- bounds (common to x,y,z)
              -> Int               -- grid subdivisions
              -> IO [Triangle]
marchingCubes f level ab n = concatMapM (polygonise level) gridcells
  where
  gridcells = map (\vcube -> toGridCell vcube (map f vcube)) (voxelGrid n ab)
