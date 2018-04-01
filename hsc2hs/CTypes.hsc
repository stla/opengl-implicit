{-# LANGUAGE ForeignFunctionInterface #-}
module MarchingCubes.CTypes 
  (  XYZ 
   , Triangle
   , GridCell (..)
   , CGridCell
   , c_Polygonise
   , gridCellToCGridCell
   , cTriangleToTriangle )
  where
import           Foreign
import           Foreign.C.Types

#include "marchingcubes.h"

data CXYZ = CXYZ {
    __x :: CDouble
  , __y :: CDouble
  , __z :: CDouble
} deriving Show

instance Storable CXYZ where
    sizeOf    __ = #{size XYZ_T}
    alignment __ = #{alignment XYZ_T}
    peek ptr = do
      x' <- #{peek XYZ_T, x} ptr
      y' <- #{peek XYZ_T, y} ptr
      z' <- #{peek XYZ_T, z} ptr
      return CXYZ { __x = x'
                  , __y = y'
                  , __z = z' }
    poke ptr (CXYZ r1 r2 r3)
      = do
          #{poke XYZ_T, x} ptr r1
          #{poke XYZ_T, y} ptr r2
          #{poke XYZ_T, z} ptr r3

type XYZ = (Double, Double, Double)

cXYZtoXYZ :: CXYZ -> XYZ
cXYZtoXYZ (CXYZ x y z) = (realToFrac x, realToFrac y, realToFrac z)


data CTriangle = CTriangle {
  __p :: [CXYZ]
} deriving Show

type Triangle = (XYZ, XYZ, XYZ)

cTriangleToTriangle :: CTriangle -> Triangle
cTriangleToTriangle (CTriangle cxyzs) = (xyz0, xyz1, xyz2)
  where 
  xyzs = map cXYZtoXYZ cxyzs
  xyz0 = xyzs !! 0 
  xyz1 = xyzs !! 1
  xyz2 = xyzs !! 2 

instance Storable CTriangle where
  sizeOf    __ = #{size Triangle_T}
  alignment __ = #{alignment Triangle_T}
  peek ptr = do
    p' <- peekArray 3 $ #{ptr Triangle_T, p} ptr
    return CTriangle { __p = p' }
  poke ptr (CTriangle r1) = do
    pokeArray (#{ptr Triangle_T, p} ptr) r1

-- foreign import ccall unsafe "testTriangle" c_testTriangle
--  :: CDouble -> CDouble -> CDouble 
--  -> IO (Ptr CTriangle)


data CGridCell = CGridCell {
    ___p  :: [CXYZ]
  , __val :: [CDouble]
} deriving Show

instance Storable CGridCell where
  sizeOf    __ = #{size GridCell_T}
  alignment __ = #{alignment GridCell_T}
  peek ptr = do
    p'   <- peekArray 8 $ #{ptr GridCell_T, p} ptr
    val' <- peekArray 8 $ #{ptr GridCell_T, val} ptr
    return CGridCell { ___p  = p'
                     , __val = val' }
  poke ptr (CGridCell r1 r2) = do
    pokeArray (#{ptr GridCell_T, p} ptr) r1
    pokeArray (#{ptr GridCell_T, val} ptr) r2

foreign import ccall unsafe "Polygonise" c_Polygonise
  :: Ptr CGridCell -> CDouble -> Ptr CTriangle
  -> IO CInt

data GridCell = GridCell {
    _p   :: [XYZ]
  , _val :: [Double]
} deriving Show

gridCellToCGridCell :: GridCell -> CGridCell
gridCellToCGridCell (GridCell xyzs vals) = 
  CGridCell {  ___p  = map hXYZtoCXYZ xyzs
             , __val = map realToFrac vals}
  where 
    hXYZtoCXYZ (x,y,z) = CXYZ {  __x = realToFrac x
                               , __y = realToFrac y
                               , __z = realToFrac z }