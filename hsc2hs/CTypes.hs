{-# LINE 1 "CTypes.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module CTypes
  (  XYZ (..)
   , Triangle
   , GridCell (..)
   , CGridCell
   , c_Polygonise
   , c_PolygoniseTri
   , gridCellToCGridCell
   , cTriangleToTriangle )
  where
import           Foreign
import           Foreign.C.Types

data CXYZ = CXYZ {
    __x :: CDouble
  , __y :: CDouble
  , __z :: CDouble
} deriving Show

instance Storable CXYZ where
    sizeOf    __ = (24)
{-# LINE 25 "CTypes.hsc" #-}
    alignment __ = 8
{-# LINE 26 "CTypes.hsc" #-}
    peek ptr = do
      x' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 28 "CTypes.hsc" #-}
      y' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 29 "CTypes.hsc" #-}
      z' <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 30 "CTypes.hsc" #-}
      return CXYZ { __x = x'
                  , __y = y'
                  , __z = z' }
    poke ptr (CXYZ r1 r2 r3)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 36 "CTypes.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 37 "CTypes.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 38 "CTypes.hsc" #-}

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
  sizeOf    __ = (72)
{-# LINE 61 "CTypes.hsc" #-}
  alignment __ = 8
{-# LINE 62 "CTypes.hsc" #-}
  peek ptr = do
    p' <- peekArray 3 $ (\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr
{-# LINE 64 "CTypes.hsc" #-}
    return CTriangle { __p = p' }
  poke ptr (CTriangle r1) = do
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr) r1
{-# LINE 67 "CTypes.hsc" #-}

-- foreign import ccall unsafe "testTriangle" c_testTriangle
--  :: CDouble -> CDouble -> CDouble
--  -> IO (Ptr CTriangle)


data CGridCell = CGridCell {
    ___p  :: [CXYZ]
  , __val :: [CDouble]
} deriving Show

instance Storable CGridCell where
  sizeOf    __ = (256)
{-# LINE 80 "CTypes.hsc" #-}
  alignment __ = 8
{-# LINE 81 "CTypes.hsc" #-}
  peek ptr = do
    p'   <- peekArray 8 $ (\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr
{-# LINE 83 "CTypes.hsc" #-}
    val' <- peekArray 8 $ (\hsc_ptr -> hsc_ptr `plusPtr` 192) ptr
{-# LINE 84 "CTypes.hsc" #-}
    return CGridCell { ___p  = p'
                     , __val = val' }
  poke ptr (CGridCell r1 r2) = do
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr) r1
{-# LINE 88 "CTypes.hsc" #-}
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 192) ptr) r2
{-# LINE 89 "CTypes.hsc" #-}

foreign import ccall unsafe "PolygoniseTri" c_PolygoniseTri
  :: Ptr CGridCell -> CDouble -> Ptr CTriangle
  -> CInt -> CInt -> CInt -> CInt
  -> IO CInt

foreign import ccall unsafe "Polygonise" c_Polygonise
  :: Ptr CGridCell -> CDouble -> Ptr CTriangle
  -> IO CInt

data GridCell = GridCell {
    _p   :: [XYZ]
  , _val :: [Double]
} deriving Show

cGridCellToGridCell :: CGridCell -> GridCell
cGridCellToGridCell (CGridCell cxyzs cvals) =
  GridCell { _p = map cXYZtoXYZ cxyzs, _val = map realToFrac cvals}

gridCellToCGridCell :: GridCell -> CGridCell
gridCellToCGridCell (GridCell xyzs vals) =
  CGridCell {  ___p  = map hXYZtoCXYZ xyzs
             , __val = map realToFrac vals}
  where
    hXYZtoCXYZ (x,y,z) = CXYZ {  __x = realToFrac x
                               , __y = realToFrac y
                               , __z = realToFrac z }
