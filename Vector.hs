module Vector where

-- |Simple 2D vector datatype. Provides vector and scalar values to allow for
--  eacy use of operators.
data Vec2F = V Float Float | S Float
  deriving (Show, Read, Eq, Ord)

-- |Make Vec2F a number type to be able to use the operators on vectors and
--  scalars.
instance Num Vec2F where
    -- |Addition operator. Adding two vectors adds the corraponding components.
    --  Adding a scalar and a vector adds the scalar to both vector components.
    --  Adding two scalars just adds the values together
    (+) (S s1) (S s2) = S (s1+s2)
    (+) (V x1 y1) (S s) = V (x1+s) (y1+s)
    (+) (S s) (V x1 y1) = V (x1+s) (y1+s)
    (+) (V x1 y1) (V x2 y2) = V (x1+x2) (y1+y2)

    -- |Extends negation to vectors.
    negate (V x y) = V (-x) (-y)
    negate (S s) = S (-s)

    -- |Multiplication operator. Two scalars are normal. Scalar and vector
    --  scales the vector by the scalar amount. Two vectors is dot product.
    (*) (S s1) (S s2) = S (s1*s2)
    (*) (V x1 y1) (S s) = V (x1*s) (y1*s)
    (*) (S s) (V x1 y1) = V (x1*s) (y1*s)
    (*) (V x1 y1) (V x2 y2) = S (x1*x2+y1*y2)

    -- |Magnitude of scalar and vector.
    abs (S s) = S (abs s)
    abs v = S (sqrt m) where S m = v*v

    -- |The sign of a scalar. A unit vector of a given vector
    signum (S s) = S (signum s)
    signum v@(V x y) = V (x/m) (y/m) where m = (\(S n) -> sqrt n) $ v*v

    -- |Converts a numeric literal to a Vec3F (scalar) type
    fromInteger n = S (fromIntegral n)

-- |Make Vec2F a Fractional type for use of division operator and fromRational.
instance Fractional Vec2F where
    -- |Division operator. Simple element-wise division.
    (/) (V x y) (S s) = V (x/s) (y/s)
    (/) (S s1) (S s2) = S (s1/s2)
    (/) s@(S _) v@(V _ _) =
        error $ "Cannot divide " ++ show s ++ " by " ++ show v
    (/) v1@(V _ _) v2@(V _ _) =
        error $ "Cannot divide " ++ show v1 ++ " by " ++ show v2

    -- |Reciprocal of a vector or scalar.
    recip = vmap (1/)

    -- |Converts a numeric floating literal to a Vec2F (scalar) type
    fromRational r = S (fromRational r)

-- |Make Vec2F a Floating type. All common floating operations simply apply
--  element-wise over a vector, and normally over a scalar.
instance Floating Vec2F where
    pi = S pi
    exp = vmap exp
    log = vmap log
    sin = vmap sin
    cos = vmap cos
    sinh = vmap sinh
    cosh = vmap cosh
    asin = vmap asin
    acos = vmap acos
    atan = vmap atan
    asinh = vmap asinh
    acosh = vmap acosh
    atanh = vmap atanh

-- |Cross product of two Vec2F vectors. Since the result will have no component
--  in the x or y direction, a scalar is returned as the z component of the
--  cross product. This assumes V a b -> (a,b,0) in a 3D space.
cross :: Vec2F -> Vec2F -> Vec2F
cross (V x1 y1) (V x2 y2) = S $ x1*y2-x2*y1
cross (S _) (V _ _) = error "No cross product on vector and scalar"
cross (V _ _) (S _) = error "No cross product on vector and scalar"
cross (S _) (S _) = error "No cross product on scalar and scalar"

-- |Rotate a vector by the radian angle passed in. Has no effect on scalars
rotateVec :: Float -> Vec2F -> Vec2F
rotateVec theta (V x y) =
    V (x*cos theta - y*sin theta) (x * sin theta + y * cos theta)
rotateVec _ a = a

-- |Alias for getting a vector's unite vector.
norm :: Vec2F -> Vec2F
norm = signum

-- |Converts a vector to a point
toPoint :: Vec2F -> (Float, Float)
toPoint (V x y) = (x,y)
toPoint (S s) = (s,s)

-- |Converts a point to a vector
toVec :: (Float, Float) -> Vec2F
toVec (x,y) = V x y

-- |Maps a function over the elements of a vector
vmap :: (Float -> Float) -> Vec2F -> Vec2F
vmap f (S s) = S (f s)
vmap f (V x y) = V (f x) (f y)

-- |Sets the magnitude of a vector
setMag :: Vec2F -> Vec2F -> Vec2F
setMag m v = m * (norm v)
