module Peano where

data Peano = Zero | Succ Peano
     deriving ( Read)

peanoToInt :: Peano -> Int
peanoToInt Zero = 0
peanoToInt (Succ b) = 1 + peanoToInt b


instance Show Peano where
	 show p = show (peanoToInt p)


instance Eq Peano where
	 Zero == Zero = True
	 Zero ==  _ = False
	 _ == Zero = False
	 (Succ a) == (Succ b) = (a == b)

instance Num Peano where
	 a + Zero = a
	 a + ( Succ b) = Succ ( a + b)
	 a - Zero = a
	 a - b = Succ ( a - (Succ b))
	 Zero * a = Zero
	 a * Zero = Zero
	 a * Succ b = a + (a * b)
	 signum Zero = 0
	 signum (Succ b) = 1
	 abs a = a
	 fromInteger 0 = Zero
	 fromInteger i = Succ (fromInteger (i-1))

instance Ord Peano where
	 Zero <= _ = True
	 _ <= Zero = False
	 (Succ a) <= (Succ b) = a <= b

instance Enum Peano where
	 toEnum 0 = Zero
	 toEnum i = Succ (toEnum (i-1))
	 fromEnum Zero = 0
	 fromEnum (Succ p) = 1 + (fromEnum p )

instance Integral Peano where
	 quotRem Zero _ = (Zero, Zero)
	 quotRem (Succ a) b = (Zero, Zero)
	 toInteger Zero = 0
	 toInteger (Succ p) = 1 + ( toInteger p )

instance Real Peano where
	 toRational Zero = 0
	 toRational (Succ p) = toRational p + 1