main :: IO ()
main = print "Hello World"

newtype Celsius    = Celsius Double     deriving (Show)
newtype Fahrenheit = Fahrenheit Double  deriving (Show)
newtype Kelvin     = Kelvin Double      deriving (Show)

class TempUnit a where
    toKelvin       :: a -> Kelvin
    toCelsius      :: a -> Celsius
    toFahrenheit   :: a -> Fahrenheit

instance TempUnit Kelvin where
    toKelvin                = id
    toCelsius    (Kelvin t) = Celsius $ t - 273.15
    toFahrenheit (Kelvin t) = Fahrenheit $ (t - 273.15) * 1.8  + 32

instance TempUnit Celsius where
    toCelsius                = id
    toKelvin     (Celsius t) = Kelvin $ t + 273.15
    toFahrenheit (Celsius t) = Fahrenheit $ t * 1.8  + 32
--
instance TempUnit Fahrenheit where
    toFahrenheit             = id
    toKelvin  (Fahrenheit t) = Kelvin $ (t - 32) / 1.8 + 273.15
    toCelsius (Fahrenheit t) = Celsius $ (t - 32) / 1.8
