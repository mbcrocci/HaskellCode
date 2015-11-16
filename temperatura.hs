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
    toCelsius    (Kelvin ß ) = Celsius $ ß - 273.15
    toFahrenheit (Kelvin ß) = Fahrenheit $ (ß - 273.15) * 1.8  + 32

instance TempUnit Celsius where
    toCelsius                = id
    toKelvin     (Celsius ß) = Kelvin $ ß + 273.15
    toFahrenheit (Celsius ß) = Fahrenheit $ ß * 1.8  + 32

instance TempUnit Fahrenheit where
    toFahrenheit             = id
    toKelvin  (Fahrenheit ß) = Kelvin $ (ß - 32) / 1.8 + 273.15
    toCelsius (Fahrenheit ß) = Celsius $ (ß - 32) / 1.8
