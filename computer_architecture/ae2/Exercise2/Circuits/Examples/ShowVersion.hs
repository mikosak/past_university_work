-- Print Hydra version

module Examples.ShowVersion where
import HDL.Hydra

main :: IO ()
main = putStrLn ("Hydra version: " ++ HDL.Hydra.hydraVersion)
