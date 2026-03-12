module MyEvolModule where
import GHC.Boot.TH.Syntax (Overlap(Incoherent))

data MyEvolution = LUCA
                 | Cyanobacteria       
                 | Trilobite
                 | Ichthyostega
                 | Dimetrodon
                 | Archaeopteryx
                 | Morganucodon
                 | Purgatorius
                 | Australopithecine
                 | Humans

instance Show MyEvolution where
    show LUCA = "Last Universal Common Ancestor"
    show Cyanobacteria = "Synechococcus"
    show Trilobite = "Paradoxides"
    show Ichthyostega = "Ichthyostega"
    show Dimetrodon = "Dimetrodon"
    show Archaeopteryx = "Archaeopteryx"
    show Morganucodon = "Morganucodon"
    show Purgatorius = "Purgatorius"
    show Australopithecine = "Australopithecus Afarensis"
    show Humans = "Homo Sapiens"

instance Read MyEvolution where
    readsPrec _ str = case str of
        'L':'a':'s':'t':' ':'U':'n':'i':'v':'e':'r':'s':'a':'l':' ':'C':'o':'m':'m':'o':'n':' ':'A':'n':'c':'e':'s':'t':'o':'r':rest
            -> [(LUCA, rest)]
        'S':'y':'n':'e':'c':'h':'o':'c':'o':'c':'c':'u':'s':rest
            -> [(Cyanobacteria, rest)]
        'P':'a':'r':'a':'d':'o':'x':'i':'d':'e':'s':rest
            -> [(Trilobite, rest)]
        'I':'c':'h':'t':'h':'y':'o':'s':'t':'e':'g':'a':rest
            -> [(Ichthyostega, rest)]
        'D':'i':'m':'e':'t':'r':'o':'d':'o':'n':rest
            -> [(Dimetrodon, rest)]
        'A':'r':'c':'h':'a':'e':'o':'p':'t':'e':'r':'y':'x':rest
            -> [(Archaeopteryx, rest)]
        'M':'o':'r':'g':'a':'n':'u':'c':'o':'d':'o':'n':rest
            -> [(Morganucodon, rest)]
        'P':'u':'r':'g':'a':'t':'o':'r':'i':'u':'s':rest
            -> [(Purgatorius, rest)]
        'A':'u':'s':'t':'r':'a':'l':'o':'p':'i':'t':'h':'e':'c':'u':'s':' ':'A':'f':'a':'r':'e':'n':'s':'i':'s':rest
            -> [(Australopithecine, rest)]
        'H':'o':'m':'o':' ':'S':'a':'p':'i':'e':'n':'s':rest
            -> [(Humans, rest)]
 
instance Eq MyEvolution where
    (==) LUCA LUCA = True
    (==) Cyanobacteria Cyanobacteria = True
    (==) Trilobite Trilobite = True
    (==) Ichthyostega Ichthyostega = True
    (==) Dimetrodon Dimetrodon = True
    (==) Archaeopteryx Archaeopteryx = True
    (==) Morganucodon Morganucodon = True
    (==) Purgatorius Purgatorius = True
    (==) Australopithecine Australopithecine = True
    (==) Humans Humans = True
    (==) _ _ = False
    (/=) x y = not (x == y)


