;; possible commands:

startshape x
startshape x [ ... ]
 
shape x {
    x2
    x2 [ ... ]
    x3
    CIRCLE []
    CIRCLE
    CIRCLE [ ... ]
}

phi = 4

shape x2 {
    asdf
}

phi = 1 + sqrt(2)

shape LeftOrRightFlower 
rule {
    LeftFlower []
}
rule 0.3 {
    LeftFlower [flip 90]
}
