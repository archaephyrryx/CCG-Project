#!/bin/bash
for i in {a..z}
do
    printf "size(1inch,1inch);\ndefaultpen(fontsize(12pt));\nlabel(\"$i\", (0,0), p=currentpen);\n" >"$i.asy";
    asy "$i.asy";
    pstopnm "$i.eps";
    pnmtopng "$i.eps001.ppm" >"$i.png";
done

