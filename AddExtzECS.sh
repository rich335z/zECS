#/bin/sh
cd /apps/DEV/INFDS/zECS
cd /apps/DEV/INFDS/zECS/txt
for f in *; do mv "$f" "$f.txt"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/asm
for f in *; do mv "$f" "$f.asm"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/cbl
for f in *; do mv "$f" "$f.cbl"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/cpy
for f in *; do mv "$f" "$f.cpy"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/exec
for f in *; do mv "$f" "$f.exec"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/idcams
for f in *; do mv "$f" "$f.idcams"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/jcl
for f in *; do mv "$f" "$f.jcl"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/mac
for f in *; do mv "$f" "$f.mac"; done 2> /dev/null
cd /apps/DEV/INFDS/zECS/rdo
for f in *; do mv "$f" "$f.rdo"; done 2> /dev/null
cd ..
